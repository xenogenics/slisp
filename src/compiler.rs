use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque},
    io::Read,
    rc::Rc,
};

use bincode::{Decode, Encode};
use strum::IntoEnumIterator;

use crate::{
    atom::{Atom, Span},
    error::Error,
    grammar::ListsParser,
    ir::{
        Arguments, Backquote, Binding, Bindings, CallSite, ConstantDefinition, ExternalDefinition,
        FunctionDefinition, Operator, Quote, Statement, Statements, TopLevelStatement, Value,
    },
    opcodes::{Arity, Immediate, OpCode, OpCodes},
    vm::{RunParameters, VirtualMachine},
};

//
// Context.
//

#[derive(Clone, Debug, Default)]
pub(crate) struct Context {
    name: Box<str>,
    arity: Arity,
    lambda: bool,
    closure: BTreeSet<Box<str>>,
    locals: HashMap<Box<str>, Vec<usize>>,
    stackn: usize,
    stream: Stream,
}

impl Context {
    fn new(name: Box<str>, arity: Arity, lambda: bool) -> Self {
        Self {
            name,
            arity,
            lambda,
            closure: BTreeSet::new(),
            locals: HashMap::default(),
            stackn: 0,
            stream: Stream::default(),
        }
    }

    fn deconstruct_binding(&mut self, mappings: &[(Box<str>, Vec<Operator>)]) {
        //
        // Process the deconstruction operators.
        //
        for (_, operators) in mappings.iter().rev() {
            //
            // Duplicate the argument.
            //
            self.stream.push_back(OpCode::Dup(1).into());
            //
            // Convert the operators.
            //
            for op in operators {
                match op {
                    Operator::Car => self.stream.push_back(OpCode::Car.into()),
                    Operator::Cdr => self.stream.push_back(OpCode::Cdr.into()),
                    _ => unreachable!(),
                }
            }
            //
            // Swap the result and the original argument.
            //
            self.stream.push_back(OpCode::Swp.into());
        }
        //
        // Clear the original argument.
        //
        self.stream.push_back(OpCode::Pop(1).into());
    }

    fn deconstruct_arguments(&mut self, args: &Arguments) -> usize {
        self.deconstruct_arguments_with_closure(args, &BTreeSet::new())
    }

    fn deconstruct_arguments_with_closure(
        &mut self,
        args: &Arguments,
        closure: &BTreeSet<Box<str>>,
    ) -> usize {
        let mut argcnt = args.len() + closure.len();
        //
        // Skip if there is no deconstruction.
        //
        if !args.has_deconstructions() {
            return argcnt;
        }
        //
        // Shift closure and arguments.
        //
        if !closure.is_empty() {
            let opcode = OpCode::Rtm(argcnt, closure.len());
            self.stream.push_back(opcode.into());
        }
        //
        // Process the bindings.
        //
        for b in args.bindings() {
            //
            // Handle binding deconstruction.
            //
            if let Binding::Deconstructed(mappings) = b {
                self.deconstruct_binding(mappings);
            }
            //
            // Update the argument count.
            //
            argcnt = argcnt - 1 + b.len();
            //
            // Rotate the arguments.
            //
            self.stream.push_back(OpCode::Rtm(argcnt, b.len()).into());
        }
        //
        // Done.
        //
        argcnt
    }

    fn track_arguments(&mut self, args: &Arguments) {
        self.track_arguments_and_closure(args, &BTreeSet::new());
    }

    fn track_arguments_and_closure(&mut self, args: &Arguments, closure: &BTreeSet<Box<str>>) {
        //
        // Track the arguments.
        //
        args.names().rev().for_each(|v| {
            self.locals.entry(v.clone()).or_default().push(self.stackn);
            self.stackn += 1;
        });
        //
        // Track the closure.
        //
        closure.iter().for_each(|v| {
            self.locals.entry(v.clone()).or_default().push(self.stackn);
            self.stackn += 1;
        });
    }

    #[cfg(test)]
    pub(crate) fn stream(&self) -> &Stream {
        &self.stream
    }
}

//
// OpCode or reference.
//

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum LabelOrOpCode {
    Branch(Box<str>),
    BranchIfNot(Box<str>),
    Extcall(Box<str>),
    Funcall(Box<str>),
    OpCode(OpCode),
}

impl From<OpCode> for LabelOrOpCode {
    fn from(value: OpCode) -> Self {
        Self::OpCode(value)
    }
}

//
// Stream.
//

type Stream = VecDeque<LabelOrOpCode>;

//
// Symbols and OpCodes.
//

#[derive(Default, Encode, Decode)]
pub struct Artifacts {
    extfuns: Vec<(Box<str>, ExternalDefinition)>,
    intfuns: Vec<(Box<str>, usize, Arity)>,
    symbols: BTreeMap<Box<str>, u32>,
    opcodes: OpCodes,
}

impl Artifacts {
    pub fn new(
        extfuns: Vec<(Box<str>, ExternalDefinition)>,
        intfuns: Vec<(Box<str>, usize, Arity)>,
        symbols: BTreeMap<Box<str>, u32>,
        opcodes: OpCodes,
    ) -> Self {
        Self {
            extfuns,
            intfuns,
            symbols,
            opcodes,
        }
    }

    pub fn external_functions(&self) -> &[(Box<str>, ExternalDefinition)] {
        &self.extfuns
    }

    pub fn internal_functions(&self) -> &[(Box<str>, usize, Arity)] {
        &self.intfuns
    }

    pub fn symbols(&self) -> &BTreeMap<Box<str>, u32> {
        &self.symbols
    }

    pub fn opcodes(&self) -> &[OpCode] {
        &self.opcodes
    }
}

//
// Compiler trait.
//

pub trait CompilerTrait: Clone {
    fn eval(self, atom: Rc<Atom>, params: RunParameters) -> Result<Rc<Atom>, Error>;
    fn expand(self, atom: Rc<Atom>, params: RunParameters) -> Result<Rc<Atom>, Error>;
    fn load(&mut self, atom: Rc<Atom>) -> Result<(), Error>;
    fn compile(self, entrypoint: &str) -> Result<Artifacts, Error>;
}

//
// Compiler.
//

#[derive(Clone, Default)]
pub struct Compiler {
    consts: BTreeMap<Box<str>, Quote>,
    blocks: BTreeMap<Box<str>, Context>,
    defuns: BTreeMap<Box<str>, Arity>,
    macros: BTreeSet<Box<str>>,
    exfuns: BTreeMap<Box<str>, ExternalDefinition>,
    labels: BTreeMap<Box<str>, usize>,
    symbls: BTreeMap<Box<str>, u32>,
    lcount: usize,
    ecount: usize,
}

impl Compiler {
    fn label(&mut self, prefix: &str) -> Box<str> {
        let label = format!("{prefix}_{:04}", self.lcount).into_boxed_str();
        self.lcount += 1;
        label
    }
}

//
// Compiler trait.
//

impl CompilerTrait for Compiler {
    fn eval(mut self, expr: Rc<Atom>, params: RunParameters) -> Result<Rc<Atom>, Error> {
        //
        // Define the symbol.
        //
        let symbol = format!("__eval__{:04}", self.ecount);
        self.ecount += 1;
        //
        // Wrap the statement into a top-level function.
        //
        let entrypoint = Atom::cons(
            Atom::symbol("def"),
            Atom::cons(
                Atom::symbol(&symbol),
                Atom::cons(
                    Atom::nil(),
                    Atom::cons(Atom::nil(), Atom::cons(expr, Atom::nil())),
                ),
            ),
        );
        //
        // Convert the entrypoint into a statement.
        //
        let topl = TopLevelStatement::from_atom(entrypoint, &mut self.macros)?;
        //
        // Load the top-level statement.
        //
        self.load_statement(topl)?;
        //
        // Generate the bytecode.
        //
        let artifacts = self.compile(&symbol)?;
        let mut symbols = artifacts.symbols().clone();
        //
        // Build the virtual machine.
        //
        let mut vm = VirtualMachine::new(&symbol, params);
        //
        // Run.
        //
        let result = vm.run(&artifacts, &mut symbols)?;
        //
        // Convert the result into an atom.
        //
        Atom::from_value(result, &symbols)
    }

    fn expand(mut self, expr: Rc<Atom>, params: RunParameters) -> Result<Rc<Atom>, Error> {
        //
        // Split the atom.
        //
        let Atom::Pair(_, symbol, args) = expr.as_ref() else {
            return Err(Error::ExpectedPair(expr.span()));
        };
        //
        // Make sure the symbol is a symbol.
        //
        let Atom::Symbol(_, name) = symbol.as_ref() else {
            return Err(Error::ExpectedSymbol(symbol.span()));
        };
        //
        // Make sure the symbol is a macro.
        //
        if !self.macros.contains(name) {
            return Err(Error::ExpectedMacro(symbol.span()));
        }
        //
        // Unmark the macro symbol.
        //
        self.macros.remove(name);
        //
        // Quote the arguments.
        //
        let args: Vec<_> = args.iter().collect();
        let args = args.into_iter().rev().fold(Atom::nil(), |acc, v| {
            Atom::cons(Atom::cons(Atom::symbol("quote"), v), acc)
        });
        //
        // Evaluate the macro.
        //
        let expr = Atom::cons(Atom::symbol(name), args);
        self.eval(expr, params)
    }

    fn load(&mut self, atom: Rc<Atom>) -> Result<(), Error> {
        let stmt = TopLevelStatement::from_atom(atom, &mut self.macros)?;
        self.load_statement(stmt)
    }

    fn compile(self, entrypoint: &str) -> Result<Artifacts, Error> {
        //
        // Collect the live defuns.
        //
        let liveset = self.collect_liveset(entrypoint)?;
        //
        // Serialize the live external functions.
        //
        let exfuns: Vec<_> = self
            .exfuns
            .into_iter()
            .filter(|(k, _)| {
                liveset
                    .as_ref()
                    .map(|v| v.contains(k.as_ref()))
                    .unwrap_or(true)
            })
            .collect();
        //
        // Serialize the live streams.
        //
        let (defuns, stream) = self
            .blocks
            .into_iter()
            .filter(|(k, _)| {
                liveset
                    .as_ref()
                    .map(|v| v.contains(k.as_ref()))
                    .unwrap_or(true)
            })
            .fold(
                (Vec::new(), Vec::new()),
                |(mut offsets, mut opcodes), (name, ctxt)| {
                    offsets.push((name, opcodes.len(), ctxt.arity));
                    opcodes.extend(ctxt.stream);
                    (offsets, opcodes)
                },
            );
        //
        // Convert the stream to opcodes.
        //
        let opcodes = stream
            .into_iter()
            .map(|v| match v {
                LabelOrOpCode::Branch(v) => {
                    let delta = self.labels.get(&v).copied().ok_or(Error::InvalidLabel(v))?;
                    Ok(OpCode::Br(delta as isize))
                }
                LabelOrOpCode::BranchIfNot(v) => {
                    let delta = self.labels.get(&v).copied().ok_or(Error::InvalidLabel(v))?;
                    Ok(OpCode::Brn(delta as isize))
                }
                LabelOrOpCode::Extcall(sym) => {
                    //
                    // Get the address of the symbol.
                    //
                    let index = exfuns
                        .iter()
                        .enumerate()
                        .find_map(|(i, (k, _))| (k == &sym).then_some(i))
                        .ok_or(Error::InvalidSymbol(sym))?;
                    //
                    // Push the funcall.
                    //
                    Ok(OpCode::Psh(Immediate::extcall(index)))
                }
                LabelOrOpCode::Funcall(sym) => {
                    //
                    // Get the address of the symbol.
                    //
                    let (addr, argcnt) = defuns
                        .iter()
                        .find_map(|(k, a, n)| (k == &sym).then_some((*a, *n)))
                        .ok_or(Error::InvalidSymbol(sym))?;
                    //
                    // Push the funcall.
                    //
                    Ok(OpCode::Psh(Immediate::funcall(addr, argcnt)))
                }
                LabelOrOpCode::OpCode(v) => Ok(v),
            })
            .collect::<Result<_, Error>>()?;
        //
        // Done.
        //
        Ok(Artifacts::new(exfuns, defuns, self.symbls, opcodes))
    }
}

//
// Live defuns.
//

impl Compiler {
    fn collect_liveset_for_context(
        &self,
        name: &str,
        ctxt: &Context,
        index: &mut HashSet<String>,
    ) -> Result<(), Error> {
        //
        // Track the funcall.
        //
        index.insert(name.to_owned());
        //
        // Collect the funcalls.
        //
        let funcalls: HashSet<_> = ctxt
            .stream
            .iter()
            .filter_map(|v| match v {
                LabelOrOpCode::Extcall(v) | LabelOrOpCode::Funcall(v) => Some(v.to_owned()),
                _ => None,
            })
            .filter(|v| !index.contains(v.as_ref()))
            .collect();
        //
        // Process the funcalls.
        //
        funcalls.into_iter().try_for_each(|v| {
            //
            // Bail-out if the funcall is an external call.
            //
            if self.exfuns.contains_key(&v) {
                index.insert(v.into_string());
                return Ok(());
            }
            //
            // Grab the block.
            //
            let Some(ctxt) = self.blocks.get(&v) else {
                return Err(Error::UnresolvedSymbol(v));
            };
            //
            // Process the block.
            //
            self.collect_liveset_for_context(v.as_ref(), ctxt, index)
        })
    }

    fn collect_liveset(&self, entrypoint: &str) -> Result<Option<HashSet<String>>, Error> {
        let mut result = HashSet::new();
        //
        // Grab the main block.
        //
        let Some((name, ctxt)) = self.blocks.iter().find(|(k, _)| k.as_ref() == entrypoint) else {
            return Ok(None);
        };
        //
        // Collect the funcalls.
        //
        self.collect_liveset_for_context(name.as_ref(), ctxt, &mut result)?;
        //
        // Done.
        //
        Ok(Some(result))
    }
}

//
// Statement loading.
//

impl Compiler {
    fn load_statement(&mut self, stmt: TopLevelStatement) -> Result<(), Error> {
        match stmt {
            TopLevelStatement::Constant(_, v) => self.compile_constant(v),
            TopLevelStatement::External(_, v) => self.compile_external(v),
            TopLevelStatement::Function(_, v) => self.compile_function(v),
            TopLevelStatement::Macro(_, v) => {
                let name = v.name().clone();
                let result = self.compile_function(v)?;
                self.macros.insert(name);
                Ok(result)
            }
            TopLevelStatement::Use(_, v) => self.load_modules(v),
        }
    }
}

//
// Module loading.
//

impl Compiler {
    fn load_modules(&mut self, stmts: Statements) -> Result<(), Error> {
        stmts.iter().try_for_each(|v| {
            //
            // Grab the quote.
            //
            let Statement::Quote(_, quote) = v else {
                return Err(Error::ExpectedQuote(Span::None));
            };
            //
            // Process the quote.
            //
            match quote {
                Quote::Pair(car, cdr) => {
                    //
                    // Get the name of the module.
                    //
                    let Quote::Value(Value::Symbol(name)) = car.as_ref() else {
                        return Err(Error::ExpectedSymbol(Span::None));
                    };
                    //
                    // Collect the names of the items.
                    //
                    let items: Vec<_> = cdr
                        .iter()
                        .map(|v| match v {
                            Quote::Value(Value::Symbol(v)) => Ok(v.as_ref()),
                            _ => Err(Error::ExpectedSymbol(Span::None)),
                        })
                        .collect::<Result<_, _>>()?;
                    self.load_module(name, Some(&items))
                }
                Quote::Value(Value::Symbol(v)) => self.load_module(v, None),
                _ => Err(Error::ExpectedPairOrSymbol(Span::None)),
            }
        })
    }

    fn load_module(&mut self, name: &str, items: Option<&[&str]>) -> Result<(), Error> {
        //
        // Compute the source path.
        //
        let root = std::env::var("SLISP_LIBRARY_PATH")?;
        let path = format!("{root}/{name}.sl");
        //
        // Open the source file.
        //
        let mut source = String::new();
        let mut file = std::fs::File::open(path)?;
        file.read_to_string(&mut source)?;
        //
        // Create a temporary compiler for the source file.
        //
        let mut compiler = Self::default();
        compiler.lift_operators()?;
        //
        // Parse the source file.
        //
        let parser = ListsParser::new();
        let atoms = parser
            .parse(&mut compiler, &source)
            .map_err(|v| Error::Parse(v.to_string()))?;
        //
        // Process the atoms internally.
        //
        for atom in atoms {
            //
            // Convert the atom.
            //
            let stmt = TopLevelStatement::from_atom(atom, &mut self.macros)?;
            //
            // Check the capture list.
            //
            if let Some(capture) = items
                && let Some(name) = stmt.name()
                && !capture.contains(&name)
            {
                continue;
            }
            //
            // Process the statement.
            //
            self.load_statement(stmt)?;
        }
        //
        // Done.
        //
        Ok(())
    }
}

//
// Compilation.
//

impl Compiler {
    fn compile_constant(&mut self, val: ConstantDefinition) -> Result<(), Error> {
        //
        // Track the constant.
        //
        self.consts.insert(val.name().clone(), val.value().clone());
        //
        // Done.
        //
        Ok(())
    }

    fn compile_external(&mut self, exfun: ExternalDefinition) -> Result<(), Error> {
        //
        // Add the external function to the defuns.
        //
        self.defuns.insert(exfun.name().clone(), exfun.arity());
        //
        // Track the external function.
        //
        self.exfuns.insert(exfun.name().clone(), exfun);
        //
        // Done.
        //
        Ok(())
    }

    fn compile_function(&mut self, defun: FunctionDefinition) -> Result<(), Error> {
        let arity = defun.arguments().arity();
        let mut argcnt = defun.arguments().len();
        let mut ctxt = Context::new(defun.name().clone(), arity, false);
        //
        // Track the function arguments.
        //
        ctxt.track_arguments(defun.arguments());
        //
        // Track the function definition.
        //
        self.defuns.insert(defun.name().clone(), arity);
        //
        // Process the arguments.
        //
        if argcnt > 0 {
            //
            // Rotate the arguments and the return address.
            //
            ctxt.stream.push_back(OpCode::Rot(argcnt + 1).into());
            //
            // Deconstruct the arguments.
            //
            argcnt = ctxt.deconstruct_arguments(defun.arguments());
        }
        //
        // Compile the statements.
        //
        self.compile_statements(&mut ctxt, defun.statements())?;
        //
        // Generate the postamble if necessary.
        //
        if !defun.statements().is_tail_call() {
            //
            // Pop the arguments.
            //
            if argcnt > 0 {
                ctxt.stream.push_back(OpCode::Rot(argcnt + 1).into());
                ctxt.stream.push_back(OpCode::Pop(argcnt).into());
            }
            //
            // Inject the return call.
            //
            ctxt.stream.push_back(OpCode::Ret.into());
            ctxt.stackn -= 1;
        }
        //
        // Save the block.
        //
        self.blocks.insert(defun.name().clone(), ctxt);
        //
        // Done.
        //
        Ok(())
    }

    fn compile_arguments(&mut self, ctxt: &mut Context, stmts: &Statements) -> Result<(), Error> {
        stmts
            .iter()
            .rev()
            .try_for_each(|v| self.compile_statement(ctxt, v))
    }

    fn compile_bindings(
        &mut self,
        ctxt: &mut Context,
        bindings: &Bindings,
    ) -> Result<usize, Error> {
        bindings.iter().try_fold(0, |acc, (binding, stmt)| {
            //
            // Compile the statement.
            //
            self.compile_statement(ctxt, stmt)?;
            //
            // Binding deconstruction.
            //
            match binding {
                Binding::Deconstructed(mappings) => {
                    //
                    // Deconstruct the binding.
                    //
                    ctxt.deconstruct_binding(mappings);
                    //
                    // Consume the original value.
                    //
                    ctxt.stackn -= 1;
                    //
                    // Process the bindings' indices.
                    //
                    binding.names().rev().for_each(|name| {
                        //
                        // Track the bindings' indices.
                        //
                        ctxt.locals
                            .entry(name.clone())
                            .or_default()
                            .push(ctxt.stackn);
                        //
                        // Increase the stack.
                        //
                        ctxt.stackn += 1;
                    });
                    //
                    // Done.
                    //
                    Ok(acc + binding.len())
                }
                Binding::Passthrough(name) => {
                    //
                    // Track the binding's index.
                    //
                    ctxt.locals
                        .entry(name.clone())
                        .or_default()
                        .push(ctxt.stackn - 1);
                    //
                    // Done.
                    //
                    Ok(acc + 1)
                }
            }
        })
    }

    fn clear_bindings(&mut self, ctxt: &mut Context, bindings: &Bindings) -> Result<(), Error> {
        bindings
            .iter()
            .flat_map(|(binding, _)| binding.names())
            .try_for_each(|name| {
                //
                // Clear the binding.
                //
                if let Some(v) = ctxt.locals.get_mut(name) {
                    //
                    // Remove the top reference.
                    //
                    v.pop();
                    //
                    // Clear the entry if there is no more references.
                    //
                    if v.is_empty() {
                        ctxt.locals.remove(name);
                    }
                }
                //
                // Done.
                //
                Ok(())
            })
    }

    fn compile_statements(&mut self, ctxt: &mut Context, stmts: &Statements) -> Result<(), Error> {
        stmts.iter().enumerate().try_for_each(|(indx, stmt)| {
            //
            // Drop the previous result.
            //
            if indx > 0 {
                ctxt.stream.push_back(OpCode::Pop(1).into());
                ctxt.stackn -= 1;
            }
            //
            // Compile the statement.
            //
            self.compile_statement(ctxt, stmt)
        })
    }

    pub(crate) fn compile_statement(
        &mut self,
        ctxt: &mut Context,
        stmt: &Statement,
    ) -> Result<(), Error> {
        match stmt {
            Statement::Apply(atom, op, args, CallSite::Any) => {
                //
                // Check the arity.
                //
                let arity = match op.as_ref() {
                    Statement::Lambda(_, args, _) => Some(args.arity()),
                    Statement::Operator(_, op) => Some(Arity::Some(op.argument_count() as u16)),
                    Statement::Symbol(_, sym) => self.defuns.get(sym).map(|v| v.clone()),
                    Statement::This(_) => Some(ctxt.arity),
                    _ => None,
                };
                //
                // In case of a self call, replicate the closure.
                //
                if matches!(op.as_ref(), Statement::This(_)) && ctxt.lambda {
                    //
                    // Grab the closure symbols.
                    //
                    ctxt.closure.iter().try_for_each(|v| {
                        //
                        // Get the symbol index.
                        //
                        // NOTE(xrg): in this instance it's the first occurence
                        // that matters.
                        //
                        let Some(index) = ctxt.locals.get(v).and_then(|v| v.first()) else {
                            return Err(Error::InvalidSymbol(v.clone()));
                        };
                        //
                        // Inject the push.
                        //
                        let opcode = OpCode::Get(ctxt.stackn - *index).into();
                        ctxt.stream.push_back(opcode);
                        ctxt.stackn += 1;
                        //
                        // Done.
                        //
                        Ok(())
                    })?;
                }
                //
                // Compile the arguments.
                //
                self.compile_arguments(ctxt, args)?;
                //
                // Compile the operator.
                //
                self.compile_statement(ctxt, op)?;
                //
                // Generate a pack or a call.
                //
                let reduce_len = match (op.as_ref(), arity) {
                    //
                    // Raw operator.
                    //
                    (Statement::Operator(..), _) => args.len(),
                    //
                    // Self operator within a lambda.
                    //
                    (Statement::This(_), Some(arity)) if ctxt.lambda => match arity {
                        //
                        // If the function capture everything, generate a call.
                        //
                        Arity::All => {
                            ctxt.stream.push_back(OpCode::Call(args.len()).into());
                            ctxt.closure.len() + args.len()
                        }
                        //
                        // Generate a call if the number of arguments is met.
                        //
                        Arity::Some(n) if args.len() == n as usize => {
                            ctxt.stream.push_back(OpCode::Call(args.len()).into());
                            ctxt.closure.len() + args.len()
                        }
                        Arity::SomeWithRem(n) if args.len() >= n as usize => {
                            ctxt.stream.push_back(OpCode::Call(args.len()).into());
                            ctxt.closure.len() + args.len()
                        }
                        Arity::None if args.is_empty() => {
                            ctxt.stream.push_back(OpCode::Call(args.len()).into());
                            ctxt.closure.len() + args.len()
                        }
                        //
                        // Generate a pack if there is not enough arguments.
                        //
                        Arity::Some(n) if args.len() < n as usize => {
                            let paklen = ctxt.closure.len() + args.len() + 1;
                            let opcode = OpCode::Pak(args.len(), paklen);
                            ctxt.stream.push_back(opcode.into());
                            paklen - 1
                        }
                        Arity::SomeWithRem(n) if args.len() < n as usize => {
                            let paklen = ctxt.closure.len() + args.len() + 1;
                            let opcode = OpCode::Pak(args.len(), paklen);
                            ctxt.stream.push_back(opcode.into());
                            paklen - 1
                        }
                        _ => return Err(Error::TooManyArguments(atom.span())),
                    },
                    //
                    // Self operator without arity (unreachable).
                    //
                    (Statement::This(_), None) => {
                        unreachable!("Arity is known for the self applicator")
                    }
                    //
                    // Any applicator with a known arity.
                    //
                    (_, Some(arity)) => match arity {
                        //
                        // If the function captures everything, just generate a call.
                        //
                        Arity::All => {
                            ctxt.stream.push_back(OpCode::Call(args.len()).into());
                            args.len()
                        }
                        //
                        // Generate a call if the number of arguments is met.
                        //
                        Arity::Some(n) if args.len() == n as usize => {
                            ctxt.stream.push_back(OpCode::Call(args.len()).into());
                            args.len()
                        }
                        Arity::SomeWithRem(n) if args.len() >= n as usize => {
                            ctxt.stream.push_back(OpCode::Call(args.len()).into());
                            args.len()
                        }
                        Arity::None if args.is_empty() => {
                            ctxt.stream.push_back(OpCode::Call(args.len()).into());
                            args.len()
                        }
                        //
                        // Generate a pack if there is not enough arguments.
                        //
                        Arity::Some(n) if args.len() < n as usize => {
                            let paklen = args.len() + 1;
                            let opcode = OpCode::Pak(args.len(), paklen);
                            ctxt.stream.push_back(opcode.into());
                            paklen - 1
                        }
                        Arity::SomeWithRem(n) if args.len() < n as usize => {
                            let paklen = args.len() + 1;
                            let opcode = OpCode::Pak(args.len(), paklen);
                            ctxt.stream.push_back(opcode.into());
                            paklen - 1
                        }
                        //
                        // Too many arguments.
                        //
                        _ => return Err(Error::TooManyArguments(atom.span())),
                    },
                    //
                    // Any applicator with unknown arity.
                    //
                    _ => {
                        ctxt.stream.push_back(OpCode::Call(args.len()).into());
                        args.len()
                    }
                };
                //
                // Update the stack.
                //
                ctxt.stackn -= reduce_len;
                //
                // Done.
                //
                Ok(())
            }
            Statement::Apply(_, op, args, CallSite::Tail) => {
                //
                // Get the symbol of the tail call.
                //
                let symbol = match op.as_ref() {
                    Statement::Symbol(_, symbol) => symbol.clone(),
                    Statement::This(_) => ctxt.name.clone(),
                    _ => return Err(Error::ExpectedSymbol(Span::None)),
                };
                //
                // Compile the arguments.
                //
                self.compile_arguments(ctxt, args)?;
                //
                // Grab the argument count.
                //
                let arity = self.defuns.get(&symbol).copied().unwrap();
                //
                // Pack the arguments depending on the arity.
                //
                let argcnt = match arity {
                    Arity::All => {
                        /*
                         * Pack the arguments into a list.
                         */
                        ctxt.stream.push_back(OpCode::Lst(args.len()).into());
                        /*
                         * Pop the argument.
                         */
                        ctxt.stream.push_back(OpCode::Rot(2).into());
                        ctxt.stream.push_back(OpCode::Pop(1).into());
                        /*
                         * Update the stack.
                         */
                        ctxt.stackn -= args.len();
                        ctxt.stackn += 1;
                        /*
                         * Return the argument count.
                         */
                        1
                    }
                    Arity::Some(n) if n == 1 => {
                        let cnt = n as usize;
                        /*
                         * Rotate the stack to push the original arguments ahead.
                         */
                        ctxt.stream.push_back(OpCode::Rot(2 * cnt).into());
                        /*
                         * Pop the old arguments.
                         */
                        ctxt.stream.push_back(OpCode::Pop(cnt).into());
                        /*
                         * Return the argument count.
                         */
                        cnt
                    }
                    Arity::Some(n) => {
                        let cnt = n as usize;
                        /*
                         * Rotate the stack to push the original arguments ahead.
                         */
                        ctxt.stream.push_back(OpCode::Rtm(2 * cnt, cnt).into());
                        /*
                         * Pop the old arguments.
                         */
                        ctxt.stream.push_back(OpCode::Pop(cnt).into());
                        /*
                         * Return the argument count.
                         */
                        cnt
                    }
                    Arity::SomeWithRem(n) if args.len() - n as usize == 0 => {
                        let cnt = n as usize + 1;
                        /*
                         * Push an empty list.
                         */
                        ctxt.stream.push_back(OpCode::Psh(Immediate::Nil).into());
                        /*
                         * Restore the new arguments order.
                         */
                        ctxt.stream.push_back(OpCode::Rot(cnt).into());
                        /*
                         * Rotate the stack to push the original arguments ahead.
                         */
                        ctxt.stream.push_back(OpCode::Rot(2 * cnt).into());
                        /*
                         * Pop the old arguments.
                         */
                        ctxt.stream.push_back(OpCode::Pop(cnt).into());
                        /*
                         * Return the argument count.
                         */
                        cnt
                    }
                    Arity::SomeWithRem(n) => {
                        let cnt = n as usize + 1;
                        let rem = args.len() - n as usize;
                        /*
                         * Prepare the new arguments.
                         */
                        ctxt.stream.push_back(OpCode::Rtm(args.len(), rem).into());
                        /*
                         * Pack the remainder arguments into a list.
                         */
                        ctxt.stream.push_back(OpCode::Lst(rem).into());
                        /*
                         * Restore the new arguments order.
                         */
                        ctxt.stream.push_back(OpCode::Rot(cnt).into());
                        /*
                         * Rotate the stack to push the original arguments ahead.
                         */
                        ctxt.stream.push_back(OpCode::Rtm(2 * cnt, cnt).into());
                        /*
                         * Pop the old arguments.
                         */
                        ctxt.stream.push_back(OpCode::Pop(cnt).into());
                        /*
                         * Return the argument count.
                         */
                        cnt
                    }
                    Arity::None => 0,
                };
                //
                // Compute the offset of the branch.
                //
                let offset = ctxt.stream.len() - (argcnt > 0) as usize;
                //
                // Generate the branch.
                //
                ctxt.stream.push_back(OpCode::Br(-(offset as isize)).into());
                //
                // Done.
                //
                Ok(())
            }
            Statement::Expand(_, name, args) => {
                //
                // Clone the compiler state and unmark the macro.
                //
                let mut comp = self.clone();
                comp.macros.remove(name);
                //
                // Evaluate the macro.
                //
                let expr = Atom::cons(Atom::symbol(name), args.atom());
                let expn = comp.eval(expr, RunParameters::default())?;
                //
                // Check if the result is valid.
                //
                if expn.is_nil() {
                    return Err(Error::MacroExpansion(name.clone()));
                }
                //
                // Compile the result.
                //
                let stmt = Statement::from_atom(expn, &self.macros)?;
                self.compile_statement(ctxt, &stmt)
            }
            Statement::Lambda(_, args, statements) => {
                //
                // Generate a name for the lambda.
                //
                let name = self.label("LAMBDA");
                //
                // Build the next context.
                //
                let mut next = Context::new(name.clone(), args.arity(), true);
                //
                // Grab the closure.
                //
                let mut closure = stmt.closure();
                //
                // Remove self from the closure.
                //
                closure.remove("self");
                //
                // Remove the global symbols from the closure.
                //
                self.defuns.keys().for_each(|v| {
                    closure.remove(v);
                });
                //
                // Track the function arguments.
                //
                next.track_arguments_and_closure(args, &closure);
                //
                // Compute the preamble depth.
                //
                let mut argcnt = args.len() + closure.len();
                //
                // Process the arguments.
                //
                if argcnt > 0 {
                    //
                    // Rotate the arguments and the return address.
                    //
                    next.stream.push_back(OpCode::Rot(argcnt + 1).into());
                    //
                    // Deconstruct the arguments.
                    //
                    argcnt = next.deconstruct_arguments_with_closure(args, &closure);
                }
                //
                // Update the next context's closure.
                //
                next.closure.extend(closure);
                //
                // Compile the statements.
                //
                self.compile_statements(&mut next, statements)?;
                //
                // Generate the postamble if necessary.
                //
                if argcnt > 0 {
                    next.stream.push_back(OpCode::Rot(argcnt + 1).into());
                    next.stream.push_back(OpCode::Pop(argcnt).into());
                }
                //
                // Inject the return call.
                //
                next.stream.push_back(OpCode::Ret.into());
                next.stackn -= 1;
                //
                // Grab the closure symbols.
                //
                next.closure.iter().try_for_each(|v| {
                    //
                    // Get the symbol index.
                    //
                    let Some(index) = ctxt.locals.get(v).and_then(|v| v.last()) else {
                        return Err(Error::InvalidSymbol(v.clone()));
                    };
                    //
                    // Inject the push.
                    //
                    let opcode = OpCode::Get(ctxt.stackn - *index).into();
                    ctxt.stream.push_back(opcode);
                    ctxt.stackn += 1;
                    //
                    // Done.
                    //
                    Ok(())
                })?;
                //
                // Determine the type of funcall.
                //
                let funcall = if self.exfuns.contains_key(&name) {
                    LabelOrOpCode::Extcall(name.clone())
                } else {
                    LabelOrOpCode::Funcall(name.clone())
                };
                //
                // Inject the funcall.
                //
                ctxt.stream.push_back(funcall);
                ctxt.stackn += 1;
                //
                // Pack the closure.
                //
                let opcode = OpCode::Pak(0, next.closure.len() + 1);
                ctxt.stream.push_back(opcode.into());
                ctxt.stackn -= next.closure.len();
                //
                // Save the block.
                //
                self.blocks.insert(name, next);
                //
                // Done.
                //
                Ok(())
            }
            Statement::Operator(_, v) => {
                //
                // Get the opcode for the operator.
                //
                let opcode = match v {
                    //
                    // Function application.
                    //
                    Operator::Apply => OpCode::Apply.into(),
                    //
                    // Arithmetics.
                    //
                    Operator::Add => OpCode::Add.into(),
                    Operator::Sub => OpCode::Sub.into(),
                    Operator::Mul => OpCode::Mul.into(),
                    Operator::Div => OpCode::Div.into(),
                    Operator::Mod => OpCode::Mod.into(),
                    Operator::Ge => OpCode::Ge.into(),
                    Operator::Gt => OpCode::Gt.into(),
                    Operator::Le => OpCode::Le.into(),
                    Operator::Lt => OpCode::Lt.into(),
                    //
                    // Logic.
                    //
                    Operator::And => OpCode::And.into(),
                    Operator::Equ => OpCode::Equ.into(),
                    Operator::Neq => OpCode::Neq.into(),
                    Operator::Not => OpCode::Not.into(),
                    Operator::Or => OpCode::Or.into(),
                    //
                    // Bits.
                    //
                    Operator::BitAnd => OpCode::BitAnd.into(),
                    Operator::BitNot => OpCode::BitNot.into(),
                    Operator::BitOr => OpCode::BitOr.into(),
                    Operator::BitXor => OpCode::BitXor.into(),
                    //
                    // List.
                    //
                    Operator::Car => OpCode::Car.into(),
                    Operator::Cdr => OpCode::Cdr.into(),
                    Operator::Conc => OpCode::Conc.into(),
                    Operator::Cons => OpCode::Cons.into(),
                    //
                    // Bytes, string, and symbol.
                    //
                    Operator::Bytes => OpCode::Bytes.into(),
                    Operator::Chr => OpCode::Chr.into(),
                    Operator::Str => OpCode::Str.into(),
                    Operator::Sym => OpCode::Sym.into(),
                    Operator::Unpack => OpCode::Unpack.into(),
                    //
                    // Predicates.
                    //
                    Operator::IsByt => OpCode::IsByt.into(),
                    Operator::IsChr => OpCode::IsChr.into(),
                    Operator::IsNum => OpCode::IsNum.into(),
                    Operator::IsLst => OpCode::IsLst.into(),
                    Operator::IsNil => OpCode::IsNil.into(),
                    Operator::IsStr => OpCode::IsStr.into(),
                    Operator::IsSym => OpCode::IsSym.into(),
                    Operator::IsTru => OpCode::IsTru.into(),
                    Operator::IsWld => OpCode::IsWld.into(),
                };
                //
                // Push the opcode.
                //
                ctxt.stream.push_back(opcode);
                //
                // Update the stack.
                //
                ctxt.stackn += 1;
                //
                // Done.
                //
                Ok(())
            }
            Statement::IfThenElse(_, cond, then, else_) => {
                //
                // Compile the condition.
                //
                self.compile_statement(ctxt, cond)?;
                //
                // Generate the branch to the else block.
                //
                let name = self.label("BEGIN_ELSE");
                let start = ctxt.stream.len();
                let label = LabelOrOpCode::BranchIfNot(name.clone());
                ctxt.stream.push_back(label);
                //
                // Update the stack (the branch consumes the result).
                //
                ctxt.stackn -= 1;
                //
                // Compile THEN.
                //
                self.compile_statement(ctxt, then)?;
                //
                // Ignore the stack value for the THEN branch.
                //
                ctxt.stackn -= 1;
                //
                // Track the label.
                //
                let delta = (ctxt.stream.len() - start) + 1;
                self.labels.insert(name, delta);
                //
                // Build the label past the else block.
                //
                let name = self.label("END_ELSE");
                let start = ctxt.stream.len();
                //
                // Generate the branch past the else block if necessary.
                //
                if !then.is_tail_call() {
                    let label = LabelOrOpCode::Branch(name.clone());
                    ctxt.stream.push_back(label);
                }
                //
                // Compile ELSE.
                //
                match else_ {
                    Some(else_) => self.compile_statement(ctxt, else_)?,
                    None => {
                        let value = Statement::Value(Atom::nil(), Value::Nil);
                        self.compile_statement(ctxt, &value)?
                    }
                }
                //
                // Track the label.
                //
                let delta = ctxt.stream.len() - start;
                self.labels.insert(name, delta);
                //
                // Done.
                //
                Ok(())
            }
            Statement::Let(_, bindings, statements) => {
                //
                // Compile the bindings.
                //
                let argcnt = self.compile_bindings(ctxt, bindings)?;
                //
                // Compile the statements.
                //
                self.compile_statements(ctxt, statements)?;
                //
                // Pop the bindings.
                //
                if argcnt > 0 {
                    ctxt.stream.push_back(OpCode::Rot(argcnt + 1).into());
                    ctxt.stream.push_back(OpCode::Pop(argcnt).into());
                    ctxt.stackn -= argcnt;
                }
                //
                // Clear the bindings from the locals.
                //
                self.clear_bindings(ctxt, bindings)?;
                //
                // Done.
                //
                Ok(())
            }
            Statement::Prog(_, stmts) => self.compile_statements(ctxt, stmts),
            Statement::Backquote(_, quote) => self.compile_backquote(ctxt, quote),
            Statement::Quote(_, quote) => self.compile_quote(ctxt, quote),
            Statement::Symbol(_, symbol) => self.compile_symbol(ctxt, symbol),
            Statement::This(_) => self.compile_this(ctxt),
            Statement::Value(_, value) => self.compile_value(ctxt, value),
        }
    }

    fn compile_backquote(&mut self, ctxt: &mut Context, quote: &Backquote) -> Result<(), Error> {
        match quote {
            //
            // Process pairs terminated with a splice.
            //
            // NOTE(xrg): expressions of the form `(e0 ,@e1) where e1 is a
            // terminal atom need special handling as the spliced pair case
            // below would try to execute (conc e1 nil), which returns nil.
            //
            Backquote::Pair(car, cdr) if car.is_splice() && cdr.is_nil() => {
                self.compile_backquote(ctxt, car.as_ref())
            }
            //
            // Process spliced pairs (conc).
            //
            Backquote::Pair(car, cdr) if car.is_splice() => {
                //
                // Process car and cdr.
                //
                self.compile_backquote(ctxt, cdr.as_ref())?;
                self.compile_backquote(ctxt, car.as_ref())?;
                //
                // Push cons, consume 2 and producing 1.
                //
                let opcode = OpCode::Conc.into();
                //
                // Push the opcode.
                //
                ctxt.stream.push_back(opcode);
                //
                // Update the stack.
                //
                ctxt.stackn -= 1;
                //
                // Done.
                //
                Ok(())
            }
            //
            // Process regular pairs (cons).
            //
            Backquote::Pair(car, cdr) => {
                //
                // Process car and cdr.
                //
                self.compile_backquote(ctxt, cdr.as_ref())?;
                self.compile_backquote(ctxt, car.as_ref())?;
                //
                // Push cons, consume 2 and producing 1.
                //
                let opcode = OpCode::Cons.into();
                //
                // Push the opcode.
                //
                ctxt.stream.push_back(opcode);
                //
                // Update the stack.
                //
                ctxt.stackn -= 1;
                //
                // Done.
                //
                Ok(())
            }
            //
            // Process unquotes.
            //
            Backquote::Unquote(stmt) | Backquote::UnquoteSplice(stmt) => {
                self.compile_statement(ctxt, stmt)
            }
            //
            // Process values.
            //
            Backquote::Value(value) => self.compile_value(ctxt, value),
        }
    }

    fn compile_quote(&mut self, ctxt: &mut Context, quote: &Quote) -> Result<(), Error> {
        match quote {
            Quote::Pair(car, cdr) => {
                //
                // Process car and cdr.
                //
                self.compile_quote(ctxt, cdr.as_ref())?;
                self.compile_quote(ctxt, car.as_ref())?;
                //
                // Push cons, consume 2 and producing 1.
                //
                let opcode = OpCode::Cons.into();
                //
                // Push the opcode.
                //
                ctxt.stream.push_back(opcode);
                //
                // Update the stack.
                //
                ctxt.stackn -= 1;
                //
                // Done.
                //
                Ok(())
            }
            Quote::Value(value) => self.compile_value(ctxt, value),
        }
    }

    fn compile_symbol(&mut self, ctxt: &mut Context, sym: &Box<str>) -> Result<(), Error> {
        //
        // Get the opcode.
        //
        let opcode = match ctxt.locals.get(sym).and_then(|v| v.last()) {
            Some(index) => OpCode::Get(ctxt.stackn - *index).into(),
            None if self.consts.contains_key(sym) => {
                let value = self.consts.get(sym).unwrap().clone();
                return self.compile_quote(ctxt, &value);
            }
            None if self.exfuns.contains_key(sym) => LabelOrOpCode::Extcall(sym.clone()),
            None => LabelOrOpCode::Funcall(sym.clone()),
        };
        //
        // Push the opcode.
        //
        ctxt.stream.push_back(opcode);
        //
        // Update the stack tracker.
        //
        ctxt.stackn += 1;
        //
        // Done.
        //
        Ok(())
    }

    fn compile_this(&mut self, ctxt: &mut Context) -> Result<(), Error> {
        //
        // Define the opcode.
        //
        let opcode = LabelOrOpCode::Funcall(ctxt.name.clone());
        //
        // Push the opcode.
        //
        ctxt.stream.push_back(opcode);
        //
        // Update the stack tracker.
        //
        ctxt.stackn += 1;
        //
        // Done.
        //
        Ok(())
    }

    fn compile_value(&mut self, ctxt: &mut Context, value: &Value) -> Result<(), Error> {
        let opcode = self.value_opcode(ctxt, value);
        ctxt.stream.push_back(opcode);
        ctxt.stackn += 1;
        Ok(())
    }

    fn value_opcode(&mut self, ctxt: &mut Context, value: &Value) -> LabelOrOpCode {
        match value {
            Value::Nil => OpCode::Psh(Immediate::Nil).into(),
            Value::True => OpCode::Psh(Immediate::True).into(),
            Value::Char(v) => OpCode::Psh(Immediate::Char(*v)).into(),
            Value::Number(v) => OpCode::Psh(Immediate::Number(*v)).into(),
            Value::String(v) => {
                //
                // Push the terminating nil.
                //
                let opcode = OpCode::Psh(Immediate::Nil).into();
                ctxt.stream.push_back(opcode);
                //
                // Build the list of chars.
                //
                v.bytes().rev().for_each(|v| {
                    let imm = Immediate::Char(v);
                    ctxt.stream.push_back(OpCode::Psh(imm).into());
                    ctxt.stream.push_back(OpCode::Cons.into());
                });
                //
                // Create the string.
                //
                OpCode::Str.into()
            }
            Value::Symbol(v) => {
                //
                // Check the symbol table.
                //
                let len = self.symbls.len();
                let idx = self.symbls.entry(v.clone()).or_insert(len as u32);
                //
                // Push the symbol.
                //
                OpCode::Psh(Immediate::Symbol(*idx)).into()
            }
            Value::Wildcard => OpCode::Psh(Immediate::Wildcard).into(),
        }
    }
}

//
// Lifters.
//

impl Compiler {
    pub fn lift_operators(&mut self) -> Result<(), Error> {
        Operator::iter()
            .map(Self::lift)
            .try_for_each(|v| self.load_statement(v))
    }

    fn lift(op: Operator) -> TopLevelStatement {
        let opname = op.to_string();
        let argcnt = op.argument_count();
        let macros = BTreeSet::default();
        //
        // Build the argument list.
        //
        let args = (0..argcnt).rev().fold(Atom::nil(), |acc, v| {
            let name = format!("#{v}");
            let symb = Atom::symbol(name.as_str());
            Atom::cons(symb, acc)
        });
        //
        // Build the apply statement.
        //
        let apply = Atom::cons(Atom::symbol(opname.as_str()), args.clone());
        //
        // Build the atom for the operator.
        //
        let atom = Atom::cons(
            Atom::symbol(opname.as_str()),
            Atom::cons(args, Atom::cons(apply, Atom::nil())),
        );
        //
        // Build the function definition.
        //
        let defun = FunctionDefinition::from_atom(atom.clone(), &macros).unwrap();
        //
        // Done.
        //
        TopLevelStatement::Function(atom, defun)
    }
}
