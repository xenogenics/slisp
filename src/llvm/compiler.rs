use inkwell::{
    IntPredicate,
    builder::Builder,
    memory_buffer::MemoryBuffer,
    module::{Linkage, Module},
    targets::TargetTriple,
    values::{BasicValue, BasicValueEnum, FunctionValue},
};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    rc::Rc,
};

use crate::{
    Compiler as CompilerTrait,
    error::Error,
    reader::{
        Arguments, Arity, Atom, FunctionDefinition, Operator, Statement, Statements,
        TopLevelStatement, Value,
    },
};

//
// Target triple.
//

const TARGET_TRIPLE: &str = env!("VERGEN_RUSTC_HOST_TRIPLE");

//
// Context.
//

#[derive(Clone, Debug, Default)]
struct Context<'a> {
    #[allow(dead_code)]
    name: Box<str>,
    #[allow(dead_code)]
    arity: Arity,
    locals: HashMap<Box<str>, Vec<BasicValueEnum<'a>>>,
    lcntr: usize,
    vcntr: usize,
}

impl<'a> Context<'a> {
    fn new(name: Box<str>, arity: Arity) -> Self {
        Self {
            name,
            arity,
            locals: HashMap::default(),
            lcntr: 0,
            vcntr: 0,
        }
    }

    fn track_arguments(&mut self, func: FunctionValue<'a>, args: &Arguments) {
        args.names().enumerate().for_each(|(i, n)| {
            let arg = func.get_nth_param(i as u32).unwrap();
            self.locals.entry(n.clone()).or_default().push(arg);
        });
    }

    fn label(&mut self) -> String {
        let res = format!("L{:04}", self.lcntr);
        self.lcntr += 1;
        res
    }

    fn value(&mut self) -> String {
        let res = format!("v{:04}", self.vcntr);
        self.vcntr += 1;
        res
    }
}

//
// Proxy.
//

pub struct Proxy<'ctx> {
    context: &'ctx inkwell::context::Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

impl<'ctx> Proxy<'ctx> {
    pub fn new(context: &'ctx inkwell::context::Context, name: &str) -> Result<Self, Error> {
        //
        // Create the top-level module.
        //
        let module = context.create_module(name);
        module.set_triple(&TargetTriple::create(TARGET_TRIPLE));
        //
        // Create the IR builder.
        //
        let builder = context.create_builder();
        //
        // Load the core module.
        //
        let pathbuf = std::path::PathBuf::from("src/llvm/core.ll");
        let membuf = MemoryBuffer::create_from_file(&pathbuf)?;
        let core = context.create_module_from_ir(membuf)?;
        //
        // Link the core into the module.
        //
        module.link_in_module(core)?;
        //
        // Set the linkage of all imported functions to internal.
        //
        module
            .get_functions()
            .filter(|f| !f.as_global_value().is_declaration())
            .for_each(|f| f.set_linkage(Linkage::Internal));
        //
        // Build self.
        //
        let result = Self {
            context,
            module,
            builder,
        };
        //
        // Done.
        //
        Ok(result)
    }

    //
    // Constructors.
    //

    fn new_nil(&self, ctxt: &mut Context<'ctx>) -> Result<BasicValueEnum<'ctx>, Error> {
        let func = self.module.get_function("new_nil").unwrap();
        let rslt = self.builder.build_call(func, &[], &ctxt.value())?;
        Ok(rslt.try_as_basic_value().unwrap_left())
    }

    fn new_tru(&self, ctxt: &mut Context<'ctx>) -> Result<BasicValueEnum<'ctx>, Error> {
        let func = self.module.get_function("new_tru").unwrap();
        let rslt = self.builder.build_call(func, &[], &ctxt.value())?;
        Ok(rslt.try_as_basic_value().unwrap_left())
    }

    fn new_chr(
        &self,
        ctxt: &mut Context<'ctx>,
        value: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let func = self.module.get_function("new_chr").unwrap();
        let rslt = self
            .builder
            .build_call(func, &[value.into()], &ctxt.value())?;
        Ok(rslt.try_as_basic_value().unwrap_left())
    }

    fn new_int(
        &self,
        ctxt: &mut Context<'ctx>,
        value: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let func = self.module.get_function("new_int").unwrap();
        let rslt = self
            .builder
            .build_call(func, &[value.into()], &ctxt.value())?;
        Ok(rslt.try_as_basic_value().unwrap_left())
    }

    fn new_fun(
        &self,
        ctxt: &mut Context<'ctx>,
        value: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let func = self.module.get_function("new_fun").unwrap();
        let rslt = self
            .builder
            .build_call(func, &[value.into()], &ctxt.value())?;
        Ok(rslt.try_as_basic_value().unwrap_left())
    }

    fn new_wld(&self, ctxt: &mut Context<'ctx>) -> Result<BasicValueEnum<'ctx>, Error> {
        let func = self.module.get_function("new_wld").unwrap();
        let rslt = self.builder.build_call(func, &[], &ctxt.value())?;
        Ok(rslt.try_as_basic_value().unwrap_left())
    }

    //
    // Clone/drop value.
    //

    fn clone_val(
        &self,
        ctxt: &mut Context<'ctx>,
        arg: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("clone").unwrap();
        let r = self.builder.build_call(f, &[arg.into()], &ctxt.value())?;
        Ok(r.try_as_basic_value().unwrap_left())
    }
    fn drop_val(&self, ctxt: &mut Context<'ctx>, arg: BasicValueEnum<'ctx>) -> Result<(), Error> {
        let f = self.module.get_function("drop").unwrap();
        let v = self.builder.build_call(f, &[arg.into()], &ctxt.value())?;
        v.try_as_basic_value().unwrap_right();
        Ok(())
    }

    //
    // Arithmetics.
    //

    fn add(
        &self,
        ctxt: &mut Context<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("add").unwrap();
        let a = &[args[1].into(), args[0].into()];
        let v = self.builder.build_call(f, a, &ctxt.value())?;
        Ok(v.try_as_basic_value().unwrap_left())
    }

    fn sub(
        &self,
        ctxt: &mut Context<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("sub").unwrap();
        let a = &[args[1].into(), args[0].into()];
        let v = self.builder.build_call(f, a, &ctxt.value())?;
        Ok(v.try_as_basic_value().unwrap_left())
    }

    fn mul(
        &self,
        ctxt: &mut Context<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("mul").unwrap();
        let a = &[args[1].into(), args[0].into()];
        let v = self.builder.build_call(f, a, &ctxt.value())?;
        Ok(v.try_as_basic_value().unwrap_left())
    }

    fn div(
        &self,
        ctxt: &mut Context<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("div").unwrap();
        let a = &[args[1].into(), args[0].into()];
        let v = self.builder.build_call(f, a, &ctxt.value())?;
        Ok(v.try_as_basic_value().unwrap_left())
    }

    fn rem(
        &self,
        ctxt: &mut Context<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("mod").unwrap();
        let a = &[args[1].into(), args[0].into()];
        let v = self.builder.build_call(f, a, &ctxt.value())?;
        Ok(v.try_as_basic_value().unwrap_left())
    }

    //
    // Comparison operations.
    //

    fn ge(
        &self,
        ctxt: &mut Context<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("ge").unwrap();
        let a = &[args[1].into(), args[0].into()];
        let v = self.builder.build_call(f, a, &ctxt.value())?;
        Ok(v.try_as_basic_value().unwrap_left())
    }

    fn gt(
        &self,
        ctxt: &mut Context<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("gt").unwrap();
        let a = &[args[1].into(), args[0].into()];
        let v = self.builder.build_call(f, a, &ctxt.value())?;
        Ok(v.try_as_basic_value().unwrap_left())
    }

    fn le(
        &self,
        ctxt: &mut Context<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("le").unwrap();
        let a = &[args[1].into(), args[0].into()];
        let v = self.builder.build_call(f, a, &ctxt.value())?;
        Ok(v.try_as_basic_value().unwrap_left())
    }

    fn lt(
        &self,
        ctxt: &mut Context<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("lt").unwrap();
        let a = &[args[1].into(), args[0].into()];
        let v = self.builder.build_call(f, a, &ctxt.value())?;
        Ok(v.try_as_basic_value().unwrap_left())
    }

    //
    // Boolean operations.
    //

    fn and(
        &self,
        ctxt: &mut Context<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("and").unwrap();
        let a = &[args[1].into(), args[0].into()];
        let v = self.builder.build_call(f, a, &ctxt.value())?;
        Ok(v.try_as_basic_value().unwrap_left())
    }

    fn or(
        &self,
        ctxt: &mut Context<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("or").unwrap();
        let a = &[args[1].into(), args[0].into()];
        let v = self.builder.build_call(f, a, &ctxt.value())?;
        Ok(v.try_as_basic_value().unwrap_left())
    }

    //
    // Bitwise operations.
    //

    fn bitand(
        &self,
        ctxt: &mut Context<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("bitand").unwrap();
        let a = &[args[1].into(), args[0].into()];
        let v = self.builder.build_call(f, a, &ctxt.value())?;
        Ok(v.try_as_basic_value().unwrap_left())
    }

    fn bitor(
        &self,
        ctxt: &mut Context<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("bitor").unwrap();
        let a = &[args[1].into(), args[0].into()];
        let v = self.builder.build_call(f, a, &ctxt.value())?;
        Ok(v.try_as_basic_value().unwrap_left())
    }

    fn bitxor(
        &self,
        ctxt: &mut Context<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("bitxor").unwrap();
        let a = &[args[1].into(), args[0].into()];
        let v = self.builder.build_call(f, a, &ctxt.value())?;
        Ok(v.try_as_basic_value().unwrap_left())
    }

    //
    // List operations.
    //

    fn car(
        &self,
        ctxt: &mut Context<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("car").unwrap();
        let a = &[args[0].into()];
        let v = self.builder.build_call(f, a, &ctxt.value())?;
        Ok(v.try_as_basic_value().unwrap_left())
    }

    fn cdr(
        &self,
        ctxt: &mut Context<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("cdr").unwrap();
        let a = &[args[0].into()];
        let v = self.builder.build_call(f, a, &ctxt.value())?;
        Ok(v.try_as_basic_value().unwrap_left())
    }

    fn cons(
        &self,
        ctxt: &mut Context<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>, Error> {
        let f = self.module.get_function("cons").unwrap();
        let a = &[args[1].into(), args[0].into()];
        let v = self.builder.build_call(f, a, &ctxt.value())?;
        Ok(v.try_as_basic_value().unwrap_left())
    }
}

//
// Compiler.
//

#[derive(Clone)]
pub struct Compiler<'a> {
    proxy: Rc<Proxy<'a>>,
    defuns: BTreeMap<Box<str>, (Arity, FunctionValue<'a>)>,
    macros: BTreeSet<Box<str>>,
}

//
// New.
//

impl<'a> Compiler<'a> {
    pub fn new(state: Rc<Proxy<'a>>) -> Self {
        Self {
            proxy: state,
            defuns: Default::default(),
            macros: Default::default(),
        }
    }
}

//
// Compiler trait.
//

impl CompilerTrait for Compiler<'_> {
    type Artifacts = String;

    fn eval(self, _: Rc<Atom>, _: crate::RunParameters) -> Result<Rc<Atom>, Error> {
        todo!()
    }

    fn expand(self, _: Rc<Atom>, _: crate::RunParameters) -> Result<Rc<Atom>, Error> {
        todo!()
    }

    fn load(&mut self, atom: Rc<Atom>) -> Result<(), Error> {
        let stmt = TopLevelStatement::from_atom(atom, &mut self.macros)?;
        self.load_statement(stmt)
    }

    fn compile(self, entrypoint: &str) -> Result<Self::Artifacts, Error> {
        //
        // Get the entrypoint.
        //
        let epfn = self
            .proxy
            .module
            .get_function(entrypoint)
            .ok_or(Error::EntrypointNotDefined)?;
        //
        // Check for "main".
        //
        if entrypoint == "main" {
            epfn.as_global_value().set_name("main.0");
        }
        //
        // Get the drop method.
        //
        let drop = self.proxy.module.get_function("drop").unwrap();
        //
        // Define the main function.
        //
        let ftyp = self.proxy.context.i32_type().fn_type(&[], false);
        let func = self.proxy.module.add_function("main", ftyp, None);
        //
        // Position the builder.
        //
        let entry = self.proxy.context.append_basic_block(func, "entry");
        self.proxy.builder.position_at_end(entry);
        //
        // Call the entrypoint.
        //
        let rslt = self.proxy.builder.build_call(epfn, &[], "rslt")?;
        let rslt = rslt.try_as_basic_value().unwrap_left();
        //
        // Extract the type of the result and truncate it.
        //
        let rtyp = self
            .proxy
            .builder
            .build_extract_value(rslt.into_struct_value(), 0, "rtyp")?;
        let rval = self.proxy.builder.build_int_truncate(
            rtyp.into_int_value(),
            self.proxy.context.i32_type(),
            "rval",
        )?;
        //
        // Drop the result.
        //
        self.proxy
            .builder
            .build_call(drop, &[rslt.into()], "drop")?;
        //
        // Return the exit code.
        //
        self.proxy.builder.build_return(Some(&rval))?;
        //
        // Generate the artifacts.
        //
        let artifacts = self.proxy.module.print_to_string().to_string();
        //
        // Done.
        //
        Ok(artifacts)
    }
}

//
// Statement loading.
//

impl Compiler<'_> {
    fn load_statement(&mut self, stmt: TopLevelStatement) -> Result<(), Error> {
        match stmt {
            TopLevelStatement::Constant(_, _) => todo!(),
            TopLevelStatement::External(_, _) => todo!(),
            TopLevelStatement::Function(_, v) => self.compile_function(v),
            TopLevelStatement::Macro(_, _) => todo!(),
            TopLevelStatement::Use(_, _) => todo!(),
        }
    }
}

//
// Compilation.
//

impl<'a> Compiler<'a> {
    fn compile_function(&mut self, defun: FunctionDefinition) -> Result<(), Error> {
        let arity = defun.arguments().arity();
        let mut ctxt = Context::new(defun.name().clone(), arity);
        //
        // Build the argument types.
        //
        let vtyp = self.proxy.module.get_struct_type("Value").unwrap();
        let alen = defun.arguments().len();
        let atyp: Vec<_> = std::iter::repeat_n(vtyp.into(), alen).collect();
        //
        // Add the function.
        //
        let ftyp = vtyp.fn_type(&atyp, false);
        let func = self.proxy.module.add_function(&defun.name(), ftyp, None);
        //
        // Build the entry basic block and position the builder there.
        //
        let head_bb = self.proxy.context.append_basic_block(func, &ctxt.label());
        self.proxy.builder.position_at_end(head_bb);
        //
        // Track the function arguments.
        //
        ctxt.track_arguments(func, defun.arguments());
        //
        // Track the function definition.
        //
        self.defuns.insert(defun.name().clone(), (arity, func));
        //
        // Compile the statements.
        //
        let stmts = self.compile_statements(&mut ctxt, defun.statements(), func)?;
        //
        // Drop the arguments.
        //
        let locals: Vec<_> = ctxt.locals.values().flatten().cloned().collect();
        locals
            .into_iter()
            .try_for_each(|v| self.proxy.drop_val(&mut ctxt, v))?;
        //
        // Return the last statement.
        //
        let result = stmts.last().unwrap();
        self.proxy.builder.build_return(Some(result))?;
        //
        // Done.
        //
        Ok(())
    }

    fn compile_statements(
        &mut self,
        ctxt: &mut Context<'a>,
        stmts: &Statements,
        func: FunctionValue<'a>,
    ) -> Result<Vec<BasicValueEnum<'a>>, Error> {
        stmts
            .iter()
            .map(|v| self.compile_statement(ctxt, v, func))
            .collect::<Result<_, _>>()
    }

    fn compile_arguments(
        &mut self,
        ctxt: &mut Context<'a>,
        stmts: &Statements,
        func: FunctionValue<'a>,
    ) -> Result<Vec<BasicValueEnum<'a>>, Error> {
        stmts
            .iter()
            .rev()
            .map(|v| self.compile_statement(ctxt, v, func))
            .collect::<Result<_, _>>()
    }

    fn compile_statement(
        &mut self,
        ctxt: &mut Context<'a>,
        stmt: &Statement,
        func: FunctionValue<'a>,
    ) -> Result<BasicValueEnum<'a>, Error> {
        match stmt {
            Statement::Apply(_, op, args, _) => {
                //
                // Compile the arguments.
                //
                let args = self.compile_arguments(ctxt, args, func)?;
                //
                // Process the applicator.
                //
                match op.as_ref() {
                    Statement::Operator(_, op) => self.compile_operator(ctxt, op, &args),
                    Statement::Symbol(_, v) if self.defuns.contains_key(v) => {
                        //
                        // Get the function.
                        //
                        let (arity, func) = self.defuns.get(v).copied().unwrap();
                        //
                        // Process the arity.
                        //
                        match arity {
                            Arity::All => {
                                todo!("Requires cons'ing the values into a list");
                            }
                            Arity::Some(n) if n as usize == args.len() => {
                                let arg: Vec<_> = args.into_iter().map(Into::into).collect();
                                let val = ctxt.value();
                                let res = self.proxy.builder.build_call(func, &arg, &val)?;
                                Ok(res.try_as_basic_value().unwrap_left())
                            }
                            Arity::SomeWithRem(n) if n as usize <= args.len() => {
                                todo!("Requires cons'ing the remainder values into a list");
                            }
                            Arity::None if args.is_empty() => {
                                let val = ctxt.value();
                                let res = self.proxy.builder.build_call(func, &[], &val)?;
                                Ok(res.try_as_basic_value().unwrap_left())
                            }
                            _ => {
                                todo!("Requires closure packing");
                            }
                        }
                    }
                    Statement::This(_) => {
                        //
                        // Get the function.
                        //
                        let (arity, func) = self.defuns.get(&ctxt.name).copied().unwrap();
                        //
                        // Process the arity.
                        //
                        match arity {
                            Arity::All => {
                                todo!("Requires cons'ing the values into a list");
                            }
                            Arity::Some(n) if n as usize == args.len() => {
                                let arg: Vec<_> = args.into_iter().map(Into::into).collect();
                                let val = ctxt.value();
                                let res = self.proxy.builder.build_call(func, &arg, &val)?;
                                Ok(res.try_as_basic_value().unwrap_left())
                            }
                            Arity::SomeWithRem(n) if n as usize <= args.len() => {
                                todo!("Requires cons'ing the remainder values into a list");
                            }
                            Arity::None if args.is_empty() => {
                                let val = ctxt.value();
                                let res = self.proxy.builder.build_call(func, &[], &val)?;
                                Ok(res.try_as_basic_value().unwrap_left())
                            }
                            _ => {
                                todo!("Requires closure packing");
                            }
                        }
                    }
                    _ => {
                        todo!("Requires dynamic call support");
                    }
                }
            }
            Statement::Expand(..) => todo!(),
            Statement::Lambda(..) => todo!(),
            Statement::Operator(..) => {
                todo!("Push the Funcall value of the operator");
            }
            Statement::IfThenElse(_, cond, then, else_) => {
                let vtyp = self.proxy.module.get_struct_type("Value").unwrap();
                //
                // Build the basic blocks.
                //
                let then_bb = self.proxy.context.append_basic_block(func, &ctxt.label());
                let else_bb = self.proxy.context.append_basic_block(func, &ctxt.label());
                let tail_bb = self.proxy.context.append_basic_block(func, &ctxt.label());
                //
                // Compile the condition.
                //
                let cond_bv = self
                    .compile_statement(ctxt, cond, func)?
                    .into_struct_value();
                //
                // Extract the result type.
                //
                let type_bv = self
                    .proxy
                    .builder
                    .build_extract_value(cond_bv, 0, &ctxt.value())?
                    .into_int_value();
                //
                // Build the condition result.
                //
                let comp_bv = self.proxy.builder.build_int_compare(
                    IntPredicate::EQ,
                    type_bv,
                    self.proxy.context.i64_type().const_int(1, false),
                    &ctxt.value(),
                )?;
                //
                // Position the builder to the THEN basic block.
                //
                self.proxy.builder.position_at_end(then_bb);
                //
                // Compile the THEN branch.
                //
                let then_bv = self.compile_statement(ctxt, then, func)?;
                //
                // Position the builder to the ELSE basic block.
                //
                self.proxy.builder.position_at_end(else_bb);
                //
                // Handle the ELSE branch.
                //
                let eres = if let Some(else_) = else_ {
                    self.compile_statement(ctxt, else_, func)?
                } else {
                    vtyp.const_zero().into()
                };
                //
                // Insert the exit branch in the THEN basic block.
                //
                self.proxy.builder.position_at_end(then_bb);
                self.proxy.builder.build_unconditional_branch(tail_bb)?;
                //
                // Insert the exit branch in the ELSE basic block.
                //
                self.proxy.builder.position_at_end(else_bb);
                self.proxy.builder.build_unconditional_branch(tail_bb)?;
                //
                // Build the condition statement.
                //
                let head_bb = then_bb.get_previous_basic_block().unwrap();
                self.proxy.builder.position_at_end(head_bb);
                self.proxy
                    .builder
                    .build_conditional_branch(comp_bv, then_bb, else_bb)?;
                //
                // Return the value.
                //
                self.proxy.builder.position_at_end(tail_bb);
                let phi = self.proxy.builder.build_phi(vtyp, &ctxt.value())?;
                phi.add_incoming(&[(&then_bv, then_bb), (&eres, else_bb)]);
                //
                // Done.
                //
                Ok(phi.as_basic_value())
            }
            Statement::Let(..) => todo!(),
            Statement::Prog(_, stmts) => {
                let rslt = self.compile_statements(ctxt, stmts, func)?;
                let last = rslt.last().copied().unwrap();
                Ok(last)
            }
            Statement::Backquote(..) => todo!(),
            Statement::Quote(..) => todo!(),
            Statement::Symbol(_, symbol) => self.compile_symbol(ctxt, symbol),
            Statement::This(_) => self.compile_this(ctxt),
            Statement::Value(_, v) => self.compile_value(ctxt, v),
        }
    }

    fn compile_operator(
        &mut self,
        ctxt: &mut Context<'a>,
        oper: &Operator,
        args: &[BasicValueEnum<'a>],
    ) -> Result<BasicValueEnum<'a>, Error> {
        match oper {
            Operator::Apply => todo!(),
            Operator::Add => self.proxy.add(ctxt, args),
            Operator::Sub => self.proxy.sub(ctxt, args),
            Operator::Mul => self.proxy.mul(ctxt, args),
            Operator::Div => self.proxy.div(ctxt, args),
            Operator::Mod => self.proxy.rem(ctxt, args),
            Operator::Ge => self.proxy.ge(ctxt, args),
            Operator::Gt => self.proxy.gt(ctxt, args),
            Operator::Le => self.proxy.le(ctxt, args),
            Operator::Lt => self.proxy.lt(ctxt, args),
            Operator::And => self.proxy.and(ctxt, args),
            Operator::Equ => todo!(),
            Operator::Neq => todo!(),
            Operator::Not => todo!(),
            Operator::Or => self.proxy.or(ctxt, args),
            Operator::BitAnd => self.proxy.bitand(ctxt, args),
            Operator::BitNot => todo!(),
            Operator::BitOr => self.proxy.bitor(ctxt, args),
            Operator::BitXor => self.proxy.bitxor(ctxt, args),
            Operator::Car => self.proxy.car(ctxt, args),
            Operator::Cdr => self.proxy.cdr(ctxt, args),
            Operator::Conc => todo!(),
            Operator::Cons => self.proxy.cons(ctxt, args),
            Operator::Bytes => todo!(),
            Operator::Chr => todo!(),
            Operator::Str => todo!(),
            Operator::Sym => todo!(),
            Operator::Unpack => todo!(),
            Operator::IsByt => todo!(),
            Operator::IsChr => todo!(),
            Operator::IsNum => todo!(),
            Operator::IsLst => todo!(),
            Operator::IsNil => todo!(),
            Operator::IsStr => todo!(),
            Operator::IsSym => todo!(),
            Operator::IsTru => todo!(),
            Operator::IsWld => todo!(),
        }
    }

    fn compile_symbol(
        &mut self,
        ctxt: &mut Context<'a>,
        sym: &Box<str>,
    ) -> Result<BasicValueEnum<'a>, Error> {
        match ctxt.locals.get(sym).and_then(|v| v.last()).cloned() {
            Some(v) => self.proxy.clone_val(ctxt, v),
            _ => todo!(),
        }
    }

    fn compile_this(&mut self, ctxt: &mut Context<'a>) -> Result<BasicValueEnum<'a>, Error> {
        let (_, func) = self.defuns.get(&ctxt.name).unwrap();
        let rslt = self.proxy.builder.build_ptr_to_int(
            func.as_global_value().as_pointer_value(),
            self.proxy.context.i64_type(),
            &ctxt.value(),
        )?;
        self.proxy.new_fun(ctxt, rslt.as_basic_value_enum())
    }

    fn compile_value(
        &mut self,
        ctxt: &mut Context<'a>,
        value: &Value,
    ) -> Result<BasicValueEnum<'a>, Error> {
        match value {
            Value::Nil => self.proxy.new_nil(ctxt),
            Value::True => self.proxy.new_tru(ctxt),
            Value::Char(v) => self.proxy.new_chr(
                ctxt,
                self.proxy
                    .context
                    .i64_type()
                    .const_int(*v as u64, false)
                    .into(),
            ),
            Value::Number(v) => self.proxy.new_int(
                ctxt,
                self.proxy
                    .context
                    .i64_type()
                    .const_int(*v as u64, true)
                    .into(),
            ),
            Value::String(_) | Value::Symbol(_) => todo!(),
            Value::Wildcard => self.proxy.new_wld(ctxt),
        }
    }
}
