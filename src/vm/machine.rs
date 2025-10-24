use crate::{
    compiler::Artifacts,
    error::Error,
    opcodes::{Arity, Immediate, OpCode},
    vm::{Stack, Value, ffi::Stub},
};

//
// Parameters.
//

#[derive(Clone, Copy)]
pub struct RunParameters {
    stack_size: usize,
    trace: bool,
    depth: usize,
}

impl RunParameters {
    pub fn new(stack_size: usize, trace: bool, depth: usize) -> Self {
        Self {
            stack_size,
            trace,
            depth,
        }
    }
}

impl Default for RunParameters {
    fn default() -> Self {
        Self {
            stack_size: 1024,
            trace: false,
            depth: 10,
        }
    }
}

//
// Virtual machine.
//

pub struct VirtualMachine {
    entry: String,
    stack: Stack,
    trace: bool,
    depth: usize,
}

impl VirtualMachine {
    pub fn new(entry: &str, params: RunParameters) -> Self {
        Self {
            entry: entry.to_string(),
            stack: Stack::new(params.stack_size),
            trace: params.trace,
            depth: params.depth,
        }
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    pub fn run(&mut self, artifacts: Artifacts) -> Result<Value, Error> {
        let mut print_label = true;
        //
        // Get the artifacts.
        //
        let syms = artifacts.symbols();
        let ops = artifacts.opcodes();
        //
        // Bind the FFIs.
        //
        let mut ffis = Self::bind(&artifacts)?;
        //
        // Look-up the entrypoint function.
        //
        let entrypoint_fn = syms
            .iter()
            .find_map(|(k, v, _)| (k.as_ref() == self.entry.as_str()).then_some(v))
            .copied();
        //
        // Make sure it exists.
        //
        let Some(mut pc) = entrypoint_fn else {
            return Err(Error::EntrypointNotDefined);
        };
        //
        // Push the initial return value.
        //
        self.stack.push(Value::Link(ops.len()));
        //
        // Interpreter loop.
        //
        loop {
            //
            // Check if we are done.
            //
            if pc >= ops.len() {
                break;
            }
            //
            // Execute the current opcode.
            //
            let ppc = pc;
            pc = self.execute(ops, &mut ffis, pc)?;
            //
            // Print trace.
            //
            if self.trace {
                //
                // Print the function label if PPC points to its first opcode.
                //
                if syms.iter().find(|(_, n, _)| *n == ppc).is_some() {
                    print_label = true;
                }
                //
                // Print the current label.
                //
                if print_label {
                    //
                    // Get the index of the first function not containing PPC.
                    //
                    let index = syms
                        .iter()
                        .position(|(_, n, _)| *n > ppc)
                        .unwrap_or(syms.len());
                    //
                    // Print the label.
                    //
                    if let Some((e, _, _)) = syms.get(index - 1) {
                        println!("{e}:");
                    }
                    //
                    // Reset the print_label flag.
                    //
                    print_label = false;
                }
                //
                // Print the returning function label on the next iteration.
                //
                if matches!(ops[ppc], OpCode::Ret) {
                    print_label = true;
                }
                //
                // Capture the stack content.
                //
                let stack: Vec<_> = self.stack.iter().rev().take(self.depth).collect();
                let depth = stack.len();
                //
                // Format the stack content.
                //
                for (i, v) in stack.into_iter().enumerate() {
                    if i == 0 {
                        let op = format!("{}", ops[ppc]);
                        if depth == 1 {
                            println!("    {ppc:04} {op:<16} - {i:>2}: {v}");
                        } else {
                            println!("    {ppc:04} {op:<16} ┌ {i:>2}: {v}");
                        }
                    } else if i == depth - 1 {
                        println!("    .    {:<16} └ {i:>2}: {v}", "");
                    } else {
                        println!("    .    {:<16} │ {i:>2}: {v}", "");
                    }
                }
            }
        }
        //
        // Done.
        //
        Ok(self.stack.pop())
    }

    fn execute(&mut self, ops: &[OpCode], ffis: &mut [Stub], pc: usize) -> Result<usize, Error> {
        //
        // Execute the opcode.
        //
        match ops[pc] {
            //
            // Function application.
            //
            // NOTE(xrg): call a function/closure/etc. on a list of arguments
            // instead through an applicative form. Example:
            //
            // (+ 1 2) -> (+ '(1 2))
            //
            OpCode::Apply => {
                let mut argcnt = 0;
                //
                // Get the function handle.
                //
                let fun = self.stack.pop();
                let val = self.stack.pop();
                //
                // Process the value.
                //
                match val {
                    //
                    // In case of a list, flatten it.
                    //
                    Value::Pair(..) => {
                        let values: Vec<_> = val.iter().collect();
                        argcnt = values.len();
                        values
                            .into_iter()
                            .rev()
                            .for_each(|v| self.stack.push(v.clone()));
                    }
                    //
                    // In case of NIL, ignore it.
                    //
                    Value::Immediate(Immediate::Nil) => (),
                    //
                    // Unsupported.
                    //
                    _ => panic!("Expected a list"),
                }
                //
                // Call the function.
                //
                self.stack.push(fun);
                return self.call(ffis, pc, argcnt);
            }
            //
            // Arithmetics.
            //
            OpCode::Add => {
                let a = self.stack.pop().as_immediate().as_number();
                let b = self.stack.pop().as_immediate().as_number();
                let v = Value::from(Immediate::Number(a + b));
                self.stack.push(v);
            }
            OpCode::Sub => {
                let a = self.stack.pop().as_immediate().as_number();
                let b = self.stack.pop().as_immediate().as_number();
                let v = Value::from(Immediate::Number(a - b));
                self.stack.push(v);
            }
            OpCode::Mul => {
                let a = self.stack.pop().as_immediate().as_number();
                let b = self.stack.pop().as_immediate().as_number();
                let v = Value::from(Immediate::Number(a * b));
                self.stack.push(v);
            }
            OpCode::Div => {
                let a = self.stack.pop().as_immediate().as_number();
                let b = self.stack.pop().as_immediate().as_number();
                let v = Value::from(Immediate::Number(a / b));
                self.stack.push(v);
            }
            OpCode::Mod => {
                let a = self.stack.pop().as_immediate().as_number();
                let b = self.stack.pop().as_immediate().as_number();
                let v = Value::from(Immediate::Number(a % b));
                self.stack.push(v);
            }
            OpCode::Ge => {
                let a = self.stack.pop().as_immediate().as_number();
                let b = self.stack.pop().as_immediate().as_number();
                self.stack.push(Value::from(Immediate::from(a >= b)));
            }
            OpCode::Gt => {
                let a = self.stack.pop().as_immediate().as_number();
                let b = self.stack.pop().as_immediate().as_number();
                self.stack.push(Value::from(Immediate::from(a > b)));
            }
            OpCode::Le => {
                let a = self.stack.pop().as_immediate().as_number();
                let b = self.stack.pop().as_immediate().as_number();
                self.stack.push(Value::from(Immediate::from(a <= b)));
            }
            OpCode::Lt => {
                let a = self.stack.pop().as_immediate().as_number();
                let b = self.stack.pop().as_immediate().as_number();
                self.stack.push(Value::from(Immediate::from(a < b)));
            }
            //
            // Logics.
            //
            OpCode::And => {
                let a = matches!(self.stack.pop(), Value::Immediate(Immediate::Nil));
                let b = matches!(self.stack.pop(), Value::Immediate(Immediate::Nil));
                self.stack.push(Value::Immediate((!a && !b).into()));
            }
            OpCode::Equ => {
                let a = self.stack.pop();
                let b = self.stack.pop();
                self.stack.push(Value::Immediate((a == b).into()));
            }
            OpCode::Neq => {
                let a = self.stack.pop();
                let b = self.stack.pop();
                self.stack.push(Value::Immediate((a != b).into()));
            }
            OpCode::Not => {
                let a = matches!(self.stack.pop(), Value::Immediate(Immediate::Nil));
                self.stack.push(Value::Immediate(a.into()));
            }
            OpCode::Or => {
                let a = matches!(self.stack.pop(), Value::Immediate(Immediate::Nil));
                let b = matches!(self.stack.pop(), Value::Immediate(Immediate::Nil));
                self.stack.push(Value::Immediate((!a || !b).into()));
            }
            //
            // Bits.
            //
            OpCode::BitAnd => {
                let a = self.stack.pop().as_immediate().as_number();
                let b = self.stack.pop().as_immediate().as_number();
                self.stack.push(Value::Immediate((a & b).into()));
            }
            OpCode::BitNot => {
                let a = self.stack.pop().as_immediate().as_number();
                self.stack.push(Value::Immediate((!a).into()));
            }
            OpCode::BitOr => {
                let a = self.stack.pop().as_immediate().as_number();
                let b = self.stack.pop().as_immediate().as_number();
                self.stack.push(Value::Immediate((a | b).into()));
            }
            OpCode::BitXor => {
                let a = self.stack.pop().as_immediate().as_number();
                let b = self.stack.pop().as_immediate().as_number();
                self.stack.push(Value::Immediate((a ^ b).into()));
            }
            //
            // List operations.
            //
            OpCode::Car => {
                let result = match self.stack.pop() {
                    Value::Pair(value, _) => value.as_ref().clone(),
                    _ => Immediate::Nil.into(),
                };
                self.stack.push(result);
            }
            OpCode::Cdr => {
                let result = match self.stack.pop() {
                    Value::Pair(_, value) => value.as_ref().clone(),
                    _ => Immediate::Nil.into(),
                };
                self.stack.push(result);
            }
            OpCode::Conc => match (self.stack.pop(), self.stack.pop()) {
                (a @ Value::Pair(..), b) => {
                    let res = Value::conc(a.into(), b.into());
                    self.stack.push(res);
                }
                (_, b) => self.stack.push(b),
            },
            OpCode::Cons => {
                let (a, b) = (self.stack.pop(), self.stack.pop());
                let value = Value::Pair(a.into(), b.into());
                self.stack.push(value);
            }
            //
            // String operation.
            //
            OpCode::Bytes => {
                let value = match self.stack.pop() {
                    v @ Value::Pair(..) => {
                        //
                        // Collect the character elements of the list.
                        //
                        let bytes: Vec<_> = v
                            .iter()
                            .filter_map(|v| match v {
                                Value::Immediate(Immediate::Char(v)) => Some(*v),
                                _ => None,
                            })
                            .collect();
                        //
                        // Done.
                        //
                        Value::Bytes(bytes.into())
                    }
                    Value::Immediate(Immediate::Number(v)) => {
                        let bytes = vec![0; v as usize];
                        Value::Bytes(bytes.into())
                    }
                    _ => Value::Immediate(Immediate::Nil),
                };
                self.stack.push(value);
            }
            OpCode::Chr => {
                let value = match self.stack.pop() {
                    Value::Immediate(imm) => match imm {
                        Immediate::Number(n) => Value::Immediate(Immediate::Char(n as u8)),
                        Immediate::Char(_) => Value::Immediate(imm),
                        _ => Value::Immediate(Immediate::Nil),
                    },
                    _ => Value::Immediate(Immediate::Nil),
                };
                self.stack.push(value);
            }
            OpCode::Unpack => {
                let value = match self.stack.pop() {
                    Value::Bytes(v) => {
                        v.iter()
                            .rev()
                            .fold(Value::Immediate(Immediate::Nil), |acc, v| {
                                Value::Pair(
                                    Value::Immediate(Immediate::Char(*v)).into(),
                                    acc.into(),
                                )
                            })
                    }
                    Value::String(v) => (&v[..v.len() - 1]).iter().rev().fold(
                        Value::Immediate(Immediate::Nil),
                        |acc, v| {
                            Value::Pair(Value::Immediate(Immediate::Char(*v)).into(), acc.into())
                        },
                    ),
                    Value::Immediate(Immediate::Symbol(v)) => {
                        let pos = v.iter().position(|v| *v == 0).unwrap_or(v.len());
                        v[..pos]
                            .iter()
                            .rev()
                            .fold(Value::Immediate(Immediate::Nil), |acc, v| {
                                Value::Pair(
                                    Value::Immediate(Immediate::Char(*v)).into(),
                                    acc.into(),
                                )
                            })
                    }
                    _ => Value::Immediate(Immediate::Nil),
                };
                self.stack.push(value);
            }
            OpCode::Str => {
                let value = match self.stack.pop() {
                    v @ Value::Pair(..) => {
                        //
                        // Collect the character elements of the list.
                        //
                        let mut bytes: Vec<_> = v
                            .iter()
                            .filter_map(|v| match v {
                                Value::Immediate(Immediate::Char(v)) => Some(*v),
                                _ => None,
                            })
                            .collect();
                        //
                        // Push the null termination.
                        //
                        bytes.push(0);
                        //
                        // Done.
                        //
                        Value::String(bytes.into())
                    }
                    _ => Value::Immediate(Immediate::Nil),
                };
                self.stack.push(value);
            }
            OpCode::Sym => {
                let value = match self.stack.pop() {
                    Value::String(bytes) => {
                        if bytes.len() > 15 {
                            Value::Immediate(Immediate::Nil)
                        } else {
                            let mut raw = [0; 15];
                            raw[0..bytes.len()].copy_from_slice(&bytes);
                            Value::Immediate(Immediate::Symbol(raw))
                        }
                    }
                    _ => Value::Immediate(Immediate::Nil),
                };
                self.stack.push(value);
            }
            //
            // Predicates.
            //
            OpCode::IsByt => {
                let r = matches!(self.stack.pop(), Value::Bytes(_));
                self.stack.push(Value::Immediate(r.into()));
            }
            OpCode::IsChr => {
                let r = matches!(self.stack.pop(), Value::Immediate(Immediate::Char(_)));
                self.stack.push(Value::Immediate(r.into()));
            }
            OpCode::IsNum => {
                let r = matches!(self.stack.pop(), Value::Immediate(Immediate::Number(_)));
                self.stack.push(Value::Immediate(r.into()));
            }
            OpCode::IsLst => {
                let r = matches!(
                    self.stack.pop(),
                    Value::Immediate(Immediate::Nil) | Value::Pair(..)
                );
                self.stack.push(Value::Immediate(r.into()));
            }
            OpCode::IsNil => {
                let r = matches!(self.stack.pop(), Value::Immediate(Immediate::Nil));
                self.stack.push(Value::Immediate(r.into()));
            }
            OpCode::IsStr => {
                let r = matches!(self.stack.pop(), Value::String(..));
                self.stack.push(Value::Immediate(r.into()));
            }
            OpCode::IsSym => {
                let r = matches!(self.stack.pop(), Value::Immediate(Immediate::Symbol(_)));
                self.stack.push(Value::Immediate(r.into()));
            }
            OpCode::IsTru => {
                let r = matches!(self.stack.pop(), Value::Immediate(Immediate::True));
                self.stack.push(Value::Immediate(r.into()));
            }
            OpCode::IsWld => {
                let r = matches!(self.stack.pop(), Value::Immediate(Immediate::Wildcard));
                self.stack.push(Value::Immediate(r.into()));
            }
            //
            // Control flow.
            //
            OpCode::Br(v) => return Ok((pc as isize + v) as usize),
            OpCode::Brn(v) => {
                if matches!(self.stack.pop(), Value::Immediate(Immediate::Nil)) {
                    return Ok((pc as isize + v) as usize);
                }
            }
            OpCode::Call(argcnt) => return self.call(ffis, pc, argcnt),
            OpCode::Ret => return Ok(self.stack.unlink().as_link()),
            //
            // Stack operations.
            //
            OpCode::Dup(v) => self.stack.dup(v),
            OpCode::Get(v) => self.stack.get(v),
            OpCode::Lst(n) => self.stack.list(n),
            OpCode::Pak(m, n) => self.stack.pack(m, n),
            OpCode::Pop(v) => self.stack.drop(v),
            OpCode::Psh(v) => self.stack.push(Value::from(v)),
            OpCode::Rot(n) => self.stack.rotate(n),
            OpCode::Rtm(m, n) => self.stack.rotate_n(m, n),
            OpCode::Swp => self.stack.swap(),
        }
        //
        // Done.
        //
        Ok(pc + 1)
    }
}

//
// Helpers.
//

impl VirtualMachine {
    fn bind(artifacts: &Artifacts) -> Result<Vec<Stub>, Error> {
        artifacts
            .external_functions()
            .iter()
            .map(|(_, v)| Stub::try_from(v.clone()))
            .collect()
    }

    fn call(&mut self, ffis: &mut [Stub], pc: usize, argcnt: usize) -> Result<usize, Error> {
        match self.stack.pop() {
            Value::Closure(v) => {
                //
                // Unpack the closure.
                //
                let (argpak, paklen) = self.stack.unpack(v);
                //
                // Decode the funcall.
                //
                match self.stack.pop().as_immediate() {
                    Immediate::Extcall(index) => {
                        //
                        // Grab the external function definition.
                        //
                        let Some(ffi) = ffis.get_mut(index as usize) else {
                            panic!("No such external function definition: {index}");
                        };
                        //
                        // Get the function's arity.
                        //
                        let Arity::Some(argexp) = ffi.arity() else {
                            unreachable!();
                        };
                        //
                        // Pack in case of currying.
                        //
                        if argcnt + argpak < argexp as usize {
                            let imm = Immediate::Extcall(index);
                            self.stack.push(Value::Immediate(imm));
                            self.stack.pack(argcnt + argpak, argcnt + paklen);
                            Ok(pc + 1)
                        }
                        //
                        // Execute the foreign call.
                        //
                        else {
                            let values = self.stack.slice_n(argexp as usize);
                            let result = ffi.call(values)?;
                            self.stack.drop(argexp as usize);
                            self.stack.push(result);
                            Ok(pc + 1)
                        }
                    }
                    Immediate::Funcall(addr, Arity::None) => {
                        self.stack.push(Value::Link(pc + 1));
                        return Ok(addr as usize);
                    }
                    Immediate::Funcall(addr, Arity::All) => {
                        //
                        // Collect the arguments into a list.
                        //
                        self.stack.list(argcnt);
                        //
                        // Push the return link and go to the funcall address.
                        //
                        self.stack.push(Value::Link(pc + 1));
                        return Ok(addr as usize);
                    }
                    Immediate::Funcall(addr, arity @ Arity::Some(argexp)) => {
                        //
                        // Pack in case of currying.
                        //
                        if argcnt + argpak < argexp as usize {
                            let imm = Immediate::Funcall(addr, arity);
                            self.stack.push(Value::Immediate(imm));
                            self.stack.pack(argcnt + argpak, argcnt + paklen);
                            Ok(pc + 1)
                        }
                        //
                        // Push the return link and go to the funcall address.
                        //
                        else {
                            self.stack.push(Value::Link(pc + 1));
                            return Ok(addr as usize);
                        }
                    }
                    Immediate::Funcall(addr, arity @ Arity::SomeWithRem(argexp)) => {
                        //
                        // Pack in case of currying.
                        //
                        if argcnt + argpak < argexp as usize {
                            let imm = Immediate::Funcall(addr, arity);
                            self.stack.push(Value::Immediate(imm));
                            self.stack.pack(argcnt + argpak, argcnt + paklen);
                            Ok(pc + 1)
                        }
                        //
                        // Push the return link and go to the funcall address.
                        //
                        else {
                            //
                            // Prepare the arguments, only if there are more
                            // provided than expected.
                            //
                            if argcnt + argpak > argexp as usize {
                                for _ in 0..argexp {
                                    self.stack.rotate(argcnt + argpak);
                                }
                            }
                            //
                            // Collect the remaining arguments into a list.
                            //
                            self.stack.list(argcnt + argpak - argexp as usize);
                            //
                            // Rotate the arguments.
                            //
                            self.stack.rotate(argexp as usize + 1);
                            //
                            // Push the link value.
                            //
                            self.stack.push(Value::Link(pc + 1));
                            return Ok(addr as usize);
                        }
                    }
                    _ => {
                        panic!("Expected an extcall, funcall or syscall");
                    }
                }
            }
            Value::Immediate(Immediate::Extcall(index)) => {
                //
                // Grab the external function definition.
                //
                let Some(ffi) = ffis.get_mut(index as usize) else {
                    panic!("No such external function definition: {index}");
                };
                //
                // Get the function's arity.
                //
                let Arity::Some(argexp) = ffi.arity() else {
                    unreachable!();
                };
                //
                // Pack in case of currying.
                //
                if argcnt < argexp as usize {
                    let imm = Immediate::Extcall(index);
                    self.stack.push(Value::Immediate(imm));
                    self.stack.pack(argcnt, argcnt + 1);
                    Ok(pc + 1)
                }
                //
                // Execute the foreign call.
                //
                else {
                    let values = self.stack.slice_n(argexp as usize);
                    let result = ffi.call(values)?;
                    self.stack.drop(argexp as usize);
                    self.stack.push(result);
                    Ok(pc + 1)
                }
            }
            Value::Immediate(Immediate::Funcall(addr, Arity::None)) => {
                self.stack.push(Value::Link(pc + 1));
                return Ok(addr as usize);
            }
            Value::Immediate(Immediate::Funcall(addr, Arity::All)) => {
                //
                // Collect the arguments into a list.
                //
                self.stack.list(argcnt);
                //
                // Push the return link and go to the funcall address.
                //
                self.stack.push(Value::Link(pc + 1));
                return Ok(addr as usize);
            }
            Value::Immediate(Immediate::Funcall(addr, arity @ Arity::Some(argexp))) => {
                //
                // Pack in case of currying.
                //
                if argcnt < argexp as usize {
                    let imm = Immediate::Funcall(addr, arity);
                    self.stack.push(Value::Immediate(imm));
                    self.stack.pack(argcnt, argcnt + 1);
                    Ok(pc + 1)
                }
                //
                // Push the return link and go to the funcall address.
                //
                else {
                    self.stack.push(Value::Link(pc + 1));
                    return Ok(addr as usize);
                }
            }
            Value::Immediate(Immediate::Funcall(addr, arity @ Arity::SomeWithRem(argexp))) => {
                //
                // Pack in case of currying.
                //
                if argcnt < argexp as usize {
                    let imm = Immediate::Funcall(addr, arity);
                    self.stack.push(Value::Immediate(imm));
                    self.stack.pack(argcnt, argcnt + 1);
                    Ok(pc + 1)
                }
                //
                // Push the return link and go to the funcall address.
                //
                else {
                    //
                    // Prepare the arguments, only if there are more
                    // provided than expected.
                    //
                    if argcnt > argexp as usize {
                        for _ in 0..argexp {
                            self.stack.rotate(argcnt);
                        }
                    }
                    //
                    // Collect the remaining arguments into a list.
                    //
                    self.stack.list(argcnt - argexp as usize);
                    //
                    // Rotate the arguments.
                    //
                    self.stack.rotate(argexp as usize + 1);
                    //
                    // Push the link value.
                    //
                    self.stack.push(Value::Link(pc + 1));
                    return Ok(addr as usize);
                }
            }
            _ => panic!("Expected a closure, extcall, funcall or syscall"),
        }
    }
}
