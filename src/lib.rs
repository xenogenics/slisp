#![allow(clippy::borrowed_box)]

use std::rc::Rc;

use crate::reader::Atom;

#[macro_use]
extern crate lalrpop_util;

//
// Modules.
//

pub mod bytecode;
pub mod error;
pub mod llvm;
pub mod reader;

//
// Traits.
//

pub trait Compiler: Clone {
    type Artifacts;

    fn eval(self, atom: Rc<Atom>, params: RunParameters) -> Result<Rc<Atom>, error::Error>;
    fn expand(self, atom: Rc<Atom>, params: RunParameters) -> Result<Rc<Atom>, error::Error>;
    fn load(&mut self, atom: Rc<Atom>) -> Result<(), error::Error>;
    fn compile(self, entrypoint: &str) -> Result<Self::Artifacts, error::Error>;
}

//
// Run parameters.
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
