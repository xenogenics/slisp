use std::{collections::BTreeMap, rc::Rc};

use crate::{atom::Atom, error::Error, opcodes::Immediate};

//
// Closure.
//

pub type Closure = (usize, Box<[Value]>);

//
// Cell.
//

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Cell(Value, Value);

impl Cell {
    pub fn new(a: Value, b: Value) -> Self {
        Self(a, b)
    }

    pub fn car(&self) -> &Value {
        &self.0
    }

    pub fn cdr(&self) -> &Value {
        &self.1
    }
}

//
// Value.
//

#[derive(Clone, Debug, Eq)]
pub enum Value {
    Bytes(Rc<[u8]>),
    Closure(Rc<Closure>),
    Immediate(Immediate),
    Link(usize),
    Pair(Rc<Cell>),
    String(Rc<[u8]>),
}

impl Value {
    pub fn as_immediate(&self) -> Immediate {
        match self {
            Value::Immediate(v) => v.clone(),
            _ => panic!("Expected an immediate value"),
        }
    }

    pub fn as_link(&self) -> usize {
        match self {
            Value::Link(v) => *v,
            _ => panic!("Expected a return link"),
        }
    }

    pub fn as_mut_ptr(&self) -> *mut u8 {
        match self {
            Value::Bytes(v) => {
                //
                // NOTE(xrg): This call is used for "bytes" types in external
                // signatures. These bytes are buffers that are expected to be
                // modified, so we transmute here.
                //
                unsafe { std::mem::transmute(v.as_ptr()) }
            }
            _ => std::ptr::null_mut(),
        }
    }

    pub fn as_raw_cstr(&self) -> *mut i8 {
        match self {
            Value::String(v) => {
                //
                // NOTE(xrg): This call is used for "string" types in external
                // signatures. These strings are stricly read-only but the FFI
                // interface requires *mut pointers, so we transmute here.
                //
                unsafe { std::mem::transmute(v.as_ptr()) }
            }
            _ => std::ptr::null_mut(),
        }
    }

    pub fn conc(a: Value, b: Value) -> Value {
        match a {
            Value::Pair(cell) => {
                let Cell(car, cdr) = cell.as_ref().clone();
                Self::Pair(Cell(car, Self::conc(cdr, b)).into())
            }
            _ => b,
        }
    }

    pub fn iter(&self) -> ValueIterator<'_> {
        ValueIterator(self)
    }

    fn fmt_pair(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Immediate(Immediate::Nil) => Ok(()),
            Value::Pair(cell) => {
                if matches!(cell.cdr(), Value::Immediate(Immediate::Nil)) {
                    write!(f, "{}", cell.car())
                } else {
                    write!(f, "{} ", cell.car())?;
                    cell.cdr().fmt_pair(f)
                }
            }
            v => write!(f, ". {v}"),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bytes(_) => write!(f, "#<bytes>"),
            Value::Closure(v) => {
                let (args, vals) = v.as_ref();
                let vals = vals.to_vec();
                let vals: Vec<_> = vals.into_iter().map(|v| v.to_string()).collect();
                write!(f, "K({},{})", args, vals.join(","))
            }
            Value::Immediate(v) => write!(f, "{v}"),
            Value::Link(v) => write!(f, "#L({v})"),
            Value::Pair(..) => {
                write!(f, "(")?;
                self.fmt_pair(f)?;
                write!(f, ")")
            }
            Value::String(value) => {
                let sub = &value[..value.len() - 1];
                let val = unsafe { std::str::from_utf8_unchecked(sub) };
                write!(f, "\"{val}\"")
            }
        }
    }
}

impl From<Immediate> for Value {
    fn from(value: Immediate) -> Self {
        Self::Immediate(value)
    }
}

impl Value {
    pub fn from_atom(value: Rc<Atom>, syms: &BTreeMap<Box<str>, u32>) -> Result<Self, Error> {
        match value.as_ref() {
            Atom::Nil(_)
            | Atom::True(_)
            | Atom::Char(..)
            | Atom::Number(..)
            | Atom::Symbol(..)
            | Atom::Wildcard(_) => Immediate::from_atom(value, syms).map(Self::Immediate),
            Atom::Pair(_, car, cdr) => {
                let car = Value::from_atom(car.clone(), syms)?;
                let cdr = Value::from_atom(cdr.clone(), syms)?;
                Ok(Self::Pair(Cell(car, cdr).into()))
            }
            Atom::String(_, v) => {
                let mut bytes = v.as_bytes().to_vec();
                bytes.push(0);
                Ok(Self::String(bytes.into()))
            }
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            //
            // Handle wildcards.
            //
            (Value::Immediate(Immediate::Wildcard), _) => true,
            (_, Value::Immediate(Immediate::Wildcard)) => true,
            //
            // Handle member equality.
            //
            (Value::Bytes(a), Value::Bytes(b)) => a == b,
            (Value::Closure(a), Value::Closure(b)) => a == b,
            (Value::Immediate(a), Value::Immediate(b)) => a == b,
            (Value::Link(a), Value::Link(b)) => a == b,
            (Value::Pair(a), Value::Pair(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            //
            // Default.
            //
            _ => false,
        }
    }
}

//
// Value iterator.
//

pub struct ValueIterator<'a>(&'a Value);

impl<'a> std::iter::Iterator for ValueIterator<'a> {
    type Item = &'a Value;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0 {
            Value::Pair(cell) => {
                let result = cell.car();
                self.0 = cell.cdr();
                Some(result)
            }
            _ => None,
        }
    }
}
