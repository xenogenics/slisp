use bincode::{Decode, Encode};
use std::{collections::BTreeMap, rc::Rc};

use crate::{
    error::Error,
    reader::{Arity, Atom, Span},
};

//
// Closure.
//

pub type Closure = (usize, Box<[Value]>);

//
// Immediates.
//

#[derive(Clone, Copy, Debug, PartialEq, Eq, Encode, Decode)]
pub enum Immediate {
    Nil,
    True,
    Char(u8),
    Number(i64),
    Extcall(u32),
    Funcall(u32, Arity),
    Symbol(u32),
    Wildcard,
}

impl Immediate {
    pub const fn extcall(idx: usize) -> Self {
        Self::Extcall(idx as u32)
    }

    pub const fn funcall(idx: usize, arity: Arity) -> Self {
        Self::Funcall(idx as u32, arity)
    }

    pub const fn as_funcall(&self) -> (u32, Arity) {
        match self {
            Immediate::Funcall(idx, cnt) => (*idx, *cnt),
            _ => panic!("Expected a funcall"),
        }
    }

    pub const fn as_number(&self) -> i64 {
        match self {
            Immediate::Char(v) => *v as i64,
            Immediate::Number(v) => *v,
            _ => panic!("Expected a number"),
        }
    }

    pub fn into_atom(self, syms: &BTreeMap<Box<str>, u32>) -> Result<Rc<Atom>, Error> {
        match self {
            Self::Nil => Ok(Atom::Nil(Span::None).into()),
            Self::True => Ok(Atom::True(Span::None).into()),
            Self::Char(v) => Ok(Atom::Char(Span::None, v).into()),
            Self::Number(v) => Ok(Atom::Number(Span::None, v).into()),
            Self::Symbol(v) => {
                let (sym, _) = syms
                    .iter()
                    .find(|(_, i)| **i == v)
                    .ok_or(Error::SymbolNotFound)?;
                Ok(Atom::Symbol(Span::None, sym.clone()).into())
            }
            Self::Wildcard => Ok(Atom::Wildcard(Span::None).into()),
            _ => todo!(),
        }
    }
}

impl std::fmt::Display for Immediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Immediate::Nil => write!(f, "nil"),
            Immediate::True => write!(f, "T"),
            Immediate::Char(c) => match *c as char {
                '\0' => write!(f, "^\\0"),
                '\x1B' => write!(f, "^\\e"),
                '\n' => write!(f, "^\\n"),
                '\r' => write!(f, "^\\r"),
                ' ' => write!(f, "^\\s"),
                '\t' => write!(f, "^\\t"),
                '"' => write!(f, "^\""),
                '\\' => write!(f, "^\\"),
                _ => write!(f, "^{}", *c as char),
            },

            Immediate::Number(n) => write!(f, "{n}"),
            Immediate::Extcall(v) => write!(f, "#X({v})",),
            Immediate::Funcall(v, arity) => write!(f, "#F({v},{arity})"),
            Immediate::Symbol(v) => write!(f, "#S({v})"),
            Immediate::Wildcard => write!(f, "_"),
        }
    }
}

impl From<bool> for Immediate {
    fn from(value: bool) -> Self {
        if value { Self::True } else { Self::Nil }
    }
}

impl From<i64> for Immediate {
    fn from(value: i64) -> Self {
        Self::Number(value)
    }
}

impl Immediate {
    pub fn from_atom(value: Rc<Atom>, syms: &BTreeMap<Box<str>, u32>) -> Result<Self, Error> {
        match value.as_ref() {
            Atom::Nil(_) => Ok(Self::Nil),
            Atom::True(_) => Ok(Self::True),
            Atom::Char(_, v) => Ok(Self::Char(*v)),
            Atom::Number(_, v) => Ok(Self::Number(*v)),
            Atom::Symbol(_, v) => {
                let idx = syms.get(v).ok_or(Error::SymbolNotFound)?;
                Ok(Immediate::Symbol(*idx))
            }
            Atom::Wildcard(_) => Ok(Self::Wildcard),
            _ => Err(Error::ExpectedImmediate(value.span())),
        }
    }
}

//
// Pair.
//

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Pair(Value, Value);

impl Pair {
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
    Pair(Rc<Pair>),
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
                let Pair(car, cdr) = cell.as_ref().clone();
                Self::Pair(Pair(car, Self::conc(cdr, b)).into())
            }
            _ => b,
        }
    }

    pub fn into_atom(self, syms: &BTreeMap<Box<str>, u32>) -> Result<Rc<Atom>, Error> {
        match self {
            Self::Bytes(v) => {
                let value = v
                    .iter()
                    .rev()
                    .fold(Atom::nil(), |acc, v| Atom::cons(Atom::char(*v), acc));
                Ok(value)
            }
            Self::Immediate(v) => v.into_atom(syms),
            Self::Pair(cell) => {
                let car = cell.car().clone().into_atom(syms)?;
                let cdr = cell.cdr().clone().into_atom(syms)?;
                Ok(Atom::Pair(Span::None, car, cdr).into())
            }
            Self::String(v) => {
                let sub = &v[..v.len() - 1];
                let val = unsafe { std::str::from_utf8_unchecked(sub) };
                Ok(Atom::string(val))
            }
            _ => Err(Error::ExpectedPairOrImmediate(Span::None)),
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
                Ok(Self::Pair(Pair(car, cdr).into()))
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
