use std::rc::Rc;

use crate::{atom::Atom, error::Error, opcodes::Immediate};

//
// Closure.
//

pub type Closure = (usize, Box<[Value]>);

//
// Value.
//

#[derive(Clone, Debug, Eq)]
pub enum Value {
    Bytes(Rc<[u8]>),
    Closure(Rc<Closure>),
    Immediate(Immediate),
    Link(usize),
    Pair(Box<Value>, Box<Value>),
    String(Rc<[u8]>),
}

impl Value {
    pub fn as_immediate(&self) -> Immediate {
        match self {
            Value::Immediate(v) => *v,
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

    pub fn conc(a: Box<Value>, b: Value) -> Value {
        match *a {
            Value::Pair(car, cdr) => Self::Pair(car, Self::conc(cdr, b).into()),
            _ => b.clone(),
        }
    }

    pub fn iter(&self) -> ValueIterator<'_> {
        ValueIterator(self)
    }

    fn fmt_pair(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Immediate(Immediate::Nil) => Ok(()),
            Value::Pair(car, cdr) => {
                if matches!(cdr.as_ref(), Value::Immediate(Immediate::Nil)) {
                    write!(f, "{car}")
                } else {
                    write!(f, "{car} ")?;
                    cdr.fmt_pair(f)
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

impl TryFrom<Rc<Atom>> for Value {
    type Error = Error;

    fn try_from(value: Rc<Atom>) -> Result<Self, Self::Error> {
        match value.as_ref() {
            Atom::Nil(_)
            | Atom::True(_)
            | Atom::Char(..)
            | Atom::Number(..)
            | Atom::Symbol(..)
            | Atom::Wildcard(_) => Immediate::try_from(value).map(Self::Immediate),
            Atom::Pair(_, car, cdr) => {
                let car: Self = car.clone().try_into()?;
                let cdr: Self = cdr.clone().try_into()?;
                Ok(Self::Pair(car.into(), cdr.into()))
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
            (Value::Pair(car0, cdr0), Value::Pair(car1, cdr1)) => car0 == car1 && cdr0 == cdr1,
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
            Value::Pair(car, cdr) => {
                let result = car.as_ref();
                self.0 = cdr.as_ref();
                Some(result)
            }
            _ => None,
        }
    }
}
