use std::rc::Rc;

use crate::{
    opcodes::Immediate,
    vm::{Closure, Value, value::Cell},
};

#[derive(Debug)]
pub struct Stack(Vec<Value>);

impl Stack {
    pub fn new(capacity: usize) -> Self {
        Self(Vec::with_capacity(capacity))
    }

    pub fn iter(&self) -> impl std::iter::DoubleEndedIterator<Item = &Value> {
        self.0.iter()
    }

    pub fn push(&mut self, v: Value) {
        self.0.push(v);
    }

    pub fn peek(&self) -> &Value {
        unsafe { self.0.last().unwrap_unchecked() }
    }

    pub fn pop(&mut self) -> Value {
        unsafe { self.0.pop().unwrap_unchecked() }
    }

    pub fn slice_n(&mut self, n: usize) -> &[Value] {
        &self.0[self.0.len() - n..]
    }

    pub fn drop(&mut self, n: usize) {
        for _ in 0..n {
            self.0.pop();
        }
    }

    pub fn dup(&mut self, n: usize) {
        let len = self.0.len();
        for i in len - n..len {
            self.push(self.0[i].clone());
        }
    }

    pub fn get(&mut self, n: usize) {
        self.push(self.0[self.0.len() - n].clone());
    }

    pub fn list(&mut self, n: usize) {
        //
        // Return NIL on empty list.
        //
        if n == 0 {
            self.0.push(Value::Immediate(Immediate::Nil));
        }
        //
        // Combine the N elements into a list.
        else {
            //
            // Build the list from the N elements of the stack.
            //
            let result = self.0[self.0.len() - n..]
                .iter()
                .fold(Value::Immediate(Immediate::Nil), |acc, v| {
                    Value::Pair(Cell::new(v.clone(), acc).into())
                });
            //
            // Drop the N element from the stack.
            //
            for _ in 0..n {
                self.0.pop();
            }
            //
            // Push the result.
            //
            self.0.push(result);
        }
    }

    pub const fn rotate(&mut self, n: usize) {
        unsafe {
            let len = self.0.len();
            let cur = self.0.as_ptr().add(len - 1).read();
            let p = self.0.as_mut_ptr().add(len - n);
            p.copy_to(p.add(1), n - 1);
            p.write(cur);
        }
    }

    pub fn rotate_n(&mut self, m: usize, n: usize) {
        let offst = self.0.len() - m;
        self.0[offst..].rotate_right(n);
    }

    pub const fn swap(&mut self) {
        unsafe {
            let n = self.0.len();
            let a = self.0.as_mut_ptr().add(n - 1);
            let b = self.0.as_mut_ptr().add(n - 2);
            let v = a.read();
            a.write(b.read());
            b.write(v);
        }
    }

    pub fn pack(&mut self, args: usize, total: usize) {
        let vals: Box<[Value]> = self.0[self.0.len() - total..].into();
        self.drop(total);
        self.push(Value::Closure((args, vals).into()))
    }

    pub fn unpack(&mut self, closure: Rc<Closure>) -> (usize, usize) {
        let result = (closure.0, closure.1.len());
        self.0.extend_from_slice(closure.1.as_ref());
        result
    }

    pub fn unlink(&mut self) -> Value {
        self.0.remove(self.0.len() - 2)
    }
}

impl std::fmt::Display for Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let values: Vec<_> = self.0.iter().map(ToString::to_string).collect();
        write!(f, "{}", values.join(","))
    }
}
