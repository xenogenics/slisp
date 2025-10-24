//
// Modules.
//

mod ffi;
mod machine;
mod stack;
mod value;

//
// Usages.
//

pub use machine::{RunParameters, VirtualMachine};
pub use stack::Stack;
pub use value::{Closure, Value};
