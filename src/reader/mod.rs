//
// Grammar.
//

lalrpop_mod!(
    #[allow(clippy::all)]
    grammar,
    "/reader/grammar.rs"
);

//
// Modules.
//

mod atom;
mod ir;

//
// Tests.
//

#[cfg(test)]
pub mod tests;

//
// Usages.
//

pub use atom::{Atom, AtomIterator, Span};
pub use grammar::{ExpressionParser, ListsParser};
pub use ir::{
    Arguments, Arity, Backquote, Binding, Bindings, CallSite, ConstantDefinition,
    ExternalDefinition, ExternalType, FunctionDefinition, Operator, Quote, Statement, Statements,
    TopLevelStatement, Value,
};
