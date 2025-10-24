use std::rc::Rc;

use crate::{
    bytecode::{Artifacts, CompilerTrait, RunParameters},
    error::Error,
    reader::{atom::Atom, grammar::ListsParser, ir::TopLevelStatement},
};
use map_macro::btree_set;

//
// Null compiler.
//

#[derive(Clone)]
pub struct NullCompiler;

impl CompilerTrait for NullCompiler {
    fn eval(self, _: Rc<Atom>, _: RunParameters) -> Result<Rc<Atom>, Error> {
        Err(Error::NotSupported)
    }

    fn expand(self, _: Rc<Atom>, _: RunParameters) -> Result<Rc<Atom>, Error> {
        Err(Error::NotSupported)
    }

    fn load(&mut self, _: Rc<Atom>) -> Result<(), Error> {
        Ok(())
    }

    fn compile(self, _: &str) -> Result<Artifacts, Error> {
        Ok(Default::default())
    }
}

//
// Intermediate representation.
//

fn parse(stmt: &str) -> Vec<Rc<Atom>> {
    let parser = ListsParser::new();
    let mut compiler = NullCompiler;
    parser.parse(&mut compiler, stmt).unwrap()
}

#[test]
fn def_single_statement() -> Result<(), Error> {
    let atoms = parse("(def ADD (A B) (+ A B))");
    let defuns: Vec<_> = atoms
        .into_iter()
        .map(|v| TopLevelStatement::from_atom(v, &Default::default()))
        .collect::<Result<_, _>>()?;
    assert_eq!(btree_set! {}, defuns[0].closure());
    let stmt = defuns[0].statements().iter().next().unwrap();
    assert_eq!(btree_set! {"A".into(), "B".into()}, stmt.closure());
    Ok(())
}

#[test]
fn def_with_lambda_with_external_bindings() -> Result<(), Error> {
    let atoms = parse("(def test(a) ((\\ (b) (+ a b)) 1))");
    let defuns: Vec<_> = atoms
        .into_iter()
        .map(|v| TopLevelStatement::from_atom(v, &Default::default()))
        .collect::<Result<_, _>>()?;
    assert_eq!(btree_set! {}, defuns[0].closure());
    let stmt = defuns[0].statements().iter().next().unwrap();
    assert_eq!(btree_set! {"a".into()}, stmt.closure());
    let stmt = stmt.statements().next().unwrap();
    assert_eq!(btree_set! {"a".into()}, stmt.closure());
    Ok(())
}

#[test]
fn def_with_nested_lambda_with_external_bindings() -> Result<(), Error> {
    let atoms = parse(
        r#"
            (def test(a)
                ((\ (b)
                    ((\ (c) (+ a (+ b c ))) 1)) 2))
            "#,
    );
    let defuns: Vec<_> = atoms
        .into_iter()
        .map(|v| TopLevelStatement::from_atom(v, &Default::default()))
        .collect::<Result<_, _>>()?;
    assert_eq!(btree_set! {}, defuns[0].closure());
    let stmt = defuns[0].statements().iter().next().unwrap();
    assert_eq!(btree_set! {"a".into()}, stmt.closure());
    let stmt = stmt.statements().next().unwrap();
    assert_eq!(btree_set! {"a".into()}, stmt.closure());
    let stmt = stmt.statements().next().unwrap();
    assert_eq!(btree_set! {"a".into(), "b".into()}, stmt.closure());
    Ok(())
}

#[test]
fn def_with_let_and_lambda_with_external_bindings() -> Result<(), Error> {
    let atoms = parse("(def test(a) (let ((inc . (\\ (b) (+ a b)))) (- 2 (inc 1))))");
    let defuns: Vec<_> = atoms
        .into_iter()
        .map(|v| TopLevelStatement::from_atom(v, &Default::default()))
        .collect::<Result<_, _>>()?;
    println!("{:?}", defuns);
    println!("{:?}", defuns[0].closure());
    let stmt = defuns[0].statements().iter().next().unwrap();
    println!("{:?}", stmt.closure());
    let stmt = stmt.statements().next().unwrap();
    println!("{:?}", stmt.closure());
    Ok(())
}
