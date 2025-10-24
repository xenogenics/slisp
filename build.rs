use vergen::{Emitter, RustcBuilder};

extern crate lalrpop;

fn main() {
    //
    // LALRpop.
    //
    lalrpop::process_root().unwrap();
    //
    // Vergen.
    //
    let rustc = RustcBuilder::all_rustc().unwrap();
    Emitter::default()
        .add_instructions(&rustc)
        .unwrap()
        .emit()
        .unwrap();
    //
    // Rerun if IR files have changes.
    //
    println!("cargo:rerun-if-changed=src/llvm/helpers.ll");
}
