use std::{
    io::{Read, Write},
    path::Path,
};

use clap::{Parser, ValueEnum, arg};
use inkwell::context::Context;
use sl::{Compiler as CompilerTrait, bytecode, llvm, reader::ListsParser};
use strum_macros::Display;
use thiserror::Error;

#[derive(Clone, Default, Display, ValueEnum)]
#[strum(serialize_all = "kebab-case")]
enum Mode {
    #[default]
    ByteCode,
    LlvmIr,
}

#[derive(Parser)]
struct Arguments {
    #[arg(short, long)]
    file: String,
    #[arg(short, long, default_value_t)]
    mode: Mode,
    #[arg(short, long)]
    output: String,
}

#[derive(Debug, Error)]
enum Error {
    #[error(transparent)]
    Compile(#[from] sl::error::Error),
    #[error(transparent)]
    Encode(#[from] bincode::error::EncodeError),
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error("Parse error: {0}")]
    Parse(String),
}

fn main() -> Result<(), Error> {
    //
    // Parse the arguments.
    //
    let args = Arguments::parse();
    //
    // Open the source file.
    //
    let mut source = String::new();
    let mut file = std::fs::File::open(&args.file)?;
    file.read_to_string(&mut source)?;
    //
    // Create the compiler.
    //
    match args.mode {
        Mode::ByteCode => {
            let mut compiler = bytecode::Compiler::default();
            compiler.lift_operators()?;
            //
            // Parse the source file.
            //
            let parser = ListsParser::new();
            let _ = parser
                .parse(&mut compiler, &source)
                .map_err(|v| Error::Parse(v.to_string()))?;
            //
            // Generate the bytecode.
            //
            let artifacts = compiler.compile("main")?;
            //
            // Write the serialize output.
            //
            let conf = bincode::config::standard();
            let mut file = std::fs::File::create(&args.output)?;
            bincode::encode_into_std_write(artifacts, &mut file, conf)?;
        }
        Mode::LlvmIr => {
            let context = Context::create();
            //
            // Create the LLVM state.
            //
            let path = Path::new(&args.file);
            let stem = path.file_stem().unwrap().to_string_lossy();
            let name = stem.as_ref();
            let state = llvm::Proxy::new(&context, name)?;
            //
            // Build the compiler that will be used for tilde-expansion.
            //
            let mut compiler = bytecode::Compiler::default();
            //
            // Parse the source file.
            //
            let parser = ListsParser::new();
            let atoms = parser
                .parse(&mut compiler, &source)
                .map_err(|v| Error::Parse(v.to_string()))?;
            //
            // Compile the statements using the LLVM compiler.
            //
            let mut compiler = llvm::Compiler::new(state.into());
            atoms.into_iter().try_for_each(|v| compiler.load(v))?;
            let artifacts = compiler.compile("main")?;
            //
            // Write the serialize output.
            //
            let mut file = std::fs::File::create(&args.output)?;
            file.write_all(artifacts.as_bytes())?;
        }
    }
    //
    // Done.
    //
    Ok(())
}
