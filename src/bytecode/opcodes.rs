use bincode::{Decode, Encode};

use crate::bytecode::value::Immediate;

//
// Opcodes.
//

#[derive(Clone, Copy, Debug, PartialEq, Eq, Encode, Decode)]
#[repr(u8)]
pub enum OpCode {
    //
    // Function application.
    //
    Apply,
    //
    // Arithmetics.
    //
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Ge,
    Gt,
    Le,
    Lt,
    //
    // Logic operations.
    //
    And,
    Equ,
    Neq,
    Not,
    Or,
    //
    // Bit operations.
    //
    BitAnd,
    BitNot,
    BitOr,
    BitXor,
    //
    // List operations.
    //
    Car,
    Cdr,
    Conc,
    Cons,
    //
    // Bytes, string, and symbol operations.
    //
    Bytes,
    Chr,
    Str,
    Sym,
    Unpack,
    //
    // Predicates.
    //
    IsByt,
    IsChr,
    IsLst,
    IsNil,
    IsNum,
    IsStr,
    IsSym,
    IsTru,
    IsWld,
    //
    // Control flow.
    //
    Br(isize),
    Brn(isize),
    Call(usize),
    Ret,
    //
    // Stack operations.
    //
    Dup(usize),
    Get(usize),
    Lst(usize),
    Pak(usize, usize),
    Pop(usize),
    Psh(Immediate),
    Rot(usize),
    Rtm(usize, usize),
    Swp,
}

impl std::fmt::Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Apply => write!(f, "Apply"),
            OpCode::Add => write!(f, "Add"),
            OpCode::Sub => write!(f, "Sub"),
            OpCode::Mul => write!(f, "Mul"),
            OpCode::Div => write!(f, "Div"),
            OpCode::Mod => write!(f, "Mod"),
            OpCode::Ge => write!(f, "Ge"),
            OpCode::Gt => write!(f, "Gt"),
            OpCode::Le => write!(f, "Le"),
            OpCode::Lt => write!(f, "Lt"),
            OpCode::And => write!(f, "And"),
            OpCode::Equ => write!(f, "Equ"),
            OpCode::Neq => write!(f, "Neq"),
            OpCode::Not => write!(f, "Not"),
            OpCode::Or => write!(f, "Or"),
            OpCode::BitAnd => write!(f, "BitAnd"),
            OpCode::BitNot => write!(f, "BitNot"),
            OpCode::BitOr => write!(f, "BitOr"),
            OpCode::BitXor => write!(f, "BitXor"),
            OpCode::Car => write!(f, "Car"),
            OpCode::Cdr => write!(f, "Cdr"),
            OpCode::Conc => write!(f, "Conc"),
            OpCode::Cons => write!(f, "Cons"),
            OpCode::Bytes => write!(f, "Bytes"),
            OpCode::Chr => write!(f, "Chr"),
            OpCode::Unpack => write!(f, "Split"),
            OpCode::Str => write!(f, "Str"),
            OpCode::Sym => write!(f, "Sym"),
            OpCode::IsByt => write!(f, "IsByt"),
            OpCode::IsChr => write!(f, "IsChr"),
            OpCode::IsLst => write!(f, "IsLst"),
            OpCode::IsNil => write!(f, "IsNil"),
            OpCode::IsNum => write!(f, "IsNum"),
            OpCode::IsStr => write!(f, "IsStr"),
            OpCode::IsSym => write!(f, "IsSym"),
            OpCode::IsTru => write!(f, "IsTru"),
            OpCode::IsWld => write!(f, "IsWld"),
            OpCode::Br(v) => write!(f, "Br({v})"),
            OpCode::Brn(v) => write!(f, "Brn({v})"),
            OpCode::Call(v) => write!(f, "Call({v})"),
            OpCode::Ret => write!(f, "Ret"),
            OpCode::Dup(n) => write!(f, "Dup({n})"),
            OpCode::Get(n) => write!(f, "Get({n})"),
            OpCode::Lst(n) => write!(f, "Lst({n})"),
            OpCode::Pak(m, n) => write!(f, "Pak({m},{n})"),
            OpCode::Pop(n) => write!(f, "Pop({n})"),
            OpCode::Psh(v) => write!(f, "Psh({v})"),
            OpCode::Rot(n) => write!(f, "Rot({n})"),
            OpCode::Rtm(m, n) => write!(f, "Rtm({m},{n})"),
            OpCode::Swp => write!(f, "Swp"),
        }
    }
}

pub type OpCodes = Vec<OpCode>;
