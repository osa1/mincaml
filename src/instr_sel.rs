#![allow(dead_code)]

// Instruction selection
//
// Converts closure-converted program to the x86_64 subset, using virtual registers as instruction
// arguments.
//
// Integer binary operations
// -------------------------
//
// let x = a + b -> movq a, x; addq b, x;
// let x = a - b -> movq a, x; subq b, x;
//
// Integer unary operations
// ------------------------
//
// let x = - y -> movq y x; negq x:
//
// Floats
// ------
//
// - MOVSD — Move or Merge Scalar Double-Precision Floating-Point Value
// - UCOMISD — Unordered Compare Scalar Double-Precision Floating-Point Values and Set EFLAGS
//
// Conditionals
// ------------
//
// if x == y then e1 else e2 (when x is integer) ->
//      cmpq x y;
//      je e1;
//    e2:
//      ...
//    e1:
//      ...
//
//
// if x <= y then e1 else e2 (when x is integer) ->
//       cmpq x y;
//       jle e1;
//    e2:
//       ...
//    e1:
//       ...

use crate::closure_convert::{Fun, Expr, Id};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Reg {
    RAX,
    XMM0,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Operand {
    Imm(u64),
    Reg(Reg),
    Var(Id),
}

pub type Label = String;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Instr {
    Movq(Operand, Operand),
    Addq(Operand, Operand),
    Subq(Operand, Operand),
    Cmpq(Operand, Operand),
    Je(Label),
    Call(Label),
    Pushq(Operand),
    Popq(Operand),
}

pub struct InstrSel {}

impl InstrSel {
    pub fn new() -> InstrSel {
        InstrSel {}
    }

    pub fn instr_sel(&mut self, _f: Fun) -> Vec<Instr> {
        todo!()
    }
}
