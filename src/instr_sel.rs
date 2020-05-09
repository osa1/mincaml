// Conditionals
// ------------
//
// Conditionals in the language: >, >=, <, <=, ==, <>
//
// In x86, for words:
//
// - Set EFLAGS with cmp
//
// Then
//
// - jg: jump if greater, signed
// - jge: jump if greater or equal, signed
// - jl: jump if less, signed
// - jle: jump if less or equal, signed
// - je: jump if equal
// - jne: jump if not equal
//
// (Reference: http://unixwiz.net/techtips/x86-jumps.html)
//
// For floats:
//
// - Set EFLAGS with ucomisd: "Unordered Compare Scalar Double-Precision Floating-Point Values"
//
// (Reference: https://www.felixcloutier.com/x86/ucomisd)
//
// Use the same branching instructions.
//
// Moving floats
// -------------
//
// - MOVSD — Move or Merge Scalar Double-Precision Floating-Point Value
//   Example: `movsd %xmm0, -32(%rbp)` to move a double to a struct field.
//            `movsd -56(%rbp), %xmm0` the other way around
//
// Integer arithmetic
// ------------------
//
// - addq/subq
//
// (Reference: gcc -S output)
//
// Float arithmetic
// ----------------
//
// - addsd/subsd/divsd/mulsd
//
// Integer to double example: `cvtsi2sdq %rax, %xmm0` (NOTE: I don't think we need this tho?)
//
// (Reference: gcc -S output)
//
// Registers
// ---------
//
// rsp and rbp are used for stack manipulation. Remaining args are used for parameter passing in
// this order:
//
// - rax, rcx, rdx, rbx, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15 (14 regs)
// - For doubles: xmm0 - xmm7 (TODO: can we not use the rest?)
//
// All registers are callee-save. (TODO: Should we follow ABI? I can't see any reason..)

#![allow(dead_code)]

use crate::closure_convert;
use crate::closure_convert::{Asgn, Atom, Expr, Fun, Label};
use crate::common::*;
use crate::ctx::{Ctx, VarId};
use crate::knormal::BinOp;

#[rustfmt::skip]
#[derive(Debug)]
pub enum WordReg {
    RAX, RCX, RDX, RBX, RSI, RDI, RSP, RBP, R8, R9, R10, R11, R12, R13, R14, R15,
}

#[rustfmt::skip]
#[derive(Debug)]
pub enum DoubleReg {
    XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7
}

#[derive(Debug)]
pub enum Arg {
    Var(VarId),
    Imm(i64),
    Mem(Label),
    WordReg(WordReg),
    DoubleReg(DoubleReg),
}

#[derive(Debug)]
pub enum Instr {
    Movq { src: Arg, dest: Arg },
    Addq { src: Arg, dest: Arg },
    Subq { src: Arg, dest: Arg },
    Negq { src: Arg },
    // TODO: What's the upper bound on offsets?
    // movq arg1, offset(arg2). arg2 needs to be a Mem after register allocation
    MovqKnownDestOffset { src: Arg, dest: Arg, offset: usize },
    // movq offset(arg1), arg2. arg1 needs to be a Mem after register allocation
    MovqKnownSrcOffset { src: Arg, dest: Arg, offset: usize },
    // movq (%rax,%rdx,8), %rdx
    MovqRelativeSrc { src: Arg, offset: Arg, dest: Arg },
    // movq $123, (%rax,%rdx,8)
    MovqRelativeDest { src: Arg, dest: Arg, offset: Arg },
}

#[derive(Debug)]
pub struct Block {
    label: Label,
    instrs: Vec<Instr>,
}

#[derive(Debug)]
pub struct Program {
    pub blocks: Vec<Block>,
    pub floats: Vec<(Label, f64)>,
}

struct IsCtx<'ctx> {
    ctx: &'ctx mut Ctx,
    blocks: Vec<Block>,
    floats: Vec<(Label, f64)>,
}

impl<'ctx> IsCtx<'ctx> {
    fn fresh_label(&mut self) -> Label {
        self.ctx.fresh_label()
    }
}

pub fn instr_sel(ctx: &mut Ctx, fun: Fun) -> Program {
    let mut ctx = IsCtx { ctx, blocks: Vec::with_capacity(fun.blocks.len()), floats: vec![] };

    for block in fun.blocks {
        let block = instr_sel_block(&mut ctx, block);
        ctx.blocks.push(block);
    }

    Program { blocks: ctx.blocks, floats: ctx.floats }
}

fn instr_sel_block(ctx: &mut IsCtx, block: closure_convert::Block) -> Block {
    let mut instrs: Vec<Instr> = Vec::with_capacity(block.stmts.len());

    for Asgn { lhs, rhs } in block.stmts {
        match rhs {
            Expr::Atom(Atom::Unit) => {
                instrs.push(Instr::Movq { src: Arg::Imm(0), dest: Arg::Var(lhs) });
            }
            Expr::Atom(Atom::Int(i)) => {
                // TODO: Immediate argument can be 64-bit wide?
                instrs.push(Instr::Movq { src: Arg::Imm(i), dest: Arg::Var(lhs) });
            }
            Expr::Atom(Atom::Float(f)) => {
                // I think there float immediate is not a thing, we need to go through memory
                let label = ctx.fresh_label();
                ctx.floats.push((label, f));
                instrs.push(Instr::Movq { src: Arg::Mem(label), dest: Arg::Var(lhs) });
            }
            Expr::Atom(Atom::Var(var)) => {
                instrs.push(Instr::Movq { src: Arg::Var(var), dest: Arg::Var(lhs) });
            }
            Expr::IBinOp(BinOp { op, arg1, arg2 }) => {
                instrs.push(Instr::Movq { src: Arg::Var(arg1), dest: Arg::Var(lhs) });
                instrs.push(match op {
                    IntBinOp::Add => Instr::Addq { src: Arg::Var(arg2), dest: Arg::Var(lhs) },
                    IntBinOp::Sub => Instr::Subq { src: Arg::Var(arg2), dest: Arg::Var(lhs) },
                });
            }
            Expr::FBinOp(BinOp { op: _, arg1: _, arg2: _ }) => todo!(),
            Expr::Neg(arg) => {
                instrs.push(Instr::Movq { src: Arg::Var(arg), dest: Arg::Var(lhs) });
                instrs.push(Instr::Negq { src: Arg::Var(lhs) });
            }
            Expr::FNeg(_) => todo!(),
            Expr::App(_, _) => todo!(),
            Expr::ExtApp(_, _) => todo!(),
            Expr::Tuple(_) => todo!(),
            Expr::TupleIdx(tuple, idx) => {
                // TODO: Update after adding closure header
                instrs.push(Instr::MovqKnownSrcOffset {
                    src: Arg::Var(tuple),
                    offset: idx,
                    dest: Arg::Var(lhs),
                });
            }
            Expr::Get(array, Atom::Var(offset)) => {
                instrs.push(Instr::MovqRelativeSrc {
                    src: Arg::Var(array),
                    offset: Arg::Var(offset),
                    dest: Arg::Var(lhs),
                });
            }
            Expr::Get(array, Atom::Int(offset)) => {
                instrs.push(Instr::MovqKnownSrcOffset {
                    src: Arg::Var(array),
                    offset: offset as usize,
                    dest: Arg::Var(lhs),
                });
            }
            Expr::Put(array, offset, val) => todo!(),
            _ => todo!(),
        }
    }

    todo!()
}

fn encode_double(d: f64) -> [u8; 8] {
    let arr: [f64; 1] = [d];
    unsafe { ::std::mem::transmute(arr) }
}

#[test]
fn double_encode_test() {
    assert_eq!(
        encode_double(237.8325f64),
        [113, 61, 10, 215, 163, 186, 109, 64]
    );
}
