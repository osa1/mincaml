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
//
// Random notes
// ------------
//
// According to Modern Comp. Impl. by Appel some compilers do instr sel after reg alloc to avoid
// modelling instructions without machine registers (as we do below). I don't understand how that
// is possible -- we only know about reg requirements *after* deciding on which instructions to
// use. For example a floating point addition will require different regs than an integer addition.
// Some instructions allow immediate values while others require mem/reg operands etc.

#![allow(dead_code)]

use crate::closure_convert;
use crate::closure_convert::{Asgn, Atom, Exit, Expr, Fun, Label};
use crate::common::*;
use crate::ctx::{Ctx, VarId};
use crate::knormal::BinOp;

use fxhash::{FxHashMap, FxHashSet};

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
    // A call instruction. Arguments will be moved to their argument registers in register
    // allocation -- they're not used in the final instruction, which just looks like `call f`.
    Call { fun: Arg, args: Vec<Arg>, dest: Arg },
    // ret instruction doesn't have an operand, the argument is to be used in register allocation
    // to move the return value to the its register according to the calling convention.
    // None is for returning unit. (TODO: Do we really need this special case? Why not return 0?)
    Ret { value: Option<Arg> },
    Jmp { target: Label },
    // The conditional jump after this will be taken based on whether `arg1 cmp arg2` holds.
    Cmp { arg1: Arg, arg2: Arg },

    // NOTE: We don't need all kinds of conditional jumps to map our 6 comparison ops, but it's
    // easier to debug then the mapping is more direct. So we map our 6 comparison ops to 6 x86
    // instructions.

    // NOTE: These insturctions only have one operand, the "else_" arguments are here to help with
    // CFG generation. When printing instructions we generate e.g. `je label1; jmp label2;`.

    // Jump if arg1 == arg2
    Je { target: Label, else_: Label },
    // Jump if arg1 != arg2
    Jne { target: Label, else_: Label },
    // Jump if arg1 < arg2
    Jl { target: Label, else_: Label },
    // Jump if arg1 <= arg2
    Jle { target: Label, else_: Label },
    // Jump if arg1 > arg2
    Jg { target: Label, else_: Label },
    // Jump if arg1 >= arg2
    Jge { target: Label, else_: Label },
}

#[derive(Debug)]
pub struct Block {
    label: Label,
    instrs: Vec<Instr>,
}

#[derive(Debug)]
pub struct BlockDetails {
    // Live variables on incoming edges of instructions.
    live_ins: Vec<FxHashSet<VarId>>,
    // Live variables on outgoing edges of instruction.
    live_outs: Vec<FxHashSet<VarId>>,
    // Block indices of successors of this block.
    succs: Vec<usize>,
    // Block indices of predecessors of this block.
    preds: Vec<usize>,
}

pub struct IsCtx<'ctx> {
    pub ctx: &'ctx mut Ctx,
    // Blocks in the program. Includes all functions.
    pub blocks: Vec<Block>,
    // Successors, predecessors etc. are held in a separate field to avoid borrow checking issues
    pub block_details: Vec<BlockDetails>,
    // Maps block labels to their indices
    pub block_indices: FxHashMap<Label, usize>,
    // Generated float literals
    pub floats: Vec<(Label, f64)>,
}

impl<'ctx> IsCtx<'ctx> {
    fn fresh_label(&mut self) -> Label {
        self.ctx.fresh_label()
    }
}

pub fn instr_sel(ctx: &mut IsCtx, fun: Fun) {
    assert_eq!(ctx.blocks.len(), ctx.block_details.len());

    let first_block_idx = ctx.blocks.len();

    // Allocate an index for each block so that we can generate successors and predecessors when
    // generating instruction in instr_sel_block
    for (block_idx, block) in fun.blocks.iter().enumerate() {
        ctx.block_details.push(BlockDetails {
            live_ins: vec![],
            live_outs: vec![],
            succs: vec![],
            preds: vec![],
        });
        ctx.block_indices
            .insert(block.label, first_block_idx + block_idx);
    }

    // Generate instructions, add predecessor and sucessors
    for (block_idx, block) in fun.blocks.iter().enumerate() {
        let block = instr_sel_block(ctx, block, first_block_idx + block_idx);
        ctx.blocks.push(block);
    }

    // Generate live-ins and live-outs
    // TODO
    // This will cause a lot of borrow checking issues ...
    //
    // let mut did_update: bool = true;
    // while did_update {
    //     did_update = false;
    //     for block_idx in 0 .. fun.blocks().len() {
    //         let block_idx = first_block_idx + block_idx;

    //     }
    // }
}

fn instr_sel_block(ctx: &mut IsCtx, block: &closure_convert::Block, block_idx: usize) -> Block {
    let mut instrs: Vec<Instr> = Vec::with_capacity(block.stmts.len());

    for Asgn { lhs, rhs } in &block.stmts {
        match rhs {
            Expr::Atom(Atom::Unit) => {
                instrs.push(Instr::Movq {
                    src: Arg::Imm(0),
                    dest: Arg::Var(*lhs),
                });
            }
            Expr::Atom(Atom::Int(i)) => {
                // TODO: Immediate argument can be 64-bit wide?
                instrs.push(Instr::Movq {
                    src: Arg::Imm(*i),
                    dest: Arg::Var(*lhs),
                });
            }
            Expr::Atom(Atom::Float(f)) => {
                // I think there float immediate is not a thing, we need to go through memory
                let label = ctx.fresh_label();
                ctx.floats.push((label, *f));
                instrs.push(Instr::Movq {
                    src: Arg::Mem(label),
                    dest: Arg::Var(*lhs),
                });
            }
            Expr::Atom(Atom::Var(var)) => {
                instrs.push(Instr::Movq {
                    src: Arg::Var(*var),
                    dest: Arg::Var(*lhs),
                });
            }
            Expr::IBinOp(BinOp { op, arg1, arg2 }) => {
                instrs.push(Instr::Movq {
                    src: Arg::Var(*arg1),
                    dest: Arg::Var(*lhs),
                });
                instrs.push(match op {
                    IntBinOp::Add => Instr::Addq {
                        src: Arg::Var(*arg2),
                        dest: Arg::Var(*lhs),
                    },
                    IntBinOp::Sub => Instr::Subq {
                        src: Arg::Var(*arg2),
                        dest: Arg::Var(*lhs),
                    },
                });
            }
            Expr::FBinOp(BinOp {
                op: _,
                arg1: _,
                arg2: _,
            }) => todo!(),
            Expr::Neg(arg) => {
                instrs.push(Instr::Movq {
                    src: Arg::Var(*arg),
                    dest: Arg::Var(*lhs),
                });
                instrs.push(Instr::Negq {
                    src: Arg::Var(*lhs),
                });
            }
            Expr::FNeg(_) => todo!(),
            Expr::App(fun, args) => {
                let args = args.iter().map(|var| Arg::Var(*var)).collect();
                instrs.push(Instr::Call {
                    fun: Arg::Var(*fun),
                    args,
                    dest: Arg::Var(*lhs),
                });
            }
            Expr::ExtApp(_, _) => todo!(),
            Expr::Tuple(_) => todo!(),
            Expr::TupleIdx(tuple, idx) => {
                // TODO: Update after adding closure header
                instrs.push(Instr::MovqKnownSrcOffset {
                    src: Arg::Var(*tuple),
                    offset: *idx,
                    dest: Arg::Var(*lhs),
                });
            }
            Expr::Get(array, Atom::Var(offset)) => {
                instrs.push(Instr::MovqRelativeSrc {
                    src: Arg::Var(*array),
                    offset: Arg::Var(*offset),
                    dest: Arg::Var(*lhs),
                });
            }
            Expr::Get(array, Atom::Int(offset)) => {
                instrs.push(Instr::MovqKnownSrcOffset {
                    src: Arg::Var(*array),
                    offset: *offset as usize,
                    dest: Arg::Var(*lhs),
                });
            }
            Expr::Get(_, other) => {
                panic!("Unexpected array offset in get expression: {:?}", other);
            }
            Expr::Put(_array, _offset, _val) => todo!(),
        }
    }

    match &block.exit {
        Exit::Return(opt_var) => {
            instrs.push(Instr::Ret {
                value: opt_var.map(Arg::Var),
            });
        }
        Exit::Jump(target) => {
            instrs.push(Instr::Jmp { target: *target });
            let target_block_idx = *ctx.block_indices.get(&target).unwrap();
            ctx.block_details[target_block_idx].preds.push(block_idx);
            ctx.block_details[block_idx].succs.push(target_block_idx);
        }
        Exit::Branch {
            v1,
            v2,
            cond,
            then_label,
            else_label,
        } => {
            instrs.push(Instr::Cmp {
                arg1: Arg::Var(*v1),
                arg2: Arg::Var(*v2),
            });
            instrs.push(match cond {
                Cmp::Equal => Instr::Je {
                    target: *then_label,
                    else_: *else_label,
                },
                Cmp::NotEqual => Instr::Jne {
                    target: *then_label,
                    else_: *else_label,
                },
                Cmp::LessThan => Instr::Jl {
                    target: *then_label,
                    else_: *else_label,
                },
                Cmp::LessThanOrEqual => Instr::Jle {
                    target: *then_label,
                    else_: *else_label,
                },
                Cmp::GreaterThan => Instr::Jg {
                    target: *then_label,
                    else_: *else_label,
                },
                Cmp::GreaterThanOrEqual => Instr::Jge {
                    target: *then_label,
                    else_: *else_label,
                },
            });
            let then_block_idx = *ctx.block_indices.get(&then_label).unwrap();
            let else_block_idx = *ctx.block_indices.get(&else_label).unwrap();
            ctx.block_details[then_block_idx].preds.push(block_idx);
            ctx.block_details[else_block_idx].preds.push(block_idx);
            ctx.block_details[block_idx].succs.push(then_block_idx);
            ctx.block_details[block_idx].succs.push(else_block_idx);
        }
    }

    Block {
        label: block.label,
        instrs,
    }
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
