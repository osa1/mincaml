#![allow(dead_code, unused_imports, unreachable_code, unused_variables)]

mod instr;
mod liveness;
mod types;

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

use fxhash::{FxHashMap, FxHashSet};

use crate::anormal::BinOp;
use crate::closure_convert as cc;
use crate::common::*;
use crate::ctx::{Ctx, VarId};

use instr::*;
use types::*;

pub(crate) fn cg_fun(cc_fun: &cc::Fun) -> Fun {
    let cc::Fun {
        name,
        args,
        blocks,
        return_type,
    } = cc_fun;

    let (mut fun, entry_block) = Fun::new();

    // Map cc blocks to codegen blocks (TODO: would be good to eliminate this step)
    let mut label_to_block: FxHashMap<cc::Label, BlockIdx> = Default::default();

    for block in blocks {
        let cg_block = fun.create_block();
        label_to_block.insert(block.label, cg_block);
    }

    let mut env: FxHashMap<VarId, Arg> = Default::default();

    // TODO: Define function arguments in entry block
    for cc::Block { label, stmts, exit } in blocks {
        let mut cg_block = *label_to_block.get(label).unwrap();
        fun.switch_to_block(cg_block);

        for asgn in stmts {
            // let mut s = String::new();
            // asgn.pp(&ctx, &mut s);
            // println!("stmt: {}", s);

            let cc::Asgn { lhs, rhs } = asgn;

            let (block, val) = cg_expr(&mut fun, cg_block, rhs);
            cg_block = block;

            /*
                        let lhs_cl_var = Variable::new(ctx.get_var(*lhs).get_uniq().0.get() as usize);
                        let lhs_abi_type = rep_type_abi(ctx.var_rep_type(*lhs));
                        builder.declare_var(lhs_cl_var, lhs_abi_type);
                        builder.def_var(lhs_cl_var, val);
            */
        }
    }

    todo!()
}

fn cg_expr(fun: &mut Fun, block: BlockIdx, expr: &cc::Expr) -> (BlockIdx, Arg) {
    match expr {
        cc::Expr::Atom(cc::Atom::Unit) => (block, Arg::Imm(0)),
        cc::Expr::Atom(cc::Atom::Int(i)) => (block, Arg::Imm(*i)),
        cc::Expr::Atom(cc::Atom::Float(f)) => {
            todo!()
        }
        _ => todo!()
    }
}

/*
#[derive(Debug, Default)]
struct Module {
    // Index with BlockIdx
    succs: Vec<Vec<BlockIdx>>,
    // Index with BlockIdx
    preds: Vec<Vec<BlockIdx>>,
    // Index with BlockIdx
    block_instrs: Vec<Vec<InstrIdx>>,
    // Index with InstrIdx
    instrs: Vec<Instr>,
    // Index with InstrIdx
    live_ins: Vec<FxHashSet<Arg>>,
    // Index with InstrIdx
    live_outs: Vec<FxHashSet<Arg>>,

    current_block: BlockIdx,

    // Counters for generating blocks and instructions
    block_count: u32,
    instr_count: u32,
    var_count: u32,
}

struct ModuleBuilder<'ctx> {
    ctx: &'ctx mut Ctx,
    module: Module,
}

impl<'ctx> ModuleBuilder<'ctx> {
    fn new(ctx: &'ctx mut Ctx) -> Self {
        ModuleBuilder {
            ctx,
            module: Default::default(),
        }
    }

    fn create_block(&mut self) -> BlockIdx {
        let block_idx = self.module.block_count;
        self.module.block_count += 1;

        assert!(self.module.succs.len() == block_idx as usize);
        self.module.succs.push(vec![]);

        assert!(self.module.preds.len() == block_idx as usize);
        self.module.preds.push(vec![]);

        assert!(self.module.block_instrs.len() == block_idx as usize);
        self.module.block_instrs.push(vec![]);

        BlockIdx(block_idx)
    }

    fn new_instr(&mut self, instr: Instr) -> InstrIdx {
        let instr_idx = self.module.instr_count;
        self.module.instr_count += 1;

        assert!(self.module.instrs.len() == instr_idx as usize);
        self.module.instrs.push(instr);

        assert!(self.module.live_ins.len() == instr_idx as usize);
        self.module.live_ins.push(Default::default());

        assert!(self.module.live_outs.len() == instr_idx as usize);
        self.module.live_outs.push(Default::default());

        InstrIdx(instr_idx)
    }

    fn new_var(&mut self) -> VarIdx {
        let var_idx = self.module.var_count;
        self.module.var_count += 1;

        VarIdx(var_idx)
    }

    fn iconst(&mut self, i: i64) -> Arg {
        let var = self.new_var();
        let _instr = self.new_instr(Instr::Movq {
            src: Arg::Imm(i),
            dest: Arg::Var(var),
        });
        Arg::Var(var)
    }
}


fn instr_sel(builder: &mut ModuleBuilder, funs: &[cc::Fun]) {
    for fun in funs {
        instr_sel_fun(builder, fun);
    }
}

fn instr_sel_fun(builder: &mut ModuleBuilder, fun: &cc::Fun) {
    // Generate code
    // TODO
    let entry_block: BlockIdx = todo!();

    // Generate live-ins and live-outs
    let mut did_update = true;
    while did_update {
        did_update = false;
        for n_block in 0..fun.blocks.len() {
            let block = BlockIdx(n_block as u32 + entry_block.0);
            did_update |= update_lives(builder, block);
        }
    }
}

fn update_lives(builder: &mut ModuleBuilder, block: BlockIdx) -> bool {
    let mut did_update = false;

    let instrs: &[InstrIdx] = &builder.module.block_instrs[block.0 as usize];

    for instr_idx in instrs {
        // let instr_ins = &mut builder.module.live_ins[instr_idx.0 as usize];
        // let instr_outs = &mut builder.module.live_outs[instr_idx.0 as usize];

        let instr = &builder.module.instrs[instr_idx.0 as usize];
        match instr {
            Instr::Movq { src, dest } => {
                // successor of the instruction
                let succ = instr_idx.0 + 1;
                // live-in of the successor
                let succ_live_ins: &FxHashSet<Arg> = &builder.module.live_ins[succ as usize];

                for succ_live_in in succ_live_ins.iter() {}

                // instr_ins.insert((*src).clone());
            }
            Instr::Jmp { target } => {}
            _ => todo!(),
        }
    }

    did_update
}

fn instr_sel_expr(
    builder: &mut ModuleBuilder, block: BlockIdx, expr: &cc::Expr,
) -> (BlockIdx, Arg) {
    match expr {
        cc::Expr::Atom(cc::Atom::Unit) => (block, builder.iconst(0)),
        _ => todo!(),
    }
}

/*
fn instr_sel_block(builder: &mut ModuleBuilder, block: BlockIdx, cc_block: cc::Block) -> (BlockIdx, Arg) {

    for cc::Asgn { lhs, rhs } in cc_block.stmts {

    }

    todo!()
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
    */
*/
