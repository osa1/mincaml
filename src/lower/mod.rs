mod print;
mod types;

use crate::cg_types::RepType;
use crate::closure_convert as cc;
use crate::common::{BinOp, Cmp, IntBinOp};
use crate::ctx::{Ctx, VarId};
use crate::type_check::Type;
use crate::var::CompilerPhase::ClosureConvert;

pub use print::*;
pub use types::*;

use cranelift_entity::PrimaryMap;

// Used when debugging
#[allow(unused_imports)]
use crate::utils;

#[derive(Debug, Clone)]
enum Sequel {
    Return,

    /// Assign return value to this variable and jump to the label. Used when lowering let
    /// bindings.
    Asgn(VarId, BlockIdx),
}

impl Sequel {
    fn get_ret_var(&self, ctx: &mut CcCtx, ret_ty: RepType) -> VarId {
        use Sequel::*;
        match self {
            Asgn(var, _) => *var,
            Return => ctx.fresh_var(ret_ty),
        }
    }
}

// A block currently being built
struct BlockBuilder {
    idx: BlockIdx,
    stmts: Vec<Stmt>,
    comment: Option<String>,
}

impl BlockBuilder {
    fn new(idx: BlockIdx) -> Self {
        Self { idx, stmts: vec![], comment: None }
    }

    fn asgn(&mut self, lhs: VarId, rhs: Expr) {
        self.stmts.push(Stmt::Asgn(Asgn { lhs, rhs }));
    }

    fn expr(&mut self, expr: Expr) {
        self.stmts.push(Stmt::Expr(expr));
    }
}

// Closure conversion state
struct CcCtx<'ctx> {
    ctx: &'ctx mut Ctx,
    // Blocks generated so far for the current function
    blocks: PrimaryMap<BlockIdx, BlockData>,
}

impl<'ctx> CcCtx<'ctx> {
    fn new(ctx: &'ctx mut Ctx) -> Self {
        Self { ctx, blocks: PrimaryMap::new() }
    }

    fn fresh_var(&mut self, rep_type: RepType) -> VarId {
        self.ctx.fresh_codegen_var(ClosureConvert, rep_type)
    }

    fn create_block(&mut self) -> BlockBuilder {
        let idx = self.blocks.push(BlockData::NA);
        BlockBuilder::new(idx)
    }

    fn finish_block(&mut self, block: BlockBuilder, sequel: Sequel, value: Atom) {
        let BlockBuilder { idx, mut stmts, comment } = block;

        let exit = match sequel {
            Sequel::Return => match value {
                Atom::Unit => {
                    let tmp = self.fresh_var(RepType::Word);
                    stmts.push(Stmt::Asgn(Asgn { lhs: tmp, rhs: Expr::Atom(Atom::Unit) }));
                    Exit::Return(tmp)
                }
                Atom::Int(i) => {
                    let tmp = self.fresh_var(RepType::Word);
                    stmts.push(Stmt::Asgn(Asgn { lhs: tmp, rhs: Expr::Atom(Atom::Int(i)) }));
                    Exit::Return(tmp)
                }
                Atom::Float(f) => {
                    let tmp = self.fresh_var(RepType::Float);
                    stmts.push(Stmt::Asgn(Asgn {
                        lhs: tmp,
                        rhs: Expr::Atom(Atom::Float(f)),
                    }));
                    Exit::Return(tmp)
                }
                Atom::Var(var) => Exit::Return(var),
            },
            Sequel::Asgn(lhs, label) => {
                match value {
                    // TODO: Should we handle this case in the call site? Or make it impossible to
                    // happen somehow?
                    Atom::Var(rhs) if lhs == rhs => {}
                    _ => {
                        stmts.push(Stmt::Asgn(Asgn { lhs, rhs: Expr::Atom(value) }));
                    }
                }
                Exit::Jump(label)
            }
        };

        let block = Block { idx, comment, stmts, exit };

        self.finish_block_(block);
    }

    fn finish_block_(&mut self, block: Block) {
        let idx = block.idx;
        assert!(self.blocks[idx].is_NA());
        self.blocks[idx] = BlockData::Block(block);
    }
}

pub fn lower_fun(ctx: &mut Ctx, fun: cc::Fun) -> Fun {
    let mut ctx = CcCtx::new(ctx);

    let cc::Fun { name, args, body, return_type } = fun;

    let block = ctx.create_block();
    cc_block(&mut ctx, block, Sequel::Return, body);

    Fun { name, args, blocks: ctx.blocks, return_type }
}

// Returns whether the added block was a fork (i.e. then or else branch of an if)
fn cc_block(ctx: &mut CcCtx, mut block: BlockBuilder, sequel: Sequel, expr: cc::Expr) {
    match expr {
        cc::Expr::Unit => ctx.finish_block(block, sequel, Atom::Unit),

        cc::Expr::Int(i) => ctx.finish_block(block, sequel, Atom::Int(i)),

        cc::Expr::Float(f) => ctx.finish_block(block, sequel, Atom::Float(f)),

        cc::Expr::Neg(var) => {
            let tmp = ctx.fresh_var(RepType::Word);
            block.asgn(tmp, Expr::Neg(var));
            ctx.finish_block(block, sequel, Atom::Var(tmp));
        }

        cc::Expr::FNeg(var) => {
            let tmp = ctx.fresh_var(RepType::Float);
            block.asgn(tmp, Expr::FNeg(var));
            ctx.finish_block(block, sequel, Atom::Var(tmp));
        }

        cc::Expr::IBinOp(BinOp { op, arg1, arg2 }) => {
            let tmp = sequel.get_ret_var(ctx, RepType::Word);
            block.asgn(tmp, Expr::IBinOp(BinOp { op, arg1, arg2 }));
            ctx.finish_block(block, sequel, Atom::Var(tmp));
        }

        cc::Expr::FBinOp(BinOp { op, arg1, arg2 }) => {
            let tmp = sequel.get_ret_var(ctx, RepType::Float);
            block.asgn(tmp, Expr::FBinOp(BinOp { op, arg1, arg2 }));
            ctx.finish_block(block, sequel, Atom::Var(tmp));
        }

        cc::Expr::If(v1, v2, cmp, e1, e2) => {
            let then_block = ctx.create_block();
            let else_block = ctx.create_block();
            ctx.finish_block_(Block {
                idx: block.idx,
                comment: block.comment,
                stmts: block.stmts,
                exit: Exit::Branch {
                    v1,
                    v2,
                    cond: cmp,
                    then_block: then_block.idx,
                    else_block: else_block.idx,
                },
            });
            cc_block(ctx, then_block, sequel.clone(), *e1);
            cc_block(ctx, else_block, sequel, *e2);
        }

        cc::Expr::Var(var) => {
            ctx.finish_block(block, sequel, Atom::Var(var));
        }

        cc::Expr::Let { id, rhs, body } => {
            // TODO: When the RHS is not if-then-else we can continue extending the last block RHS
            // generates and avoid creating a block for the continuation.
            let cont_block = ctx.create_block();
            let rhs_sequel = Sequel::Asgn(id, cont_block.idx);
            cc_block(ctx, block, rhs_sequel, *rhs);
            cc_block(ctx, cont_block, sequel, *body)
        }

        cc::Expr::App(fun, args, ret_ty) => {
            let ret_tmp = sequel.get_ret_var(ctx, ret_ty);
            block.asgn(ret_tmp, Expr::App(fun, args, ret_ty));
            ctx.finish_block(block, sequel, Atom::Var(ret_tmp));
        }

        cc::Expr::Tuple(args) => {
            let ret_tmp = sequel.get_ret_var(ctx, RepType::Word);
            block.asgn(ret_tmp, Expr::Tuple { len: args.len() });
            for (arg_idx, arg) in args.iter().enumerate() {
                block.expr(Expr::TuplePut(ret_tmp, arg_idx, *arg));
            }
            ctx.finish_block(block, sequel, Atom::Var(ret_tmp));
        }

        cc::Expr::TupleGet(tuple, idx, elem_ty) => {
            let ret_tmp = sequel.get_ret_var(ctx, elem_ty);
            block.asgn(ret_tmp, Expr::TupleGet(tuple, idx, elem_ty));
            ctx.finish_block(block, sequel, Atom::Var(ret_tmp));
        }

        cc::Expr::ArrayAlloc { len, elem } => {
            let array_tmp = sequel.get_ret_var(ctx, RepType::Word);
            block.asgn(array_tmp, Expr::ArrayAlloc { len });

            let idx_var = ctx.fresh_var(RepType::Word);
            block.asgn(idx_var, Expr::Atom(Atom::Int(0)));

            let loop_cond_block = ctx.create_block();
            let mut loop_body_block = ctx.create_block();
            let cont_block = ctx.create_block();

            ctx.finish_block_(Block {
                idx: block.idx,
                comment: block.comment,
                stmts: block.stmts,
                exit: Exit::Jump(loop_cond_block.idx),
            });

            // loop_cond
            ctx.finish_block_(Block {
                idx: loop_cond_block.idx,
                comment: Some("array loop cond".to_string()),
                stmts: vec![],
                exit: Exit::Branch {
                    v1: idx_var,
                    v2: len,
                    cond: Cmp::Equal,
                    then_block: cont_block.idx,
                    else_block: loop_body_block.idx,
                },
            });

            // loop_body
            let idx_inc_var = ctx.fresh_var(RepType::Word);
            loop_body_block.expr(Expr::ArrayPut(array_tmp, idx_var, elem));
            loop_body_block.asgn(idx_inc_var, Expr::Atom(Atom::Int(1)));
            loop_body_block.asgn(
                idx_var,
                Expr::IBinOp(BinOp { op: IntBinOp::Add, arg1: idx_var, arg2: idx_inc_var }),
            );
            ctx.finish_block_(Block {
                idx: loop_body_block.idx,
                comment: Some("array body".to_string()),
                stmts: loop_body_block.stmts,
                exit: Exit::Jump(loop_cond_block.idx),
            });

            ctx.finish_block(cont_block, sequel, Atom::Var(array_tmp));
        }

        cc::Expr::ArrayGet(array, idx) => {
            let elem_ty = match &*ctx.ctx.var_type(array) {
                Type::Array(elem_ty) => RepType::from(&**elem_ty),
                other => panic!(
                    "Non-array type in array position: {:?} (type={:?})",
                    array, other
                ),
            };
            let ret_tmp = sequel.get_ret_var(ctx, elem_ty);
            block.asgn(ret_tmp, Expr::ArrayGet(array, idx));
            ctx.finish_block(block, sequel, Atom::Var(ret_tmp));
        }

        cc::Expr::ArrayPut(array, idx, val) => {
            let elem_ty = match &*ctx.ctx.var_type(array) {
                Type::Array(elem_ty) => RepType::from(&**elem_ty),
                other => panic!(
                    "Non-array type in array position: {:?} (type={:?})",
                    array, other
                ),
            };
            let ret_tmp = sequel.get_ret_var(ctx, elem_ty);
            block.asgn(ret_tmp, Expr::ArrayPut(array, idx, val));
            ctx.finish_block(block, sequel, Atom::Var(ret_tmp));
        }
    }
}
