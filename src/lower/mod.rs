mod print;
mod types;

use crate::cg_types::RepType;
use crate::closure_convert as cc;
use crate::common::{BinOp, Cmp, IntBinOp};
use crate::ctx::{Ctx, VarId};
use crate::var::CompilerPhase::ClosureConvert;

pub use types::*;

use cranelift_entity::PrimaryMap;

pub fn lower_fun(ctx: &mut Ctx, fun: cc::Fun) -> Fun {
    let mut ctx = LowerCtx::new(ctx);

    let cc::Fun {
        name,
        args,
        body,
        return_type,
    } = fun;

    let block = ctx.create_block();
    let ret_var = ctx.fresh_var(return_type);
    let exit_block = lower_expr(&mut ctx, block, ret_var, body);
    ctx.finish_block(exit_block, Atom::Var(ret_var));

    Fun {
        name,
        args,
        blocks: ctx.blocks,
        return_type,
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
        Self {
            idx,
            stmts: vec![],
            comment: None,
        }
    }

    fn asgn_(&mut self, lhs: VarId, rhs: Expr) {
        self.stmts.push(Stmt::Asgn(Asgn { lhs, rhs }));
    }

    fn asgn(mut self, lhs: VarId, rhs: Expr) -> BlockBuilder {
        self.stmts.push(Stmt::Asgn(Asgn { lhs, rhs }));
        self
    }

    fn asgn_atom(self, lhs: VarId, rhs: Atom) -> BlockBuilder {
        self.asgn(lhs, Expr::Atom(rhs))
    }

    fn expr(&mut self, expr: Expr) {
        self.stmts.push(Stmt::Expr(expr));
    }
}

// Lowering context.
struct LowerCtx<'ctx> {
    ctx: &'ctx mut Ctx,

    /// Blocks generated so far for the current function.
    blocks: PrimaryMap<BlockIdx, BlockData>,
}

impl<'ctx> LowerCtx<'ctx> {
    fn new(ctx: &'ctx mut Ctx) -> Self {
        Self {
            ctx,
            blocks: PrimaryMap::new(),
        }
    }

    fn fresh_var(&mut self, rep_type: RepType) -> VarId {
        self.ctx.fresh_codegen_var(ClosureConvert, rep_type)
    }

    fn create_block(&mut self) -> BlockBuilder {
        let idx = self.blocks.push(BlockData::NA);
        BlockBuilder::new(idx)
    }

    fn join(&mut self, b1: BlockBuilder, b2: BlockBuilder) -> BlockBuilder {
        let cont = self.create_block();
        let b1 = Block {
            idx: b1.idx,
            stmts: b1.stmts,
            comment: b1.comment,
            exit: Exit::Jump(cont.idx),
        };
        let b2 = Block {
            idx: b2.idx,
            stmts: b2.stmts,
            comment: b2.comment,
            exit: Exit::Jump(cont.idx),
        };
        self.finish_block_(b1);
        self.finish_block_(b2);
        cont
    }

    fn finish_block(&mut self, block: BlockBuilder, value: Atom) {
        let BlockBuilder {
            idx,
            mut stmts,
            comment,
        } = block;

        let exit = match value {
            Atom::Unit => {
                let tmp = self.fresh_var(RepType::Word);
                stmts.push(Stmt::Asgn(Asgn {
                    lhs: tmp,
                    rhs: Expr::Atom(Atom::Unit),
                }));
                Exit::Return(tmp)
            }
            Atom::Int(i) => {
                let tmp = self.fresh_var(RepType::Word);
                stmts.push(Stmt::Asgn(Asgn {
                    lhs: tmp,
                    rhs: Expr::Atom(Atom::Int(i)),
                }));
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
        };

        let block = Block {
            idx,
            comment,
            stmts,
            exit,
        };

        self.finish_block_(block);
    }

    fn finish_block_(&mut self, block: Block) {
        let idx = block.idx;
        assert!(self.blocks[idx].is_NA());
        self.blocks[idx] = BlockData::Block(block);
    }
}

fn lower_expr(
    ctx: &mut LowerCtx,
    mut block: BlockBuilder,
    var: VarId,
    expr: cc::Expr,
) -> BlockBuilder {
    match expr {
        cc::Expr::Unit => block.asgn_atom(var, Atom::Unit),

        cc::Expr::Int(i) => block.asgn_atom(var, Atom::Int(i)),

        cc::Expr::Float(f) => block.asgn_atom(var, Atom::Float(f)),

        cc::Expr::Neg(arg) => block.asgn(var, Expr::Neg(arg)),

        cc::Expr::FNeg(arg) => block.asgn(var, Expr::FNeg(arg)),

        cc::Expr::IBinOp(BinOp { op, arg1, arg2 }) => {
            block.asgn(var, Expr::IBinOp(BinOp { op, arg1, arg2 }))
        }

        cc::Expr::FBinOp(BinOp { op, arg1, arg2 }) => {
            block.asgn(var, Expr::FBinOp(BinOp { op, arg1, arg2 }))
        }

        cc::Expr::If(v1, v2, cmp, e1, e2) => {
            let mut then_block = ctx.create_block();
            let mut else_block = ctx.create_block();
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
            then_block = lower_expr(ctx, then_block, var, *e1);
            else_block = lower_expr(ctx, else_block, var, *e2);
            ctx.join(then_block, else_block)
        }

        cc::Expr::Var(src) => block.asgn_atom(var, Atom::Var(src)),

        cc::Expr::Let { id, rhs, body } => {
            block = lower_expr(ctx, block, id, *rhs);
            lower_expr(ctx, block, var, *body)
        }

        cc::Expr::App(fun, args, ret_ty) => block.asgn(var, Expr::App(fun, args, ret_ty)),

        cc::Expr::Tuple(args) => {
            block.asgn_(var, Expr::Tuple { len: args.len() });
            for (arg_idx, arg) in args.iter().enumerate() {
                block.expr(Expr::TuplePut(var, arg_idx, *arg));
            }
            block
        }

        cc::Expr::TupleGet(tuple, idx, elem_ty) => {
            block.asgn(var, Expr::TupleGet(tuple, idx, elem_ty))
        }

        cc::Expr::ArrayAlloc { len, elem } => {
            block.asgn_(var, Expr::ArrayAlloc { len });

            let idx_var = ctx.fresh_var(RepType::Word);
            block.asgn_(idx_var, Expr::Atom(Atom::Int(0)));

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
            loop_body_block.expr(Expr::ArrayPut(var, idx_var, elem));
            loop_body_block.asgn_(idx_inc_var, Expr::Atom(Atom::Int(1)));
            loop_body_block.asgn_(
                idx_var,
                Expr::IBinOp(BinOp {
                    op: IntBinOp::Add,
                    arg1: idx_var,
                    arg2: idx_inc_var,
                }),
            );
            ctx.finish_block_(Block {
                idx: loop_body_block.idx,
                comment: Some("array body".to_string()),
                stmts: loop_body_block.stmts,
                exit: Exit::Jump(loop_cond_block.idx),
            });

            cont_block
        }

        cc::Expr::ArrayGet(array, idx) => block.asgn(var, Expr::ArrayGet(array, idx)),

        cc::Expr::ArrayPut(array, idx, val) => block.asgn(var, Expr::ArrayPut(array, idx, val)),
    }
}
