mod print;
mod types;

use crate::anormal;
use crate::cg_types::RepType;
use crate::common::{BinOp, Cmp, IntBinOp};
use crate::ctx::{Ctx, VarId};
use crate::type_check::Type;
use crate::var::CompilerPhase::ClosureConvert;

pub use print::*;
pub use types::*;

// Used when debugging
#[allow(unused_imports)]
use crate::utils;

use fxhash::FxHashSet;

#[derive(Debug, Clone)]
enum BlockSequel {
    Return,
    // Assign return value to this variable and jump to the label. Used when lowering let bindings.
    Asgn(VarId, Label),
}

impl BlockSequel {
    fn get_ret_var(&self, ctx: &mut CcCtx, ret_ty: RepType) -> VarId {
        use BlockSequel::*;
        match self {
            Asgn(var, _) => *var,
            Return => ctx.fresh_var(ret_ty),
        }
    }
}

fn finish_block(
    ctx: &mut CcCtx, label: Label, comment: Option<String>, mut stmts: Vec<Stmt>,
    sequel: BlockSequel, value: Atom,
) -> Block {
    let exit = match sequel {
        BlockSequel::Return => match value {
            Atom::Unit => {
                let tmp = ctx.fresh_var(RepType::Word);
                stmts.push(Stmt::Asgn(Asgn {
                    lhs: tmp,
                    rhs: Expr::Atom(Atom::Unit),
                }));
                Exit::Return(tmp)
            }
            Atom::Int(i) => {
                let tmp = ctx.fresh_var(RepType::Word);
                stmts.push(Stmt::Asgn(Asgn {
                    lhs: tmp,
                    rhs: Expr::Atom(Atom::Int(i)),
                }));
                Exit::Return(tmp)
            }
            Atom::Float(f) => {
                let tmp = ctx.fresh_var(RepType::Float);
                stmts.push(Stmt::Asgn(Asgn {
                    lhs: tmp,
                    rhs: Expr::Atom(Atom::Float(f)),
                }));
                Exit::Return(tmp)
            }
            Atom::Var(var) => Exit::Return(var),
        },
        BlockSequel::Asgn(lhs, label) => {
            match value {
                // TODO: Should we handle this case in the call site? Or make it impossible to
                // happen somehow?
                Atom::Var(rhs) if lhs == rhs => {}
                _ => {
                    stmts.push(Stmt::Asgn(Asgn {
                        lhs,
                        rhs: Expr::Atom(value),
                    }));
                }
            }
            Exit::Jump(label)
        }
    };

    Block {
        label,
        comment,
        stmts,
        exit,
    }
}

// Closure conversion state
struct CcCtx<'ctx> {
    ctx: &'ctx mut Ctx,
    // Functions generated so far
    funs: Vec<Fun>,
}

impl<'ctx> CcCtx<'ctx> {
    fn fresh_var(&mut self, rep_type: RepType) -> VarId {
        self.ctx.fresh_codegen_var(ClosureConvert, rep_type)
    }

    fn fresh_label(&mut self) -> Label {
        self.ctx.fresh_label()
    }
}

pub fn lower_pgm(ctx: &mut Ctx, expr: anormal::Expr) -> (Vec<Fun>, VarId) {
    let mut cc_ctx = CcCtx { ctx, funs: vec![] };

    let main_name = cc_ctx.fresh_var(RepType::Word);
    let mut main_blocks = vec![];
    let entry_label = cc_ctx.fresh_label();
    cc_block(
        &mut cc_ctx,
        &mut main_blocks,
        entry_label,
        vec![],
        BlockSequel::Return,
        expr,
    );

    cc_ctx.funs.push(Fun {
        name: main_name,
        args: vec![],
        blocks: main_blocks,
        return_type: RepType::Word,
    });

    (cc_ctx.funs, main_name)
}

// Returns whether the added block was a fork (i.e. then or else branch of an if)
fn cc_block(
    ctx: &mut CcCtx, blocks: &mut Vec<Block>, label: Label, mut stmts: Vec<Stmt>,
    sequel: BlockSequel, expr: anormal::Expr,
) -> bool {
    match expr {
        anormal::Expr::Unit => {
            blocks.push(finish_block(ctx, label, None, stmts, sequel, Atom::Unit));
            false
        }

        anormal::Expr::Int(i) => {
            blocks.push(finish_block(ctx, label, None, stmts, sequel, Atom::Int(i)));
            false
        }

        anormal::Expr::Float(f) => {
            blocks.push(finish_block(
                ctx,
                label,
                None,
                stmts,
                sequel,
                Atom::Float(f),
            ));
            false
        }

        anormal::Expr::Neg(var) => {
            let tmp = ctx.fresh_var(RepType::Word);
            stmts.push(Stmt::Asgn(Asgn {
                lhs: tmp,
                rhs: Expr::Neg(var),
            }));
            blocks.push(finish_block(
                ctx,
                label,
                None,
                stmts,
                sequel,
                Atom::Var(tmp),
            ));
            false
        }

        anormal::Expr::FNeg(var) => {
            let tmp = ctx.fresh_var(RepType::Float);
            stmts.push(Stmt::Asgn(Asgn {
                lhs: tmp,
                rhs: Expr::FNeg(var),
            }));
            blocks.push(finish_block(
                ctx,
                label,
                None,
                stmts,
                sequel,
                Atom::Var(tmp),
            ));
            false
        }

        anormal::Expr::IBinOp(BinOp { op, arg1, arg2 }) => {
            let tmp = sequel.get_ret_var(ctx, RepType::Word);
            stmts.push(Stmt::Asgn(Asgn {
                lhs: tmp,
                rhs: Expr::IBinOp(BinOp { op, arg1, arg2 }),
            }));
            blocks.push(finish_block(
                ctx,
                label,
                None,
                stmts,
                sequel,
                Atom::Var(tmp),
            ));
            false
        }

        anormal::Expr::FBinOp(BinOp { op, arg1, arg2 }) => {
            let tmp = sequel.get_ret_var(ctx, RepType::Float);
            stmts.push(Stmt::Asgn(Asgn {
                lhs: tmp,
                rhs: Expr::FBinOp(BinOp { op, arg1, arg2 }),
            }));
            blocks.push(finish_block(
                ctx,
                label,
                None,
                stmts,
                sequel,
                Atom::Var(tmp),
            ));
            false
        }

        anormal::Expr::If(v1, v2, cmp, e1, e2) => {
            let then_label = ctx.fresh_label();
            let else_label = ctx.fresh_label();
            blocks.push(Block {
                label,
                comment: None,
                stmts,
                exit: Exit::Branch {
                    v1,
                    v2,
                    cond: cmp,
                    then_label,
                    else_label,
                },
            });
            cc_block(ctx, blocks, then_label, vec![], sequel.clone(), *e1);
            cc_block(ctx, blocks, else_label, vec![], sequel, *e2);
            true
        }

        anormal::Expr::Var(var) => {
            blocks.push(finish_block(
                ctx,
                label,
                None,
                stmts,
                sequel,
                Atom::Var(var),
            ));
            false
        }

        anormal::Expr::Let {
            id,
            ty_id: _,
            rhs,
            body,
        } => {
            let cont_label = ctx.fresh_label();
            let rhs_sequel = BlockSequel::Asgn(id, cont_label);
            let forked = cc_block(ctx, blocks, label, stmts, rhs_sequel, *rhs);

            // Is the last block of RHS is not a forked block (i.e. then or else branch of an if)
            // then we can continue extend that block. If not we'll have to use the continuation
            // block.
            if !forked {
                let Block {
                    label,
                    comment: _,
                    stmts,
                    exit,
                } = blocks.pop().unwrap();
                assert_eq!(exit, Exit::Jump(cont_label));
                cc_block(ctx, blocks, label, stmts, sequel, *body)
            } else {
                cc_block(ctx, blocks, cont_label, vec![], sequel, *body)
            }
        }

        anormal::Expr::LetRec {
            name,
            ty_id,
            mut args,
            rhs,
            body,
        } => {
            // TODO: Not sure about reusing 'name' in multiple places below.

            // After cc 'name' will refer to the closure tuple. For the function we'll need a fresh
            // variable.
            let fun_var = ctx.fresh_var(RepType::Word);

            // Free variables of the closure will be moved to tuple payload
            // NOTE: An inefficiency here is that if we have deeply nested letrecs we'll be
            // computing fvs of nested letrecs when computing the outer ones. One solution could be
            // to annotate LetRecs with fvs. Doesn't matter in practice though.
            let closure_fvs: Vec<VarId> = {
                let mut closure_fvs: FxHashSet<VarId> = Default::default();
                fvs(ctx.ctx, &*rhs, &mut closure_fvs);
                closure_fvs.remove(&name);
                for arg in &args {
                    closure_fvs.remove(arg);
                }
                closure_fvs.into_iter().collect()
            };

            // In the RHS and the body, 'name' will refer to the tuple. However in the RHS the
            // tuple will be the first argument of the function, in the body we'll allocate a
            // tuple.

            // Emit function
            args.insert(0, name); // first argument will be 'self'
            let fun_entry_label = ctx.fresh_label();
            let mut fun_blocks = vec![];
            let mut entry_block_stmts = vec![];
            // Bind captured variables in function body
            for (fv_idx, fv) in closure_fvs.iter().enumerate() {
                entry_block_stmts.push(Stmt::Asgn(Asgn {
                    lhs: *fv,
                    rhs: Expr::TupleGet(name, fv_idx + 1),
                }));
            }
            cc_block(
                ctx,
                &mut fun_blocks,
                fun_entry_label,
                entry_block_stmts,
                BlockSequel::Return,
                *rhs,
            );

            let fun_type = ctx.ctx.get_type(ty_id);
            let fun_return_type = match &*fun_type {
                Type::Fun { ret, .. } => RepType::from(&**ret),
                _ => panic!("Non-function in function position"),
            };

            ctx.funs.push(Fun {
                name: fun_var,
                args,
                blocks: fun_blocks,
                return_type: fun_return_type,
            });

            // Body
            let mut closure_tuple_args = closure_fvs;
            closure_tuple_args.insert(0, fun_var);
            stmts.push(Stmt::Asgn(Asgn {
                lhs: name,
                rhs: Expr::Tuple {
                    len: closure_tuple_args.len(),
                },
            }));
            for (arg_idx, arg) in closure_tuple_args.iter().enumerate() {
                stmts.push(Stmt::Expr(Expr::TuplePut(name, arg_idx, *arg)));
            }
            cc_block(ctx, blocks, label, stmts, sequel, *body)
        }

        anormal::Expr::App(fun, mut args) => {
            // f(x) -> f.0(f, x)
            let fun_tmp = ctx.fresh_var(RepType::Word);
            stmts.push(Stmt::Asgn(Asgn {
                lhs: fun_tmp,
                rhs: Expr::TupleGet(fun, 0),
            }));
            args.insert(0, fun);

            let fun_ret_ty = match &*ctx.ctx.var_type(fun) {
                Type::Fun { args: _, ret } => RepType::from(&**ret),
                other => panic!("Non-function in function position: {:?}", other),
            };
            let ret_tmp = sequel.get_ret_var(ctx, fun_ret_ty);

            stmts.push(Stmt::Asgn(Asgn {
                lhs: ret_tmp,
                rhs: Expr::App(fun_tmp, args, fun_ret_ty),
            }));
            blocks.push(finish_block(
                ctx,
                label,
                None,
                stmts,
                sequel,
                Atom::Var(ret_tmp),
            ));
            false
        }

        anormal::Expr::Tuple(args) => {
            let ret_tmp = sequel.get_ret_var(ctx, RepType::Word);
            stmts.push(Stmt::Asgn(Asgn {
                lhs: ret_tmp,
                rhs: Expr::Tuple { len: args.len() },
            }));
            for (arg_idx, arg) in args.iter().enumerate() {
                stmts.push(Stmt::Expr(Expr::TuplePut(ret_tmp, arg_idx, *arg)));
            }
            blocks.push(finish_block(
                ctx,
                label,
                None,
                stmts,
                sequel,
                Atom::Var(ret_tmp),
            ));
            false
        }

        anormal::Expr::TupleGet(tuple, idx) => {
            let elem_ty = match &*ctx.ctx.var_type(tuple) {
                Type::Tuple(args) => RepType::from(&args[idx]),
                other => panic!(
                    "Non-tuple type in tuple position: {:?} (type={:?})",
                    tuple, other
                ),
            };
            let ret_tmp = sequel.get_ret_var(ctx, elem_ty);
            stmts.push(Stmt::Asgn(Asgn {
                lhs: ret_tmp,
                rhs: Expr::TupleGet(tuple, idx),
            }));
            blocks.push(finish_block(
                ctx,
                label,
                None,
                stmts,
                sequel,
                Atom::Var(ret_tmp),
            ));
            false
        }

        anormal::Expr::ArrayAlloc { len, elem } => {
            let array_tmp = sequel.get_ret_var(ctx, RepType::Word);
            stmts.push(Stmt::Asgn(Asgn {
                lhs: array_tmp,
                rhs: Expr::ArrayAlloc { len },
            }));

            let idx_var = ctx.fresh_var(RepType::Word);
            stmts.push(Stmt::Asgn(Asgn {
                lhs: idx_var,
                rhs: Expr::Atom(Atom::Int(0)),
            }));

            let loop_cond_label = ctx.fresh_label();
            let loop_body_label = ctx.fresh_label();
            let cont_label = ctx.fresh_label();

            blocks.push(Block {
                label,
                comment: None,
                stmts,
                exit: Exit::Jump(loop_cond_label),
            });

            // loop_cond
            blocks.push(Block {
                label: loop_cond_label,
                comment: Some("array loop cond".to_string()),
                stmts: vec![],
                exit: Exit::Branch {
                    v1: idx_var,
                    v2: len,
                    cond: Cmp::Equal,
                    then_label: cont_label,
                    else_label: loop_body_label,
                },
            });

            // loop_body
            let idx_inc_var = ctx.fresh_var(RepType::Word);
            let loop_body_stmts = vec![
                Stmt::Expr(Expr::ArrayPut(array_tmp, idx_var, elem)),
                Stmt::Asgn(Asgn {
                    lhs: idx_inc_var,
                    rhs: Expr::Atom(Atom::Int(1)),
                }),
                Stmt::Asgn(Asgn {
                    lhs: idx_var,
                    rhs: Expr::IBinOp(BinOp {
                        op: IntBinOp::Add,
                        arg1: idx_var,
                        arg2: idx_inc_var,
                    }),
                }),
            ];
            blocks.push(Block {
                label: loop_body_label,
                comment: Some("array body".to_string()),
                stmts: loop_body_stmts,
                exit: Exit::Jump(loop_cond_label),
            });

            blocks.push(finish_block(
                ctx,
                cont_label,
                Some("array cont".to_string()),
                vec![],
                sequel,
                Atom::Var(array_tmp),
            ));
            false
        }

        anormal::Expr::ArrayGet(array, idx) => {
            let elem_ty = match &*ctx.ctx.var_type(array) {
                Type::Array(elem_ty) => RepType::from(&**elem_ty),
                other => panic!(
                    "Non-array type in array position: {:?} (type={:?})",
                    array, other
                ),
            };
            let ret_tmp = sequel.get_ret_var(ctx, elem_ty);
            stmts.push(Stmt::Asgn(Asgn {
                lhs: ret_tmp,
                rhs: Expr::ArrayGet(array, idx),
            }));
            blocks.push(finish_block(
                ctx,
                label,
                None,
                stmts,
                sequel,
                Atom::Var(ret_tmp),
            ));
            false
        }

        anormal::Expr::ArrayPut(array, idx, val) => {
            let elem_ty = match &*ctx.ctx.var_type(array) {
                Type::Array(elem_ty) => RepType::from(&**elem_ty),
                other => panic!(
                    "Non-array type in array position: {:?} (type={:?})",
                    array, other
                ),
            };
            let ret_tmp = sequel.get_ret_var(ctx, elem_ty);
            stmts.push(Stmt::Asgn(Asgn {
                lhs: ret_tmp,
                rhs: Expr::ArrayPut(array, idx, val),
            }));
            blocks.push(finish_block(
                ctx,
                label,
                None,
                stmts,
                sequel,
                Atom::Var(ret_tmp),
            ));
            false
        }
    }
}

fn fvs(ctx: &Ctx, e: &anormal::Expr, acc: &mut FxHashSet<VarId>) {
    use anormal::Expr::*;
    match e {
        Unit | Int(_) | Float(_) => {}
        IBinOp(BinOp { arg1, arg2, op: _ }) => {
            fv(ctx, *arg1, acc);
            fv(ctx, *arg2, acc);
        }
        FBinOp(BinOp { arg1, arg2, op: _ }) => {
            fv(ctx, *arg1, acc);
            fv(ctx, *arg2, acc);
        }
        Neg(arg) | FNeg(arg) => {
            fv(ctx, *arg, acc);
        }
        If(arg1, arg2, _, e1, e2) => {
            fv(ctx, *arg1, acc);
            fv(ctx, *arg2, acc);
            fvs(ctx, e1, acc);
            fvs(ctx, e2, acc);
        }
        Let {
            id,
            ty_id: _,
            rhs,
            body,
        } => {
            fvs(ctx, rhs, acc);
            fvs(ctx, body, acc);
            acc.remove(id);
        }
        Var(id) => {
            fv(ctx, *id, acc);
        }
        LetRec {
            name,
            ty_id: _,
            args,
            rhs,
            body,
        } => {
            fvs(ctx, rhs, acc);
            fvs(ctx, body, acc);
            acc.remove(name);
            for arg in args {
                acc.remove(arg);
            }
        }
        App(fun, args) => {
            fv(ctx, *fun, acc);
            for arg in args {
                fv(ctx, *arg, acc);
            }
        }
        Tuple(args) => {
            for arg in args {
                fv(ctx, *arg, acc);
            }
        }
        TupleGet(arg, _) => {
            fv(ctx, *arg, acc);
        }
        ArrayAlloc { len, elem } => {
            fv(ctx, *len, acc);
            fv(ctx, *elem, acc);
        }
        ArrayGet(arg1, arg2) => {
            fv(ctx, *arg1, acc);
            fv(ctx, *arg2, acc);
        }
        ArrayPut(arg1, arg2, arg3) => {
            fv(ctx, *arg1, acc);
            fv(ctx, *arg2, acc);
            fv(ctx, *arg3, acc);
        }
    }
}

fn fv(ctx: &Ctx, var: VarId, acc: &mut FxHashSet<VarId>) {
    if !ctx.is_builtin_var(var) {
        acc.insert(var);
    }
}
