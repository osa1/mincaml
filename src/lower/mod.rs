mod block;
mod ctx;
mod fun;
mod instr;
mod liveness;
mod print;

use block::BlockIdx;
use ctx::Ctx;
use fun::{Fun, FunSig};

use crate::anormal;
use crate::cg_types::RepType;
use crate::common::{BinOp, Cmp, FloatBinOp, IntBinOp};
use crate::ctx as ctx_;
use crate::ctx::VarId;
use crate::type_check::Type;

use instr::*;

// Used when debugging
#[allow(unused_imports)]
use crate::utils;

use fxhash::FxHashSet;

/// Entry point for code generation. Returns generated functions and variable of the main function.
// (TODO: Generate main at call site and pass it here)
pub fn lower_pgm(ctx: &mut ctx_::Ctx, expr: anormal::Expr) -> (Vec<Fun>, VarId) {
    let mut ctx = Ctx::new(ctx);

    let main_name = ctx.fresh_var(RepType::Word);
    let main_block = ctx.create_block();

    // Entry block won't have any predecessors so we can seal it before lowering
    ctx.seal_block(main_block);

    lower_block(&mut ctx, main_block, Sequel::Return, expr);


    (ctx.finish(main_name), main_name)
}

#[derive(Debug, Clone)]
enum Sequel {
    Return,
    // Assign return value to this variable and jump to the label. Used when lowering let bindings.
    Asgn(VarId, BlockIdx),
}

fn finish_block(ctx: &mut Ctx, block: BlockIdx, sequel: Sequel, value: ValueIdx) {
    match sequel {
        Sequel::Return => {
            ctx.ret(block, value);
        }
        Sequel::Asgn(lhs, target) => {
            let lhs_val = ctx.use_var(block, lhs);
            ctx.mov(block, lhs_val, value.clone());
            ctx.def_var(block, lhs, value);
            ctx.jmp(block, target);
        }
    }
}

fn lower_block(ctx: &mut Ctx, block: BlockIdx, sequel: Sequel, expr: anormal::Expr) {
    match expr {
        anormal::Expr::Unit => {
            let val = ctx.iimm(block, 0);
            finish_block(ctx, block, sequel, val);
        }

        anormal::Expr::Int(i) => {
            let val = ctx.iimm(block, i);
            finish_block(ctx, block, sequel, val);
        }

        anormal::Expr::Float(f) => {
            let val = ctx.fimm(block, f);
            finish_block(ctx, block, sequel, val);
        }

        anormal::Expr::IBinOp(BinOp { op, arg1, arg2 }) => {
            let val1 = ctx.use_var(block, arg1);
            let val2 = ctx.use_var(block, arg2);
            let val;
            match op {
                IntBinOp::Add => {
                    val = ctx.iadd(block, val1, val2);
                }
                IntBinOp::Sub => {
                    val = ctx.isub(block, val1, val2);
                }
            }
            finish_block(ctx, block, sequel, val);
        }

        anormal::Expr::FBinOp(BinOp { op, arg1, arg2 }) => {
            let val1 = ctx.use_var(block, arg1);
            let val2 = ctx.use_var(block, arg2);
            let val;
            match op {
                FloatBinOp::Add => {
                    val = ctx.fadd(block, val1, val2);
                }
                FloatBinOp::Sub => {
                    val = ctx.fsub(block, val1, val2);
                }
                FloatBinOp::Mul => {
                    val = ctx.fmul(block, val1, val2);
                }
                FloatBinOp::Div => {
                    val = ctx.fdiv(block, val1, val2);
                }
            }
            finish_block(ctx, block, sequel, val);
        }

        anormal::Expr::Neg(var) => {
            let val = ctx.use_var(block, var);
            let val = ctx.neg(block, val);
            finish_block(ctx, block, sequel, val);
        }

        anormal::Expr::FNeg(var) => {
            let val = ctx.use_var(block, var);
            let val = ctx.fneg(block, val);
            finish_block(ctx, block, sequel, val);
        }

        anormal::Expr::If(var1, var2, cmp, then_e, else_e) => {
            let then_block = ctx.create_block();
            let else_block = ctx.create_block();

            let val1 = ctx.use_var(block, var1);
            let val2 = ctx.use_var(block, var2);

            ctx.cond_jmp(block, val1, val2, cmp, then_block, else_block);
            ctx.seal_block(then_block);
            ctx.seal_block(else_block);

            lower_block(ctx, then_block, sequel.clone(), *then_e);
            lower_block(ctx, else_block, sequel, *else_e);
        }

        anormal::Expr::Let {
            id,
            ty_id: _,
            rhs,
            body,
        } => {
            // TODO: we need to declare `id` here to be able to assign to it in `rhs`
            let cont_block = ctx.create_block();
            let rhs_sequel = Sequel::Asgn(id, cont_block);
            lower_block(ctx, block, rhs_sequel, *rhs);
            ctx.seal_block(cont_block);
            lower_block(ctx, cont_block, sequel, *body);
        }

        anormal::Expr::Var(var) => {
            let val = ctx.use_var(block, var);
            finish_block(ctx, block, sequel, val);
        }

        anormal::Expr::LetRec {
            name,
            ty_id,
            mut args,
            rhs,
            body,
        } => {
            // 'name' will refer to the closure tuple. For the actual function create a fresh var
            let fun_var = ctx.fresh_var(RepType::Word);
            // Free variables of the closure will be moved to tuple payload
            // NOTE: An inefficiency here is that if we have deeply nested letrecs we'll be
            // computing fvs of nested letrecs when computing the outer ones. One solution could be
            // to annotate LetRecs with fvs. Doesn't matter in practice though.
            let closure_fvs: Vec<VarId> = {
                let mut closure_fvs: FxHashSet<VarId> = Default::default();
                fvs(ctx, &*rhs, &mut closure_fvs);
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
            ctx.fork_fun(|ctx| {
                for (arg_idx, arg) in args.iter().enumerate() {
                    ctx.def_arg(*arg, arg_idx);
                }

                // TODO: use def_var
                let entry_block = ctx.create_block();
                // Entry blocks don't have predecessors, seal it here
                ctx.seal_block(entry_block);

                let tuple_val = ctx.use_var(entry_block, name);
                // Bind captured variables in function body
                for (fv_idx, fv) in closure_fvs.iter().enumerate() {
                    let val = ctx.tuple_get(entry_block, tuple_val.clone(), fv_idx);
                    ctx.def_var(entry_block, *fv, val);
                }
                lower_block(ctx, entry_block, Sequel::Return, *rhs);

                let fun_type = ctx.get_type(ty_id);
                let fun_return_type = match &*fun_type {
                    Type::Fun { ret, .. } => RepType::from(&**ret),
                    _ => panic!("Non-function in function position"),
                };

                FunSig {
                    name: fun_var,
                    args,
                    return_type: fun_return_type,
                }
            });

            // Body
            let mut closure_tuple_args = closure_fvs;
            closure_tuple_args.insert(0, fun_var);
            let tuple_val = ctx.tuple(block, closure_tuple_args.len());
            ctx.def_var(block, name, tuple_val.clone());
            for (arg_idx, arg) in closure_tuple_args.iter().enumerate() {
                let arg_val = ctx.use_var(block, *arg);
                ctx.tuple_put(block, tuple_val.clone(), arg_idx, arg_val);
            }

            lower_block(ctx, block, sequel, *body);
        }

        anormal::Expr::App(var, args) => {
            // f(x) -> f.0(f, x)
            let tuple = ctx.use_var(block, var);
            let fun = ctx.tuple_get(block, tuple, 0);

            let mut arg_vals = Vec::with_capacity(args.len() + 1);
            arg_vals.push(fun.clone());

            for arg in args {
                let arg_val = ctx.use_var(block, arg);
                arg_vals.push(arg_val);
            }

            let fun_ret_ty = match &*ctx.var_type(var) {
                Type::Fun { args: _, ret } => RepType::from(&**ret),
                other => panic!("Non-function in function position: {:?}", other),
            };

            let call = ctx.call(block, fun, arg_vals, fun_ret_ty);
            finish_block(ctx, block, sequel, call);
        }

        anormal::Expr::Tuple(args) => {
            let tuple = ctx.tuple(block, args.len());
            for (arg_idx, arg) in args.iter().enumerate() {
                let arg_val = ctx.use_var(block, *arg);
                ctx.tuple_put(block, tuple.clone(), arg_idx, arg_val);
            }
            finish_block(ctx, block, sequel, tuple);
        }

        anormal::Expr::TupleGet(tuple, idx) => {
            let tuple = ctx.use_var(block, tuple);
            // TODO: not possible to generate instructions for this without knowing the type
            let ret = ctx.tuple_get(block, tuple, idx);
            finish_block(ctx, block, sequel, ret);
        }

        anormal::Expr::ArrayAlloc { len, elem } => {
            let len = ctx.use_var(block, len);
            let array = ctx.array_alloc(block, len.clone());
            let elem = ctx.use_var(block, elem);

            let idx = ctx.iimm(block, 0);

            let loop_cond_block = ctx.create_block();
            ctx.jmp(block, loop_cond_block);

            let loop_body_block = ctx.create_block();
            let cont_block = ctx.create_block();

            // loop_cond
            ctx.cond_jmp(
                loop_cond_block,
                idx.clone(),
                len,
                Cmp::Equal,
                cont_block,
                loop_body_block,
            );

            ctx.seal_block(loop_body_block);

            // loop_body
            ctx.array_put(loop_body_block, array.clone(), idx.clone(), elem);
            let inc = ctx.iimm(loop_body_block, 1);
            let idx_plus_one = ctx.iadd(loop_body_block, idx.clone(), inc);
            ctx.mov(loop_body_block, idx, idx_plus_one);
            ctx.jmp(loop_body_block, loop_cond_block);

            ctx.seal_block(loop_cond_block);

            finish_block(ctx, block, sequel, array);
        }

        anormal::Expr::ArrayGet(array, idx) => {
            // TODO: need the type here
            let array = ctx.use_var(block, array);
            let idx = ctx.use_var(block, idx);
            let ret = ctx.array_get(block, array, idx);
            finish_block(ctx, block, sequel, ret);
        }

        anormal::Expr::ArrayPut(array, idx, val) => {
            // TODO need the type here
            let array = ctx.use_var(block, array);
            let idx = ctx.use_var(block, idx);
            let val = ctx.use_var(block, val);
            let ret = ctx.array_put(block, array, idx, val);
            finish_block(ctx, block, sequel, ret);
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
