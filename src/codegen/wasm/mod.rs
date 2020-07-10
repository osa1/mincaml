#![allow(dead_code)]

mod fun_builder;
mod types;

use fun_builder::*;
use types::*;

use crate::cg_types::RepType;
use crate::common::*;
use crate::ctx::{Ctx, VarId};
use crate::lower;
use crate::lower::{Asgn, Fun, Stmt};

use fxhash::FxHashMap;

pub fn codegen(ctx: &mut Ctx, funs: &[lower::Fun], main_id: VarId, dump: bool) {
    let mut ctx = WasmCtx::new(ctx);

    for fun in funs {
        cg_fun(&mut ctx, fun);
    }
}

struct WasmCtx<'ctx> {
    ctx: &'ctx mut Ctx,
    fun_tys: FxHashMap<FunTy, TypeIdx>,
    fun_indices: FxHashMap<VarId, FunIdx>,
}

impl<'ctx> WasmCtx<'ctx> {
    fn new(ctx: &mut Ctx) -> WasmCtx {
        WasmCtx {
            ctx,
            fun_tys: Default::default(),
            fun_indices: Default::default(),
        }
    }

    fn add_fun_ty(&mut self, fun_ty: FunTy) -> TypeIdx {
        match self.fun_tys.get(&fun_ty) {
            Some(ty_idx) => *ty_idx,
            None => {
                let idx = TypeIdx(self.fun_tys.len() as u32);
                self.fun_tys.insert(fun_ty, idx);
                idx
            }
        }
    }

    fn get_fun_idx(&self, fun: VarId) -> FunIdx {
        match self.fun_indices.get(&fun) {
            None => {
                panic!("Function called before declared: {:?}", fun);
            }
            Some(fun_idx) => *fun_idx,
        }
    }
}

fn cg_fun(ctx: &mut WasmCtx, fun: &lower::Fun) {
    let Fun {
        name,
        args,
        blocks,
        return_type,
    } = fun;

    // Index of the current function
    let fun_idx = FunIdx(ctx.fun_indices.len() as u32);

    // Add index to the map now, to be able to handle recursive calls
    ctx.fun_indices.insert(*name, fun_idx);

    // Get function type idx
    let arg_tys: Vec<Ty> = args
        .iter()
        .map(|arg| rep_type_to_wasm(ctx.ctx.var_rep_type(*arg)))
        .collect();
    let ret_ty = rep_type_to_wasm(*return_type);
    let fun_ty = FunTy {
        args: arg_tys,
        ret: ret_ty,
    };
    let ty_idx = ctx.add_fun_ty(fun_ty);

    // TODO: This will only work in 'blocks' is in dependency order
    for (block_idx, block) in blocks {
        let block = match block {
            lower::BlockData::NA => {
                continue;
            }
            lower::BlockData::Block(block) => block,
        };

        let mut fun_builder = FunBuilder::new();

        if block.loop_header {
            fun_builder.enter_loop();
        }

        for stmt in &block.stmts {
            cg_stmt(ctx, &mut fun_builder, stmt);

            /*
                        match stmt {
                            Stmt::Asgn(Asgn { lhs, rhs }) => {

                            }
                            Stmt::Expr(expr) => {}
                        }
            */
        }

        if block.loop_header {
            fun_builder.exit_loop();
        }
    }
}

fn rep_type_to_wasm(ty: RepType) -> Ty {
    match ty {
        RepType::Word => Ty::I64,
        RepType::Float => Ty::F64,
    }
}

fn cg_stmt(ctx: &mut WasmCtx, builder: &mut FunBuilder, stmt: &lower::Stmt) {
    match stmt {
        Stmt::Asgn(Asgn { lhs, rhs }) => {
            cg_expr(ctx, builder, rhs);
            builder.local_set(*lhs);
        }
        Stmt::Expr(expr) => {
            cg_expr(ctx, builder, expr);
        }
    }
}

fn cg_expr(ctx: &mut WasmCtx, builder: &mut FunBuilder, stmt: &lower::Expr) {
    match stmt {
        lower::Expr::Atom(atom) => cg_atom(builder, atom),
        lower::Expr::IBinOp(BinOp { op, arg1, arg2 }) => {
            builder.local_get(*arg1);
            builder.local_get(*arg2);
            cg_int_binop(builder, *op);
        }
        lower::Expr::FBinOp(BinOp { op, arg1, arg2 }) => {
            builder.local_get(*arg1);
            builder.local_get(*arg2);
            cg_float_binop(builder, *op);
        }
        lower::Expr::Neg(var) => {
            // NOTE: I think Wasm doesn't have an integer negation op so we have to do `0-x` here
            builder.i64_const(0);
            builder.local_get(*var);
            cg_int_binop(builder, IntBinOp::Sub);
        }
        lower::Expr::FNeg(var) => {
            builder.local_get(*var);
            builder.f64_neg();
        }
        lower::Expr::App(fun, args, ret_ty) => {
            let fun_idx = ctx.get_fun_idx(*fun);
            for arg in args {
                builder.local_get(*arg);
            }
            builder.call(fun_idx);
        }
        lower::Expr::Tuple { len: _ } => todo!(),
        lower::Expr::TupleGet(_, _) => todo!(),
        lower::Expr::TuplePut(_, _, _) => todo!(),
        lower::Expr::ArrayAlloc { len } => todo!(),
        lower::Expr::ArrayGet(_, _) => todo!(),
        lower::Expr::ArrayPut(_, _, _) => todo!(),
    }
}

fn cg_atom(builder: &mut FunBuilder, atom: &lower::Atom) {
    match atom {
        lower::Atom::Unit => builder.i64_const(0),
        lower::Atom::Int(i) => builder.i64_const(*i),
        lower::Atom::Float(f) => builder.f64_const(*f),
        lower::Atom::Var(var) => builder.local_get(*var),
    }
}

fn cg_int_binop(builder: &mut FunBuilder, op: IntBinOp) {
    match op {
        IntBinOp::Add => builder.i64_add(),
        IntBinOp::Sub => builder.i64_sub(),
    }
}

fn cg_float_binop(builder: &mut FunBuilder, op: FloatBinOp) {
    match op {
        FloatBinOp::Add => builder.f64_add(),
        FloatBinOp::Sub => builder.f64_sub(),
        FloatBinOp::Mul => builder.f64_mul(),
        FloatBinOp::Div => builder.f64_div(),
    }
}
