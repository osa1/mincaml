use crate::cg_types::RepType;
use crate::closure_convert::{Expr, Fun};
use crate::common::{BinOp, Cmp, FloatBinOp, IntBinOp};
use crate::ctx::{Ctx, VarId};
use crate::wasm_builder::{FunctionBuilder, FunctionLocalId, ModuleBuilder, ValType};

use fxhash::FxHashMap;

pub fn codegen(ctx: &mut Ctx, funs: &[Fun], main_id: VarId) -> Vec<u8> {
    let mut builder = ModuleBuilder::new();

    // Maps function ids to their indices in the module. Used in `call` instructions and for table
    // indices of the functions.
    //
    // Table index of a function is the same as its index in the module.
    let mut func_idxs: FxHashMap<VarId, usize> = Default::default();

    // Allocate function indices
    for fun in funs {
        let old_fun_idx = func_idxs.insert(fun.name, func_idxs.len());
        debug_assert_eq!(old_fun_idx, None);
    }

    for fun in funs {
        codegen_fun(ctx, &mut builder, &func_idxs, fun);
    }

    builder.encode(main_id)
}

fn codegen_fun(
    ctx: &mut Ctx,
    builder: &mut ModuleBuilder,
    func_idxs: &FxHashMap<VarId, usize>,
    fun: &Fun,
) {
    let Fun {
        name,
        args,
        body,
        return_type,
    } = fun;

    let args: Vec<(VarId, RepType)> = args
        .iter()
        .map(|arg| (*arg, ctx.var_rep_type(*arg)))
        .collect();

    let mut func_builder = builder.new_function(*name, args.clone(), *return_type);

    // Maps function locals and arguments to their indices in the function.
    //
    // Note: `new_function` above allocates locals for arguments
    let mut locals: FxHashMap<VarId, usize> = Default::default();

    for (arg, _) in args {
        let idx = locals.len();
        locals.insert(arg, idx);
    }

    func_builder.finish();
}

fn codegen_expr(
    ctx: &mut Ctx,
    builder: &mut FunctionBuilder,
    func_idxs: &FxHashMap<VarId, usize>,
    expr: &Expr,
) {
    match expr {
        Expr::Unit => builder.i64_const(0),

        Expr::Int(i) => builder.i64_const(*i),

        Expr::Float(f) => builder.f64_const(*f),

        Expr::IBinOp(BinOp { op, arg1, arg2 }) => {
            builder.get_local(builder.id_wasm_local(*arg1));
            builder.get_local(builder.id_wasm_local(*arg2));
            match op {
                IntBinOp::Add => builder.i64_add(),
                IntBinOp::Sub => builder.i64_sub(),
            }
        }

        Expr::FBinOp(BinOp { op, arg1, arg2 }) => {
            builder.get_local(builder.id_wasm_local(*arg1));
            builder.get_local(builder.id_wasm_local(*arg2));
            match op {
                FloatBinOp::Add => builder.f64_add(),
                FloatBinOp::Sub => builder.f64_sub(),
                FloatBinOp::Mul => builder.f64_mul(),
                FloatBinOp::Div => builder.f64_div(),
            }
        }

        Expr::Neg(_) => todo!(),

        Expr::FNeg(_) => todo!(),

        Expr::If(v1, v2, cmp, then_, else_) => {
            // <cmp(v1, v2)>
            // block
            //   block
            //     br_if 1
            //     <else branch>
            //     br 2
            //   end
            //   <then branch>
            // end
            codegen_cmp(ctx, builder, *v1, *v2, *cmp);
            let block_ty = expr_type(ctx, then_);
            builder.block(block_ty, |builder| {
                builder.block(block_ty, |builder| {
                    builder.br_if(1);
                    codegen_expr(ctx, builder, func_idxs, else_);
                    builder.br(2);
                });
                codegen_expr(ctx, builder, func_idxs, then_);
            });
        }

        Expr::Let { id, rhs, body } => {
            codegen_expr(ctx, builder, func_idxs, rhs);
            let id_local = builder.new_local(*id, ctx.var_rep_type(*id));
            builder.set_local(id_local);
            codegen_expr(ctx, builder, func_idxs, body);
        }

        Expr::Var(var) => builder.get_local(builder.id_wasm_local(*var)),

        Expr::App(fun, args, ret_ty) => {
            todo!()
        }

        Expr::Tuple(_) => todo!(),

        Expr::TupleGet(_, _, _) => todo!(),

        Expr::ArrayAlloc { len, elem } => todo!(),

        Expr::ArrayGet(_, _) => todo!(),

        Expr::ArrayPut(_, _, _) => todo!(),
    }
}

fn codegen_cmp(ctx: &mut Ctx, builder: &mut FunctionBuilder, v1: VarId, v2: VarId, cmp: Cmp) {
    let ty = ctx.var_rep_type(v1);
    builder.get_local(builder.id_wasm_local(v2));
    builder.get_local(builder.id_wasm_local(v1));
    match ty {
        RepType::Word => match cmp {
            Cmp::Equal => builder.i64_eq(),
            Cmp::NotEqual => builder.i64_ne(),
            Cmp::LessThan => builder.i64_lt_s(),
            Cmp::LessThanOrEqual => builder.i64_le_s(),
            Cmp::GreaterThan => builder.i64_gt_s(),
            Cmp::GreaterThanOrEqual => builder.i64_ge_s(),
        },
        RepType::Float => match cmp {
            Cmp::Equal => builder.f64_eq(),
            Cmp::NotEqual => builder.f64_ne(),
            Cmp::LessThan => builder.f64_lt(),
            Cmp::LessThanOrEqual => builder.f64_le(),
            Cmp::GreaterThan => builder.f64_gt(),
            Cmp::GreaterThanOrEqual => builder.f64_ge(),
        },
    }
}

fn expr_type(ctx: &mut Ctx, expr: &Expr) -> RepType {
    match expr {
        Expr::Unit
        | Expr::Int(_)
        | Expr::IBinOp(_)
        | Expr::Neg(_)
        | Expr::Tuple(_)
        | Expr::ArrayAlloc { .. } => RepType::Word,

        Expr::Float(_) | Expr::FBinOp(_) | Expr::FNeg(_) => RepType::Float,

        Expr::If(_, _, _, then_, _) => expr_type(ctx, &*then_),

        Expr::Let {
            id: _,
            rhs: _,
            body,
        } => expr_type(ctx, &*body),

        Expr::Var(var) => ctx.var_rep_type(*var),

        Expr::App(_, _, ty) => *ty,

        Expr::TupleGet(_, _, rep_ty) => *rep_ty,

        Expr::ArrayGet(arr, _) => match &*ctx.var_type(*arr) {
            crate::type_check::Type::Array(ty) => RepType::from(&**ty),
            _ => panic!(),
        },

        Expr::ArrayPut(_, _, val) => ctx.var_rep_type(*val),
    }
}
