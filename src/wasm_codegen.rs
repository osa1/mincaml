use crate::cg_types::RepType;
use crate::closure_convert::{Expr, Fun};
use crate::common::{BinOp, Cmp, FloatBinOp, IntBinOp};
use crate::ctx::{Ctx, VarId};
use crate::type_check::Type;
use crate::wasm_builder::{FunctionBuilder, GlobalId, ModuleBuilder, ValType};

use fxhash::FxHashMap;

pub fn codegen(ctx: &mut Ctx, funs: &[Fun], main_id: VarId) -> Vec<u8> {
    let mut builder = ModuleBuilder::new();

    // Maps function ids to their indices in the module. Used in `call` instructions and for table
    // indices of the functions.
    //
    // Table index of a function is the same as its index in the module.
    let mut func_idxs: FxHashMap<VarId, usize> = Default::default();

    // Allocate hp (heap pointer) and hp_lim globals (heap pointer limit)
    let hp = builder.new_global(ValType::I32);
    let hp_lim = builder.new_global(ValType::I32);

    // Allocate function indices
    for fun in funs {
        let old_fun_idx = func_idxs.insert(fun.name, func_idxs.len());
        debug_assert_eq!(old_fun_idx, None);
    }

    for fun in funs {
        codegen_fun(ctx, &mut builder, &func_idxs, fun, hp, hp_lim);
    }

    builder.encode(main_id)
}

fn codegen_fun(
    ctx: &mut Ctx,
    builder: &mut ModuleBuilder,
    func_idxs: &FxHashMap<VarId, usize>,
    fun: &Fun,
    hp: GlobalId,
    hp_lim: GlobalId,
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

    codegen_expr(ctx, &mut func_builder, func_idxs, body, &hp, &hp_lim);

    func_builder.finish();
}

fn codegen_expr(
    ctx: &mut Ctx,
    builder: &mut FunctionBuilder,
    func_idxs: &FxHashMap<VarId, usize>,
    expr: &Expr,
    hp: &GlobalId,
    hp_lim: &GlobalId,
) {
    match expr {
        Expr::Unit => builder.i32_const(0),

        Expr::Int(i) => builder.i32_const((*i).try_into().unwrap()),

        // TODO: 64-bit floats truncated to 32-bit
        Expr::Float(f) => builder.f32_const((*f) as f32),

        Expr::IBinOp(BinOp { op, arg1, arg2 }) => {
            builder.get_local(builder.id_wasm_local(*arg1));
            builder.get_local(builder.id_wasm_local(*arg2));
            match op {
                IntBinOp::Add => builder.i32_add(),
                IntBinOp::Sub => builder.i32_sub(),
            }
        }

        Expr::FBinOp(BinOp { op, arg1, arg2 }) => {
            builder.get_local(builder.id_wasm_local(*arg1));
            builder.get_local(builder.id_wasm_local(*arg2));
            match op {
                FloatBinOp::Add => builder.f32_add(),
                FloatBinOp::Sub => builder.f32_sub(),
                FloatBinOp::Mul => builder.f32_mul(),
                FloatBinOp::Div => builder.f32_div(),
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
                    codegen_expr(ctx, builder, func_idxs, else_, hp, hp_lim);
                    builder.br(2);
                });
                codegen_expr(ctx, builder, func_idxs, then_, hp, hp_lim);
            });
        }

        Expr::Let { id, rhs, body } => {
            codegen_expr(ctx, builder, func_idxs, rhs, hp, hp_lim);
            let id_local = builder.new_local(*id, ctx.var_rep_type(*id));
            builder.set_local(id_local);
            codegen_expr(ctx, builder, func_idxs, body, hp, hp_lim);
        }

        Expr::Var(var) => builder.get_local(builder.id_wasm_local(*var)),

        Expr::App(fun, args, _) => {
            let fun_idx = *func_idxs.get(fun).unwrap();
            for arg in args {
                builder.get_local(builder.id_wasm_local(*arg))
            }
            builder.call(fun_idx);
        }

        Expr::Tuple(elems) => {
            let len: i32 = elems.len().try_into().unwrap();

            let tuple = builder.new_local_(RepType::Word);
            alloc(builder, &hp, &hp_lim, |builder| builder.i32_const(len));
            builder.set_local(tuple);

            for (elem_idx, elem) in elems.iter().enumerate() {
                builder.get_local(tuple);
                builder.i32_const((elem_idx as i32) * 4);
                builder.i32_add();

                builder.get_local_id(elem);
                builder.i32_store(0);
            }

            builder.get_local(tuple);
        }

        Expr::TupleGet(tuple, idx, rep_ty) => {
            builder.get_local_id(tuple);
            let offset = idx * 4;
            match rep_ty {
                RepType::Word => {
                    builder.i32_load(offset.try_into().unwrap());
                }
                RepType::Float => {
                    builder.f32_load(offset.try_into().unwrap());
                }
            }
        }

        Expr::ArrayAlloc { len, elem } => {
            let array = builder.new_local_(RepType::Word);
            alloc(builder, &hp, &hp_lim, |builder| builder.get_local_id(len));
            builder.set_local(array);

            // Initialized as 0
            let counter = builder.new_local_(RepType::Word);

            builder.loop_(RepType::Word, |builder| {
                builder.get_local(counter);
                builder.get_local_id(len);
                builder.i32_eq();

                builder.block(RepType::Word, |builder| {
                    builder.br_if(1);

                    // Store address
                    builder.get_local(counter);
                    builder.i32_const(4);
                    builder.i32_mul();
                    builder.get_local(array);
                    builder.i32_add();

                    builder.get_local_id(elem);
                    builder.i32_store(0);
                });
            });

            builder.get_local(array);
        }

        Expr::ArrayGet(array, idx) => {
            builder.get_local_id(array);
            builder.get_local_id(idx);
            builder.i32_const(4);
            builder.i32_mul();
            builder.i32_add();

            let array_type = ctx.var_type(*array);
            let elem_type = match &*array_type {
                Type::Array(elem_type) => RepType::from(&**elem_type),
                other => panic!(
                    "Non-array {} in array location: {:?}",
                    ctx.get_var(*array),
                    other
                ),
            };

            match elem_type {
                RepType::Word => builder.i32_load(0),
                RepType::Float => builder.i32_load(0),
            }
        }

        Expr::ArrayPut(array, idx, value) => {
            builder.get_local_id(array);
            builder.get_local_id(idx);
            builder.i32_const(4);
            builder.i32_mul();
            builder.i32_add();

            builder.get_local_id(value);

            let value_type = ctx.var_type(*value);
            let elem_type = match &*value_type {
                Type::Array(elem_type) => RepType::from(&**elem_type),
                other => panic!(
                    "Non-array {} in array location: {:?}",
                    ctx.get_var(*array),
                    other
                ),
            };

            match elem_type {
                RepType::Word => builder.i32_store(0),
                RepType::Float => builder.f32_store(0),
            }
        }
    }
}

fn alloc<F>(
    builder: &mut FunctionBuilder,
    hp_global: &GlobalId,
    hp_lim_global: &GlobalId,
    push_words: F,
) where
    F: Fn(&mut FunctionBuilder),
{
    builder.global_get(hp_global);
    push_words(builder);
    builder.i32_const(4);
    builder.i32_mul();
    builder.i32_add();

    builder.global_get(hp_lim_global);
    builder.i32_lt_s();

    builder.block(RepType::Word, |builder| {
        builder.br_if(1);

        // hp + (amt * 8) >= hp_lim

        builder.i32_const(1);
        builder.memory_grow();

        builder.global_get(hp_lim_global);
        builder.i32_const(65536);
        builder.i32_add();
        builder.global_set(hp_lim_global);
    });

    // hp + (amt * 8) < hp_lim
    builder.global_get(hp_global); // return value

    // Update hp
    builder.global_get(hp_global);
    push_words(builder);
    builder.i32_const(4);
    builder.i32_mul();
    builder.i32_add();
    builder.global_set(hp_global);
}

fn codegen_cmp(ctx: &mut Ctx, builder: &mut FunctionBuilder, v1: VarId, v2: VarId, cmp: Cmp) {
    let ty = ctx.var_rep_type(v1);
    builder.get_local(builder.id_wasm_local(v2));
    builder.get_local(builder.id_wasm_local(v1));
    match ty {
        RepType::Word => match cmp {
            Cmp::Equal => builder.i32_eq(),
            Cmp::NotEqual => builder.i32_ne(),
            Cmp::LessThan => builder.i32_lt_s(),
            Cmp::LessThanOrEqual => builder.i32_le_s(),
            Cmp::GreaterThan => builder.i32_gt_s(),
            Cmp::GreaterThanOrEqual => builder.i32_ge_s(),
        },
        RepType::Float => match cmp {
            Cmp::Equal => builder.f32_eq(),
            Cmp::NotEqual => builder.f32_ne(),
            Cmp::LessThan => builder.f32_lt(),
            Cmp::LessThanOrEqual => builder.f32_le(),
            Cmp::GreaterThan => builder.f32_gt(),
            Cmp::GreaterThanOrEqual => builder.f32_ge(),
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
