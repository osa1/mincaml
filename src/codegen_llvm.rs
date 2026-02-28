use cranelift_codegen::entity::EntityRef;
use inkwell::OptimizationLevel;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, FunctionValue, GlobalValue, PointerValue,
};
use inkwell::{AddressSpace, IntPredicate};

use fxhash::{FxHashMap, FxHashSet};

use crate::cg_types::RepType;
use crate::common::{BinOp, Cmp, FloatBinOp, IntBinOp};
use crate::ctx::{Ctx, VarId};
use crate::lower;

#[allow(unused)]
pub fn codegen(ctx: &mut Ctx, funs: &[lower::Fun], main_id: VarId, dump: bool) -> Vec<u8> {
    // `Context` is a container for all LLVM entities, including modules (compilation units).
    let context = Context::create();

    // The compilation unit.
    //
    // TODO: I'm not sure what the "name" argument is for?
    let module = context.create_module("hi");

    let mut import_env: FxHashMap<VarId, GlobalValue> = Default::default();
    let mut fun_env: FxHashMap<VarId, FunctionValue> = Default::default();

    // Declare `malloc`.
    let i64_type = context.i64_type();
    // It's a bit strange that function types are created via a method on the return type. I think
    // this is an `inkwell` thing rather than LLVM.
    let malloc_type = i64_type.fn_type(&[i64_type.into()], false);
    let malloc_id = module.add_function("malloc", malloc_type, Some(Linkage::External));

    // Declare built-ins.
    //
    // Note: Built-ins are imported as closures (i.e. statically defined structs) rather than
    // functions.
    //
    // Note: apparently LLVM doesn't care about function vs. data pointers (at least in some
    // contexts). Inkwell generates this warning:
    // ```
    // Starting from version 15.0, LLVM doesn't differentiate between pointer types. Use
    // Context::ptr_type instead.
    // ```
    let fn_ptr_type = context.ptr_type(AddressSpace::default());
    let static_closure_type = context.struct_type(&[fn_ptr_type.into()], false);
    for (builtin_var_id, _ty_id) in ctx.builtins() {
        let var = ctx.get_var(*builtin_var_id);
        let name = var.symbol_name();
        let builtin_id = module.add_global(static_closure_type, None, &*name);
        let old = import_env.insert(*builtin_var_id, builtin_id);
        assert!(old.is_none());
    }

    let mut main_fun_id: Option<FunctionValue> = None;

    // Declare functions
    for lower::Fun {
        name,
        args,
        return_type,
        ..
    } in funs
    {
        let ret: BasicTypeEnum = match return_type {
            RepType::Word => context.i64_type().into(),
            RepType::Float => context.f64_type().into(),
        };

        let args: Vec<BasicMetadataTypeEnum> = args
            .iter()
            .map(|arg| match ctx.var_rep_type(*arg) {
                RepType::Word => context.i64_type().into(),
                RepType::Float => context.f64_type().into(),
            })
            .collect();

        let fun_type = ret.fn_type(&args, false);

        let name_str = ctx.get_var(*name).name();

        let fun_id = module.add_function(
            &*name_str,
            fun_type,
            Some(inkwell::module::Linkage::Internal),
        );

        if *name == main_id {
            main_fun_id = Some(fun_id);
            assert!(args.is_empty());
            assert_eq!(*return_type, RepType::Word);
        }

        let old = fun_env.insert(*name, fun_id);
        assert!(old.is_none());
    }

    // Generate code for functions.
    for fun in funs {
        codegen_fun(ctx, &context, fun, &import_env, &fun_env, malloc_id, false);
    }

    make_main(&context, &module, main_fun_id.unwrap());

    // TODO: Not sure what this does...
    Target::initialize_native(&InitializationConfig::default()).unwrap();

    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).unwrap();
    let machine = target
        .create_target_machine(
            &triple,
            "generic", // cpu
            "",        // features
            OptimizationLevel::Default,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .unwrap();

    let buf = machine
        .write_to_memory_buffer(&module, FileType::Object)
        .unwrap();

    buf.as_slice().to_vec()
}

fn codegen_fun(
    ctx: &mut Ctx,
    context: &Context,
    fun: &lower::Fun,
    import_env: &FxHashMap<VarId, GlobalValue>,
    fun_env: &FxHashMap<VarId, FunctionValue>,
    malloc: FunctionValue,
    dump: bool,
) {
    let lower::Fun {
        name,
        args,
        blocks,
        return_type,
    } = fun;

    let fun_val = *fun_env.get(name).unwrap();

    let mut label_to_block: FxHashMap<lower::BlockIdx, BasicBlock> = Default::default();

    // First block implicitly becomes the entry block in LLVM, so sort the based on index. (index 0
    // is the entry block)
    // TODO: We also don't seem to explicitly set the entry block in Cranelift?
    let mut blocks_sorted: Vec<&lower::Block> = blocks
        .values()
        .filter_map(lower::BlockData::get_block)
        .collect();

    blocks_sorted.sort_by_key(|b| b.idx);

    for (block_idx, block) in blocks_sorted.iter().enumerate() {
        let basic_block = context.append_basic_block(fun_val, &format!("b{block_idx}"));
        label_to_block.insert(block.idx, basic_block);
    }

    let builder = context.create_builder();

    // Stack-allocate all arguments and locals. AFAIU this is different from Cranelift: in LLVM we
    // don't have a non-SSA IR for front-ends, we directly generate SSA form. To avoid dealing with
    // SSA-ification here we stack-allocate all arguments and locals, then run the `mem2reg` pass
    // which moves those stack allocated values to locals/register and handles SSA-ification.
    //
    // Allocas must be in the entry block for `mem2reg` to promote them.
    let entry_block = *label_to_block.get(&lower::BlockIdx::new(0)).unwrap();
    builder.position_at_end(entry_block);

    let mut local_env: FxHashMap<VarId, PointerValue> = Default::default();

    // Add arguments to env.
    for (arg_idx, arg) in args.iter().enumerate() {
        let arg_name = ctx.get_var(*arg).name();
        let arg_val: BasicValueEnum = fun_val.get_nth_param(arg_idx as u32).unwrap();
        let arg_ptr: PointerValue = builder
            .build_alloca(arg_val.get_type(), &*arg_name)
            .unwrap();
        builder.build_store(arg_ptr, arg_val).unwrap();
        let old = local_env.insert(*arg, arg_ptr);
        assert!(old.is_none());
    }

    // Add locals to env.
    let mut declared: FxHashSet<VarId> = Default::default();
    for lower::Block { stmts, .. } in blocks_sorted.iter() {
        for stmt in stmts {
            match stmt {
                lower::Stmt::Asgn(lower::Asgn { lhs, rhs: _ }) => {
                    if !declared.contains(lhs) {
                        let arg_name = ctx.get_var(*lhs).name();
                        declared.insert(*lhs);
                        let var_rep_type = ctx.var_rep_type(*lhs);
                        let ty: BasicTypeEnum = match var_rep_type {
                            RepType::Word => context.i64_type().into(),
                            RepType::Float => context.f64_type().into(),
                        };
                        let ptr = builder.build_alloca(ty, &*arg_name).unwrap();
                        local_env.insert(*lhs, ptr);
                    }
                }
                lower::Stmt::Expr(_) => {}
            }
        }
    }

    // Generate code for blocks.
    for block in blocks_sorted.iter() {
        let lower::Block {
            idx,
            comment: _,
            stmts,
            exit,
        } = block;

        let basic_block = *label_to_block.get(idx).unwrap();
        builder.position_at_end(basic_block);

        for stmt in stmts {
            match stmt {
                lower::Stmt::Asgn(lower::Asgn { lhs, rhs }) => {
                    let val = codegen_expr(
                        ctx, context, &builder, import_env, fun_env, &local_env, malloc, rhs,
                    )
                    .unwrap();
                    let lhs_alloca = local_env.get(lhs).unwrap();
                    builder.build_store(*lhs_alloca, val).unwrap();
                }

                lower::Stmt::Expr(expr) => {
                    codegen_expr(
                        ctx, context, &builder, import_env, fun_env, &local_env, malloc, expr,
                    );
                }
            }
        }

        match exit {
            lower::Exit::Return(var) => {
                let ret = use_var(
                    ctx, context, *var, import_env, fun_env, &local_env, &builder,
                );
                builder.build_return(Some(&ret)).unwrap();
            }

            lower::Exit::Branch {
                v1,
                v2,
                cond,
                then_block,
                else_block,
            } => {
                let pred = match cond {
                    Cmp::Equal => IntPredicate::EQ,
                    Cmp::NotEqual => IntPredicate::NE,
                    Cmp::LessThan => IntPredicate::SLT,
                    Cmp::LessThanOrEqual => IntPredicate::SLE,
                    Cmp::GreaterThan => IntPredicate::SGT,
                    Cmp::GreaterThanOrEqual => IntPredicate::SGE,
                };
                let v1 = use_var(ctx, context, *v1, import_env, fun_env, &local_env, &builder);
                let v2 = use_var(ctx, context, *v2, import_env, fun_env, &local_env, &builder);
                let cmp = builder
                    .build_int_compare(pred, v1.into_int_value(), v2.into_int_value(), "exit")
                    .unwrap();
                let then_ll = *label_to_block.get(then_block).unwrap();
                let else_ll = *label_to_block.get(else_block).unwrap();
                builder
                    .build_conditional_branch(cmp, then_ll, else_ll)
                    .unwrap();
            }

            lower::Exit::Jump(block_idx) => {
                let ll_block = *label_to_block.get(block_idx).unwrap();
                builder.build_unconditional_branch(ll_block).unwrap();
            }
        }
    }
}

fn codegen_expr<'a>(
    ctx: &mut Ctx,
    context: &'a Context,
    builder: &Builder<'a>,
    import_env: &FxHashMap<VarId, GlobalValue<'a>>,
    fun_env: &FxHashMap<VarId, FunctionValue<'a>>,
    local_env: &FxHashMap<VarId, PointerValue<'a>>,
    malloc: FunctionValue<'a>,
    expr: &lower::Expr,
) -> Option<BasicValueEnum<'a>> {
    match expr {
        lower::Expr::Atom(lower::Atom::Unit) => Some(context.i64_type().const_int(0, false).into()),

        lower::Expr::Atom(lower::Atom::Int(i)) => {
            Some(context.i64_type().const_int(*i as u64, false).into())
        }

        lower::Expr::Atom(lower::Atom::Float(f)) => Some(context.f64_type().const_float(*f).into()),

        lower::Expr::Atom(lower::Atom::Var(var)) => {
            let var_rep_type = ctx.var_rep_type(*var);
            let var_type: BasicTypeEnum = match var_rep_type {
                RepType::Word => context.i64_type().into(),
                RepType::Float => context.f64_type().into(),
            };
            let var_name = ctx.get_var(*var).name();
            let var_alloca = use_var(ctx, context, *var, import_env, fun_env, local_env, builder);
            let val = builder
                .build_load(var_type, var_alloca.into_pointer_value(), &*var_name)
                .unwrap();
            Some(val)
        }

        lower::Expr::IBinOp(BinOp { op, arg1, arg2 }) => {
            let val1 = use_var(ctx, context, *arg1, import_env, fun_env, local_env, builder);
            let val2 = use_var(ctx, context, *arg2, import_env, fun_env, local_env, builder);
            match op {
                IntBinOp::Add => Some(
                    builder
                        .build_int_add(val1.into_int_value(), val2.into_int_value(), "sum")
                        .unwrap()
                        .into(),
                ),
                IntBinOp::Sub => Some(
                    builder
                        .build_int_sub(val1.into_int_value(), val2.into_int_value(), "sum")
                        .unwrap()
                        .into(),
                ),
            }
        }

        lower::Expr::FBinOp(BinOp { op, arg1, arg2 }) => {
            let val1 = use_var(ctx, context, *arg1, import_env, fun_env, local_env, builder);
            let val2 = use_var(ctx, context, *arg2, import_env, fun_env, local_env, builder);
            match op {
                FloatBinOp::Add => Some(
                    builder
                        .build_float_add(val1.into_float_value(), val2.into_float_value(), "add")
                        .unwrap()
                        .into(),
                ),
                FloatBinOp::Sub => Some(
                    builder
                        .build_float_sub(val1.into_float_value(), val2.into_float_value(), "sub")
                        .unwrap()
                        .into(),
                ),
                FloatBinOp::Mul => Some(
                    builder
                        .build_float_mul(val1.into_float_value(), val2.into_float_value(), "mul")
                        .unwrap()
                        .into(),
                ),
                FloatBinOp::Div => Some(
                    builder
                        .build_float_div(val1.into_float_value(), val2.into_float_value(), "div")
                        .unwrap()
                        .into(),
                ),
            }
        }

        lower::Expr::Neg(var) => {
            let val = use_var(ctx, context, *var, import_env, fun_env, local_env, builder);

            Some(
                builder
                    .build_int_neg(val.into_int_value(), "neg")
                    .unwrap()
                    .into(),
            )
        }

        lower::Expr::FNeg(var) => {
            let val = use_var(ctx, context, *var, import_env, fun_env, local_env, builder);
            Some(
                builder
                    .build_float_neg(val.into_float_value(), "neg")
                    .unwrap()
                    .into(),
            )
        }

        lower::Expr::App(fun, args, ret_type) => {
            let ret_type: BasicTypeEnum = match ret_type {
                RepType::Word => context.i64_type().into(),
                RepType::Float => context.f64_type().into(),
            };

            let param_types: Vec<BasicMetadataTypeEnum> = args
                .iter()
                .map(|arg| match ctx.var_rep_type(*arg) {
                    RepType::Word => context.i64_type().into(),
                    RepType::Float => context.f64_type().into(),
                })
                .collect();

            let fun_type = ret_type.fn_type(&param_types, false);

            let fun_ptr = use_var(ctx, context, *fun, import_env, fun_env, local_env, builder)
                .into_pointer_value();

            let arg_vals: Vec<BasicMetadataValueEnum> = args
                .iter()
                .map(|arg| {
                    use_var(ctx, context, *arg, import_env, fun_env, local_env, builder).into()
                })
                .collect();

            let call = builder
                .build_indirect_call(fun_type, fun_ptr, &arg_vals, "call")
                .unwrap();

            let ret_val = call.try_as_basic_value().expect_basic("welp");

            Some(ret_val)
        }

        lower::Expr::Tuple { len } => {
            let malloc_arg = context.i64_type().const_int((len * 8) as u64, false);
            let malloc_call = builder
                .build_direct_call(malloc, &[malloc_arg.into()], "malloc")
                .unwrap();
            Some(malloc_call.try_as_basic_value().expect_basic("welp"))
        }

        lower::Expr::TupleGet(tuple, idx, elem_type) => {
            let elem_type: BasicTypeEnum = match elem_type {
                RepType::Word => context.i64_type().into(),
                RepType::Float => context.f64_type().into(),
            };
            let tuple = use_var(
                ctx, context, *tuple, import_env, fun_env, local_env, builder,
            );
            let elem_ptr = unsafe {
                builder
                    .build_gep(
                        context.i64_type(),
                        tuple.into_pointer_value(),
                        &[context.i64_type().const_int(*idx as u64, false)],
                        "tuple.get idx",
                    )
                    .unwrap()
            };
            let val = builder
                .build_load(elem_type, elem_ptr, "tuple.get")
                .unwrap();
            Some(val)
        }

        lower::Expr::TuplePut(tuple, idx, val) => {
            let tuple = use_var(
                ctx, context, *tuple, import_env, fun_env, local_env, builder,
            );
            let elem_ptr = unsafe {
                builder
                    .build_gep(
                        context.i64_type(),
                        tuple.into_pointer_value(),
                        &[context.i64_type().const_int(*idx as u64, false)],
                        "tuple.set idx",
                    )
                    .unwrap()
            };
            let val = use_var(ctx, context, *val, import_env, fun_env, local_env, builder);
            builder.build_store(elem_ptr, val).unwrap();
            None
        }

        lower::Expr::ArrayAlloc { len } => {
            let len = use_var(ctx, context, *len, import_env, fun_env, local_env, builder);
            let word_size = context.i64_type().const_int(8, false);
            let size_val = builder
                .build_int_mul(len.into_int_value(), word_size.into(), "size")
                .unwrap();
            let malloc_call = builder
                .build_direct_call(malloc, &[size_val.into()], "array.new")
                .unwrap()
                .try_as_basic_value()
                .expect_basic("welp");
            Some(malloc_call)
        }

        lower::Expr::ArrayGet(array, idx) => {
            let var_type = ctx.var_type(*array);
            let elem_type: BasicTypeEnum = match &*var_type {
                crate::type_check::Type::Array(elem_type) => match RepType::from(&**elem_type) {
                    RepType::Word => context.i64_type().into(),
                    RepType::Float => context.f64_type().into(),
                },
                other => panic!(
                    "Non-array {} in array location: {:?}",
                    ctx.get_var(*array),
                    other
                ),
            };

            let array = use_var(
                ctx, context, *array, import_env, fun_env, local_env, builder,
            );
            let idx = use_var(ctx, context, *idx, import_env, fun_env, local_env, builder);
            let elem_ptr = unsafe {
                builder
                    .build_gep(
                        context.i64_type(),
                        array.into_pointer_value(),
                        &[idx.into_int_value()],
                        "array.get offset",
                    )
                    .unwrap()
            };
            let ret = builder
                .build_load(elem_type, elem_ptr, "array.get")
                .unwrap();
            Some(ret)
        }

        lower::Expr::ArrayPut(array, idx, val) => {
            let array = use_var(
                ctx, context, *array, import_env, fun_env, local_env, builder,
            );
            let idx = use_var(ctx, context, *idx, import_env, fun_env, local_env, builder);
            let elem_ptr = unsafe {
                builder
                    .build_gep(
                        context.i64_type(),
                        array.into_pointer_value(),
                        &[idx.into_int_value()],
                        "array.get offset",
                    )
                    .unwrap()
            };
            let val = use_var(ctx, context, *val, import_env, fun_env, local_env, builder);
            builder.build_store(elem_ptr, val).unwrap();
            None
        }
    }
}

// In the Cranelift backend we have an `Env` that maps `VarId`s to one of: imports, local functions,
// local variables. I'm too lazy to do the same here but it's actually a good idea and we should do
// it later.
fn use_var<'a>(
    ctx: &mut Ctx,
    context: &'a Context,
    var: VarId,

    // Imported closures.
    import_env: &FxHashMap<VarId, GlobalValue<'a>>,

    // Functions in the compilation unit.
    fun_env: &FxHashMap<VarId, FunctionValue<'a>>,

    // Function locals.
    local_env: &FxHashMap<VarId, PointerValue<'a>>,

    builder: &Builder<'a>,
) -> BasicValueEnum<'a> {
    let var_name = ctx.get_var(var).name();
    let var_rep_type = ctx.var_rep_type(var);
    let var_type: BasicTypeEnum = match var_rep_type {
        RepType::Word => context.i64_type().into(),
        RepType::Float => context.f64_type().into(),
    };
    if let Some(ptr) = local_env.get(&var) {
        return builder.build_load(var_type, *ptr, &*var_name).unwrap();
    }

    if let Some(fun) = fun_env.get(&var) {
        return fun.as_global_value().as_pointer_value().into();
    }

    if let Some(import) = import_env.get(&var) {
        return import.as_pointer_value().into();
    }

    panic!("Unbound variable");
}

fn make_main<'a>(context: &'a Context, module: &Module<'a>, main: FunctionValue<'a>) {
    let i32_type = context.i32_type();
    let main_type = i32_type.fn_type(&[], false);
    let main_fn = module.add_function("main", main_type, Some(Linkage::External));

    let builder = context.create_builder();
    let block = context.append_basic_block(main_fn, "entry");
    builder.position_at_end(block);

    builder.build_direct_call(main, &[], "").unwrap();
    builder
        .build_return(Some(&i32_type.const_int(0, false)))
        .unwrap();
}
