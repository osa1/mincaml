use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{FunctionValue, GlobalValue};

use fxhash::FxHashMap;

use crate::cg_types::RepType;
use crate::ctx::{Ctx, VarId};
use crate::lower;

pub fn codegen(ctx: &mut Ctx, funs: &[lower::Fun], main_id: VarId, dump: bool) -> Vec<u8> {
    // `Context` is a container for all LLVM entities, including modules (compilation units).
    let context = Context::create();

    // The compilation unit.
    //
    // TODO: I'm not sure what the "name" argument is for?
    let module = context.create_module("hi");

    let mut global_env: FxHashMap<VarId, GlobalValue> = Default::default();
    let mut fun_env: FxHashMap<VarId, FunctionValue> = Default::default();

    // Declare `malloc`.
    let i64_type = context.i64_type();
    // It's a bit strange that function types are created via a method on the return type. I think
    // this is an `inkwell` thing rather than LLVM.
    let malloc_type = i64_type.fn_type(&[i64_type.into()], false);
    let malloc_id = module.add_function(
        "malloc",
        malloc_type,
        Some(inkwell::module::Linkage::External),
    );

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
        let old = global_env.insert(*builtin_var_id, builtin_id);
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
            crate::cg_types::RepType::Word => context.i64_type().into(),
            crate::cg_types::RepType::Float => context.f64_type().into(),
        };

        let args: Vec<BasicMetadataTypeEnum> = args
            .iter()
            .map(|arg| match ctx.var_rep_type(*arg) {
                crate::cg_types::RepType::Word => context.i64_type().into(),
                crate::cg_types::RepType::Float => context.f64_type().into(),
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

    todo!();
}

fn codegen_fun() {}
