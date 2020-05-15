use cranelift_codegen::binemit::NullTrapSink;
use cranelift_codegen::entity::EntityRef;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::entities::{Block, FuncRef, GlobalValue, SigRef, Value};
use cranelift_codegen::ir::types::*;
use cranelift_codegen::ir::MemFlags;
use cranelift_codegen::ir::{AbiParam, InstBuilder, Signature};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings;
use cranelift_codegen::verifier::verify_function;
use cranelift_codegen::Context;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{default_libcall_names, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBackend, ObjectBuilder, ObjectProduct};

use fxhash::FxHashMap;
use std::fs::File;
use std::io::Write;

use crate::cg_types::RepType;
use crate::closure_convert as cc;
use crate::common::{Cmp, FloatBinOp, IntBinOp};
use crate::ctx::{Ctx, VarId};

pub fn codegen(ctx: &mut Ctx, funs: &[cc::Fun], main_id: VarId) {
    //
    // State shared between all functions in the compilation unit
    //

    // Module and FunctionBuilderContext are used for the whole compilation unit. Each function
    // gets its own FunctionBuilder.

    let codegen_flags: settings::Flags = settings::Flags::new(settings::builder());
    let mut module: Module<ObjectBackend> = Module::new(ObjectBuilder::new(
        // How does this know I'm building for x86_64 Linux?
        cranelift_native::builder().unwrap().finish(codegen_flags),
        [1, 2, 3, 4, 5, 6, 7, 8], // TODO: what is this?
        default_libcall_names(),
    ));

    let mut fn_builder_ctx: FunctionBuilderContext = FunctionBuilderContext::new();

    // Malloc is defined at the module level.
    let malloc_id: FuncId = module
        .declare_function(
            "malloc",
            Linkage::Import,
            &Signature {
                params: vec![AbiParam::new(I64)],
                returns: vec![AbiParam::new(I64)],
                call_conv: CallConv::SystemV,
            },
        )
        .unwrap();

    let print_int_id: DataId = module
        .declare_data("print_int", Linkage::Import, false, false, None)
        .unwrap();

    let mut data_map: FxHashMap<VarId, DataId> = Default::default();
    data_map.insert(ctx.print_int_var(), print_int_id);

    // Map function names do FuncIds. When a variable used is a function we need to (1) import it
    // in the using function (using `module.declare_func_in_func` which is a terrible name for
    // "import function") (2) get the address value using `ins.builder().func_addr`.
    let mut fun_map: FxHashMap<VarId, FuncId> = Default::default();
    for cc::Fun {
        name,
        args,
        return_type,
        ..
    } in funs
    {
        let params: Vec<AbiParam> = args
            .iter()
            .map(|arg| AbiParam::new(rep_type_abi(ctx.var_rep_type(*arg))))
            .collect();

        let returns: Vec<AbiParam> = vec![AbiParam::new(rep_type_abi(*return_type))];

        let sig = Signature {
            params,
            returns,
            call_conv: CallConv::SystemV,
        };
        let id: FuncId = module
            .declare_function(&*ctx.get_var(*name).name(), Linkage::Local, &sig)
            .unwrap();
        fun_map.insert(*name, id);
    }

    //
    // End of shared state
    //

    // Generate code for functions

    for cc::Fun {
        name,
        args,
        blocks,
        return_type,
    } in funs
    {
        let mut context: Context = module.make_context();

        let signature: &mut Signature = &mut context.func.signature;
        for arg in args {
            let arg_type = ctx.var_rep_type(*arg);
            let arg_abi_type = rep_type_abi(arg_type);
            signature.params.push(AbiParam::new(arg_abi_type));
        }
        signature
            .returns
            .push(AbiParam::new(rep_type_abi(*return_type)));

        let func_name = ctx.get_var(*name).name();
        let func_id = module
            .declare_function(&*func_name, Linkage::Local, &signature)
            .unwrap();

        let malloc: FuncRef = module.declare_func_in_func(malloc_id, &mut context.func);

        let mut builder: FunctionBuilder =
            FunctionBuilder::new(&mut context.func, &mut fn_builder_ctx);

        {
            let mut label_to_block: FxHashMap<cc::Label, Block> = Default::default();

            for block in blocks {
                let cranelift_block = builder.create_block();
                label_to_block.insert(block.label, cranelift_block);
            }

            let entry_block = *label_to_block.get(&blocks[0].label).unwrap();
            builder.switch_to_block(entry_block);
            builder.append_block_params_for_function_params(entry_block);

            let arg_map: FxHashMap<VarId, Value> = args
                .iter()
                .enumerate()
                .map(|(arg_idx, arg)| (*arg, builder.block_params(entry_block)[arg_idx]))
                .collect();

            for cc::Block { label, stmts, exit } in blocks {
                let cranelift_block = *label_to_block.get(label).unwrap();
                builder.switch_to_block(cranelift_block);

                for asgn in stmts {
                    // let mut s = String::new();
                    // asgn.pp(&ctx, &mut s);
                    // println!("stmt: {}", s);

                    let cc::Asgn { lhs, rhs } = asgn;

                    let val = rhs_value(
                        ctx,
                        &module,
                        &mut builder,
                        &arg_map,
                        &fun_map,
                        &data_map,
                        malloc,
                        rhs,
                    );
                    let var = declare_var(ctx, &mut builder, *lhs);
                    builder.def_var(var, val);
                }

                match exit {
                    cc::Exit::Return(var) => {
                        let var = use_var(
                            ctx,
                            &module,
                            &mut builder,
                            &arg_map,
                            &fun_map,
                            &data_map,
                            *var,
                        );
                        builder.ins().return_(&[var]);
                    }
                    cc::Exit::Branch {
                        v1,
                        v2,
                        cond,
                        then_label,
                        else_label,
                    } => {
                        let cond = cranelift_cond(*cond);
                        // TODO: float comparisons?
                        let v1 = use_var(
                            ctx,
                            &module,
                            &mut builder,
                            &arg_map,
                            &fun_map,
                            &data_map,
                            *v1,
                        );
                        let v2 = use_var(
                            ctx,
                            &module,
                            &mut builder,
                            &arg_map,
                            &fun_map,
                            &data_map,
                            *v2,
                        );
                        let then_block = *label_to_block.get(then_label).unwrap();
                        builder.ins().br_icmp(cond, v1, v2, then_block, &[]);
                        let else_block = *label_to_block.get(else_label).unwrap();
                        builder.ins().jump(else_block, &[]);
                    }
                    cc::Exit::Jump(label) => {
                        let cranelift_block = *label_to_block.get(label).unwrap();
                        // Not sure about the arguments here...
                        builder.ins().jump(cranelift_block, &[]);
                    }
                }

                builder.seal_block(cranelift_block);
            }
        }

        builder.finalize();

        let flags = settings::Flags::new(settings::builder());
        let res = verify_function(&context.func, &flags);

        println!("{}", context.func.display(None));
        if let Err(errors) = res {
            panic!("{}", errors);
        }

        module
            .define_function(func_id, &mut context, &mut NullTrapSink {})
            .unwrap();
        module.clear_context(&mut context);
    }

    // Generate main
    let main_func_id: FuncId = *fun_map.get(&main_id).unwrap();
    make_main(&mut module, &mut fn_builder_ctx, main_func_id);

    module.finalize_definitions();

    let object: ObjectProduct = module.finish();
    let bytes: Vec<u8> = object.emit().unwrap();
    File::create("a.o").unwrap().write_all(&bytes).unwrap();
}

fn rhs_value(
    ctx: &mut Ctx, module: &Module<ObjectBackend>, builder: &mut FunctionBuilder,
    arg_map: &FxHashMap<VarId, Value>, fun_map: &FxHashMap<VarId, FuncId>,
    data_map: &FxHashMap<VarId, DataId>, malloc: FuncRef, rhs: &cc::Expr,
) -> Value {
    match rhs {
        cc::Expr::Atom(cc::Atom::Unit) => builder.ins().iconst(I64, 0),
        cc::Expr::Atom(cc::Atom::Int(i)) => builder.ins().iconst(I64, *i),
        cc::Expr::Atom(cc::Atom::Float(f)) => builder.ins().f64const(*f),
        cc::Expr::Atom(cc::Atom::Var(var)) => {
            use_var(ctx, module, builder, arg_map, fun_map, data_map, *var)
        }

        cc::Expr::IBinOp(cc::BinOp { op, arg1, arg2 }) => {
            let arg1 = use_var(ctx, module, builder, arg_map, fun_map, data_map, *arg1);
            let arg2 = use_var(ctx, module, builder, arg_map, fun_map, data_map, *arg2);
            match op {
                IntBinOp::Add => builder.ins().iadd(arg1, arg2),
                IntBinOp::Sub => builder.ins().isub(arg1, arg2),
            }
        }

        cc::Expr::FBinOp(cc::BinOp { op, arg1, arg2 }) => {
            let arg1 = use_var(ctx, module, builder, arg_map, fun_map, data_map, *arg1);
            let arg2 = use_var(ctx, module, builder, arg_map, fun_map, data_map, *arg2);
            match op {
                FloatBinOp::Add => builder.ins().fadd(arg1, arg2),
                FloatBinOp::Sub => builder.ins().fsub(arg1, arg2),
                FloatBinOp::Mul => builder.ins().fmul(arg1, arg2),
                FloatBinOp::Div => builder.ins().fdiv(arg1, arg2),
            }
        }

        cc::Expr::Neg(var) => {
            let arg = builder.use_var(varid_var(ctx, *var));
            builder.ins().ineg(arg)
        }

        cc::Expr::FNeg(var) => {
            let arg = builder.use_var(varid_var(ctx, *var));
            builder.ins().fneg(arg)
        }

        cc::Expr::App(fun, args, ret_type) => {
            let params: Vec<AbiParam> = args
                .iter()
                .map(|arg| {
                    let arg_ty = ctx.var_rep_type(*arg);
                    AbiParam::new(rep_type_abi(arg_ty))
                })
                .collect();

            let returns: Vec<AbiParam> = vec![AbiParam::new(rep_type_abi(*ret_type))];
            let fun_sig = Signature {
                params,
                returns,
                call_conv: CallConv::SystemV,
            };
            let fun_sig_ref: SigRef = builder.import_signature(fun_sig);

            let callee = use_var(ctx, module, builder, arg_map, fun_map, data_map, *fun);

            let arg_vals: Vec<Value> = args
                .iter()
                .map(|arg| use_var(ctx, module, builder, arg_map, fun_map, data_map, *arg))
                .collect();
            let call = builder.ins().call_indirect(fun_sig_ref, callee, &arg_vals);
            builder.inst_results(call)[0]
        }

        cc::Expr::Tuple(args) => {
            let malloc_arg = builder.ins().iconst(I64, args.len() as i64);
            let malloc_call = builder.ins().call(malloc, &[malloc_arg]);
            let tuple = builder.inst_results(malloc_call)[0];
            for (arg_idx, arg) in args.iter().enumerate() {
                let arg = use_var(ctx, module, builder, &arg_map, &fun_map, data_map, *arg);
                // TODO: hard-coded word size
                builder
                    .ins()
                    .store(MemFlags::new(), arg, tuple, (arg_idx * 8) as i32);
            }
            tuple
        }

        cc::Expr::TupleIdx(tuple, idx) => {
            let tuple = use_var(ctx, module, builder, &arg_map, &fun_map, &data_map, *tuple);
            // TODO: field type
            // TODO: hard-coded word size
            builder
                .ins()
                .load(I64, MemFlags::new(), tuple, (idx * 8) as i32)
        }

        _ => {
            panic!("Unimplemented expr: {:?}", rhs);
        }
    }
}

fn declare_var(ctx: &mut Ctx, builder: &mut FunctionBuilder, var: VarId) -> Variable {
    let var_type = ctx.var_rep_type(var);
    let var_abi_type = rep_type_abi(var_type);
    let cranelift_var = varid_var(ctx, var);
    builder.declare_var(cranelift_var, var_abi_type);

    // let var = ctx.get_var(var);
    // println!("declare_var: {} -> {:?}", var, cranelift_var);

    cranelift_var
}

fn use_var(
    ctx: &Ctx, module: &Module<ObjectBackend>, builder: &mut FunctionBuilder,
    arg_map: &FxHashMap<VarId, Value>, fun_map: &FxHashMap<VarId, FuncId>,
    data_map: &FxHashMap<VarId, DataId>, var: VarId,
) -> Value {
    if let Some(val) = arg_map.get(&var) {
        return *val;
    }

    if let Some(func_id) = fun_map.get(&var) {
        let func_ref = module.declare_func_in_func(*func_id, builder.func);
        return builder.ins().func_addr(I64, func_ref);
    }

    if let Some(data_id) = data_map.get(&var) {
        let data_ref: GlobalValue = module.declare_data_in_func(*data_id, builder.func);
        return builder.ins().global_value(I64, data_ref);
    }

    // let var_ = ctx.get_var(var);
    let cl_var = varid_var(ctx, var);
    // println!("use_var: {} -> {:?}", var_, cl_var);
    builder.use_var(cl_var)
}

fn varid_var(ctx: &Ctx, var: VarId) -> Variable {
    let var = ctx.get_var(var);
    Variable::new(var.get_uniq().0.get() as usize)
}

fn rep_type_abi(ty: RepType) -> Type {
    match ty {
        RepType::Word => I64,
        RepType::Float => F64,
    }
}

fn cranelift_cond(cond: Cmp) -> IntCC {
    match cond {
        Cmp::Equal => IntCC::Equal,
        Cmp::NotEqual => IntCC::NotEqual,
        Cmp::LessThan => IntCC::SignedLessThan,
        Cmp::LessThanOrEqual => IntCC::SignedLessThanOrEqual,
        Cmp::GreaterThan => IntCC::SignedGreaterThan,
        Cmp::GreaterThanOrEqual => IntCC::SignedLessThanOrEqual,
    }
}

// main_id: Variable for the main expression.
fn make_main(
    module: &mut Module<ObjectBackend>, fun_ctx: &mut FunctionBuilderContext, main_id: FuncId,
) {
    let mut context = module.make_context();
    context.func.signature = Signature {
        params: vec![],
        returns: vec![AbiParam::new(I32)],
        call_conv: CallConv::SystemV,
    };
    let main_func_id: FuncId = module
        .declare_function("main", Linkage::Export, &context.func.signature)
        .unwrap();
    let mut builder: FunctionBuilder = FunctionBuilder::new(&mut context.func, fun_ctx);
    let block = builder.create_block();
    builder.switch_to_block(block);
    let expr_func_ref: FuncRef = module.declare_func_in_func(main_id, builder.func);
    builder.ins().call(expr_func_ref, &[]);
    let ret = builder.ins().iconst(I32, 0);
    builder.ins().return_(&[ret]);
    builder.seal_block(block);

    let flags = settings::Flags::new(settings::builder());
    let res = verify_function(&context.func, &flags);

    println!("{}", context.func.display(None));
    if let Err(errors) = res {
        panic!("{}", errors);
    }

    module
        .define_function(main_func_id, &mut context, &mut NullTrapSink {})
        .unwrap();
    module.clear_context(&mut context);
}
