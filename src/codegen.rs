use cranelift_codegen::entity::EntityRef;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::entities::{Block, FuncRef, SigRef, Value};
use cranelift_codegen::ir::types::*;
use cranelift_codegen::ir::MemFlags;
use cranelift_codegen::ir::{AbiParam, ExternalName, Function, InstBuilder, Signature};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings;
use cranelift_codegen::verifier::verify_function;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{default_libcall_names, FuncId, Linkage, Module};
use cranelift_native;
use cranelift_object::{ObjectBackend, ObjectBuilder};

use fxhash::FxHashMap;

use crate::cg_types::RepType;
use crate::closure_convert as cc;
use crate::common::{Cmp, FloatBinOp, IntBinOp};
use crate::ctx::{Ctx, VarId};

struct CgCtx {}

pub fn codegen(
    ctx: &mut Ctx,
    cc::Fun {
        name,
        entry,
        args,
        blocks,
        return_type,
    }: &cc::Fun,
) -> Function {
    let codegen_flags: settings::Flags = settings::Flags::new(settings::builder());
    let mut module: Module<ObjectBackend> = Module::new(ObjectBuilder::new(
        // How does this know I'm building for x86_64 Linux?
        cranelift_native::builder().unwrap().finish(codegen_flags),
        [1, 2, 3, 4, 5, 6, 7, 8], // TODO: what is this?
        default_libcall_names(),
    ));

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

    let mut sig = Signature::new(CallConv::SystemV);
    for arg in args {
        let arg_type = ctx.var_rep_type(*arg);
        let arg_abi_type = rep_type_abi(arg_type);
        sig.params.push(AbiParam::new(arg_abi_type));
    }
    sig.returns.push(AbiParam::new(rep_type_abi(*return_type)));

    let mut fn_builder_ctx = FunctionBuilderContext::new();

    let mut func = Function::with_name_signature(
        ExternalName::User {
            namespace: 0,
            index: entry.0.get(),
        },
        sig,
    );

    // FUNC_IN_FUNC???????????????????
    let malloc = module.declare_func_in_func(malloc_id, &mut func);

    {
        let mut builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);

        let mut label_to_block: FxHashMap<cc::Label, Block> = Default::default();

        for block in blocks {
            let cranelift_block = builder.create_block();
            label_to_block.insert(block.label, cranelift_block);
        }

        for arg in args {
            let _ = declare_var(ctx, &mut builder, *arg);
        }

        // TODO: When do I need this exactly?
        // let entry_block = *label_to_block.get(entry).unwrap();
        // builder.append_block_params_for_function_params(entry_block);

        for cc::Block { label, stmts, exit } in blocks {
            let cranelift_block = *label_to_block.get(label).unwrap();
            builder.switch_to_block(cranelift_block);

            for cc::Asgn { lhs, rhs } in stmts {
                let val = rhs_value(ctx, &mut builder, malloc, rhs);
                let var = declare_var(ctx, &mut builder, *lhs);
                builder.def_var(var, val);
            }

            match exit {
                cc::Exit::Return(Some(var)) => {
                    let var = builder.use_var(varid_var(ctx, *var));
                    builder.ins().return_(&[var]);
                }
                cc::Exit::Return(None) => {
                    builder.ins().return_(&[]);
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
                    let v1 = builder.use_var(varid_var(ctx, *v1));
                    let v2 = builder.use_var(varid_var(ctx, *v2));
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
        }
    }

    let flags = settings::Flags::new(settings::builder());
    let res = verify_function(&func, &flags);

    println!("{}", func.display(None));
    if let Err(errors) = res {
        panic!("{}", errors);
    }

    func
}

fn rhs_value(ctx: &Ctx, builder: &mut FunctionBuilder, malloc: FuncRef, rhs: &cc::Expr) -> Value {
    // let mut inst_builder = builder.ins();

    match rhs {
        cc::Expr::Atom(cc::Atom::Unit) => builder.ins().iconst(I64, 0),
        cc::Expr::Atom(cc::Atom::Int(i)) => builder.ins().iconst(I64, *i),
        cc::Expr::Atom(cc::Atom::Float(f)) => builder.ins().f64const(*f),
        cc::Expr::Atom(cc::Atom::Var(var)) => builder.use_var(varid_var(ctx, *var)),

        cc::Expr::IBinOp(cc::BinOp { op, arg1, arg2 }) => {
            let arg1 = builder.use_var(varid_var(ctx, *arg1));
            let arg2 = builder.use_var(varid_var(ctx, *arg2));
            match op {
                IntBinOp::Add => builder.ins().iadd(arg1, arg2),
                IntBinOp::Sub => builder.ins().isub(arg1, arg2),
            }
        }

        cc::Expr::FBinOp(cc::BinOp { op, arg1, arg2 }) => {
            let arg1 = builder.use_var(varid_var(ctx, *arg1));
            let arg2 = builder.use_var(varid_var(ctx, *arg2));
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

        cc::Expr::App(fun, args) => {
            let fun_sig: SigRef = todo!();
            let callee = builder.use_var(varid_var(ctx, *fun));
            let arg_vals: Vec<Value> = args
                .iter()
                .map(|arg| builder.use_var(varid_var(ctx, *arg)))
                .collect();
            let call = builder.ins().call_indirect(fun_sig, callee, &arg_vals);
            builder.inst_results(call)[0]
        }

        cc::Expr::Tuple(args) => {
            let malloc_arg = builder.ins().iconst(I64, args.len() as i64);
            let malloc_call = builder.ins().call(malloc, &[malloc_arg]);
            let tuple = builder.inst_results(malloc_call)[0];
            for (arg_idx, arg) in args.iter().enumerate() {
                let arg = builder.use_var(varid_var(ctx, *arg));
                // TODO: hard-coded word size
                builder
                    .ins()
                    .store(MemFlags::new(), arg, tuple, (arg_idx * 8) as i32);
            }
            tuple
        }

        cc::Expr::TupleIdx(tuple, idx) => {
            let tuple = builder.use_var(varid_var(ctx, *tuple));
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
    cranelift_var
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
