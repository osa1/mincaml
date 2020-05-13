#![allow(dead_code)]

use cranelift_codegen::entity::EntityRef;
use cranelift_codegen::ir::entities::{Block, Value};
use cranelift_codegen::ir::types::*;
use cranelift_codegen::ir::{AbiParam, ExternalName, Function, InstBuilder, Signature};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings;
use cranelift_codegen::verifier::verify_function;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{default_libcall_names, FuncId, Linkage, Module};
use cranelift_native;
use cranelift_object::{ObjectBackend, ObjectBuilder};

use fxhash::FxHashMap;

use crate::closure_convert as cc;
use crate::common::{FloatBinOp, IntBinOp};
use crate::ctx::{Ctx, VarId};
use crate::type_check as tc;

struct CgCtx {}

pub fn codegen(
    ctx: &Ctx, cc::Fun {
        name,
        entry,
        args,
        blocks,
    }: &cc::Fun,
) -> Function {
    let codegen_flags: settings::Flags = settings::Flags::new(settings::builder());
    let mut module: Module<ObjectBackend> = Module::new(ObjectBuilder::new(
        // How does this know I'm building for x86_64 Linux?
        cranelift_native::builder().unwrap().finish(codegen_flags),
        [1, 2, 3, 4, 5, 6, 7, 8], // TODO: what is this?
        default_libcall_names(),
    ));

    let malloc: FuncId = module
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
        let arg_type = ctx.var_type(*arg).unwrap();
        let arg_abi_type = type_abi_type(&*arg_type);
        sig.params.push(AbiParam::new(arg_abi_type));
    }
    // TODO: return type
    // sig.returns.push(AbiParam::new(I32));

    let mut fn_builder_ctx = FunctionBuilderContext::new();

    let mut func = Function::with_name_signature(
        ExternalName::User {
            namespace: 0,
            index: entry.0.get(),
        },
        sig,
    );
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

        let entry_block = *label_to_block.get(entry).unwrap();
        builder.append_block_params_for_function_params(entry_block);

        for cc::Block { label, stmts, exit } in blocks {
            let cranelift_block = *label_to_block.get(label).unwrap();
            builder.switch_to_block(cranelift_block);

            for cc::Asgn { lhs, rhs } in stmts {
                let val = rhs_value(ctx, &mut builder, rhs);
                let var = declare_var(ctx, &mut builder, *lhs);
                builder.def_var(var, val);
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

fn type_abi_type(ty: &tc::Type) -> Type {
    match ty {
        tc::Type::Var(_) => panic!("type_abi_type: type variable: {:?}", ty),
        tc::Type::Unit
        | tc::Type::Bool
        | tc::Type::Int
        | tc::Type::Fun { .. }
        | tc::Type::Tuple(_)
        | tc::Type::Array(_) => I64,
        tc::Type::Float => F64,
    }
}

fn rhs_value(ctx: &Ctx, builder: &mut FunctionBuilder, rhs: &cc::Expr) -> Value {
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
            // https://github.com/bytecodealliance/simplejit-demo/blob/master/src/jit.rs#L395
            todo!()
        }
        _ => todo!(),
    }
}

fn declare_var(ctx: &Ctx, builder: &mut FunctionBuilder, var: VarId) -> Variable {
    let var_type = ctx.var_type(var).unwrap();
    let var_abi_type = type_abi_type(&*var_type);
    let cranelift_var = varid_var(ctx, var);
    builder.declare_var(cranelift_var, var_abi_type);
    cranelift_var
}

fn varid_var(ctx: &Ctx, var: VarId) -> Variable {
    let var = ctx.get_var(var);
    Variable::new(var.get_uniq().0.get() as usize)
}
