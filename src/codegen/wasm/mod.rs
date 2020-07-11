#![allow(dead_code)]

mod encoding;
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

pub fn codegen(ctx: &mut Ctx, funs: &[lower::Fun], main: VarId, dump: bool) -> Vec<u8> {
    let mut ctx = WasmCtx::new(ctx);

    // Module structure:
    //
    // 1. type section
    // 2. import section: NA? or do we need to import RTS stuff?
    // 3. function section
    // 4. table section: NA
    // 5. memory section: NA
    // 6. global section: NA
    // 7. export section: do we need to export main? TODO
    // 8. start section
    // 9. element section: NA (I don't understand what this is for)
    // 10. code section
    // 11. data section: NA
    //
    // So we only generate (1), (3), (8), (10)

    let fun_section = cg_fun_section(&mut ctx, funs);

    // Get function types for the types section
    let mut fun_tys: Vec<(FunTy, TypeIdx)> = ctx.fun_tys.into_iter().collect();
    fun_tys.sort_by_key(|(_fun_ty, type_idx)| *type_idx);
    let fun_tys: Vec<FunTy> = fun_tys.into_iter().map(|(fun_ty, _)| fun_ty).collect();

    todo!()
}

struct WasmCtx<'ctx> {
    ctx: &'ctx mut Ctx,
    fun_tys: FxHashMap<FunTy, TypeIdx>,
    fun_indices: FxHashMap<VarId, FunIdx>,
}

fn cg_fun_section(ctx: &mut WasmCtx, funs: &[lower::Fun]) -> Vec<u8> {
    let mut section_bytes = vec![];
    section_bytes.push(10);

    let mut code = Vec::with_capacity(funs.len());
    let mut section_size = 0;

    for fun in funs {
        let fun_code = cg_fun(ctx, fun);
        section_size += fun_code.len();
        code.push(fun_code);
    }

    encoding::encode_u32_uleb128(section_size as u32, &mut section_bytes);
    for code in code {
        section_bytes.extend_from_slice(&code);
    }

    section_bytes
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

fn cg_fun(ctx: &mut WasmCtx, fun: &lower::Fun) -> Vec<u8> {
    let Fun {
        name,
        args: _,
        blocks,
        return_type: _,
    } = fun;

    // Index of the current function
    let fun_idx = FunIdx(ctx.fun_indices.len() as u32);

    // Add index to the map now, to be able to handle recursive calls
    ctx.fun_indices.insert(*name, fun_idx);

    // Get function type idx
    // let arg_tys: Vec<Ty> = args
    //     .iter()
    //     .map(|arg| rep_type_to_wasm(ctx.ctx.var_rep_type(*arg)))
    //     .collect();
    // let ret_ty = rep_type_to_wasm(*return_type);
    // let fun_ty = FunTy {
    //     args: arg_tys,
    //     ret: ret_ty,
    // };
    // let ty_idx = ctx.add_fun_ty(fun_ty);

    let mut fun_builder = FunBuilder::new();

    // TODO: This will only work in 'blocks' is in dependency order
    for (_block_idx, block) in blocks {
        let block = match block {
            lower::BlockData::NA => {
                continue;
            }
            lower::BlockData::Block(block) => block,
        };

        if block.loop_header {
            fun_builder.enter_loop();
        }

        for stmt in &block.stmts {
            cg_stmt(ctx, &mut fun_builder, stmt);
        }

        if block.loop_header {
            fun_builder.exit_loop();
        }
    }

    let (fun_bytes, fun_locals) = fun_builder.finish();

    let mut locals_bytes = vec![];
    encoding::encode_vec(
        &fun_locals,
        &mut |local, buf| {
            encoding::encode_u32_uleb128(1, buf);
            encoding::encode_ty(rep_type_to_wasm(ctx.ctx.var_rep_type(*local)), buf);
        },
        &mut locals_bytes,
    );

    let code_size = (locals_bytes.len() + fun_bytes.len()) as u32;
    let mut code = vec![];
    encoding::encode_u32_uleb128(code_size, &mut code);
    code.extend_from_slice(&locals_bytes);
    code.extend_from_slice(&fun_bytes);
    code
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
        lower::Expr::App(fun, args, _ret_ty) => {
            let fun_idx = ctx.get_fun_idx(*fun);
            for arg in args {
                builder.local_get(*arg);
            }
            builder.call(fun_idx);
        }
        lower::Expr::Tuple { len: _ } => todo!(),
        lower::Expr::TupleGet(_, _) => todo!(),
        lower::Expr::TuplePut(_, _, _) => todo!(),
        lower::Expr::ArrayAlloc { len: _ } => todo!(),
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
