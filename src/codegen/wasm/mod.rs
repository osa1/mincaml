#![allow(dead_code)]

mod fun_builder;
mod types;

use fun_builder::*;
use types::*;

use crate::cg_types::RepType;
use crate::ctx::{Ctx, VarId};
use crate::lower;
use crate::lower::{Fun, Stmt, Asgn};

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
            match stmt {
                Stmt::Asgn(Asgn { lhs, rhs }) => {}
                Stmt::Expr(expr) => {}
            }
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
