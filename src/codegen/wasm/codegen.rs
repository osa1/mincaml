// Wasm high-level enough, we don't need even a-normalization. Here we directly generate Wasm from
// parsed expressions.

#![allow(dead_code, unused_variables)]

use super::alloc::gen_alloc;
use super::encoding;
use super::instr::*;
use super::{
    rep_type_to_wasm,
    types::{FunIdx, GlobalIdx, LocalIdx, Ty},
};
use crate::ctx::{Ctx, VarId};
use crate::parser::Expr;

use fxhash::{FxHashMap, FxHashSet};

use std::mem::replace;

struct ModuleCtx {
    globals: FxHashMap<VarId, GlobalIdx>,
    fun_ctx: FunCtx,
    // Maps functions to their free variables, for closure conversion
    fvs: FxHashMap<VarId, FxHashSet<VarId>>,
    // Functions generated so far
    funs: FxHashMap<VarId, WasmFun>,
    // Index of the next function to be generated. We don't use `funs.len()` here as we allocate
    // function indices before inserting them to `funs`.
    next_fun_idx: FunIdx,
}

struct WasmFun {
    // Variable name of the function
    var: VarId,
    // Function code
    code: Vec<u8>,
    // Index of the function
    fun_idx: FunIdx,
    // Index of the function in the module's table
    fun_tbl_idx: u32,
}

struct FunCtx {
    // Named locals mapped to their indices
    locals: FxHashMap<VarId, LocalIdx>,
    // Used to generate fresh unnamed locals
    n_locals: LocalIdx,
    bytes: Vec<u8>,
}

impl FunCtx {
    fn add_local(&mut self, var: VarId) -> LocalIdx {
        assert!(!self.locals.contains_key(&var));
        let local_idx = LocalIdx(self.locals.len() as u32);
        self.locals.insert(var, local_idx);
        local_idx
    }

    fn fresh_local(&mut self) -> LocalIdx {
        let local_idx = self.n_locals;
        self.n_locals.0 += 1;
        local_idx
    }
}

impl ModuleCtx {
    // Generates code for a function and returns the function's index in the module table.
    fn new_closure(
        &mut self, ctx: &mut Ctx, fun_bndr: VarId, fvs: &[VarId], args: &[VarId], body: &Expr,
    ) -> u32 {
        // Locals will be [self (bndr), arg1, arg2, ...]
        let locals: FxHashMap<VarId, LocalIdx> = ::std::iter::once(fun_bndr)
            .chain(args.iter().copied())
            .enumerate()
            .map(|(i, v)| (v, LocalIdx(i as u32)))
            .collect();

        // In the function body we'll bind free variables as first thing
        let mut code = vec![];
        for (fv_idx, fv) in fvs.iter().enumerate() {
            field_read(
                LocalIdx(0),         // self
                (fv_idx + 1) as u32, // first field is the function's index in the table
                rep_type_to_wasm(ctx.var_rep_type(*fv)),
                &mut code,
            );
        }

        let current_fun_ctx = replace(
            &mut self.fun_ctx,
            FunCtx {
                locals,
                n_locals: LocalIdx((args.len() + 1) as u32),
                bytes: code,
            },
        );

        let fun_idx = self.next_fun_idx;
        self.next_fun_idx = FunIdx(self.next_fun_idx.0 + 1);

        cg_expr(ctx, self, body);

        let FunCtx {
            locals: _,
            n_locals: _,
            bytes,
        } = replace(&mut self.fun_ctx, current_fun_ctx);

        self.funs.insert(
            fun_bndr,
            WasmFun {
                var: fun_bndr,
                code: bytes,
                fun_idx,
                fun_tbl_idx: fun_idx.0,
            },
        );

        // The invariant here is that the function index and its table location are the same. E.g.
        // function 5 is in 5th location in the module table.
        fun_idx.0
    }
}

pub fn codegen(ctx: &mut Ctx, expr: &Expr) {
    // We don't need to make a pass to collect function binders as functions are always defined
    // before used (using LetRec syntax), so we just add the imported stuff to globals.

    let globals = {
        let mut globals: FxHashMap<VarId, GlobalIdx> = Default::default();
        let mut next_global_idx = 0;
        for (builtin_id, _builtin_ty) in ctx.builtins() {
            globals.insert(*builtin_id, GlobalIdx(next_global_idx));
            next_global_idx += 1;
        }
        globals
    };

    let fun_ctx = FunCtx {
        locals: Default::default(),
        n_locals: LocalIdx(0),
        bytes: vec![],
    };

    let mut module_ctx = ModuleCtx {
        globals,
        fun_ctx,
        fvs: Default::default(),
        funs: Default::default(),
        next_fun_idx: FunIdx(0),
    };

    cg_expr(ctx, &mut module_ctx, expr);
}

fn cg_expr(ctx: &mut Ctx, module_ctx: &mut ModuleCtx, expr: &Expr) {
    match expr {
        Expr::Unit => {
            i64_const(0, &mut module_ctx.fun_ctx.bytes);
        }

        Expr::Bool(b) => {
            i64_const(if *b { 1 } else { 0 }, &mut module_ctx.fun_ctx.bytes);
        }

        Expr::Int(i) => {
            i64_const(*i, &mut module_ctx.fun_ctx.bytes);
        }

        Expr::Float(f) => {
            f64_const(*f, &mut module_ctx.fun_ctx.bytes);
        }

        Expr::Not(b) => todo!(),

        Expr::Neg(_) => {}

        Expr::IntBinOp(_, _, _) => {}

        Expr::FNeg(_) => {}

        Expr::FloatBinOp(_, _, _) => {}

        Expr::Cmp(_, _, _) => {}

        Expr::If(_, _, _) => {}

        Expr::Let { bndr, rhs, body } => {
            cg_expr(ctx, module_ctx, rhs);
            let local_idx = module_ctx.fun_ctx.add_local(*bndr);
            local_set(local_idx, &mut module_ctx.fun_ctx.bytes);
        }

        Expr::Var(var) => match module_ctx.fun_ctx.locals.get(var) {
            Some(local_idx) => {
                local_get(*local_idx, &mut module_ctx.fun_ctx.bytes);
            }
            None => match module_ctx.globals.get(var) {
                Some(global_idx) => {
                    global_get(*global_idx, &mut module_ctx.fun_ctx.bytes);
                }
                None => {
                    panic!(
                        "Unbound variable: {}\n\
                         Locals: {:?}\n\
                         Globals: {:?}",
                        ctx.get_var(*var),
                        module_ctx.fun_ctx.locals,
                        module_ctx.globals
                    );
                }
            },
        },

        Expr::LetRec {
            bndr,
            args,
            rhs,
            body,
        } => {
            // Collect function's free variables. TODO: redundant clone below to avoid borrowchk
            // issues
            let fvs = match module_ctx.fvs.get(bndr) {
                None => {
                    let mut fvs_: FxHashSet<VarId> = Default::default();
                    fvs(rhs, &mut fvs_);
                    module_ctx.fvs.insert(*bndr, fvs_);
                    module_ctx.fvs.get(bndr).unwrap().clone()
                }
                Some(fvs) => fvs.clone(),
            };

            let fvs_vec = fvs.iter().copied().collect::<Vec<_>>();
            let fun_tbl_idx = module_ctx.new_closure(ctx, *bndr, &fvs_vec, args, rhs);

            let bndr_idx = module_ctx.fun_ctx.add_local(*bndr);

            // Allocate the closure
            gen_alloc(((fvs.len() + 1) * 8) as u32, &mut module_ctx.fun_ctx.bytes);
            local_set(bndr_idx, &mut module_ctx.fun_ctx.bytes);

            // Store function pointer (index in module table)
            let bytes = &mut module_ctx.fun_ctx.bytes;
            local_get(bndr_idx, bytes); // address = base + 0
            i64_const(fun_tbl_idx as i64, bytes); // value
            i64_store(bytes);

            // Store free variables
            for fv in &fvs_vec {
                let ty = rep_type_to_wasm(ctx.var_rep_type(*fv));
                let fv_idx = module_ctx.fun_ctx.locals.get(fv).unwrap();
                field_store(bndr_idx, fv_idx.0 + 1, *fv_idx, ty, bytes);
            }

            cg_expr(ctx, module_ctx, body);
        }

        Expr::App { fun, args } => {}

        Expr::Tuple(_) => {}
        Expr::LetTuple { bndrs, rhs, body } => {}
        Expr::Array { len, elem } => {}
        Expr::Get(_, _) => {}
        Expr::Put(_, _, _) => {}
    }
}

fn fvs(expr: &Expr, acc: &mut FxHashSet<VarId>) {
    match expr {
        Expr::Unit | Expr::Bool(_) | Expr::Int(_) | Expr::Float(_) => {}
        Expr::Not(e) | Expr::Neg(e) | Expr::FNeg(e) => fvs(e, acc),
        Expr::Cmp(e1, _, e2) => {}
        Expr::FloatBinOp(e1, _, e2) | Expr::Put(e1, _, e2) | Expr::IntBinOp(e1, _, e2) => {
            fvs(e1, acc);
            fvs(e2, acc);
        }
        Expr::If(e1, e2, e3) => {
            fvs(e1, acc);
            fvs(e2, acc);
            fvs(e3, acc);
        }
        Expr::Let { bndr, rhs, body } => {
            // Order doesn't matter as we don't have shadowing
            fvs(rhs, acc);
            fvs(body, acc);
            acc.remove(bndr);
        }
        Expr::Var(var) => {
            acc.insert(*var);
        }
        Expr::LetRec {
            bndr,
            args,
            rhs,
            body,
        } => {
            // Order doesn't matter as we don't have shadowing
            fvs(rhs, acc);
            fvs(body, acc);
            acc.remove(bndr);
            for arg in args {
                acc.remove(arg);
            }
        }
        Expr::App { fun, args } => {
            fvs(fun, acc);
            for arg in args {
                fvs(arg, acc);
            }
        }
        Expr::Tuple(es) => {
            for e in es {
                fvs(e, acc);
            }
        }
        Expr::LetTuple { bndrs, rhs, body } => {
            fvs(rhs, acc);
            fvs(body, acc);
            for bndr in bndrs {
                acc.remove(bndr);
            }
        }
        Expr::Array { len, elem } => {
            fvs(len, acc);
            fvs(elem, acc);
        }
        Expr::Get(e, _) => {
            fvs(e, acc);
        }
    }
}

fn field_read(base: LocalIdx, offset: u32, ty: Ty, bytes: &mut Vec<u8>) {
    i32_const(offset as i32, bytes);
    i32_const(8, bytes);
    i32_mul(bytes); // offset
    local_get(base, bytes); // base
    i32_add(bytes); // address = base + offset
    match ty {
        Ty::I64 => {
            i64_load(bytes);
        }
        Ty::F64 => {
            f64_load(bytes);
        }
    }
}

fn field_store(base: LocalIdx, offset: u32, value: LocalIdx, ty: Ty, bytes: &mut Vec<u8>) {
    i32_const(offset as i32, bytes);
    i32_const(8, bytes);
    i32_mul(bytes); // offset
    local_get(base, bytes); // base
    i32_add(bytes); // address = base + offset
    local_get(value, bytes); // value
    match ty {
        Ty::I64 => {
            i64_store(bytes);
        }
        Ty::F64 => {
            f64_store(bytes);
        }
    }
}
