// Wasm high-level enough, we don't need even a-normalization. Here we directly generate Wasm from
// parsed expressions.

use super::alloc::gen_alloc;
use super::encoding;
use super::instr::*;
use super::{
    rep_type_to_wasm,
    types::{type_to_closure_type, FunIdx, FunTy, GlobalIdx, LocalIdx, Ty, TypeIdx},
};
use crate::ctx::{Ctx, VarId};
use crate::var::CompilerPhase;
use crate::cg_types::RepType;
use crate::ast::*;

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
    // Maps function types to type indices, for the 'type' section
    fun_tys: FxHashMap<FunTy, TypeIdx>,
    // Maps functions to their types' indices in 'type' section. Used to generate 'function'
    // section which maps functions to their type indices. Nth element is Nth function's type
    // index.
    fun_ty_indices: Vec<TypeIdx>,
}

pub struct WasmFun {
    // Variable name of the function
    pub var: VarId,
    // Types of locals. Nth local has type locals[N].
    pub locals: Vec<Ty>,
    // Function code
    pub code: Vec<u8>,
    // Index of the function
    pub fun_idx: FunIdx,
    // Type index of the function
    pub fun_ty_idx: TypeIdx,
    // Index of the function in the module's table
    pub fun_tbl_idx: u32,
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
        &mut self, ctx: &mut Ctx, ty_idx: TypeIdx, fun_bndr: VarId, fvs: &[VarId], args: &[VarId],
        body: &Expr,
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
            locals,
            n_locals: _,
            mut bytes,
        } = replace(&mut self.fun_ctx, current_fun_ctx);

        bytes.push(0x0B);

        let mut locals = locals.into_iter().collect::<Vec<_>>();
        locals.sort_by_key(|(_, idx)| *idx);
        let locals = locals
            .into_iter()
            .map(|(var, _)| rep_type_to_wasm(RepType::from(&*ctx.var_type(var))))
            .collect::<Vec<_>>();

        self.funs.insert(
            fun_bndr,
            WasmFun {
                var: fun_bndr,
                locals,
                code: bytes,
                fun_idx,
                fun_tbl_idx: fun_idx.0,
                fun_ty_idx: ty_idx,
            },
        );

        // The invariant here is that the function index and its table location are the same. E.g.
        // function 5 is in 5th location in the module table.
        fun_idx.0
    }
}

/// Compile given program into a Wasm module
pub fn codegen_module(ctx: &mut Ctx, expr: &Expr) -> Vec<u8> {
    // We don't need to make a pass to collect function binders as functions are always defined
    // before used (using LetRec syntax), so we just add the imported stuff to globals.

    let mut fun_tys: FxHashMap<FunTy, TypeIdx> = Default::default();
    let mut fun_ty_indices: Vec<TypeIdx> = vec![];

    let globals = {
        let mut globals: FxHashMap<VarId, GlobalIdx> = Default::default();
        let mut next_global_idx = 0;
        for (builtin_var, builtin_ty) in ctx.builtins() {
            // Allocate global index
            globals.insert(*builtin_var, GlobalIdx(next_global_idx));
            next_global_idx += 1;

            // Allocate type index
            let fun_ty = type_to_closure_type(ctx, *builtin_var, *builtin_ty);

            let ty_idx = match fun_tys.get(&fun_ty) {
                Some(ty_idx) => *ty_idx,
                None => TypeIdx(fun_tys.len() as u32),
            };
            fun_tys.insert(fun_ty, ty_idx);
            fun_ty_indices.push(ty_idx);
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
        fun_tys,
        fun_ty_indices,
    };

    cg_expr(ctx, &mut module_ctx, expr);

    let FunCtx {
        locals: main_locals,
        n_locals: _,
        bytes: mut main_bytes,
    } = module_ctx.fun_ctx;

    // Register 'main' function

    let main_wasm_fun = {
        main_bytes.push(0x0F); // return
        main_bytes.push(0x0B); // end of expression

        let main_ty = FunTy {
            args: vec![],
            ret: None,
        };
        let main_ty_idx = match module_ctx.fun_tys.get(&main_ty) {
            Some(ty_idx) => *ty_idx,
            None => {
                let type_idx = TypeIdx(module_ctx.fun_tys.len() as u32);
                module_ctx.fun_tys.insert(main_ty, type_idx); // no need for this as we're done with code gen
                type_idx
            }
        };
        module_ctx.fun_ty_indices.push(main_ty_idx);

        let main_fun_idx = module_ctx.next_fun_idx;

        let mut main_locals = main_locals.into_iter().collect::<Vec<_>>();
        main_locals.sort_by_key(|(_, idx)| *idx);
        let main_locals = main_locals
            .into_iter()
            .map(|(var, _)| rep_type_to_wasm(RepType::from(&*ctx.var_type(var))))
            .collect::<Vec<_>>();

        let main_var = ctx.fresh_codegen_var(CompilerPhase::ClosureConvert, RepType::Word);

        WasmFun {
            var: main_var,
            locals: main_locals,
            code: main_bytes,
            fun_idx: main_fun_idx,
            fun_ty_idx: main_ty_idx,
            fun_tbl_idx: main_fun_idx.0,
        }
    };

    module_ctx.funs.insert(main_wasm_fun.var, main_wasm_fun);

    //
    // Generate the module
    //

    let mut module_bytes = vec![0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00];

    //
    // 1. type section
    //

    encoding::encode_type_section(
        module_ctx
            .fun_tys
            .iter()
            .map(|(x1, x2)| (x1.clone(), x2.clone()))
            .collect(),
        &mut module_bytes,
    );

    //
    // 2. import section: import RTS stuff
    //

    encoding::encode_import_section(ctx, &module_ctx.fun_tys, &mut module_bytes);

    //
    // 3. function section: Nth index here is the Ntf function's (in 'code' section) type index
    //

    encoding::encode_function_section(
        ctx.builtins().len(),
        &module_ctx.fun_ty_indices,
        &mut module_bytes,
    );

    //
    // 4. table section: initialize function table, size = number of functions in the module
    //

    // Number of functions to be used in table and element sections. Does not include the generated
    // 'main' function as that's never called by the program code.
    let n_funs = (ctx.builtins().len() + module_ctx.funs.len()) as u32;
    encoding::encode_table_section(n_funs, &mut module_bytes);

    // 5. memory section: NA

    //
    // 6. global section: variables 'hp' (heap pointer) and 'hp_lim' (heap limit). Both are u32
    //    values.
    //

    encoding::encode_global_section(&mut module_bytes);

    // 7. export section: NA (no need to export start function)

    //
    // 8. start section
    //

    // TODO

    //
    // 9. element section: initializes the function table. Nth element is index to Nth function.
    //

    encoding::encode_element_section(n_funs, &mut module_bytes);

    //
    // 10. code section
    //

    let mut funs = module_ctx
        .funs
        .into_iter()
        .map(|(_, v)| v)
        .collect::<Vec<_>>();
    funs.sort_by_key(|fun| fun.fun_idx);

    encoding::encode_code_section(&funs, &mut module_bytes);

    // 11. data section: NA

    //
    // All done
    //

    module_bytes
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
            // Allocate function type
            let fun_ty = type_to_closure_type(ctx, *bndr, ctx.var_type_id(*bndr));
            let type_idx = match module_ctx.fun_tys.get(&fun_ty) {
                Some(type_idx) => *type_idx,
                None => {
                    let type_idx = TypeIdx(module_ctx.fun_tys.len() as u32);
                    module_ctx.fun_tys.insert(fun_ty, type_idx);
                    type_idx
                }
            };
            module_ctx.fun_ty_indices.push(type_idx);

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
            let fun_tbl_idx = module_ctx.new_closure(ctx, type_idx, *bndr, &fvs_vec, args, rhs);

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

        Expr::App { fun, args } => {
            // f.0(f, ..args);

            // Compile and bind closure
            cg_expr(ctx, module_ctx, fun);
            let closure_local = module_ctx.fun_ctx.fresh_local();
            local_set(closure_local, &mut module_ctx.fun_ctx.bytes);

            // Push closure arg
            local_get(closure_local, &mut module_ctx.fun_ctx.bytes);

            // Compile args. No need to bind them as we'll use each arg only once
            for arg in args {
                cg_expr(ctx, module_ctx, arg);
            }

            // Get function from the closure
            local_get(closure_local, &mut module_ctx.fun_ctx.bytes);
            field_read(closure_local, 0, Ty::I64, &mut module_ctx.fun_ctx.bytes);

            // Get function type
            todo!()
        }

        Expr::Tuple(_) => todo!(),
        Expr::LetTuple { bndrs, rhs, body } => todo!(),
        Expr::Array { len, elem } => todo!(),
        Expr::Get(_, _) => todo!(),
        Expr::Put(_, _, _) => todo!(),
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
