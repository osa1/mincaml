mod alloc;
mod codegen;
mod encoding;
mod fun_builder;
mod instr;
mod types;

use fun_builder::*;
use types::*;

use crate::cg_types::RepType;
use crate::common::*;
use crate::ctx::{Ctx, VarId};
use crate::lower;
use crate::lower::{Asgn, Fun, Stmt};

use fxhash::FxHashMap;

pub fn codegen(ctx: &mut Ctx, funs: &[lower::Fun], main: VarId, _dump: bool) -> Vec<u8> {
    let mut ctx = WasmCtx::new(ctx, funs);

    let mut module_bytes = vec![0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00];

    // Module structure:
    //
    // 1. type section
    // 2. import section: import RTS stuff
    // 3. function section: Nth index here is the Nth function's (in 'code' section) type index
    // 4. table section: initialize function table, size = number of functions in the module
    // 5. memory section: NA
    // 6. global section: variables 'hp' (heap pointer) and 'hp_lim' (heap limit). Both are u32
    //    values.
    // 7. export section: NA (no need to export start function)
    // 8. start section
    // 9. element section: initializes the function table. Nth element is index to Nth function.
    // 10. code section
    // 11. data section: NA
    //
    // So we only generate (1), (3), (8), (10)

    // Number of functions to be used in table and element sections. Does not include the generated
    // 'main' function as that's never called by the program code.
    let n_funs = (ctx.ctx.builtins().len() + funs.len()) as u32;

    let code_section = cg_code_section(&mut ctx, funs, main);

    encoding::encode_type_section(
        ctx.fun_tys
            .iter()
            .map(|(x1, x2)| (x1.clone(), x2.clone()))
            .collect(),
        &mut module_bytes,
    );
    encoding::encode_import_section(&ctx, &mut module_bytes);

    encoding::encode_function_section(
        ctx.ctx.builtins().len(),
        &ctx.fun_ty_indices,
        &mut module_bytes,
    );

    encoding::encode_table_section(n_funs, &mut module_bytes);
    encoding::encode_global_section(&mut module_bytes);

    // start function is generated last, after all the MinCaml functions
    let start_fun_idx = FunIdx(n_funs);
    encoding::encode_start_section(start_fun_idx, &mut module_bytes);

    encoding::encode_element_section(n_funs, &mut module_bytes);

    module_bytes.extend_from_slice(&code_section);

    module_bytes
}

struct WasmCtx<'ctx> {
    pub ctx: &'ctx mut Ctx,
    // Maps function types to their type indices in 'type' section
    pub fun_tys: FxHashMap<FunTy, TypeIdx>,
    // Maps functions to their types' indices in 'type' section. Used to generate 'function'
    // section which maps functions to their type indices. Nth element is Nth function's type
    // index.
    fun_ty_indices: Vec<TypeIdx>,
    // Function indices of the functions in the current module. First function in the module has
    // index N where N is the number of imports. We don't do direct function calls so the only
    // place where we use is this is in generated 'main' function to call the program's entry.
    fun_indices: FxHashMap<VarId, FunIdx>,
    // Globals in the module mapped to their table indices
    globals: FxHashMap<VarId, TableIdx>,
}

impl<'ctx> WasmCtx<'ctx> {
    fn new(ctx: &'ctx mut Ctx, funs: &[lower::Fun]) -> WasmCtx<'ctx> {
        let mut ctx = WasmCtx {
            ctx,
            fun_tys: Default::default(),
            fun_ty_indices: vec![],
            fun_indices: Default::default(),
            globals: Default::default(),
        };

        let mut next_table_idx = 0;

        // Imports get the first slots in global index space
        for (builtin_var, builtin_ty) in ctx.ctx.builtins() {
            let fun_ty = match &*ctx.ctx.get_type(*builtin_ty) {
                crate::type_check::Type::Fun { args, ret } => {
                    let mut args: Vec<Ty> = args
                        .iter()
                        .map(|arg| rep_type_to_wasm(RepType::from(arg)))
                        .collect();
                    args.insert(0, Ty::I64); // closure argument
                    let ret = Some(rep_type_to_wasm(RepType::from(&**ret)));
                    FunTy { args, ret }
                }
                other => panic!("Non-function builtin: {:?} : {:?}", builtin_var, other),
            };

            match ctx.fun_tys.get(&fun_ty) {
                Some(ty_idx) => {
                    ctx.fun_ty_indices.push(*ty_idx);
                }
                None => {
                    let ty_idx = TypeIdx(ctx.fun_tys.len() as u32);
                    ctx.fun_tys.insert(fun_ty, ty_idx);
                    ctx.fun_ty_indices.push(ty_idx);
                }
            };

            ctx.globals.insert(*builtin_var, TableIdx(next_table_idx));
            next_table_idx += 1;
        }

        let n_imports = ctx.ctx.builtins().len();

        // Next are functions in the current module
        for (
            i,
            Fun {
                name,
                args,
                blocks: _,
                return_type,
            },
        ) in funs.iter().enumerate()
        {
            let fun_idx = FunIdx((i + n_imports) as u32);
            ctx.fun_indices.insert(*name, fun_idx);

            let fun_ty = {
                let mut args: Vec<Ty> = args
                    .iter()
                    .map(|arg| rep_type_to_wasm(RepType::from(&*ctx.ctx.var_type(*arg))))
                    .collect();
                args.insert(0, Ty::I64); // closure argument
                let ret = Some(rep_type_to_wasm(*return_type));
                FunTy { args, ret }
            };

            match ctx.fun_tys.get(&fun_ty) {
                Some(ty_idx) => {
                    ctx.fun_ty_indices.push(*ty_idx);
                }
                None => {
                    let ty_idx = TypeIdx(ctx.fun_tys.len() as u32);
                    ctx.fun_tys.insert(fun_ty, ty_idx);
                    ctx.fun_ty_indices.push(ty_idx);
                }
            };

            ctx.globals.insert(*name, TableIdx(next_table_idx));
            next_table_idx += 1;
        }

        // Add main
        let main_fun_ty = FunTy {
            args: vec![],
            ret: None,
        };
        match ctx.fun_tys.get(&main_fun_ty) {
            Some(ty_idx) => {
                ctx.fun_ty_indices.push(*ty_idx);
            }
            None => {
                let ty_idx = TypeIdx(ctx.fun_tys.len() as u32);
                ctx.fun_tys.insert(main_fun_ty, ty_idx);
                ctx.fun_ty_indices.push(ty_idx);
            }
        };

        println!("globals: {:#?}", ctx.globals);
        println!("types: {:#?}", ctx.fun_tys);

        ctx
    }
}

fn cg_code_section(ctx: &mut WasmCtx, funs: &[lower::Fun], main: VarId) -> Vec<u8> {
    // section(vec(code))
    let mut section_bytes = vec![];
    section_bytes.push(10);

    let mut code: Vec<Vec<u8>> = Vec::with_capacity(funs.len());
    let mut section_size = 0;

    for fun in funs {
        let fun_code = cg_fun(ctx, fun);
        section_size += fun_code.len();
        code.push(fun_code);
    }

    let main_code = cg_main(ctx, main);
    section_size += main_code.len();
    code.push(main_code);

    let mut vec_size_encoding = vec![];
    // +1 for the start function (main_code above)
    encoding::encode_u32_uleb128((funs.len() + 1) as u32, &mut vec_size_encoding);
    section_size += vec_size_encoding.len();

    encoding::encode_u32_uleb128(section_size as u32, &mut section_bytes);
    section_bytes.extend_from_slice(&vec_size_encoding);
    for code in code {
        section_bytes.extend_from_slice(&code);
    }

    section_bytes
}

fn cg_main(ctx: &mut WasmCtx, main: VarId) -> Vec<u8> {
    let main_fun_idx = *ctx.fun_indices.get(&main).unwrap();

    let mut fun_builder = FunBuilder::new();
    fun_builder.i64_const(0); // dummy closure argument
    fun_builder.call(main_fun_idx);
    fun_builder.ret();

    let fun_bytes = fun_builder.finish().0;

    let mut locals_bytes = vec![];
    encoding::encode_vec::<()>(&[], &mut |_, _| {}, &mut locals_bytes);

    let code_size = (locals_bytes.len() + fun_bytes.len()) as u32;
    let mut code = vec![];
    encoding::encode_u32_uleb128(code_size, &mut code);
    code.extend_from_slice(&locals_bytes);
    code.extend_from_slice(&fun_bytes);
    code
}

fn cg_fun(ctx: &mut WasmCtx, fun: &lower::Fun) -> Vec<u8> {
    let Fun {
        name,
        args: _,
        blocks,
        return_type: _,
    } = fun;

    let mut fun_builder = FunBuilder::new();

    let mut block_idx = lower::BlockIdx::from_u32(0);
    loop {
        let lower::Block {
            stmts,
            exit,
            loop_header,
            ..
        } = match &blocks[block_idx] {
            lower::BlockData::NA => {
                panic!("Block not available: {}", block_idx);
            }
            lower::BlockData::Block(block) => block,
        };

        assert!(!loop_header); // loops not handled yet

        for stmt in stmts {
            cg_stmt(ctx, &mut fun_builder, stmt);
        }

        match exit {
            lower::Exit::Return(var) => {
                fun_builder.local_get(*var);
                fun_builder.ret();
                break;
            }
            lower::Exit::Branch {
                v1,
                v2,
                cond,
                then_block,
                else_block,
            } => todo!(),
            lower::Exit::Jump(target) => {
                block_idx = *target;
            }
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

pub fn rep_type_to_wasm(ty: RepType) -> Ty {
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
        lower::Expr::Atom(atom) => cg_atom(ctx, builder, atom),
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
        lower::Expr::App(fun, args, ret_ty) => {
            for arg in args {
                builder.local_get(*arg);
            }
            builder.local_get(*fun);

            let fun_ty = FunTy {
                args: args
                    .iter()
                    .map(|arg| rep_type_to_wasm(ctx.ctx.var_rep_type(*arg)))
                    .collect(),
                ret: Some(rep_type_to_wasm(RepType::from(*ret_ty))),
            };

            let fun_ty_idx = ctx
                .fun_tys
                .get(&fun_ty)
                .expect(&format!("Can't find function type: {:?}", fun_ty));
            builder.call_indirect(*fun_ty_idx);
        }
        lower::Expr::Tuple { len } => {
            let bytes = len * 8;
            builder.alloc(bytes as u32);
        }
        lower::Expr::TupleGet(tuple, idx) => {
            builder.i32_const(*idx as i32);
            builder.i32_const(8);
            builder.i32_mul(); // offset
            builder.local_get(*tuple); // base
            builder.i32_add(); // address = base + offset
            let tuple_ty = ctx.ctx.var_type(*tuple);
            let elem_ty = match &*tuple_ty {
                crate::type_check::Type::Tuple(elem_tys) => &elem_tys[*idx],
                crate::type_check::Type::Fun { .. } => {
                    // See DISGUSTING HACK in codegen::native
                    &crate::type_check::Type::Int
                }
                other => panic!("{:?} in tuple position", other),
            };
            match RepType::from(elem_ty) {
                RepType::Word => {
                    builder.i64_load();
                }
                RepType::Float => {
                    builder.f64_load();
                }
            }
        }
        lower::Expr::TuplePut(tuple, idx, elem) => {
            // value, address, store
            builder.local_get(*elem); // value
            builder.i32_const(*idx as i32);
            builder.i32_const(8);
            builder.i32_mul(); // offset
            builder.local_get(*tuple); // base
            builder.i32_add(); // address = base + offset
            let elem_ty = ctx.ctx.var_rep_type(*elem);
            match elem_ty {
                RepType::Word => {
                    builder.i64_store();
                }
                RepType::Float => {
                    builder.f64_store();
                }
            }
        }
        lower::Expr::ArrayAlloc { len: _ } => todo!(),
        lower::Expr::ArrayGet(_, _) => todo!(),
        lower::Expr::ArrayPut(_, _, _) => todo!(),
    }
}

fn cg_atom(ctx: &mut WasmCtx, builder: &mut FunBuilder, atom: &lower::Atom) {
    match atom {
        lower::Atom::Unit => builder.i64_const(0),
        lower::Atom::Int(i) => builder.i64_const(*i),
        lower::Atom::Float(f) => builder.f64_const(*f),
        lower::Atom::Var(var_id) => {
            // The variable can be global or local. If global then it's a statically known table
            // index to a function so we generate a `const`. If local then we do `local.get`.
            let var = ctx.ctx.get_var(*var_id);
            if var.is_builtin() {
                todo!()
            } else {
                builder.local_get(*var_id)
            }
        }
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
