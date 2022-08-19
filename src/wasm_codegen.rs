use crate::cg_types::RepType;
use crate::ctx::{Ctx, VarId};
use crate::lower;
use crate::lower::Fun;
use crate::wasm_builder::{FunctionBuilder, FunctionLocalId, ModuleBuilder};

use fxhash::FxHashMap;

pub fn codegen(ctx: &mut Ctx, funs: &[Fun], main_id: VarId) -> Vec<u8> {
    let mut builder = ModuleBuilder::new();

    // Maps function ids to their indices in the module. Used in `call` instructions and for table
    // indices of the functions.
    //
    // Table index of a function is the same as its index in the module.
    let mut func_idxs: FxHashMap<VarId, usize> = Default::default();

    // Allocate function indices
    for fun in funs {
        let old_fun_idx = func_idxs.insert(fun.name, func_idxs.len());
        debug_assert_eq!(old_fun_idx, None);
    }

    for fun in funs {
        codegen_fun(ctx, &mut builder, &func_idxs, fun);
    }

    builder.encode(main_id)
}

fn codegen_fun(
    ctx: &mut Ctx, builder: &mut ModuleBuilder, func_idxs: &FxHashMap<VarId, usize>, fun: &Fun,
) {
    let lower::Fun { name, args, blocks, return_type } = fun;

    let args: Vec<(VarId, RepType)> = args
        .iter()
        .map(|arg| (*arg, ctx.var_rep_type(*arg)))
        .collect();

    let mut func_builder = builder.new_function(*name, args.clone(), *return_type);

    // Maps function locals and arguments to their indices in the function.
    //
    // Note: `new_function` above allocates locals for arguments
    let mut locals: FxHashMap<VarId, usize> = Default::default();

    for (arg, _) in args {
        let idx = locals.len();
        locals.insert(arg, idx);
    }

    for lower::Block { stmts, .. } in blocks.values().filter_map(lower::BlockData::get_block) {
        for stmt in stmts {
            match stmt {
                lower::Stmt::Asgn(lower::Asgn { lhs, rhs: _ }) => {
                    if locals.contains_key(lhs) {
                        continue;
                    }

                    let idx = locals.len();
                    locals.insert(*lhs, idx);
                }

                lower::Stmt::Expr(_) => {}
            }
        }
    }

    func_builder.finish();
}
