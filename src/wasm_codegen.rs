use crate::ctx::{Ctx, VarId};
use crate::lower::Fun;
use crate::wasm_builder::{FunctionBuilder, ModuleBuilder};

pub fn codegen(ctx: &mut Ctx, funs: &[Fun], main_id: VarId) -> Vec<u8> {
    let mut builder = ModuleBuilder::new();

    for fun in funs {
        codegen_fun(ctx, &mut builder);
    }

    builder.encode()
}

fn codegen_fun(ctx: &mut Ctx, builder: &mut ModuleBuilder) {}
