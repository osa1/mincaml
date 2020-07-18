use crate::{
    cg_types::RepType,
    ctx::{Ctx, VarId},
    type_check,
    type_check::Type,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ty {
    I32,
    I64,
    // F32, unused
    F64,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunTy {
    pub args: Vec<Ty>,
    pub ret: Option<Ty>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunIdx(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeIdx(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalIdx(pub u32);

#[derive(Debug, Clone, Copy)]
pub struct GlobalIdx(pub u32);

// FIXME: The name is misleading, in the spec "tableidx" is for the index of a table, not for index
// of a table element. Perhaps call this "ElementIdx"?
#[derive(Debug, Clone, Copy)]
pub struct TableIdx(pub u32);

//
// Converting front-end types to Wasm types
//

pub fn type_to_closure_type(ctx: &Ctx, ty_id: type_check::TypeIdx) -> FunTy {
    match ctx.get_type(ty_id) {
        Type::Fun { args, ret } => {
            let mut args: Vec<Ty> = args
                .iter()
                .map(|arg| rep_type_to_wasm(RepType::from(&ctx.get_type(*arg))))
                .collect();
            args.insert(0, Ty::I64); // closure argument
            let ret = Some(rep_type_to_wasm(RepType::from(&ctx.get_type(ret))));
            FunTy { args, ret }
        }
        _ => panic!(
            "Non-function in function position: {:?}",
            ctx.get_type(ty_id)
        ),
    }
}

pub fn rep_type_to_wasm(ty: RepType) -> Ty {
    match ty {
        RepType::Word => Ty::I64,
        RepType::Float => Ty::F64,
    }
}
