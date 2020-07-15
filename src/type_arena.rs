use crate::var::Uniq;

use cranelift_entity::{entity_impl, PrimaryMap};
use fxhash::FxHashMap;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeIdx(u32);
entity_impl!(TypeIdx, "ty");

pub struct TypeArena {
    arena: PrimaryMap<TypeIdx, Type>,
    intern: FxHashMap<Type, TypeIdx>,
}

pub type TyVar = Uniq;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Float,
    Fun { args: Vec<TypeIdx>, ret: Box<TypeIdx> },
    Tuple(Vec<TypeIdx>),
    Array(Box<TypeIdx>),
    Var(TyVar),
}
