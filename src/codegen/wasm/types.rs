#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ty {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunTy {
    pub args: Vec<Ty>,
    pub ret: Ty,
}

#[derive(Debug, Clone, Copy)]
pub struct FunIdx(pub u32);

#[derive(Debug, Clone, Copy)]
pub struct TypeIdx(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalIdx(pub u32);
