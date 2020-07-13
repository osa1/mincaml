#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ty {
    // I32, unused
    I64,
    // F32, unused
    F64,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunTy {
    pub args: Vec<Ty>,
    pub ret: Option<Ty>,
}

#[derive(Debug, Clone, Copy)]
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
