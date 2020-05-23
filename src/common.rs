use crate::ctx::VarId;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Cmp {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug, Clone)]
pub struct BinOp<A> {
    pub op: A,
    pub arg1: VarId,
    pub arg2: VarId,
}

#[derive(Debug, Clone, Copy)]
pub enum FloatBinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy)]
pub enum IntBinOp {
    Add,
    Sub,
}
