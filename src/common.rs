use crate::ctx::VarId;

use std::fmt;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatBinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntBinOp {
    Add,
    Sub,
    // Mul,
    // Div,
}

impl fmt::Display for Cmp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Cmp::*;
        let s = match self {
            Equal => "=",
            NotEqual => "<>",
            LessThan => "<",
            LessThanOrEqual => "<=",
            GreaterThan => ">",
            GreaterThanOrEqual => ">=",
        };
        s.fmt(f)
    }
}

impl FloatBinOp {
    pub fn to_str(&self) -> &'static str {
        match self {
            FloatBinOp::Add => "+.",
            FloatBinOp::Sub => "-.",
            FloatBinOp::Mul => "*.",
            FloatBinOp::Div => "/.",
        }
    }
}

impl IntBinOp {
    pub fn to_str(&self) -> &'static str {
        match self {
            IntBinOp::Add => "+.",
            IntBinOp::Sub => "-.",
        }
    }
}

impl Cmp {
    pub fn to_str(&self) -> &'static str {
        match self {
            Cmp::Equal => "=",
            Cmp::NotEqual => "<>",
            Cmp::LessThan => "<",
            Cmp::LessThanOrEqual => "<=",
            Cmp::GreaterThan => ">",
            Cmp::GreaterThanOrEqual => ">=",
        }
    }
}
