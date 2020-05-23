use crate::cg_types::RepType;
use crate::common::{BinOp, Cmp, FloatBinOp, IntBinOp};
use crate::ctx::VarId;
use crate::var::Uniq;

pub type Label = Uniq; // FIXME how to represent this best?

// Functions
#[derive(Debug)]
pub struct Fun {
    pub name: VarId,
    pub args: Vec<VarId>,
    pub blocks: Vec<Block>,
    pub return_type: RepType,
}

// Basic blocks
#[derive(Debug)]
pub struct Block {
    pub label: Label,
    pub stmts: Vec<Asgn>,
    pub exit: Exit,
}

// Assignments
#[derive(Debug)]
pub struct Asgn {
    pub lhs: VarId,
    pub rhs: Expr,
}

// Assignment right-hand sides
#[derive(Debug)]
pub enum Expr {
    Atom(Atom),
    IBinOp(BinOp<IntBinOp>),
    FBinOp(BinOp<FloatBinOp>),
    Neg(VarId),
    FNeg(VarId),
    App(VarId, Vec<VarId>, RepType),
    // Tuple allocation
    Tuple(Vec<VarId>), // TODO: Lower this more?
    // Tuple field read
    TupleGet(VarId, usize),
    // Array allocation
    ArrayAlloc { len: VarId, elem: VarId },
    // Array field read
    ArrayGet(VarId, VarId),
    // Array field write
    ArrayPut(VarId, VarId, VarId),
}

#[derive(Debug, PartialEq)]
pub enum Atom {
    Unit,
    Int(i64),
    Float(f64),
    Var(VarId),
}

// Exit nodes of basic blocks
#[derive(Debug, PartialEq)]
pub enum Exit {
    Return(VarId),
    Branch {
        v1: VarId,
        v2: VarId,
        cond: Cmp,
        then_label: Label,
        else_label: Label,
    },
    Jump(Label),
}