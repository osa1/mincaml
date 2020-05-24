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
    pub comment: Option<String>,
    pub stmts: Vec<Stmt>,
    pub exit: Exit,
}

#[derive(Debug)]
pub enum Stmt {
    Asgn(Asgn),
    Expr(Expr),
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
    Tuple { len: usize },
    // Tuple field read
    TupleGet(VarId, usize),
    // Tuple field write
    TuplePut(VarId, usize, VarId),
    // Array allocation
    ArrayAlloc { len: VarId }, // TODO: Lower this more
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
