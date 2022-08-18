use crate::cg_types::RepType;
use crate::common::{BinOp, Cmp, FloatBinOp, IntBinOp};
use crate::ctx::VarId;

use cranelift_entity::{entity_impl, PrimaryMap};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockIdx(u32);
entity_impl!(BlockIdx, "b");

// Functions
#[derive(Debug)]
pub struct Fun {
    pub name: VarId,
    pub args: Vec<VarId>,
    pub blocks: PrimaryMap<BlockIdx, BlockData>,
    pub return_type: RepType,
}

#[derive(Debug)]
pub enum BlockData {
    NA,
    Block(Block),
}

impl BlockData {
    #[allow(non_snake_case)]
    pub fn is_NA(&self) -> bool {
        match self {
            BlockData::NA => true,
            BlockData::Block(_) => false,
        }
    }

    pub fn get_block(&self) -> Option<&Block> {
        match self {
            BlockData::NA => None,
            BlockData::Block(block) => Some(block),
        }
    }
}

// Basic blocks
#[derive(Debug)]
pub struct Block {
    pub idx: BlockIdx,
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
    ArrayAlloc { len: VarId },
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
#[derive(Debug, PartialEq, Eq)]
pub enum Exit {
    Return(VarId),
    Branch { v1: VarId, v2: VarId, cond: Cmp, then_block: BlockIdx, else_block: BlockIdx },
    Jump(BlockIdx),
}
