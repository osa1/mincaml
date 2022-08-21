use crate::cg_types::RepType;
use crate::common::{BinOp, Cmp, FloatBinOp, IntBinOp};
use crate::ctx::VarId;

use cranelift_entity::{entity_impl, PrimaryMap};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockIdx(u32);
entity_impl!(BlockIdx, "b");

/// A function
#[derive(Debug)]
pub struct Fun {
    /// Name of the function
    pub name: VarId,

    /// Argument names of the function
    pub args: Vec<VarId>,

    /// Basic blocks of the function. `BlockIdx(0)` is the entry block.
    pub blocks: PrimaryMap<BlockIdx, BlockData>,

    /// Return type of the function
    pub return_type: RepType,
}

/// Body of a basic block. The basic block may be in construction, in which case the body will be
/// `BlockData::NA`.
#[derive(Debug)]
pub enum BlockData {
    /// Placeholder for a basic block currently being constructed
    NA,

    /// Body of a basic block
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

/// A basic block
#[derive(Debug)]
pub struct Block {
    /// Index of the basic block
    pub idx: BlockIdx,

    /// Any comments attached to the basic block. Debug printers for the basic block will print
    /// this string to help with debugging.
    pub comment: Option<String>,

    /// Statements in the basic block
    pub stmts: Vec<Stmt>,

    /// Exit node of the basic block
    pub exit: Exit,
}

// TODO: I don't remember why we have this distinction?
#[derive(Debug)]
pub enum Stmt {
    Asgn(Asgn),
    Expr(Expr),
}

/// An assignment
#[derive(Debug)]
pub struct Asgn {
    /// Left-hand side of the assignment
    pub lhs: VarId,

    /// Right-hand side of the assignment
    pub rhs: Expr,
}

/// An assignment right-hand side
#[derive(Debug)]
pub enum Expr {
    Atom(Atom),

    /// Integer binary operation
    IBinOp(BinOp<IntBinOp>),

    /// Float binary operation
    FBinOp(BinOp<FloatBinOp>),

    /// Integer negation
    Neg(VarId),

    /// Float negation
    FNeg(VarId),

    /// Function application. `RepType` is the runtime representation of the return value.
    App(VarId, Vec<VarId>, RepType),

    /// Tuple allocation
    Tuple {
        len: usize,
    },

    /// Tuple field read
    TupleGet(VarId, usize, RepType),

    /// Tuple field write
    TuplePut(VarId, usize, VarId),

    /// Array allocation
    ArrayAlloc {
        len: VarId,
    },

    /// Array field read
    ArrayGet(VarId, VarId),

    /// Array field write
    ArrayPut(VarId, VarId, VarId),
}

#[derive(Debug, PartialEq)]
pub enum Atom {
    Unit,
    Int(i64),
    Float(f64),
    Var(VarId),
}

/// A basic block exit node
#[derive(Debug, PartialEq, Eq)]
pub enum Exit {
    /// Function return: return value of the variable
    Return(VarId),

    /// Conditional jump: compare `v1` with `v2` according to `cond`, jump to `then_block` if true,
    /// jump to `else_block` otherwise.
    Branch { v1: VarId, v2: VarId, cond: Cmp, then_block: BlockIdx, else_block: BlockIdx },

    /// Unconditional jump
    Jump(BlockIdx),
}
