#![allow(unused_imports, dead_code)]

use crate::anormal::BinOp;
use crate::cg_types::*;
use crate::common::*;
use crate::ctx::*;

use fxhash::{FxHashMap, FxHashSet};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockIdx(pub u32);

#[derive(Debug, Clone, Copy)]
pub struct InstrIdx(pub u32);

#[derive(Debug, Clone, Copy)]
pub struct FunIdx(pub u32);

// Functions
#[derive(Debug)]
pub struct Fun {
    pub name: VarId,
    pub n_args: usize,
    pub blocks: Vec<Block>,
    pub return_type: RepType,
}

// Basic blocks
#[derive(Debug)]
pub struct Block {
    pub idx: BlockIdx,
    pub stmts: Vec<Expr>,
    pub exit: Exit,
}

// TODO: Make this type Copy
#[derive(Debug, Clone)]
pub enum Value {
    // Return value of instruction
    Ret(InstrIdx),
    // Nth argument of the current function
    Arg(usize),
    // Immediate int
    ImmI64(i64),
    // Immediate float
    ImmF64(f64),
    // A function
    Fun(FunIdx),
}

impl From<InstrIdx> for Value {
    fn from(instr: InstrIdx) -> Self {
        Value::Ret(instr)
    }
}

// Exit nodes of basic blocks
#[derive(Debug)]
pub enum Exit {
    // We always return a variable to keep things simpler in instruction selection: the 'ret'
    // instruction doesn't take any arguments, so we need a temporary for the return value in all
    // cases. 'None' is for returning unit.
    Return(Value),
    Branch {
        v1: Value,
        v2: Value,
        cond: Cmp,
        then_: BlockIdx,
        else_: BlockIdx,
    },
    Jump(BlockIdx),
}

// Assignment right-hand sides
#[derive(Debug)]
pub enum Expr {
    Atom(Atom),
    IBinOp(BinOp<IntBinOp>),
    FBinOp(BinOp<FloatBinOp>),
    Neg(Value),
    FNeg(Value),
    App(Value, Vec<Value>, RepType),
    // Tuple allocation
    Tuple(Vec<Value>), // TODO: Lower this more?
    // Tuple field read
    TupleGet(Value, usize),
    // Array allocation
    ArrayAlloc { len: Value, elem: Value },
    // Array field read
    ArrayGet(Value, Value),
    // Array field write
    ArrayPut(Value, Value, Value),
}

#[derive(Debug)]
pub enum Atom {
    Unit,
    Int(i64),
    Float(f64),
}
