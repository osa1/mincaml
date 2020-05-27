#![allow(dead_code)]

use super::block::BlockIdx;
use crate::cg_types::RepType;
use crate::common::Cmp;

use cranelift_entity::entity_impl;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InstrIdx(u32);
entity_impl!(InstrIdx, "b");

// A linked list of instructions. For first node `prev == idx`, for last `next == idx`.
#[derive(Debug)]
pub struct Instr {
    pub idx: InstrIdx,
    pub next: InstrIdx,
    pub prev: InstrIdx,
    pub kind: InstrKind,
}

#[derive(Debug)]
pub enum Value {
    // Nth block argument
    Arg(usize),
    // Result of instruction
    Instr(InstrIdx),
}

#[derive(Debug)]
pub enum InstrKind {
    Value(Value),
    // Integer addition
    IAdd(Value, Value),
    // Integer subtraction
    ISub(Value, Value),
    // Float addition
    FAdd(Value, Value),
    // Float subtraction
    FSub(Value, Value),
    // Float multiplication
    FMul(Value, Value),
    // Float division
    FDiv(Value, Value),
    // Integer negation
    Neg(Value),
    // Float negation
    FNeg(Value),
    // Function call
    Call(Value, Vec<Value>, RepType),
    // Tuple allocation
    Tuple {
        len: usize,
    },
    // Tuple field read
    TupleGet(Value, usize),
    // Tuple field write
    TuplePut(Value, usize, Value),
    // Array allocation
    ArrayAlloc {
        len: Value,
    },
    // Array field read
    ArrayGet(Value, Value),
    // Array field write
    ArrayPut(Value, Value, Value),

    // Control-flow instructions. These are the last instructions of a block.

    // A direct jump
    Jump(BlockIdx),
    // A conditional jump
    CondJmp {
        v1: Value,
        v2: Value,
        cond: Cmp,
        then_target: BlockIdx,
        else_target: BlockIdx,
    },
    // Function return
    Return,
}

impl InstrKind {
    /// Is this instruction a jump or ret?
    pub fn is_control_instr(&self) -> bool {
        match self {
            InstrKind::Jump(_) | InstrKind::CondJmp { .. } | InstrKind::Return => true,
            _ => false,
        }
    }

    // TODO: Replace the Vec with an iterator
    pub fn targets(&self) -> Vec<BlockIdx> {
        match self {
            InstrKind::Jump(target) => vec![*target],
            InstrKind::CondJmp {
                then_target,
                else_target,
                ..
            } => vec![*then_target, *else_target],
            _ => vec![],
        }
    }
}
