#![allow(dead_code)]

use super::{block::BlockIdx, fun::Fun};
use crate::cg_types::RepType;
use crate::common::Cmp;
use crate::ctx::{Ctx, VarId};

use cranelift_entity::entity_impl;
use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InstrIdx(u32);
entity_impl!(InstrIdx, "i");

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhiIdx(u32);
entity_impl!(PhiIdx, "φ");

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueIdx(u32);
entity_impl!(ValueIdx, "v");

// A linked list of instructions. For first node `prev == idx`, for last `next == idx`.
#[derive(Debug)]
pub struct Instr {
    pub idx: InstrIdx,
    pub block: BlockIdx,
    pub next: InstrIdx,
    pub prev: InstrIdx,
    pub kind: InstrKind,
}

// A phi
#[derive(Debug)]
pub struct Phi {
    // Owner of the phi.
    pub owner: BlockIdx,
    // Operands of the phi. Nth operand is for the Nth predecessor.
    pub values: Vec<ValueIdx>,
}

// Values
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    // A global value, like a built-in or a function
    Global(VarId),
    // Nth function argument
    Arg(usize),
    // Result of instruction
    Instr(InstrIdx),
    // A phi
    Phi(PhiIdx),
}

impl From<InstrIdx> for Value {
    fn from(i: InstrIdx) -> Self {
        Value::Instr(i)
    }
}

impl From<PhiIdx> for Value {
    fn from(p: PhiIdx) -> Self {
        Value::Phi(p)
    }
}

#[derive(Debug)]
pub enum InstrKind {
    // A move
    // Mov(ValueIdx, ValueIdx),
    // Integer constant
    IImm(i64),
    // Float constant
    FImm(f64),
    // Integer addition
    IAdd(ValueIdx, ValueIdx),
    // Integer subtraction
    ISub(ValueIdx, ValueIdx),
    // Float addition
    FAdd(ValueIdx, ValueIdx),
    // Float subtraction
    FSub(ValueIdx, ValueIdx),
    // Float multiplication
    FMul(ValueIdx, ValueIdx),
    // Float division
    FDiv(ValueIdx, ValueIdx),
    // Integer negation
    Neg(ValueIdx),
    // Float negation
    FNeg(ValueIdx),
    // Function call
    Call(ValueIdx, Vec<ValueIdx>, RepType),
    // Tuple allocation
    Tuple {
        len: usize,
    },
    // Tuple field read
    TupleGet(ValueIdx, usize),
    // Tuple field write
    TuplePut(ValueIdx, usize, ValueIdx),
    // Array allocation
    ArrayAlloc {
        len: ValueIdx,
    },
    // Array field read
    ArrayGet(ValueIdx, ValueIdx),
    // Array field write
    ArrayPut(ValueIdx, ValueIdx, ValueIdx),

    // Control-flow instructions. These are the last instructions of a block.

    // A direct jump
    Jmp(BlockIdx),
    // A conditional jump
    CondJmp {
        v1: ValueIdx,
        v2: ValueIdx,
        cond: Cmp,
        then_target: BlockIdx,
        else_target: BlockIdx,
    },
    // Function return
    Return(ValueIdx),
}

impl InstrKind {
    // TODO: Replace the Vec with an iterator
    pub fn uses(&self) -> Vec<ValueIdx> {
        match self {
            InstrKind::IImm(_)
            | InstrKind::FImm(_)
            | InstrKind::Tuple { .. }
            | InstrKind::Jmp(_) => vec![],
            InstrKind::IAdd(v1, v2)
            | InstrKind::ISub(v1, v2)
            | InstrKind::FAdd(v1, v2)
            | InstrKind::FSub(v1, v2)
            | InstrKind::FMul(v1, v2)
            | InstrKind::FDiv(v1, v2)
            | InstrKind::TuplePut(v1, _, v2)
            | InstrKind::ArrayGet(v1, v2) => vec![*v1, *v2],
            InstrKind::Neg(v) | InstrKind::FNeg(v) => vec![*v],
            InstrKind::Call(v, vs, _) => {
                let mut vs = vs.clone();
                vs.insert(0, *v);
                vs
            }
            InstrKind::TupleGet(v, _) => vec![*v],
            InstrKind::ArrayAlloc { len } => vec![*len],
            InstrKind::ArrayPut(v1, v2, v3) => vec![*v1, *v2, *v3],
            InstrKind::CondJmp {
                v1,
                v2,
                cond: _,
                then_target: _,
                else_target: _,
            } => vec![*v1, *v2],
            InstrKind::Return(v) => vec![*v],
        }
    }

    /// Is this instruction a jump or ret?
    pub fn is_control_instr(&self) -> bool {
        match self {
            InstrKind::Jmp(_) | InstrKind::CondJmp { .. } | InstrKind::Return(_) => true,
            _ => false,
        }
    }

    // TODO: Replace the Vec with an iterator
    pub fn targets(&self) -> Vec<BlockIdx> {
        match self {
            InstrKind::Jmp(target) => vec![*target],
            InstrKind::CondJmp {
                then_target,
                else_target,
                ..
            } => vec![*then_target, *else_target],
            _ => vec![],
        }
    }
}

//
// Debug interface
//

pub struct ValueDebug<'a> {
    value: &'a Value,
    ctx: &'a Ctx,
}

impl Value {
    pub fn debug<'a>(&'a self, ctx: &'a Ctx) -> ValueDebug<'a> {
        ValueDebug { value: self, ctx }
    }
}

impl<'a> fmt::Debug for ValueDebug<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            Value::Global(var) => var.debug_display(self.ctx).fmt(f),
            Value::Arg(arg_idx) => write!(f, "arg {}", arg_idx),
            Value::Instr(instr_idx) => write!(f, "{}", instr_idx),
            Value::Phi(phi_idx) => write!(f, "{}", phi_idx),
        }
    }
}

// Provides a better `Debug` impl for `ValueIdx`: shows the value instead of index
pub struct ValueIdxDebug<'a> {
    value: ValueIdx,
    ctx: &'a Ctx,
    fun: &'a Fun,
}

impl ValueIdx {
    pub fn debug<'a>(&'a self, ctx: &'a Ctx, fun: &'a Fun) -> ValueIdxDebug<'a> {
        ValueIdxDebug {
            value: *self,
            ctx,
            fun,
        }
    }
}

impl<'a> fmt::Debug for ValueIdxDebug<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = &self.fun.values[self.value];
        value.debug(self.ctx).fmt(f)
    }
}
