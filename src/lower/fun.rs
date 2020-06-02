use super::block::{Block, BlockIdx};
use super::instr::{Instr, InstrIdx, Phi, PhiIdx, Value, ValueIdx};

use crate::cg_types::RepType;
use crate::ctx::VarId;

use cranelift_entity::{PrimaryMap, SecondaryMap};

#[derive(Debug)]
pub struct Fun {
    pub name: VarId,
    pub args: Vec<VarId>,
    pub blocks: PrimaryMap<BlockIdx, Block>,
    pub instrs: PrimaryMap<InstrIdx, Instr>,
    pub preds: SecondaryMap<BlockIdx, Vec<BlockIdx>>,
    pub values: PrimaryMap<ValueIdx, Value>,
    pub phis: PrimaryMap<PhiIdx, Phi>,
    pub return_type: RepType,
}

#[derive(Debug)]
pub struct FunSig {
    pub name: VarId,
    pub args: Vec<VarId>,
    pub return_type: RepType,
}
