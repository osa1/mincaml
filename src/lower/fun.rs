use super::block::{Block, BlockIdx};
use super::cfg::CFG;
use crate::cg_types::RepType;
use crate::ctx::VarId;

use cranelift_entity::PrimaryMap;

#[derive(Debug)]
pub struct Fun {
    pub name: VarId,
    pub args: Vec<VarId>,
    pub blocks: PrimaryMap<BlockIdx, Block>,
    pub cfg: CFG,
    pub return_type: RepType,
}

#[derive(Debug)]
pub struct FunSig {
    pub name: VarId,
    pub args: Vec<VarId>,
    pub return_type: RepType,
}
