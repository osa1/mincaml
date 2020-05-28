use super::instr::InstrIdx;

use cranelift_entity::entity_impl;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockIdx(u32);
entity_impl!(BlockIdx, "b");

#[derive(Debug)]
pub struct Block {
    /// First instruction of the block
    pub first_instr: InstrIdx,
    /// Last instruction of the block
    pub last_instr: InstrIdx,
    /// A block is terminated after adding a jump or ret.
    pub terminated: bool,
}

pub const PLACEHOLDER_INSTR_IDX: u32 = u32::MAX;

impl Default for Block {
    fn default() -> Self {
        Block {
            first_instr: InstrIdx::from_u32(PLACEHOLDER_INSTR_IDX),
            last_instr: InstrIdx::from_u32(PLACEHOLDER_INSTR_IDX),
            terminated: false,
        }
    }
}

pub fn is_placeholder_instr(instr_idx: InstrIdx) -> bool {
    instr_idx.as_u32() == PLACEHOLDER_INSTR_IDX
}
