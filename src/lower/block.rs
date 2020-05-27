use super::instr::InstrIdx;

use cranelift_entity::{entity_impl, PrimaryMap};

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

pub const INSTR_PLACEHOLDER: InstrIdx = InstrIdx::from_u32(u32::MAX);

impl Default for Block {
    fn default() -> Self {
        Block {
            first_instr: INSTR_PLACEHOLDER,
            last_instr: INSTR_PLACEHOLDER,
            terminated: false,
        }
    }
}
