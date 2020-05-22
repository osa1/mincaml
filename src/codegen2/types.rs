use super::instr::*;

#[derive(Debug, Default, PartialEq, PartialOrd, Ord, Eq, Hash, Copy, Clone)]
pub(crate) struct BlockIdx(pub(crate) u32);

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq, Hash, Copy, Clone)]
pub(crate) struct InstrIdx(pub(crate) u32);

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq, Hash, Copy, Clone)]
pub(crate) struct VarIdx(pub(crate) u32);

pub(crate) struct CFG {
    // Successors of blocks. Index with `BlockIdx`.
    succs: Vec<Vec<BlockIdx>>,
    // Successors of blocks. Index with `BlockIdx`.
    preds: Vec<Vec<BlockIdx>>,
}

pub(crate) struct FunctionBuilder {
    cfg: CFG,
    entry_block: BlockIdx,
    exit_block: BlockIdx,
    // Index with `BlockIdx`
    blocks: Vec<Block>,
    // Index with `InstrIdx`
    instrs: Vec<Instr>,
    num_vars: u32,
}

pub(crate) struct Block {
    idx: BlockIdx,
    instrs: Vec<InstrIdx>,
}

impl FunctionBuilder {
    pub(crate) fn exit_block(&self) -> BlockIdx {
        self.exit_block
    }

    pub(crate) fn blocks<'a>(&'a self) -> impl Iterator<Item = BlockIdx> + 'a {
        self.blocks.iter().map(|block| block.idx)
    }

    pub(crate) fn block_entry(&self, block: BlockIdx) -> InstrIdx {
        self.blocks[block.0 as usize].instrs[0]
    }

    pub(crate) fn block_instrs<'a>(
        &'a self, block: BlockIdx,
    ) -> impl DoubleEndedIterator<Item = InstrIdx> + 'a {
        self.blocks[block.0 as usize].instrs.iter().cloned()
    }

    pub(crate) fn block_preds(&self, block: BlockIdx) -> &[BlockIdx] {
        &self.cfg.preds[block.0 as usize]
    }

    pub(crate) fn instr(&self, instr: InstrIdx) -> &Instr {
        &self.instrs[instr.0 as usize]
    }

    pub(crate) fn vars(&self) -> impl Iterator<Item = VarIdx> {
        (0..self.num_vars).map(VarIdx)
    }
}
