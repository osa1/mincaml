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

pub(crate) struct Fun {
    cfg: CFG,
    entry_block: BlockIdx,
    exit_blocks: Vec<BlockIdx>,
    // Index with `BlockIdx`
    blocks: Vec<Block>,
    // Index with `InstrIdx`
    instrs: Vec<Instr>,
    current_block: BlockIdx,
    num_vars: u32,
}

pub(crate) struct Block {
    idx: BlockIdx,
    instrs: Vec<InstrIdx>,
}

impl Fun {
    pub(crate) fn new() -> (Self, BlockIdx) {
        let entry_block_idx = BlockIdx(0);
        let entry_block = Block {
            idx: entry_block_idx,
            instrs: vec![],
        };
        let fun = Fun {
            cfg: CFG {
                succs: vec![vec![]],
                preds: vec![vec![]],
            },
            entry_block: entry_block_idx,
            exit_blocks: vec![],
            blocks: vec![entry_block],
            instrs: vec![],
            current_block: entry_block_idx,
            num_vars: 0,
        };
        (fun, entry_block_idx)
    }

    pub(crate) fn create_block(&mut self) -> BlockIdx {
        let block_idx = BlockIdx(self.blocks.len() as u32);
        self.blocks.push(Block {
            idx: block_idx,
            instrs: vec![],
        });
        block_idx
    }

    pub(crate) fn switch_to_block(&mut self, block: BlockIdx) {
        self.current_block = block;
    }

    pub(crate) fn exit_blocks(&self) -> &[BlockIdx] {
        &self.exit_blocks
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
