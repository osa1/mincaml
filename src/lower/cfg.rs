use cranelift_entity::SecondaryMap;

use super::types::BlockIdx;

#[derive(Debug)]
pub struct CFG {
    successors: SecondaryMap<BlockIdx, Vec<BlockIdx>>,
    predecessors: SecondaryMap<BlockIdx, Vec<BlockIdx>>,
}

impl CFG {
    pub fn new() -> Self {
        Self {
            successors: SecondaryMap::new(),
            predecessors: SecondaryMap::new(),
        }
    }

    pub fn add_successor(&mut self, block: BlockIdx, succ: BlockIdx) {
        self.successors[block].push(succ);
        self.predecessors[succ].push(block);
    }
}
