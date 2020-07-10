use super::types::{LocalIdx, Ty};
use crate::ctx::VarId;

use fxhash::FxHashMap;

pub struct FunBuilder {
    locals: FxHashMap<VarId, LocalIdx>,
    // Encoding of the function body
    bytes: Vec<u8>,
}

impl FunBuilder {
    pub fn new() -> Self {
        FunBuilder {
            locals: Default::default(),
            bytes: vec![],
        }
    }

    pub fn enter_loop(&mut self) {
        self.bytes.push(0x03);
        // TODO: I don't understand what is blocktype for
        self.bytes.push(0x40); // blocktype not available
    }

    pub fn exit_loop(&mut self) {
        self.bytes.push(0x0B);
    }

    fn local_idx(&mut self, var: VarId) -> LocalIdx {
        match self.locals.get(&var) {
            Some(idx) => *idx,
            None => {
                let idx = LocalIdx(self.locals.len() as u32);
                self.locals.insert(var, idx);
                idx
            }
        }
    }
}
