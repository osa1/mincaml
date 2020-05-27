use super::block::{Block, BlockIdx, INSTR_PLACEHOLDER};
use super::cfg::CFG;
use super::fun::{Fun, FunSig};
use super::instr::{Instr, InstrIdx, InstrKind, Value};

use crate::ctx::VarId;

use cranelift_entity::PrimaryMap;
use std::collections::HashMap;

pub struct Ctx {
    /// Functions generated so far
    funs: Vec<Fun>,
    /// Blocks generated so far for the current function
    blocks: PrimaryMap<BlockIdx, Block>,
    /// Control-flow graph of the current function
    cfg: CFG,
    /// Instructions of the current function
    instrs: PrimaryMap<InstrIdx, Instr>,
    /// Maps variables in scope to their values
    var_values: HashMap<VarId, Value>,
}

impl Ctx {
    pub fn new() -> Self {
        Self {
            funs: vec![],
            blocks: PrimaryMap::new(),
            cfg: CFG::new(),
            instrs: PrimaryMap::new(),
            var_values: Default::default(),
        }
    }

    /// Create a new block.
    pub fn create_block(&mut self) -> BlockIdx {
        self.blocks.push(Default::default())
    }

    /// Add an instruction to the given block.
    pub fn instr(&mut self, block: BlockIdx, instr_kind: InstrKind) -> InstrIdx {
        let Block {
            ref mut first_instr,
            ref mut last_instr,
            ref mut terminated,
        } = &mut self.blocks[block];

        assert!(!*terminated);

        let instr_idx = self.instrs.next_key();
        if *first_instr == INSTR_PLACEHOLDER {
            assert_eq!(*last_instr, INSTR_PLACEHOLDER);
            // First instruction in the block
            let instr = Instr {
                idx: instr_idx,
                next: instr_idx,
                prev: instr_idx,
                kind: instr_kind,
            };
            self.instrs.push(instr);
            *first_instr = instr_idx;
            *last_instr = instr_idx;
        } else {
            self.instrs[*last_instr].next = instr_idx;
            let instr = Instr {
                idx: instr_idx,
                next: instr_idx,
                prev: *last_instr,
                kind: instr_kind,
            };
            *last_instr = instr_idx;
            self.instrs.push(instr);
        }

        if instr_kind.is_control_instr() {
            *terminated = true;
        }

        for target in instr_kind.targets() {
            self.cfg.add_successor(block, target);
        }

        instr_idx
    }

    /// Helper for creating new functions
    pub fn fork_fun<F: FnOnce(&mut Ctx) -> FunSig>(&mut self, f: F) {
        use std::mem::replace;

        let blocks = replace(&mut self.blocks, PrimaryMap::new());
        let cfg = replace(&mut self.cfg, CFG::new());
        let instrs = replace(&mut self.instrs, PrimaryMap::new());

        let FunSig {
            name,
            args,
            return_type,
        } = f(self);

        let fun_blocks = replace(&mut self.blocks, blocks);
        let fun_cfg = replace(&mut self.cfg, cfg);
        let fun_instrs = replace(&mut self.instrs, instrs);

        let fun = Fun {
            name,
            args,
            blocks: fun_blocks,
            cfg: fun_cfg,
            return_type,
        };

        self.funs.push(fun);
    }

    /// Define a function argument
    pub fn def_arg(&mut self, var: VarId, idx: usize) {
        let old = self.var_values.insert(var, Value::Arg(idx));
        debug_assert!(old.is_none());
    }

    /// Define a variable
    pub fn def_var(&mut self, var: VarId, val: Value) {
        let old = self.var_values.insert(var, val);
        debug_assert!(old.is_none());
    }

    /// Get value of a variable
    pub fn use_var(&mut self, var: VarId) -> Value {
        *self.var_values.get(&var).expect("Unbound variable")
    }
}
