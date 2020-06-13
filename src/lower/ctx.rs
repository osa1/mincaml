// This implements the SSA construction algorithm described in
//
//     Simple and Efficient Construction of Static Single Assignment Form
//     by Braun et al.

use super::block::{is_placeholder_instr, Block, BlockIdx};
use super::fun::{Fun, FunSig};
use super::instr::{Instr, InstrIdx, InstrKind, Phi, PhiIdx, Value, ValueIdx};
// use super::print::display_id;

use crate::cg_types::RepType;
use crate::common::Cmp;
use crate::ctx;
use crate::ctx::{TypeId, VarId};
use crate::type_check::Type;
use crate::var::CompilerPhase::ClosureConvert;

use cranelift_entity::{PrimaryMap, SecondaryMap};
use fxhash::FxHashMap;
use std::rc::Rc;

pub struct Ctx<'a> {
    /// Used to generate fresh variables
    ctx: &'a mut ctx::Ctx,

    /// Functions generated so far
    funs: Vec<Fun>,

    /// Blocks generated so far for the current function
    blocks: PrimaryMap<BlockIdx, Block>,

    /// Exit blocks of the current function
    exit_blocks: Vec<BlockIdx>,

    /// Heap for values of the current function
    values: PrimaryMap<ValueIdx, Value>,

    /// Heap for phis of the current function
    phis: PrimaryMap<PhiIdx, Phi>,

    /// Instructions of the current function
    instrs: PrimaryMap<InstrIdx, Instr>,

    /// Maps blocks to their successors in the control-flow graph
    succs: SecondaryMap<BlockIdx, Vec<BlockIdx>>,

    /// Maps blocks to their predecessors in the control-flow graph
    preds: SecondaryMap<BlockIdx, Vec<BlockIdx>>,

    /// Maps values to their use sites
    value_use_sites: SecondaryMap<ValueIdx, Vec<ValueIdx>>,

    /// Maps blocks to variables defined
    block_vars: SecondaryMap<BlockIdx, FxHashMap<VarId, ValueIdx>>,

    /// Maps blocks to their phis
    block_phis: SecondaryMap<BlockIdx, Vec<PhiIdx>>,

    /// Maps phis to referencing values. In other words, for an `idx` in this Vec, `values[idx]`
    /// is `Value::Phi(idx)`. Used to remove trivial phis.
    phi_users: SecondaryMap<PhiIdx, Vec<ValueIdx>>,

    /// Phis added as loop breakers. These need to be updated when the block is sealed.
    incomplete_phis: SecondaryMap<BlockIdx, Vec<(VarId, PhiIdx)>>,
}

fn create_builtins(
    ctx: &ctx::Ctx,
) -> (
    PrimaryMap<ValueIdx, Value>,
    SecondaryMap<BlockIdx, FxHashMap<VarId, ValueIdx>>,
) {
    let mut vals = PrimaryMap::new();
    let mut block_vars: SecondaryMap<BlockIdx, FxHashMap<VarId, ValueIdx>> = SecondaryMap::new();

    let entry_block = BlockIdx::from_u32(0);

    for (var, _) in ctx.builtins() {
        let val_idx = vals.push(Value::Global(*var));
        block_vars[entry_block].insert(*var, val_idx);
    }

    (vals, block_vars)
}

impl<'a> Ctx<'a> {
    /// Create a new context.
    pub fn new(ctx: &'a mut ctx::Ctx) -> Self {
        let (values, block_vars) = create_builtins(ctx);
        Self {
            ctx,
            funs: vec![],
            blocks: PrimaryMap::new(),
            exit_blocks: vec![],
            values,
            phis: PrimaryMap::new(),
            instrs: PrimaryMap::new(),
            succs: SecondaryMap::new(),
            preds: SecondaryMap::new(),
            value_use_sites: SecondaryMap::new(),
            block_vars,
            block_phis: SecondaryMap::new(),
            phi_users: SecondaryMap::new(),
            incomplete_phis: SecondaryMap::new(),
        }
    }

    /// Finish lowering.
    pub fn finish(self, main_name: VarId) -> Vec<Fun> {
        let Ctx {
            ctx: _,
            mut funs,
            blocks,
            exit_blocks,
            values,
            phis,
            instrs,
            succs,
            preds,
            value_use_sites,
            block_phis,
            ..
        } = self;

        for block in blocks.values() {
            assert!(block.filled);
            assert!(block.sealed);
        }

        let fun = Fun {
            name: main_name,
            args: vec![],
            blocks,
            exit_blocks,
            values,
            phis,
            instrs,
            succs,
            preds,
            value_use_sites,
            block_phis,
            return_type: RepType::Word,
        };

        funs.push(fun);
        funs
    }

    /// Create a fresh variable with the given type.
    pub fn fresh_var(&mut self, rep_type: RepType) -> VarId {
        self.ctx.fresh_codegen_var(ClosureConvert, rep_type)
    }

    /// Create a new block.
    pub fn create_block(&mut self) -> BlockIdx {
        let idx = self.blocks.next_key();
        self.blocks.push(Block::new(idx))
    }

    pub fn entry_block(&self) -> BlockIdx {
        BlockIdx::from_u32(0)
    }

    pub fn seal_block(&mut self, block_idx: BlockIdx) {
        assert!(!self.blocks[block_idx].sealed);
        // TODO: clone() below to avoid borrowchk issue
        for (var, phi_idx) in self.incomplete_phis[block_idx].clone() {
            self.add_phi_operands(block_idx, var, phi_idx);
        }
        self.blocks[block_idx].sealed = true;
    }

    pub fn create_phi(&mut self, block_idx: BlockIdx, var: VarId) -> PhiIdx {
        let phi_idx = self.phis.push(Phi {
            owner: block_idx,
            values: vec![],
        });
        self.block_phis[block_idx].push(phi_idx);
        self.incomplete_phis[block_idx].push((var, phi_idx));
        phi_idx
    }

    /// Add an instruction to the given block.
    pub fn instr(&mut self, block_idx: BlockIdx, instr_kind: InstrKind) -> ValueIdx {
        let Block {
            idx: _,
            ref mut first_instr,
            ref mut last_instr,
            ref mut filled,
            sealed: _,
        } = &mut self.blocks[block_idx];

        assert!(!*filled);

        if instr_kind.is_control_instr() {
            *filled = true;
        }

        for target in instr_kind.targets() {
            self.preds[target].push(block_idx);
            self.succs[block_idx].push(target);
        }

        let instr_idx = self.instrs.next_key();
        if is_placeholder_instr(*first_instr) {
            assert!(is_placeholder_instr(*last_instr));
            // First instruction in the block
            let instr = Instr {
                idx: instr_idx,
                block: block_idx,
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
                block: block_idx,
                next: instr_idx,
                prev: *last_instr,
                kind: instr_kind,
            };
            *last_instr = instr_idx;
            self.instrs.push(instr);
        }

        self.values.push(Value::Instr(instr_idx))
    }

    /// Helper for creating new functions
    pub fn fork_fun<F: FnOnce(&mut Ctx) -> FunSig>(&mut self, f: F) {
        use std::mem::replace;

        let (fun_values, fun_block_vars) = create_builtins(self.ctx);

        let blocks = replace(&mut self.blocks, PrimaryMap::new());
        let exit_blocks = replace(&mut self.exit_blocks, vec![]);
        let values = replace(&mut self.values, fun_values);
        let phis = replace(&mut self.phis, PrimaryMap::new());
        let instrs = replace(&mut self.instrs, PrimaryMap::new());
        let succs = replace(&mut self.succs, SecondaryMap::new());
        let preds = replace(&mut self.preds, SecondaryMap::new());
        let value_uses = replace(&mut self.value_use_sites, SecondaryMap::new());
        let block_vars = replace(&mut self.block_vars, fun_block_vars);
        let block_phis = replace(&mut self.block_phis, SecondaryMap::new());
        let incomplete_phis = replace(&mut self.incomplete_phis, SecondaryMap::new());

        let FunSig {
            name,
            args,
            return_type,
        } = f(self);

        let fun_blocks = replace(&mut self.blocks, blocks);
        let fun_exit_blocks = replace(&mut self.exit_blocks, exit_blocks);
        let fun_values = replace(&mut self.values, values);
        let fun_phis = replace(&mut self.phis, phis);
        let fun_instrs = replace(&mut self.instrs, instrs);
        let fun_succs = replace(&mut self.succs, succs);
        let fun_preds = replace(&mut self.preds, preds);
        let fun_value_uses = replace(&mut self.value_use_sites, value_uses);
        let fun_block_phis = replace(&mut self.block_phis, block_phis);
        self.block_vars = block_vars;
        self.incomplete_phis = incomplete_phis;

        let fun = Fun {
            name,
            args,
            blocks: fun_blocks,
            exit_blocks: fun_exit_blocks,
            values: fun_values,
            phis: fun_phis,
            instrs: fun_instrs,
            succs: fun_succs,
            preds: fun_preds,
            value_use_sites: fun_value_uses,
            block_phis: fun_block_phis,
            return_type,
        };

        self.funs.push(fun);
    }

    /// Define a function argument
    pub fn def_arg(&mut self, var: VarId, idx: usize) {
        let val_idx = self.values.push(Value::Arg(idx));
        let entry_block = self.entry_block();
        self.block_vars[entry_block].insert(var, val_idx);
    }

    // Define a variable in the given block
    // (writeVariable in the paper)
    // pub fn def_var(&mut self, block_idx: BlockIdx, var: VarId, val: Value) {
    //     let val_idx = self.values.push(val);
    //     self.def_var_(block_idx, var, val_idx);
    // }

    pub fn def_global(&mut self, var: VarId) {
        let val_idx = self.values.push(Value::Global(var));
        let entry_block = self.entry_block();
        self.block_vars[entry_block].insert(var, val_idx);
    }

    /// Define a variable in the given block
    // (writeVariable in the paper)
    pub fn def_var(&mut self, block_idx: BlockIdx, var: VarId, val_idx: ValueIdx) {
        self.block_vars[block_idx].insert(var, val_idx);
    }

    /// Get value of a variable in the given block
    // (readVariable in the paper)
    pub fn use_var(&mut self, block_idx: BlockIdx, var: VarId) -> ValueIdx {
        match self.block_vars[block_idx].get(&var) {
            Some(val_idx) => {
                // Local value numbering
                *val_idx
            }
            None => {
                // Global value numbering
                self.use_var_recursive(block_idx, var)
            }
        }
    }

    // (readVariableRecursive in the paper)
    fn use_var_recursive(&mut self, block_idx: BlockIdx, var: VarId) -> ValueIdx {
        let sealed = self.blocks[block_idx].sealed;
        let val_idx;

        if !sealed {
            // Incomplete CFG
            let phi_idx = self.create_phi(block_idx, var);
            val_idx = self.values.push(Value::Phi(phi_idx));
        } else if self.preds[block_idx].len() == 0 {
            // Sealed and have no predecessors, the variable should've been defined
            use std::fmt::Write;
            let mut s = String::new();
            write!(s, "\n\nUndefined variable in {}: {:?} ", block_idx, var).unwrap();
            super::print::pp_id(self.ctx, var, &mut s).unwrap();
            writeln!(s, "\nblock_vars: {:?}\n\n", self.block_vars[block_idx]).unwrap();
            panic!("{}", s);
        } else if self.preds[block_idx].len() == 1 {
            // sealed
            // Optimize the common case of one predecessor: no phi needed
            val_idx = self.use_var(self.preds[block_idx][0], var);
        } else {
            // sealed
            // Break potential cycles with operandless phi
            let phi_idx = self.create_phi(block_idx, var);
            val_idx = self.values.push(Value::Phi(phi_idx));
            self.def_var(block_idx, var, val_idx);
            self.add_phi_operands(block_idx, var, phi_idx);
        }

        self.def_var(block_idx, var, val_idx);
        val_idx
    }

    // (addPhiOperands in the paper)
    fn add_phi_operands(&mut self, block_idx: BlockIdx, var: VarId, phi_idx: PhiIdx) {
        // TODO: clone below to avoid borrowchk error
        for pred in self.preds[block_idx].clone() {
            let val_idx = self.use_var(pred, var);
            self.phis[phi_idx].values.push(val_idx);
        }

        // TODO: tryRemoveTrivialPhi
    }

    /// Is the variable built-in? Used in free variable generation. Built-in variables are not
    /// free.
    pub fn is_builtin_var(&self, var: VarId) -> bool {
        self.ctx.is_builtin_var(var)
    }

    pub fn get_type(&self, ty_id: TypeId) -> Rc<Type> {
        self.ctx.get_type(ty_id)
    }

    pub fn var_type(&self, var: VarId) -> Rc<Type> {
        self.ctx.var_type(var)
    }
}

//
// Helpers for generating instructions
//

impl<'a> Ctx<'a> {
    // pub fn mov(&mut self, block: BlockIdx, value: ValueIdx, loc: ValueIdx) -> ValueIdx {
    //     self.instr(block, InstrKind::Mov(loc, value)).into()
    // }

    pub fn iimm(&mut self, block: BlockIdx, i: i64) -> ValueIdx {
        self.instr(block, InstrKind::IImm(i))
    }

    pub fn fimm(&mut self, block: BlockIdx, f: f64) -> ValueIdx {
        self.instr(block, InstrKind::FImm(f))
    }

    pub fn iadd(&mut self, block: BlockIdx, v1: ValueIdx, v2: ValueIdx) -> ValueIdx {
        let value_idx = self.instr(block, InstrKind::IAdd(v1, v2));
        self.value_use_sites[v1].push(value_idx);
        self.value_use_sites[v2].push(value_idx);
        value_idx
    }

    pub fn isub(&mut self, block: BlockIdx, v1: ValueIdx, v2: ValueIdx) -> ValueIdx {
        let value_idx = self.instr(block, InstrKind::ISub(v1, v2));
        self.value_use_sites[v1].push(value_idx);
        self.value_use_sites[v2].push(value_idx);
        value_idx
    }

    pub fn fadd(&mut self, block: BlockIdx, v1: ValueIdx, v2: ValueIdx) -> ValueIdx {
        let value_idx = self.instr(block, InstrKind::FAdd(v1, v2));
        self.value_use_sites[v1].push(value_idx);
        self.value_use_sites[v2].push(value_idx);
        value_idx
    }

    pub fn fsub(&mut self, block: BlockIdx, v1: ValueIdx, v2: ValueIdx) -> ValueIdx {
        let value_idx = self.instr(block, InstrKind::FSub(v1, v2));
        self.value_use_sites[v1].push(value_idx);
        self.value_use_sites[v2].push(value_idx);
        value_idx
    }

    pub fn fmul(&mut self, block: BlockIdx, v1: ValueIdx, v2: ValueIdx) -> ValueIdx {
        let value_idx = self.instr(block, InstrKind::FMul(v1, v2));
        self.value_use_sites[v1].push(value_idx);
        self.value_use_sites[v2].push(value_idx);
        value_idx
    }

    pub fn fdiv(&mut self, block: BlockIdx, v1: ValueIdx, v2: ValueIdx) -> ValueIdx {
        let value_idx = self.instr(block, InstrKind::FDiv(v1, v2));
        self.value_use_sites[v1].push(value_idx);
        self.value_use_sites[v2].push(value_idx);
        value_idx
    }

    pub fn neg(&mut self, block: BlockIdx, v: ValueIdx) -> ValueIdx {
        let value_idx = self.instr(block, InstrKind::Neg(v));
        self.value_use_sites[v].push(value_idx);
        value_idx
    }

    pub fn fneg(&mut self, block: BlockIdx, v: ValueIdx) -> ValueIdx {
        let value_idx = self.instr(block, InstrKind::FNeg(v));
        self.value_use_sites[v].push(value_idx);
        value_idx
    }

    pub fn tuple(&mut self, block: BlockIdx, len: usize) -> ValueIdx {
        self.instr(block, InstrKind::Tuple { len })
    }

    pub fn tuple_put(
        &mut self, block: BlockIdx, tuple: ValueIdx, idx: usize, val: ValueIdx,
    ) -> ValueIdx {
        let value_idx = self.instr(block, InstrKind::TuplePut(tuple, idx, val));
        self.value_use_sites[tuple].push(value_idx);
        self.value_use_sites[val].push(value_idx);
        value_idx
    }

    pub fn tuple_get(&mut self, block: BlockIdx, tuple: ValueIdx, idx: usize) -> ValueIdx {
        let value_idx = self.instr(block, InstrKind::TupleGet(tuple, idx));
        self.value_use_sites[tuple].push(value_idx);
        value_idx
    }

    pub fn array_alloc(&mut self, block: BlockIdx, len: ValueIdx) -> ValueIdx {
        let value_idx = self.instr(block, InstrKind::ArrayAlloc { len });
        self.value_use_sites[len].push(value_idx);
        value_idx
    }

    pub fn array_get(&mut self, block: BlockIdx, array: ValueIdx, idx: ValueIdx) -> ValueIdx {
        let value_idx = self.instr(block, InstrKind::ArrayGet(array, idx));
        self.value_use_sites[array].push(value_idx);
        self.value_use_sites[idx].push(value_idx);
        value_idx
    }

    pub fn array_put(
        &mut self, block: BlockIdx, array: ValueIdx, idx: ValueIdx, val: ValueIdx,
    ) -> ValueIdx {
        let value_idx = self.instr(block, InstrKind::ArrayPut(array, idx, val));
        self.value_use_sites[array].push(value_idx);
        self.value_use_sites[idx].push(value_idx);
        self.value_use_sites[val].push(value_idx);
        value_idx
    }

    pub fn call(
        &mut self, block: BlockIdx, f: ValueIdx, args: Vec<ValueIdx>, ret_ty: RepType,
    ) -> ValueIdx {
        // TODO: Redundant clone below
        let value_idx = self.instr(block, InstrKind::Call(f, args.clone(), ret_ty));
        self.value_use_sites[f].push(value_idx);
        for arg in args {
            self.value_use_sites[arg].push(value_idx);
        }
        value_idx
    }

    pub fn ret(&mut self, block: BlockIdx, v: ValueIdx) {
        #[cfg(debug_assertions)]
        for exit_block in &self.exit_blocks {
            assert_ne!(block, *exit_block);
        }

        self.exit_blocks.push(block);
        let value_idx = self.instr(block, InstrKind::Return(v));
        self.value_use_sites[v].push(value_idx);
    }

    pub fn jmp(&mut self, block: BlockIdx, target: BlockIdx) {
        self.instr(block, InstrKind::Jmp(target));
    }

    pub fn cond_jmp(
        &mut self, block: BlockIdx, v1: ValueIdx, v2: ValueIdx, cond: Cmp, then_target: BlockIdx,
        else_target: BlockIdx,
    ) {
        let value_idx = self.instr(
            block,
            InstrKind::CondJmp {
                v1,
                v2,
                cond,
                then_target,
                else_target,
            },
        );
        self.value_use_sites[v1].push(value_idx);
        self.value_use_sites[v2].push(value_idx);
    }
}
