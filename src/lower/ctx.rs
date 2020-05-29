use super::block::{is_placeholder_instr, Block, BlockIdx};
use super::cfg::CFG;
use super::fun::{Fun, FunSig};
use super::instr::{Instr, InstrIdx, InstrKind, Value};

use crate::cg_types::RepType;
use crate::common::Cmp;
use crate::ctx;
use crate::ctx::{TypeId, VarId};
use crate::type_check::Type;
use crate::var::CompilerPhase::ClosureConvert;

use cranelift_entity::PrimaryMap;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Ctx<'a> {
    /// Used to generate fresh variables
    ctx: &'a mut ctx::Ctx,
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

impl<'a> Ctx<'a> {
    /// Create a new context.
    pub fn new(ctx: &'a mut ctx::Ctx) -> Self {
        Self {
            ctx,
            funs: vec![],
            blocks: PrimaryMap::new(),
            cfg: CFG::new(),
            instrs: PrimaryMap::new(),
            var_values: Default::default(),
        }
    }

    /// Finish lowering.
    pub fn finish(self, main_name: VarId) -> Vec<Fun> {
        let Ctx {
            ctx: _,
            mut funs,
            blocks,
            cfg,
            instrs,
            var_values: _,
        } = self;

        let fun = Fun {
            name: main_name,
            args: vec![],
            blocks,
            instrs,
            cfg,
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

    /// Add an instruction to the given block.
    pub fn instr(&mut self, block: BlockIdx, instr_kind: InstrKind) -> InstrIdx {
        let Block {
            idx: _,
            ref mut first_instr,
            ref mut last_instr,
            ref mut terminated,
        } = &mut self.blocks[block];

        assert!(!*terminated);

        if instr_kind.is_control_instr() {
            *terminated = true;
        }

        for target in instr_kind.targets() {
            self.cfg.add_successor(block, target);
        }

        let instr_idx = self.instrs.next_key();
        if is_placeholder_instr(*first_instr) {
            assert!(is_placeholder_instr(*last_instr));
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
            instrs: fun_instrs,
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
    pub fn use_var(&self, var: VarId) -> Value {
        (*self.var_values.get(&var).expect("Unbound variable")).clone()
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
    pub fn mov(&mut self, block: BlockIdx, lhs: Value, rhs: Value) -> Value {
        self.instr(block, InstrKind::Mov(lhs, rhs)).into()
    }

    pub fn iimm(&mut self, block: BlockIdx, i: i64) -> Value {
        self.instr(block, InstrKind::IImm(i)).into()
    }

    pub fn fimm(&mut self, block: BlockIdx, f: f64) -> Value {
        self.instr(block, InstrKind::FImm(f)).into()
    }

    pub fn iadd(&mut self, block: BlockIdx, v1: Value, v2: Value) -> Value {
        self.instr(block, InstrKind::IAdd(v1, v2)).into()
    }

    pub fn isub(&mut self, block: BlockIdx, v1: Value, v2: Value) -> Value {
        self.instr(block, InstrKind::ISub(v1, v2)).into()
    }

    pub fn fadd(&mut self, block: BlockIdx, v1: Value, v2: Value) -> Value {
        self.instr(block, InstrKind::FAdd(v1, v2)).into()
    }

    pub fn fsub(&mut self, block: BlockIdx, v1: Value, v2: Value) -> Value {
        self.instr(block, InstrKind::FSub(v1, v2)).into()
    }

    pub fn fmul(&mut self, block: BlockIdx, v1: Value, v2: Value) -> Value {
        self.instr(block, InstrKind::FMul(v1, v2)).into()
    }

    pub fn fdiv(&mut self, block: BlockIdx, v1: Value, v2: Value) -> Value {
        self.instr(block, InstrKind::FDiv(v1, v2)).into()
    }

    pub fn neg(&mut self, block: BlockIdx, v: Value) -> Value {
        self.instr(block, InstrKind::Neg(v)).into()
    }

    pub fn fneg(&mut self, block: BlockIdx, v: Value) -> Value {
        self.instr(block, InstrKind::FNeg(v)).into()
    }

    pub fn tuple(&mut self, block: BlockIdx, len: usize) -> Value {
        self.instr(block, InstrKind::Tuple { len }).into()
    }

    pub fn tuple_put(&mut self, block: BlockIdx, tuple: Value, idx: usize, val: Value) -> Value {
        self.instr(block, InstrKind::TuplePut(tuple, idx, val))
            .into()
    }

    pub fn tuple_get(&mut self, block: BlockIdx, tuple: Value, idx: usize) -> Value {
        self.instr(block, InstrKind::TupleGet(tuple, idx)).into()
    }

    pub fn array_alloc(&mut self, block: BlockIdx, len: Value) -> Value {
        self.instr(block, InstrKind::ArrayAlloc { len }).into()
    }

    pub fn array_get(&mut self, block: BlockIdx, array: Value, idx: Value) -> Value {
        self.instr(block, InstrKind::ArrayGet(array, idx)).into()
    }

    pub fn array_put(&mut self, block: BlockIdx, array: Value, idx: Value, val: Value) -> Value {
        self.instr(block, InstrKind::ArrayPut(array, idx, val))
            .into()
    }

    pub fn call(&mut self, block: BlockIdx, f: Value, args: Vec<Value>, ret_ty: RepType) -> Value {
        self.instr(block, InstrKind::Call(f, args, ret_ty)).into()
    }

    pub fn ret(&mut self, block: BlockIdx, v: Value) {
        self.instr(block, InstrKind::Return(v));
    }

    pub fn jmp(&mut self, block: BlockIdx, target: BlockIdx) {
        self.instr(block, InstrKind::Jmp(target));
    }

    pub fn cond_jmp(
        &mut self, block: BlockIdx, v1: Value, v2: Value, cond: Cmp, then_target: BlockIdx,
        else_target: BlockIdx,
    ) {
        self.instr(
            block,
            InstrKind::CondJmp {
                v1,
                v2,
                cond,
                then_target,
                else_target,
            },
        );
    }
}
