use super::alloc::gen_alloc;
use super::instr::*;
use super::types::{FunIdx, LocalIdx, TypeIdx};
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

    pub fn i32_const(&mut self, i: i32) {
        i32_const(i, &mut self.bytes);
    }

    pub fn i64_const(&mut self, i: i64) {
        i64_const(i, &mut self.bytes);
    }

    pub fn f64_const(&mut self, f: f64) {
        f64_const(f, &mut self.bytes);
    }

    pub fn local_get(&mut self, var: VarId) {
        let local_idx = self.local_idx(var);
        local_get(local_idx, &mut self.bytes);
    }

    pub fn local_set(&mut self, var: VarId) {
        let local_idx = self.local_idx(var);
        local_set(local_idx, &mut self.bytes);
    }

    pub fn i32_add(&mut self) {
        i32_add(&mut self.bytes);
    }

    pub fn i32_mul(&mut self) {
        i32_mul(&mut self.bytes);
    }

    pub fn i64_store(&mut self) {
        i64_store(&mut self.bytes);
    }

    pub fn i64_load(&mut self) {
        i64_load(&mut self.bytes);
    }

    pub fn i64_add(&mut self) {
        i64_add(&mut self.bytes);
    }

    pub fn i64_sub(&mut self) {
        i64_sub(&mut self.bytes);
    }

    pub fn f64_store(&mut self) {
        f64_store(&mut self.bytes);
    }

    pub fn f64_load(&mut self) {
        f64_load(&mut self.bytes);
    }

    pub fn f64_add(&mut self) {
        f64_add(&mut self.bytes);
    }

    pub fn f64_sub(&mut self) {
        f64_sub(&mut self.bytes);
    }

    pub fn f64_mul(&mut self) {
        f64_mul(&mut self.bytes);
    }

    pub fn f64_div(&mut self) {
        f64_div(&mut self.bytes);
    }

    pub fn f64_neg(&mut self) {
        f64_neg(&mut self.bytes);
    }

    pub fn call(&mut self, fun_idx: FunIdx) {
        call(fun_idx, &mut self.bytes);
    }

    pub fn call_indirect(&mut self, fun_ty_idx: TypeIdx) {
        call_indirect(fun_ty_idx, &mut self.bytes);
    }

    pub fn ret(&mut self) {
        ret(&mut self.bytes);
    }

    pub fn alloc(&mut self, n: u32) {
        gen_alloc(n, &mut self.bytes);
    }

    pub fn finish(mut self) -> (Vec<u8>, Vec<VarId>) {
        self.bytes.push(0x0B); // end

        let mut locals: Vec<(VarId, LocalIdx)> = self.locals.into_iter().collect();
        locals.sort_by_key(|(_, idx)| *idx);

        (
            self.bytes,
            locals.into_iter().map(|(local, _)| local).collect(),
        )
    }
}
