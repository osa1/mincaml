use super::types::{FunIdx, LocalIdx, Ty};
use crate::ctx::VarId;

use fxhash::FxHashMap;

pub struct FunBuilder {
    locals: FxHashMap<VarId, LocalIdx>,
    // Encoding of the function body
    bytes: Vec<u8>,
}

/*
#[derive(Debug, Clone, Copy)]
pub enum Const {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}
*/

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

    pub fn i64_const(&mut self, i: i64) {
        self.bytes.push(0x42);
        encode_i64_sleb128(i, &mut self.bytes);
    }

    pub fn f64_const(&mut self, f: f64) {
        self.bytes.push(0x44);
        self.bytes.extend_from_slice(&f.to_le_bytes());
    }

    pub fn local_get(&mut self, var: VarId) {
        self.bytes.push(0x20);
        let local_idx = self.local_idx(var);
        encode_u32_uleb128(local_idx.0, &mut self.bytes);
    }

    pub fn local_set(&mut self, var: VarId) {
        self.bytes.push(0x21);
        let local_idx = self.local_idx(var);
        encode_u32_uleb128(local_idx.0, &mut self.bytes);
    }

    pub fn i64_add(&mut self) {
        self.bytes.push(0x6A);
    }

    pub fn i64_sub(&mut self) {
        self.bytes.push(0x6B);
    }

    pub fn f64_add(&mut self) {
        self.bytes.push(0xA0);
    }

    pub fn f64_sub(&mut self) {
        self.bytes.push(0xA1);
    }

    pub fn f64_mul(&mut self) {
        self.bytes.push(0xA2);
    }

    pub fn f64_div(&mut self) {
        self.bytes.push(0xA3);
    }

    pub fn f64_neg(&mut self) {
        self.bytes.push(0x9A);
    }

    pub fn call(&mut self, fun_idx: FunIdx) {
        self.bytes.push(0x10);
        encode_u32_uleb128(fun_idx.0, &mut self.bytes);
    }
}

fn encode_u32_uleb128(mut value: u32, buf: &mut Vec<u8>) {
    loop {
        let mut byte = (value & 0b0111_1111) as u8;
        value >>= 7;
        if value != 0 {
            byte |= 0b1000_0000;
        }
        buf.push(byte);
        if value == 0 {
            break;
        }
    }
}

fn encode_i64_sleb128(mut value: i64, buf: &mut Vec<u8>) {
    let mut more = true;
    while more {
        let mut byte = (value & 0b0111_1111) as u8;
        value >>= 7;

        let sign_bit_clear = (value & 0b0100_0000) == 0;
        if (value == 0 && sign_bit_clear) || (value == -1 && !sign_bit_clear) {
            more = false;
        } else {
            byte = byte | 0b1000_0000;
        }
        buf.push(byte);
    }
}

#[test]
fn test_encode_uleb128() {
    let mut bytes = vec![];
    encode_u32_uleb128(0, &mut bytes);
    assert_eq!(&bytes, &[0]);

    let mut bytes = vec![];
    encode_u32_uleb128(624485, &mut bytes);
    assert_eq!(&bytes, &[0xE5, 0x8E, 0x26]);
}

#[test]
fn test_encode_sleb128() {
    let mut bytes = vec![];
    encode_i64_sleb128(0, &mut bytes);
    assert_eq!(&bytes, &[0]);

    let mut bytes = vec![];
    encode_i64_sleb128(-123456, &mut bytes);
    assert_eq!(&bytes, &[0xC0, 0xBB, 0x78]);
}
