use super::encoding::{encode_i64_sleb128, encode_u32_uleb128};
use super::types::{FunIdx, GlobalIdx, LocalIdx};

pub fn i32_const(i: i32, buf: &mut Vec<u8>) {
    todo!()
}

pub fn i64_const(i: i64, buf: &mut Vec<u8>) {
    buf.push(0x42);
    encode_i64_sleb128(i, buf);
}

pub fn f64_const(f: f64, buf: &mut Vec<u8>) {
    buf.push(0x44);
    buf.extend_from_slice(&f.to_le_bytes());
}

pub fn local_get(idx: LocalIdx, buf: &mut Vec<u8>) {
    buf.push(0x20);
    encode_u32_uleb128(idx.0, buf);
}

pub fn local_set(idx: LocalIdx, buf: &mut Vec<u8>) {
    buf.push(0x21);
    encode_u32_uleb128(idx.0, buf);
}

pub fn global_get(idx: GlobalIdx, buf: &mut Vec<u8>) {
    buf.push(0x23);
    encode_u32_uleb128(idx.0, buf);
}

pub fn global_set(idx: GlobalIdx, buf: &mut Vec<u8>) {
    buf.push(0x24);
    encode_u32_uleb128(idx.0, buf);
}

pub fn i32_add(buf: &mut Vec<u8>) {
    buf.push(0x6A);
}

pub fn i32_mul(buf: &mut Vec<u8>) {
    buf.push(0x6C);
}

// memarg = {0, 0}
pub fn i64_store(buf: &mut Vec<u8>) {
    buf.push(0x37);
    buf.push(0); // align
    buf.push(0); // offset
}

// memarg = {0, 0}
pub fn i64_load(buf: &mut Vec<u8>) {
    buf.push(0x29);
    buf.push(0); // align
    buf.push(0); // offset
}

pub fn i64_add(buf: &mut Vec<u8>) {
    buf.push(0x7C);
}

pub fn i64_sub(buf: &mut Vec<u8>) {
    buf.push(0x7D);
}

// memarg = {0, 0}
pub fn f64_store(buf: &mut Vec<u8>) {
    buf.push(0x39);
    buf.push(0); // align
    buf.push(0); // offset
}

// memarg = {0, 0}
pub fn f64_load(buf: &mut Vec<u8>) {
    buf.push(0x2B);
    buf.push(0); // align
    buf.push(0); // offset
}

pub fn f64_add(buf: &mut Vec<u8>) {
    buf.push(0xA0);
}

pub fn f64_sub(buf: &mut Vec<u8>) {
    buf.push(0xA1);
}

pub fn f64_mul(buf: &mut Vec<u8>) {
    buf.push(0xA2);
}

pub fn f64_div(buf: &mut Vec<u8>) {
    buf.push(0xA3);
}

pub fn f64_neg(buf: &mut Vec<u8>) {
    buf.push(0x9A);
}

pub fn i32_gt_u(buf: &mut Vec<u8>) {
    buf.push(0x4B);
}

pub fn memory_grow(buf: &mut Vec<u8>) {
    buf.push(0x40);
    buf.push(0x00);
}

pub fn call(idx: FunIdx, buf: &mut Vec<u8>) {
    buf.push(0x10);
    encode_u32_uleb128(idx.0, buf);
}

pub fn ret(buf: &mut Vec<u8>) {
    buf.push(0x0F);
}
