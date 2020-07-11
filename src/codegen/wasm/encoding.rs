use super::instr;
use super::types::*;

pub fn encode_vec<A>(stuff: &[A], encode: &mut dyn FnMut(&A, &mut Vec<u8>), buf: &mut Vec<u8>) {
    encode_u32_uleb128(stuff.len() as u32, buf);
    for stuff in stuff {
        encode(stuff, buf);
    }
}

pub fn encode_type_section(mut fun_tys: Vec<(FunTy, TypeIdx)>, buf: &mut Vec<u8>) {
    // We need to sort the types by index so that when generating the 'code' section we can use the
    // `TypeIdx` as the index of the type in 'type' section.
    fun_tys.sort_by_key(|(_fun_ty, type_idx)| *type_idx);
    let fun_tys: Vec<FunTy> = fun_tys.into_iter().map(|(fun_ty, _)| fun_ty).collect();

    let mut vec_bytes = vec![];
    encode_vec(
        &fun_tys,
        &mut |fun_ty, buf| {
            buf.push(0x60);
            encode_result_type(&fun_ty.args, buf);
            match fun_ty.ret {
                None => encode_result_type(&[], buf),
                Some(ty) => encode_result_type(&[ty], buf),
            }
        },
        &mut vec_bytes,
    );

    buf.push(1);
    encode_u32_uleb128(vec_bytes.len() as u32, buf);
    buf.extend_from_slice(&vec_bytes);
}

pub fn encode_function_section(ty_indices: &[TypeIdx], buf: &mut Vec<u8>) {
    let mut vec_bytes = vec![];
    encode_vec(
        ty_indices,
        &mut |ty, buf| {
            encode_u32_uleb128(ty.0, buf);
        },
        &mut vec_bytes,
    );

    buf.push(3);
    encode_u32_uleb128(vec_bytes.len() as u32, buf);
    buf.extend_from_slice(&vec_bytes);
}

pub fn encode_table_section(n_funs: u32, buf: &mut Vec<u8>) {
    // section(vec(table))
    // table = 0x70 limits
    // limits = 0x00 min:u32
    //        | 0x01 min:u32 max:u32

    // TODO: I don't understand the min/max stuff, and why tables are created uninitialized

    let mut vec_bytes = vec![];
    encode_u32_uleb128(1, &mut vec_bytes);
    vec_bytes.push(0x70);
    vec_bytes.push(0x01);
    encode_u32_uleb128(n_funs, &mut vec_bytes); // min
    encode_u32_uleb128(n_funs, &mut vec_bytes); // max

    buf.push(4);
    encode_u32_uleb128(vec_bytes.len() as u32, buf);
    buf.extend_from_slice(&vec_bytes);
}

pub fn encode_global_section(buf: &mut Vec<u8>) {
    // section(vec(global))
    let mut section_bytes = vec![];
    // 2 variables: hp and hp_lim
    encode_u32_uleb128(2, &mut section_bytes);

    // NB. Global 0 is hp, 1 is hp_lim. This needs to be in sync with the indices in the `alloc`
    // module

    // hp
    section_bytes.push(0x7F); // int32
    section_bytes.push(0x1); // mutable
    section_bytes.push(0x41); // i32.const
    section_bytes.push(0); // the constant '0' in leb128
    section_bytes.push(0x0B); // end of expression

    // hp_lim
    section_bytes.push(0x7F); // int32
    section_bytes.push(0x1); // mutable
    section_bytes.push(0x41); // i32.const
    section_bytes.push(0); // the constant '0' in leb128
    section_bytes.push(0x0B); // end of expression

    buf.push(6);
    encode_u32_uleb128(section_bytes.len() as u32, buf);
    buf.extend_from_slice(&section_bytes);
}

pub fn encode_start_section(start: FunIdx, buf: &mut Vec<u8>) {
    buf.push(8);

    let mut section_bytes = vec![];
    encode_u32_uleb128(start.0, &mut section_bytes);

    encode_u32_uleb128(section_bytes.len() as u32, buf);
    buf.extend_from_slice(&section_bytes);
}

pub fn encode_element_section(n_funs: u32, buf: &mut Vec<u8>) {
    buf.push(9);

    let mut section_bytes = vec![];

    encode_u32_uleb128(1, &mut section_bytes); // vec size
    section_bytes.push(0); // tableidx
    instr::i32_const(0, &mut section_bytes); // offset
    section_bytes.push(0x0B); // end of offset expression
    for i in 0..n_funs {
        encode_u32_uleb128(i, &mut section_bytes);
    }

    encode_u32_uleb128(section_bytes.len() as u32, buf);
    buf.extend_from_slice(&section_bytes);
}

pub fn encode_result_type(ty: &[Ty], buf: &mut Vec<u8>) {
    encode_vec(ty, &mut |ty, buf| encode_ty(*ty, buf), buf);
}

pub fn encode_ty(ty: Ty, buf: &mut Vec<u8>) {
    let byte = match ty {
        Ty::I32 => 0x7F,
        Ty::I64 => 0x7E,
        Ty::F32 => 0x7D,
        Ty::F64 => 0x7C,
    };
    buf.push(byte);
}

pub fn encode_u32_uleb128(mut value: u32, buf: &mut Vec<u8>) {
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

pub fn encode_i64_sleb128(mut value: i64, buf: &mut Vec<u8>) {
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
