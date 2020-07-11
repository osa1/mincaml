use super::types::*;

pub fn encode_vec<A>(stuff: &[A], encode: &mut dyn FnMut(&A, &mut Vec<u8>), buf: &mut Vec<u8>) {
    encode_u32_uleb128(stuff.len() as u32, buf);
    for stuff in stuff {
        encode(stuff, buf);
    }
}

pub fn encode_type_section(mut fun_tys: Vec<(FunTy, TypeIdx)>, buf: &mut Vec<u8>) {
    fun_tys.sort_by_key(|(_fun_ty, type_idx)| *type_idx);
    let fun_tys: Vec<FunTy> = fun_tys.into_iter().map(|(fun_ty, _)| fun_ty).collect();

    let mut vec_bytes = vec![];
    encode_vec(
        &fun_tys,
        &mut |fun_ty, buf| {
            buf.push(0x60);
            encode_result_type(&fun_ty.args, buf);
            encode_result_type(&vec![fun_ty.ret], buf);
        },
        &mut vec_bytes,
    );

    let mut section_bytes = vec![];
    section_bytes.push(1);
    encode_u32_uleb128(vec_bytes.len() as u32, &mut section_bytes);
    section_bytes.extend_from_slice(&vec_bytes);
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
