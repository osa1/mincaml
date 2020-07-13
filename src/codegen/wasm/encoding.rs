use super::instr;
use super::{codegen::WasmFun, rep_type_to_wasm, types::*, WasmCtx};
use crate::{
    cg_types::RepType,
    ctx::{Ctx, TypeId, VarId},
};
use fxhash::FxHashMap;

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

pub(super) fn encode_import_section(
    ctx: &Ctx, fun_tys: &FxHashMap<FunTy, TypeIdx>, buf: &mut Vec<u8>,
) {
    let mut vec_bytes = vec![];
    encode_u32_uleb128(ctx.builtins().len() as u32, &mut vec_bytes);

    for (builtin_var, builtin_ty) in ctx.builtins() {
        encode_name("rts", &mut vec_bytes);
        let var = ctx.get_var(*builtin_var);
        encode_name(&*var.symbol_name(), &mut vec_bytes);
        vec_bytes.push(0x00); // function import

        let fun_ty = type_to_closure_type(ctx, *builtin_var, *builtin_ty);

        let ty_idx = fun_tys.get(&fun_ty).unwrap();
        encode_u32_uleb128(ty_idx.0, &mut vec_bytes);
    }

    buf.push(2);
    encode_u32_uleb128(vec_bytes.len() as u32, buf);
    buf.extend_from_slice(&vec_bytes);
}

pub fn encode_function_section(n_imported: usize, ty_indices: &[TypeIdx], buf: &mut Vec<u8>) {
    let mut vec_bytes = vec![];
    let ty_indices = &ty_indices[n_imported..]; // skip imported functions
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
    let mut section_bytes = vec![];

    encode_u32_uleb128(1, &mut section_bytes); // vec size
    section_bytes.push(0); // tableidx
    instr::i32_const(0, &mut section_bytes); // offset
    section_bytes.push(0x0B); // end of offset expression

    encode_u32_uleb128(n_funs, &mut section_bytes);
    for i in 0..n_funs {
        encode_u32_uleb128(i, &mut section_bytes);
    }

    buf.push(9);
    encode_u32_uleb128(section_bytes.len() as u32, buf);
    buf.extend_from_slice(&section_bytes);
}

pub fn encode_code_section(funs: &[WasmFun], buf: &mut Vec<u8>) {
    // TODO: lots of reudundant copying below, fix later

    // section(vec(code))

    let mut code_: Vec<Vec<u8>> = Vec::with_capacity(funs.len());
    let mut section_size = 0;

    let mut vec_encoding: Vec<u8> = vec![];
    encode_u32_uleb128(funs.len() as u32, &mut vec_encoding);
    section_size += vec_encoding.len();

    for fun in funs {
        // vec(locals)
        let mut locals_bytes = vec![];
        encode_vec(
            &fun.locals,
            &mut |ty, buf| {
                encode_u32_uleb128(1, buf);
                encode_ty(*ty, buf);
            },
            &mut locals_bytes,
        );

        let code_size = (locals_bytes.len() + fun.code.len()) as u32;

        let mut code = vec![];
        encode_u32_uleb128(code_size, &mut code);
        code.extend_from_slice(&locals_bytes);
        code.extend_from_slice(&fun.code);

        section_size += code.len();
        code_.push(code);
    }

    buf.push(10);
    encode_u32_uleb128(section_size as u32, buf);
    buf.extend_from_slice(&vec_encoding);
    for code in code_ {
        buf.extend_from_slice(&code);
    }
}

pub fn encode_result_type(ty: &[Ty], buf: &mut Vec<u8>) {
    encode_vec(ty, &mut |ty, buf| encode_ty(*ty, buf), buf);
}

pub fn encode_ty(ty: Ty, buf: &mut Vec<u8>) {
    let byte = match ty {
        // Ty::I32 => 0x7F,
        Ty::I64 => 0x7E,
        // Ty::F32 => 0x7D,
        Ty::F64 => 0x7C,
    };
    buf.push(byte);
}

pub fn encode_name(name: &str, buf: &mut Vec<u8>) {
    encode_u32_uleb128(name.len() as u32, buf);
    buf.extend_from_slice(name.as_bytes());
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
