pub fn encode_vec<A>(stuff: &[A], encode: &mut dyn FnMut(&A, &mut Vec<u8>), buf: &mut Vec<u8>) {
    encode_u32_uleb128(stuff.len() as u32, buf);
    for stuff in stuff {
        encode(stuff, buf);
    }
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
