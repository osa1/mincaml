// NOTE: globals 'hp' and 'hp_lim' are initialized in `encoding::encode_function_section`

use super::instr::*;
use super::types::*;

pub const HP_IDX: GlobalIdx = GlobalIdx(0);
pub const HP_LIM_IDX: GlobalIdx = GlobalIdx(1);

/// Allocates `n` bytes, returns pointer to allocation.
pub fn gen_alloc(n: u32, buf: &mut Vec<u8>) {
    // HP_IDX + n > HP_LIM
    global_get(HP_IDX, buf);
    i32_const(n as i32, buf);
    i32_add(buf);

    global_get(HP_LIM_IDX, buf);
    i32_gt_u(buf);

    buf.push(0x04); // if
    buf.push(0x7F); // block type = i32 (TODO: is this return type of args from stack?)

    // Then branch: allocate
    i32_const(1, buf);
    memory_grow(buf);

    buf.push(0x05); // else
                    // Bump hp, return old value
    global_get(HP_IDX, buf); // old value
    global_get(HP_IDX, buf);
    i32_const(n as i32, buf);
    i32_add(buf);
    global_set(HP_IDX, buf);
}
