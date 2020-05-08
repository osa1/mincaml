pub fn comma_sep(s: &str) -> String {
    let s_len = s.len();
    let mut ret = String::with_capacity(s_len + s_len / 3);

    let mut count = s_len % 3;
    if count == 0 {
        count = 3;
    }

    for c in s.chars() {
        if count == 0 {
            ret.push(',');
            count = 3;
        }
        ret.push(c);
        count -= 1;
    }

    ret
}

// Copied from take_mut crate
pub fn take<T, F>(mut_ref: &mut T, closure: F)
where
    F: FnOnce(T) -> T,
{
    use std::ptr;

    unsafe {
        let old_t = ptr::read(mut_ref);
        let new_t = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| closure(old_t)))
            .unwrap_or_else(|_| ::std::process::abort());
        ptr::write(mut_ref, new_t);
    }
}

use crate::ctx::{Ctx, VarId};
use std::fmt::Write;

#[allow(dead_code)]
pub fn show_arg_list(ctx: &Ctx, args: &[VarId]) -> String {
    let mut s = String::new();
    s.push('[');
    let mut add_comma = false;
    for arg in args {
        if add_comma {
            s.push_str(", ");
        } else {
            add_comma = true;
        }
        write!(s, "{}", ctx.get_var(*arg)).unwrap();
    }
    s.push(']');
    s
}
