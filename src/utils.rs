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
