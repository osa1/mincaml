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
