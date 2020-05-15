let rec h p = 
    let (v1,v2,v3) = p in
    let rec g z =
        let r = v1 + v2 + v3 in r
    in
        g 1
in 
    print_int (h (1,2,3))
