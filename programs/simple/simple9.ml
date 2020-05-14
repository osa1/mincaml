let rec f x =
    if x = 0 then
        ()
    else
        (f (x - 1); print_int x)
in
f 5
