let rec f n i d s =
  if i > n then s else
  f n (i + 1) (d +. 1.0) (s +. 1.0 /. d) in
print_int (int_of_float (1000000.0 *. f 100000000 2 2.0 1.0));
print_newline ()
