let rec f n =
  if n < 0 then () else
  (print_int n;
   let a = Array.make 1 f in
   a.(0) (n - 1)) in
f 9
