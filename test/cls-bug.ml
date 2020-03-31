let rec f x = x + 123 in
let rec g y = f in
print_int ((g 456) 789)
