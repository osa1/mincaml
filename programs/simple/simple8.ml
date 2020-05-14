(* Parsing of this program was wrong at some point: the semicolon actually has
 * less precedence than if-then-else, so the function body has two statements:
 * if-then-else and print_int. See also simple9. *)

let rec f x =
    if x = 0 then
        ()
    else
        f (x - 1); print_int x (* print_int is not part of the else branch! *)
in
f 5
