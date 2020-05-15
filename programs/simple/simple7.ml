(* This used to cause a stack overflow in the type checker, but even after
 * fixing it it doesn't quite work, because we don't support any form of
 * polymorphism, so the type `forall a . a -> a` is simply not allowed or
 * supported *)

(* let rec f x = f x in *)
print_int 5
