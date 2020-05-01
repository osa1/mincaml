// Instruction selection
//
// Converts closure-converted program to the x86_64 subset, using virtual registers as instruction
// arguments.
//
// Integer binary operations
// -------------------------
//
// let x = a + b -> movq a, x; addq b, x;
// let x = a - b -> movq a, x; subq b, x;
//
// Integer unary operations
// ------------------------
//
// let x = - y -> movq y x; negq x:
//
// Floats
// ------
//
// - MOVSD — Move or Merge Scalar Double-Precision Floating-Point Value
// - UCOMISD — Unordered Compare Scalar Double-Precision Floating-Point Values and Set EFLAGS
//
// Conditionals
// ------------
//
// if x == y then e1 else e2 (when x is integer) ->
//      cmpq x y;
//      je e1;
//    e2:
//      ...
//    e1:
//      ...
//
//
// if x <= y then e1 else e2 (when x is integer) ->
//       cmpq x y;
//       jle e1;
//    e2:
//       ...
//    e1:
//       ...
