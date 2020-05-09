#![allow(dead_code)]

// Conditionals
// ------------
//
// Conditionals in the language: >, >=, <, <=, ==, <>
//
// In x86, for words:
//
// - Set EFLGS with cmp
//
// Then
//
// - jg: jump if greater, signed
// - jge: jump if greater or equal, signed
// - jl: jump if less, signed
// - jle: jump if less or equal, signed
// - je: jump if equal
// - jne: jump if not equal
//
// (Reference: http://unixwiz.net/techtips/x86-jumps.html)
//
// For floats:
//
// - Set EFLAGS with ucomisd: "Unordered Compare Scalar Double-Precision Floating-Point Values"
//
// Use the same branching instructions.
//
// Moving floats
// -------------
//
// - MOVSD — Move or Merge Scalar Double-Precision Floating-Point Value
