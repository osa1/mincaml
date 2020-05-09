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
// (Reference: https://www.felixcloutier.com/x86/ucomisd)
//
// Use the same branching instructions.
//
// Moving floats
// -------------
//
// - MOVSD — Move or Merge Scalar Double-Precision Floating-Point Value
//   Example: `movsd %xmm0, -32(%rbp)` to move a double to a struct field.
//            `movsd -56(%rbp), %xmm0` the other way around
//
// Integer arithmetic
// ------------------
//
// - addq/subq
//
// (Reference: gcc -S output)
//
// Float arithmetic
// ----------------
//
// - addsd/subsd/divsd/mulsd
//
// Integer to double example: `cvtsi2sdq %rax, %xmm0` (NOTE: I don't think we need this tho?)
//
// (Reference: gcc -S output)
//
// Registers
// ---------
//
// rsp and rbp are used for stack manipulation. Remaining args are used for parameter passing in
// this order:
//
// - rax, rcx, rdx, rbx, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15 (14 regs)
// - For doubles: xmm0 - xmm7 (TODO: can we not use the rest?)
//
// All registers are callee-save.

#[rustfmt::skip]
enum WordReg {
    RAX, RCX, RDX, RBX, RSI, RDI, RSP, RBP, R8, R9, R10, R11, R12, R13, R14, R15,
}

#[rustfmt::skip]
enum DoubleReg {
    XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7
}
