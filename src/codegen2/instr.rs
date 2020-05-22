use super::types::*;
use crate::closure_convert::Label;
use crate::ctx::*;

#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub(crate) enum WordReg {
    RAX, RCX, RDX, RBX, RSI, RDI, RSP, RBP, R8, R9, R10, R11, R12, R13, R14, R15,
}

#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub(crate) enum DoubleReg {
    XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(crate) enum Arg {
    Var(VarIdx),
    Imm(i64),
    Mem(Label),
    WordReg(WordReg),
    DoubleReg(DoubleReg),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) enum Instr {
    Movq { src: Arg, dest: Arg },
    Addq { src: Arg, dest: Arg },
    Subq { src: Arg, dest: Arg },
    Negq { src: Arg },
    // TODO: What's the upper bound on offsets?
    // movq arg1, offset(arg2). arg2 needs to be a Mem after register allocation
    MovqKnownDestOffset { src: Arg, dest: Arg, offset: usize },
    // movq offset(arg1), arg2. arg1 needs to be a Mem after register allocation
    MovqKnownSrcOffset { src: Arg, dest: Arg, offset: usize },
    // movq (%rax,%rdx,8), %rdx
    MovqRelativeSrc { src: Arg, offset: Arg, dest: Arg },
    // movq $123, (%rax,%rdx,8)
    MovqRelativeDest { src: Arg, dest: Arg, offset: Arg },
    // A call instruction. Arguments will be moved to their argument registers in register
    // allocation -- they're not used in the final instruction, which just looks like `call f`.
    Call { fun: Arg, args: Vec<Arg>, dest: Arg },
    // ret instruction doesn't have an operand, the argument is to be used in register allocation
    // to move the return value to the its register according to the calling convention.
    // None is for returning unit. (TODO: Do we really need this special case? Why not return 0?)
    Ret { value: Option<Arg> },
    Jmp { target: BlockIdx },
    // The conditional jump after this will be taken based on whether `arg1 cmp arg2` holds.
    Cmp { arg1: Arg, arg2: Arg },

    // NOTE: We don't need all kinds of conditional jumps to map our 6 comparison ops, but it's
    // easier to debug then the mapping is more direct. So we map our 6 comparison ops to 6 x86
    // instructions.

    // NOTE: These insturctions only have one operand, the "else_" arguments are here to help with
    // CFG generation. When printing instructions we generate e.g. `je label1; jmp label2;`.

    // Jump if arg1 == arg2
    Je { target: BlockIdx, else_: BlockIdx },
    // Jump if arg1 != arg2
    Jne { target: BlockIdx, else_: BlockIdx },
    // Jump if arg1 < arg2
    Jl { target: BlockIdx, else_: BlockIdx },
    // Jump if arg1 <= arg2
    Jle { target: BlockIdx, else_: BlockIdx },
    // Jump if arg1 > arg2
    Jg { target: BlockIdx, else_: BlockIdx },
    // Jump if arg1 >= arg2
    Jge { target: BlockIdx, else_: BlockIdx },
}

pub(crate) struct InstrDefsIter<'a> {
    instr: &'a Instr,
}

pub(crate) struct InstrUsesIter<'a> {
    instr: &'a Instr,
}

impl Instr {
    pub(crate) fn defs(&self) -> &[VarIdx] {
        todo!()
    }

    pub(crate) fn uses(&self) -> &[VarIdx] {
        todo!();
    }

    pub(crate) fn jump_targets(&self) -> &[BlockIdx] {
        todo!()
    }
}
