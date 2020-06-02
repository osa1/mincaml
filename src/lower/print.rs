use super::block::Block;
use super::fun::Fun;
use super::instr::{Instr, InstrKind, Value, ValueIdx};

use crate::common::*;
use crate::ctx::{Ctx, VarId};

use std::fmt;

pub fn display_id(ctx: &Ctx, id: VarId) -> String {
    format!("{}", ctx.get_var(id))
}

fn print_comma_sep<A>(
    ctx: &Ctx, fun: &Fun, stuffs: &mut dyn Iterator<Item = &A>,
    show_stuff: fn(&A, ctx: &Ctx, fun: &Fun, w: &mut dyn fmt::Write) -> fmt::Result,
    w: &mut dyn fmt::Write,
) -> Result<(), fmt::Error> {
    let mut add_comma = false;
    for stuff in stuffs {
        if add_comma {
            w.write_str(", ")?;
        } else {
            add_comma = true;
        }
        show_stuff(stuff, ctx, fun, w)?;
    }
    Ok(())
}

fn pp_id(ctx: &Ctx, id: VarId, w: &mut dyn fmt::Write) -> fmt::Result {
    write!(w, "{}", ctx.get_var(id))
}

fn pp_id_ref(id: &VarId, ctx: &Ctx, fun: &Fun, w: &mut dyn fmt::Write) -> fmt::Result {
    write!(w, "{}", ctx.get_var(*id))
}

impl Fun {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        let Fun {
            name,
            args,
            blocks,
            instrs,
            preds,
            values,
            phis,
            return_type,
        } = self;

        w.write_str("function ")?;
        pp_id(ctx, *name, w)?;
        w.write_str("(")?;
        print_comma_sep(ctx, self, &mut args.iter(), pp_id_ref, w)?;
        writeln!(w, ") -> {}", return_type)?;

        writeln!(w, "args:   {:?}", args)?;
        writeln!(w, "blocks: {:?}", blocks)?;
        writeln!(w, "instrs: {:?}", instrs)?;
        writeln!(w, "preds:  {:?}", preds)?;
        writeln!(w, "values: {:?}", values)?;
        writeln!(w, "phis:   {:?}", phis)?;

        for block in blocks.values() {
            block.pp(ctx, self, w)?;
        }
        writeln!(w)
    }
}

impl Block {
    pub fn pp(&self, ctx: &Ctx, fun: &Fun, w: &mut dyn fmt::Write) -> fmt::Result {
        let Block {
            idx,
            first_instr,
            last_instr,
            filled: _,
            sealed: _,
        } = self;
        writeln!(w, "{}:", idx)?;
        // match comment {
        //     None => {
        //         writeln!(w)?;
        //     }
        //     Some(comment) => {
        //         writeln!(w, " // {}", comment)?;
        //     }
        // }
        let mut instr_idx = *first_instr;
        loop {
            let Instr {
                idx,
                next,
                prev: _,
                kind,
            } = &fun.instrs[instr_idx];
            write!(w, "    ${} = ", idx)?;
            kind.pp(ctx, fun, w)?;
            writeln!(w)?;

            if *next == *last_instr {
                break;
            }

            instr_idx = *next;
        }

        Ok(())
    }
}

fn pp_vals(
    ctx: &Ctx, fun: &Fun, v1: ValueIdx, v2: ValueIdx, w: &mut dyn fmt::Write,
) -> fmt::Result {
    v1.pp(ctx, fun, w)?;
    w.write_str(", ")?;
    v2.pp(ctx, fun, w)
}

impl Value {
    pub fn pp(&self, ctx: &Ctx, fun: &Fun, w: &mut dyn fmt::Write) -> fmt::Result {
        use Value::*;
        match self {
            Arg(idx) => write!(w, "${}", idx),
            Instr(idx) => write!(w, "${}", idx),
            Phi(idx) => write!(w, "${}", idx),
        }
    }
}

impl ValueIdx {
    pub fn pp(&self, ctx: &Ctx, fun: &Fun, w: &mut dyn fmt::Write) -> fmt::Result {
        fun.values[*self].pp(ctx, fun, w)
    }
}

impl InstrKind {
    pub fn pp(&self, ctx: &Ctx, fun: &Fun, w: &mut dyn fmt::Write) -> fmt::Result {
        use InstrKind::*;
        match self {
            Mov(from, to) => {
                w.write_str("mov ")?;
                pp_vals(ctx, fun, *from, *to, w)
            }
            IImm(v) => write!(w, "iimm {}", v),
            FImm(v) => write!(w, "fimm {}", v),
            IAdd(v1, v2) => {
                w.write_str("iadd ")?;
                pp_vals(ctx, fun, *v1, *v2, w)
            }
            ISub(v1, v2) => {
                w.write_str("isub ")?;
                pp_vals(ctx, fun, *v1, *v2, w)
            }
            FAdd(v1, v2) => {
                w.write_str("fadd ")?;
                pp_vals(ctx, fun, *v1, *v2, w)
            }
            FSub(v1, v2) => {
                w.write_str("fsub ")?;
                pp_vals(ctx, fun, *v1, *v2, w)
            }
            FMul(v1, v2) => {
                w.write_str("fmul ")?;
                pp_vals(ctx, fun, *v1, *v2, w)
            }
            FDiv(v1, v2) => {
                w.write_str("fdiv ")?;
                pp_vals(ctx, fun, *v1, *v2, w)
            }
            Neg(v) => {
                w.write_str("neg ")?;
                v.pp(ctx, fun, w)
            }
            FNeg(v) => {
                w.write_str("fneg ")?;
                v.pp(ctx, fun, w)
            }
            Call(f, args, ret_ty) => {
                w.write_str("fneg ")?;
                f.pp(ctx, fun, w)?;
                w.write_char('(')?;
                print_comma_sep(ctx, fun, &mut args.iter(), ValueIdx::pp, w)?;
                write!(w, ") -> {}", ret_ty)
            }
            Tuple { len } => write!(w, "tuple(len={})", len),
            TupleGet(tuple, idx) => {
                w.write_str("tuple ")?;
                tuple.pp(ctx, fun, w)?;
                write!(w, "[{}]", idx)
            }
            TuplePut(tuple, idx, v) => {
                w.write_str("tuple ")?;
                tuple.pp(ctx, fun, w)?;
                write!(w, "[{}] = ", idx)?;
                v.pp(ctx, fun, w)
            }
            ArrayAlloc { len } => {
                w.write_str("array(len=")?;
                len.pp(ctx, fun, w)?;
                w.write_char(')')
            }
            ArrayGet(array, idx) => {
                w.write_str("array ")?;
                array.pp(ctx, fun, w)?;
                w.write_char('[')?;
                idx.pp(ctx, fun, w)?;
                w.write_char(']')
            }
            ArrayPut(array, idx, v) => {
                w.write_str("array ")?;
                array.pp(ctx, fun, w)?;
                w.write_char('[')?;
                idx.pp(ctx, fun, w)?;
                w.write_str("] = ")?;
                v.pp(ctx, fun, w)
            }
            Jmp(target) => write!(w, "jmp {}", target),
            CondJmp {
                v1,
                v2,
                cond,
                then_target,
                else_target,
            } => {
                w.write_str("if ")?;
                v1.pp(ctx, fun, w)?;
                write!(w, " {} ", cond)?;
                v2.pp(ctx, fun, w)?;
                write!(w, "then jmp {} else jmp {}", then_target, else_target)
            }
            Return(v) => {
                w.write_str("ret ")?;
                v.pp(ctx, fun, w)
            }
        }
    }
}
