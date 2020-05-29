use super::block::Block;
use super::fun::Fun;
use super::instr::{Instr, InstrKind, Value};

use crate::common::*;
use crate::ctx::{Ctx, VarId};

use std::fmt;

fn print_comma_sep<A>(
    ctx: &Ctx, stuffs: &mut dyn Iterator<Item = &A>,
    show_stuff: fn(&A, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result, w: &mut dyn fmt::Write,
) -> Result<(), fmt::Error> {
    let mut add_comma = false;
    for stuff in stuffs {
        if add_comma {
            w.write_str(", ")?;
        } else {
            add_comma = true;
        }
        show_stuff(stuff, ctx, w)?;
    }
    Ok(())
}

fn pp_id(ctx: &Ctx, id: VarId, w: &mut dyn fmt::Write) -> fmt::Result {
    write!(w, "{}", ctx.get_var(id))
}

fn pp_id_ref(id: &VarId, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
    write!(w, "{}", ctx.get_var(*id))
}

impl Fun {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        let Fun {
            name,
            args,
            blocks,
            instrs,
            cfg,
            return_type,
        } = self;

        w.write_str("function ")?;
        pp_id(ctx, *name, w)?;
        w.write_str("(")?;
        print_comma_sep(ctx, &mut args.iter(), pp_id_ref, w)?;
        writeln!(w, ") -> {}", return_type)?;

        writeln!(w, "CFG: {:?}", cfg)?;

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
            last_instr: _,
            terminated: _,
        } = self;
        write!(w, "{}:", idx)?;
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
            writeln!(w, "    ${} = ", idx)?;
            kind.pp(ctx, w)?;
            writeln!(w)?;

            if *next == instr_idx {
                break;
            }

            instr_idx = *next;
        }

        Ok(())
    }
}

fn pp_vals(ctx: &Ctx, v1: &Value, v2: &Value, w: &mut dyn fmt::Write) -> fmt::Result {
    v1.pp(ctx, w)?;
    w.write_str(", ")?;
    v2.pp(ctx, w)
}

impl InstrKind {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        use InstrKind::*;
        match self {
            Mov(from, to) => {
                w.write_str("mov ")?;
                pp_vals(ctx, from, to, w)
            }
            IImm(v) => write!(w, "iimm {}", v),
            FImm(v) => write!(w, "fimm {}", v),
            IAdd(v1, v2) => {
                w.write_str("iadd ")?;
                pp_vals(ctx, v1, v2, w)
            }
            ISub(v1, v2) => {
                w.write_str("isub ")?;
                pp_vals(ctx, v1, v2, w)
            }
            FAdd(v1, v2) => {
                w.write_str("fadd ")?;
                pp_vals(ctx, v1, v2, w)
            }
            FSub(v1, v2) => {
                w.write_str("fsub ")?;
                pp_vals(ctx, v1, v2, w)
            }
            FMul(v1, v2) => {
                w.write_str("fmul ")?;
                pp_vals(ctx, v1, v2, w)
            }
            FDiv(v1, v2) => {
                w.write_str("fdiv ")?;
                pp_vals(ctx, v1, v2, w)
            }
            Neg(v) => {
                w.write_str("neg ")?;
                v.pp(ctx, w)
            }
            FNeg(v) => {
                w.write_str("fneg ")?;
                v.pp(ctx, w)
            }
            Call(f, args, ret_ty) => {
                w.write_str("fneg ")?;
                f.pp(ctx, w)?;
                w.write_char('(')?;
                print_comma_sep(ctx, &mut args.iter(), Value::pp, w)?;
                write!(w, ") -> {}", ret_ty)
            }
            Tuple { len } => write!(w, "tuple(len={})", len),
            TupleGet(tuple, idx) => {
                w.write_str("tuple ")?;
                tuple.pp(ctx, w)?;
                write!(w, "[{}]", idx)
            }
            TuplePut(tuple, idx, v) => {
                w.write_str("tuple ")?;
                tuple.pp(ctx, w)?;
                write!(w, "[{}] = ", idx)?;
                v.pp(ctx, w)
            }
            ArrayAlloc { len } => {
                w.write_str("array(len=")?;
                len.pp(ctx, w)?;
                w.write_char(')')
            }
            ArrayGet(array, idx) => {
                w.write_str("array ")?;
                array.pp(ctx, w)?;
                w.write_char('[')?;
                idx.pp(ctx, w)?;
                w.write_char(']')
            }
            ArrayPut(array, idx, v) => {
                w.write_str("array ")?;
                array.pp(ctx, w)?;
                w.write_char('[')?;
                idx.pp(ctx, w)?;
                w.write_str("] = ")?;
                v.pp(ctx, w)
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
                v1.pp(ctx, w)?;
                write!(w, " {} ", cond)?;
                v2.pp(ctx, w)?;
                write!(w, "then jmp {} else jmp {}", then_target, else_target)
            }
            Return(v) => {
                w.write_str("ret ")?;
                v.pp(ctx, w)
            }
        }
    }
}

impl Value {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        use Value::*;
        match self {
            Arg(idx) => write!(w, "$arg{}", idx),
            Instr(idx) => write!(w, "${}", idx),
        }
    }
}
