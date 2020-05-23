use crate::common::*;
use crate::ctx::{Ctx, VarId};

use super::types::*;

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

impl Fun {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        let Fun {
            name,
            args,
            blocks,
            return_type,
        } = self;

        w.write_str("function ")?;
        pp_id(ctx, *name, w)?;
        w.write_str("(")?;
        print_comma_sep(ctx, &mut args.iter(), pp_id_ref, w)?;
        writeln!(w, ") -> {}", return_type)?;

        for block in blocks {
            block.pp(ctx, w)?;
        }
        writeln!(w)
    }
}

impl Block {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> Result<(), fmt::Error> {
        let Block { label, stmts, exit } = self;
        writeln!(w, "{}:", label)?;
        for asgn in stmts {
            w.write_str("    ")?;
            asgn.pp(ctx, w)?;
            writeln!(w)?;
        }
        w.write_str("    ")?;
        exit.pp(ctx, w)?;
        writeln!(w)
    }
}

impl Exit {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        use Exit::*;
        match self {
            Return(var) => {
                w.write_str("return ")?;
                pp_id(ctx, *var, w)
            }
            Branch {
                v1,
                v2,
                cond,
                then_label,
                else_label,
            } => {
                w.write_str("if ")?;
                pp_id(ctx, *v1, w)?;
                write!(w, " {} ", cond)?;
                pp_id(ctx, *v2, w)?;
                write!(w, " then {} else {}", then_label, else_label)
            }
            Jump(lbl) => write!(w, "jump {}", lbl),
        }
    }
}

impl Asgn {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        let Asgn { lhs, rhs } = self;
        pp_id(ctx, *lhs, w)?;
        w.write_str(": ")?;
        match ctx.var_type_(*lhs) {
            Some(var_type) => {
                var_type.pp(w)?;
            }
            None => {
                w.write_str("???")?;
            }
        }
        w.write_str(" = ")?;
        rhs.pp(ctx, w)
    }
}

impl Expr {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        use Expr::*;
        match self {
            Atom(atom) => atom.pp(ctx, w),
            IBinOp(BinOp { op, arg1, arg2 }) => {
                pp_id(ctx, *arg1, w)?;
                let op_str = match op {
                    IntBinOp::Add => " + ",
                    IntBinOp::Sub => " - ",
                };
                write!(w, "{}", op_str)?;
                pp_id(ctx, *arg2, w)
            }
            FBinOp(BinOp { op, arg1, arg2 }) => {
                pp_id(ctx, *arg1, w)?;
                let op_str = match op {
                    FloatBinOp::Add => " +. ",
                    FloatBinOp::Sub => " -. ",
                    FloatBinOp::Mul => " *. ",
                    FloatBinOp::Div => " /. ",
                };
                write!(w, "{}", op_str)?;
                pp_id(ctx, *arg2, w)
            }
            Neg(var) => {
                w.write_str("-")?;
                pp_id(ctx, *var, w)
            }
            FNeg(var) => {
                w.write_str("-.")?;
                pp_id(ctx, *var, w)
            }
            App(fun, args, _) => {
                pp_id(ctx, *fun, w)?;
                w.write_str("(")?;
                print_comma_sep(ctx, &mut args.iter(), pp_id_ref, w)?;
                w.write_str(")")
            }
            Tuple(args) => {
                w.write_str("(")?;
                print_comma_sep(ctx, &mut args.iter(), pp_id_ref, w)?;
                w.write_str(")")
            }
            TupleGet(tuple, idx) => {
                pp_id(ctx, *tuple, w)?;
                write!(w, ".{}", idx)
            }
            ArrayAlloc { len, elem } => {
                // Printing in Rust syntax here?
                w.write_str("[")?;
                pp_id(ctx, *elem, w)?;
                w.write_str("; ")?;
                pp_id(ctx, *len, w)?;
                w.write_str("]")
            }
            ArrayGet(array, idx) => {
                pp_id(ctx, *array, w)?;
                w.write_str(".(")?;
                pp_id(ctx, *idx, w)?;
                w.write_str(")")
            }
            ArrayPut(array, idx, val) => {
                pp_id(ctx, *array, w)?;
                w.write_str(".(")?;
                pp_id(ctx, *idx, w)?;
                w.write_str(") <- ")?;
                pp_id(ctx, *val, w)
            }
        }
    }
}

impl Atom {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        use Atom::*;
        match self {
            Unit => w.write_str("()"),
            Int(i) => write!(w, "{}", i),
            // Use debug format in floats, otherwise "1.0" is printed as "1"
            Float(f) => write!(w, "{:?}", f),
            Var(var) => pp_id(ctx, *var, w),
        }
    }
}

fn pp_id(ctx: &Ctx, id: VarId, w: &mut dyn fmt::Write) -> fmt::Result {
    write!(w, "{}", ctx.get_var(id))
}

fn pp_id_ref(id: &VarId, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
    write!(w, "{}", ctx.get_var(*id))
}
