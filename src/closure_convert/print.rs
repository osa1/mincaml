use crate::closure_convert::{Expr, Fun};
use crate::common::BinOp;
use crate::ctx::Ctx;

use std::fmt;

impl Expr {
    pub fn pp(&self, ctx: &Ctx, indent: usize, w: &mut dyn fmt::Write) -> fmt::Result {
        match self {
            Expr::Unit => write!(w, "{}()", indent_str(indent)),

            Expr::Int(i) => write!(w, "{}{}", indent_str(indent), i),

            Expr::Float(f) => write!(w, "{}{}", indent_str(indent), f),

            Expr::IBinOp(BinOp { op, arg1, arg2 }) => {
                let arg1 = ctx.get_var(*arg1);
                let arg2 = ctx.get_var(*arg2);
                write!(w, "{}{} {} {}", indent_str(indent), arg1, op.as_str(), arg2)
            }

            Expr::FBinOp(BinOp { op, arg1, arg2 }) => {
                let arg1 = ctx.get_var(*arg1);
                let arg2 = ctx.get_var(*arg2);
                write!(w, "{}{} {} {}", indent_str(indent), arg1, op.as_str(), arg2)
            }

            Expr::Neg(var) => write!(w, "{}-{}", indent_str(indent), ctx.get_var(*var)),

            Expr::FNeg(var) => write!(w, "{}-.{}", indent_str(indent), ctx.get_var(*var)),

            Expr::If(var1, var2, cmp, then_, else_) => {
                let var1 = ctx.get_var(*var1);
                let var2 = ctx.get_var(*var2);
                writeln!(
                    w,
                    "{}if {} {} {} then",
                    indent_str(indent),
                    var1,
                    cmp.as_str(),
                    var2
                )?;
                then_.pp(ctx, indent + 2, w)?;
                writeln!(w)?;
                else_.pp(ctx, indent + 2, w)
            }

            Expr::Let { id, rhs, body } => {
                let id = ctx.get_var(*id);
                write!(w, "{}let {} = ", indent_str(indent), id)?;
                rhs.pp(ctx, 0, w)?; // FIXME
                writeln!(w, " in")?;
                body.pp(ctx, indent, w)
            }

            Expr::Var(var) => {
                let var = ctx.get_var(*var);
                write!(w, "{}{}", indent_str(indent), var)
            }

            Expr::App(fun, args, _) => {
                let fun = ctx.get_var(*fun);
                write!(w, "{}{}", indent_str(indent), fun)?;
                for arg in args {
                    write!(w, " {}", ctx.get_var(*arg))?;
                }
                Ok(())
            }

            Expr::Tuple(vars) => {
                write!(w, "{}(", indent_str(indent))?;
                for (var_idx, var) in vars.iter().enumerate() {
                    write!(w, "{}", ctx.get_var(*var))?;
                    if var_idx != vars.len() - 1 {
                        write!(w, ", ")?;
                    }
                }
                write!(w, ")")
            }

            Expr::TupleGet(var, idx, _) => {
                write!(w, "{}{}.{}", indent_str(indent), ctx.get_var(*var), idx)
            }

            Expr::ArrayAlloc { len, elem } => write!(
                w,
                "{}Array.create {} {}",
                indent_str(indent),
                ctx.get_var(*len),
                ctx.get_var(*elem)
            ),

            Expr::ArrayGet(arr, idx) => write!(
                w,
                "{}{}[{}]",
                indent_str(indent),
                ctx.get_var(*arr),
                ctx.get_var(*idx)
            ),

            Expr::ArrayPut(arr, idx, val) => write!(
                w,
                "{}{}[{}] = {}",
                indent_str(indent),
                ctx.get_var(*arr),
                ctx.get_var(*idx),
                ctx.get_var(*val)
            ),
        }
    }
}

impl Fun {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        let Fun { name, args, body, return_type: _ } = self;

        write!(w, "let rec {} ", ctx.get_var(*name))?;
        for arg in args {
            write!(w, "{} ", ctx.get_var(*arg))?;
        }
        writeln!(w, "=")?;
        body.pp(ctx, 2, w)
    }
}

static SPACES: &str = "                                   ";

fn indent_str(i: usize) -> &'static str {
    &SPACES[0..i]
}
