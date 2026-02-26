use crate::closure_convert::{Expr, Fun};
use crate::common::BinOp;
use crate::ctx::Ctx;

use elegance::Printer;

impl Expr {
    pub fn pp(&self, ctx: &Ctx, p: &mut Printer) -> Result<(), std::convert::Infallible> {
        match self {
            Expr::Unit => p.text("()")?,

            Expr::Int(i) => p.text_owned(i.to_string())?,

            Expr::Float(f) => p.text_owned(f.to_string())?,

            Expr::IBinOp(BinOp { op, arg1, arg2 }) => {
                let arg1 = ctx.get_var(*arg1);
                let arg2 = ctx.get_var(*arg2);
                p.text_owned(arg1.to_string())?;
                p.text(" ")?;
                p.text(op.as_str())?;
                p.text(" ")?;
                p.text_owned(arg2.to_string())?;
            }

            Expr::FBinOp(BinOp { op, arg1, arg2 }) => {
                let arg1 = ctx.get_var(*arg1);
                let arg2 = ctx.get_var(*arg2);
                p.text_owned(arg1.to_string())?;
                p.text(" ")?;
                p.text(op.as_str())?;
                p.text(" ")?;
                p.text_owned(arg2.to_string())?;
            }

            Expr::Neg(var) => {
                p.text("-")?;
                p.text_owned(ctx.get_var(*var).to_string())?;
            }

            Expr::FNeg(var) => {
                p.text("-.")?;
                p.text_owned(ctx.get_var(*var).to_string())?;
            }

            Expr::If(var1, var2, cmp, then_, else_) => {
                p.text("if")?;
                p.text(" ")?;
                p.text_owned(ctx.get_var(*var1).to_string())?;
                p.text(" ")?;
                p.text(cmp.as_str())?;
                p.text(" ")?;
                p.text_owned(ctx.get_var(*var2).to_string())?;
                p.text(" ")?;
                p.text("then")?;
                p.igroup(4, |p| {
                    p.hard_break()?;
                    then_.pp(ctx, p)?;
                    Ok(())
                })?;
                p.hard_break()?;
                p.text("else")?;
                p.igroup(4, |p| {
                    p.hard_break()?;
                    else_.pp(ctx, p)?;
                    Ok(())
                })?;
            }

            Expr::Let { id, rhs, body } => {
                let id = ctx.get_var(*id);
                p.text("let")?;
                p.text(" ")?;
                p.text_owned(id.to_string())?;
                p.text(" ")?;
                p.text("=")?;
                p.cgroup(0, |p| {
                    p.cgroup(4, |p| {
                        p.space()?;
                        rhs.pp(ctx, p)?;
                        Ok(())
                    })?;
                    p.space()?;
                    p.text("in")?;
                    Ok(())
                })?;
                p.hard_break()?;
                body.pp(ctx, p)?;
            }

            Expr::Var(var) => {
                p.text_owned(ctx.get_var(*var).to_string())?;
            }

            Expr::App(fun, args, _) => {
                p.text_owned(ctx.get_var(*fun).to_string())?;
                for arg in args {
                    p.text(" ")?;
                    p.text_owned(ctx.get_var(*arg).to_string())?;
                }
            }

            Expr::Tuple(vars) => {
                p.text("(")?;
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        p.text(",")?;
                        p.text(" ")?;
                    }
                    p.text_owned(ctx.get_var(*var).to_string())?;
                }
                p.text(")")?;
            }

            Expr::TupleGet(var, idx, _) => {
                p.text_owned(ctx.get_var(*var).to_string())?;
                p.text(".")?;
                p.text_owned(idx.to_string())?;
            }

            Expr::ArrayAlloc { len, elem } => {
                p.text("Array.create")?;
                p.text(" ")?;
                p.text_owned(ctx.get_var(*len).to_string())?;
                p.text(" ")?;
                p.text_owned(ctx.get_var(*elem).to_string())?;
            }

            Expr::ArrayGet(arr, idx) => {
                p.text_owned(ctx.get_var(*arr).to_string())?;
                p.text("[")?;
                p.text_owned(ctx.get_var(*idx).to_string())?;
                p.text("]")?;
            }

            Expr::ArrayPut(arr, idx, val) => {
                p.text_owned(ctx.get_var(*arr).to_string())?;
                p.text("[")?;
                p.text_owned(ctx.get_var(*idx).to_string())?;
                p.text("]")?;
                p.text(" ")?;
                p.text("=")?;
                p.text(" ")?;
                p.text_owned(ctx.get_var(*val).to_string())?;
            }
        }

        Ok(())
    }
}

impl Fun {
    pub fn pp(&self, ctx: &Ctx) -> String {
        let Fun {
            name,
            args,
            body,
            return_type: _,
        } = self;

        let mut p = Printer::new(String::new(), 80);
        p.text("let rec ").unwrap();
        p.text_owned(ctx.get_var(*name).to_string()).unwrap();
        for arg in args {
            p.space().unwrap();
            p.text_owned(ctx.get_var(*arg).to_string()).unwrap();
        }
        p.space().unwrap();
        p.text("=").unwrap();
        p.igroup(4, |p| {
            p.hard_break()?;
            body.pp(ctx, p)?;
            Ok(())
        })
        .unwrap();
        p.finish().unwrap()
    }
}
