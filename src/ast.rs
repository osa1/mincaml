use crate::common::*;
use crate::ctx::{Ctx, VarId};
use crate::var::CompilerPhase;

// Expressions after interning
pub type Expr = Expr_<VarId>;

// Expressions after parsing
pub type ParsedExpr = Expr_<String>;

#[derive(Debug, PartialEq)]
pub enum Expr_<I> {
    // ()
    Unit,
    // true, false
    Bool(bool),
    Int(i64),
    Float(f64),
    // not <expr>
    Not(Box<Expr_<I>>),
    // - <expr>
    Neg(Box<Expr_<I>>),
    // '<expr> + <expr>' or '<expr> - <expr>'
    IntBinOp(Box<Expr_<I>>, IntBinOp, Box<Expr_<I>>),
    // -. <expr>
    FNeg(Box<Expr_<I>>),
    // A float binary operation, e.g. '<expr> +. <expr>'
    FloatBinOp(Box<Expr_<I>>, FloatBinOp, Box<Expr_<I>>),
    // Comparison, e.g. <expr> <= <expr>
    Cmp(Box<Expr_<I>>, Cmp, Box<Expr_<I>>),
    // if <expr> then <expr> else <expr>
    If(Box<Expr_<I>>, Box<Expr_<I>>, Box<Expr_<I>>),
    // let <ident> = <expr> in <expr>
    Let {
        bndr: I,
        rhs: Box<Expr_<I>>,
        body: Box<Expr_<I>>,
    },
    // <ident>
    Var(I),
    // let rec <ident> <ident>+ = <expr> in <expr>
    LetRec {
        bndr: I,
        args: Vec<I>,
        rhs: Box<Expr_<I>>,
        body: Box<Expr_<I>>,
    },
    // <expr> <expr>+
    App {
        fun: Box<Expr_<I>>,
        args: Vec<Expr_<I>>,
    },
    // <expr> (, <expr>)+
    Tuple(Vec<Expr_<I>>),
    // let ( <ident> (, <ident>)+ ) = <expr> in <expr>
    LetTuple {
        bndrs: Vec<I>,
        rhs: Box<Expr_<I>>,
        body: Box<Expr_<I>>,
    },
    // Array.create <expr> <expr>
    Array {
        len: Box<Expr_<I>>,
        elem: Box<Expr_<I>>,
    },
    // <expr> . ( <expr> )
    Get(Box<Expr_<I>>, Box<Expr_<I>>),
    // <expr> . ( <expr> ) <- <expr>
    Put(Box<Expr_<I>>, Box<Expr_<I>>, Box<Expr_<I>>),
}

impl ParsedExpr {
    pub fn intern(self, ctx: &mut Ctx) -> Expr {
        match self {
            ParsedExpr::Unit => Expr::Unit,

            ParsedExpr::Bool(b) => Expr::Bool(b),

            ParsedExpr::Int(i) => Expr::Int(i),

            ParsedExpr::Float(f) => Expr::Float(f),

            ParsedExpr::Not(e) => Expr::Not(Box::new(e.intern(ctx))),

            ParsedExpr::Neg(e) => Expr::Neg(Box::new(e.intern(ctx))),

            ParsedExpr::IntBinOp(e1, op, e2) => {
                Expr::IntBinOp(Box::new(e1.intern(ctx)), op, Box::new(e2.intern(ctx)))
            }

            ParsedExpr::FNeg(e) => Expr::FNeg(Box::new(e.intern(ctx))),

            ParsedExpr::FloatBinOp(e1, op, e2) => {
                Expr::FloatBinOp(Box::new(e1.intern(ctx)), op, Box::new(e2.intern(ctx)))
            }

            ParsedExpr::Cmp(e1, op, e2) => {
                Expr::Cmp(Box::new(e1.intern(ctx)), op, Box::new(e2.intern(ctx)))
            }

            ParsedExpr::If(e1, e2, e3) => Expr::If(
                Box::new(e1.intern(ctx)),
                Box::new(e2.intern(ctx)),
                Box::new(e3.intern(ctx)),
            ),

            ParsedExpr::Let { bndr, rhs, body } => Expr::Let {
                bndr: intern(&bndr, ctx),
                rhs: Box::new(rhs.intern(ctx)),
                body: Box::new(body.intern(ctx)),
            },

            ParsedExpr::Var(var) => Expr::Var(intern(&var, ctx)),

            ParsedExpr::LetRec {
                bndr,
                args,
                rhs,
                body,
            } => Expr::LetRec {
                bndr: intern(&bndr, ctx),
                args: args.into_iter().map(|arg| intern(&arg, ctx)).collect(),
                rhs: Box::new(rhs.intern(ctx)),
                body: Box::new(body.intern(ctx)),
            },

            ParsedExpr::App { fun, args } => Expr::App {
                fun: Box::new(fun.intern(ctx)),
                args: args.into_iter().map(|arg| arg.intern(ctx)).collect(),
            },

            ParsedExpr::Tuple(args) => {
                Expr::Tuple(args.into_iter().map(|arg| arg.intern(ctx)).collect())
            }

            ParsedExpr::LetTuple { bndrs, rhs, body } => Expr::LetTuple {
                bndrs: bndrs.into_iter().map(|bndr| intern(&bndr, ctx)).collect(),
                rhs: Box::new(rhs.intern(ctx)),
                body: Box::new(body.intern(ctx)),
            },

            ParsedExpr::Array { len, elem } => Expr::Array {
                len: Box::new(len.intern(ctx)),
                elem: Box::new(elem.intern(ctx)),
            },

            ParsedExpr::Get(e1, e2) => {
                Expr::Get(Box::new(e1.intern(ctx)), Box::new(e2.intern(ctx)))
            }

            ParsedExpr::Put(e1, e2, e3) => Expr::Put(
                Box::new(e1.intern(ctx)),
                Box::new(e2.intern(ctx)),
                Box::new(e3.intern(ctx)),
            ),
        }
    }
}

fn intern(id: &str, ctx: &mut Ctx) -> VarId {
    if id == "_" {
        ctx.fresh_generated_var(CompilerPhase::Parser)
    } else {
        ctx.fresh_user_var(id)
    }
}
