use crate::anormal;
use crate::knormal;
pub use crate::knormal::{ppr_id, BinOp, Binder, BinderOrUnit, FloatBinOp, Id, IntBinOp};
use crate::type_check::Type;

use std::collections::HashSet;

// Same with knormal's Expr type, except this doesn't have LetRec
#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    Int(u64),
    Float(f64),
    IBinOp(BinOp<IntBinOp>),
    FBinOp(BinOp<FloatBinOp>),
    Neg(Id),
    FNeg(Id),
    IfEq(Id, Id, Box<Expr>, Box<Expr>),
    IfLE(Id, Id, Box<Expr>, Box<Expr>),
    Let {
        id: Id,
        ty: Type,
        rhs: Box<Expr>,
        body: Box<Expr>,
    },
    Var(Id),
    App(Id, Vec<Id>),
    // A C call
    ExtApp(Id, Vec<Id>),
    Tuple(Vec<Id>),
    TupleIdx(Id, usize),
    Get(Id, Id),
    Put(Id, Id, Id),
}

// A function. No free variables.
#[derive(Debug)]
pub struct Fun {
    pub name: Id,
    pub ty: Type,
    pub args: Vec<BinderOrUnit>,
    pub body: Expr,
}

pub fn closure_convert(expr: knormal::Expr) -> (Vec<Fun>, Expr) {
    let mut cc = ClosureConvert::new();
    let expr = cc.closure_convert(expr);
    (cc.funs, expr)
}

struct ClosureConvert {
    // Top-level function definitions without free variables
    funs: Vec<Fun>,
    // Used to generate temporaries
    tmp_count: usize,
}

impl ClosureConvert {
    fn new() -> ClosureConvert {
        ClosureConvert {
            funs: vec![],
            tmp_count: 0,
        }
    }

    fn new_tmp(&mut self) -> Id {
        let tmp = self.tmp_count;
        self.tmp_count += 1;
        format!("__cc.{}", tmp)
    }

    fn closure_convert(&mut self, expr: knormal::Expr) -> Expr {
        match expr {
            //
            // Boring parts
            //
            knormal::Expr::Unit => Expr::Unit,
            knormal::Expr::Int(i) => Expr::Int(i),
            knormal::Expr::Float(f) => Expr::Float(f),
            knormal::Expr::IBinOp(op) => Expr::IBinOp(op),
            knormal::Expr::FBinOp(op) => Expr::FBinOp(op),
            knormal::Expr::Neg(e) => Expr::Neg(e),
            knormal::Expr::FNeg(e) => Expr::FNeg(e),
            knormal::Expr::IfEq(e1, e2, then_, else_) => {
                let then_ = self.closure_convert(*then_);
                let else_ = self.closure_convert(*else_);
                Expr::IfEq(e1, e2, Box::new(then_), Box::new(else_))
            }
            knormal::Expr::IfLE(e1, e2, then_, else_) => {
                let then_ = self.closure_convert(*then_);
                let else_ = self.closure_convert(*else_);
                Expr::IfLE(e1, e2, Box::new(then_), Box::new(else_))
            }
            knormal::Expr::Let { id, ty, rhs, body } => {
                let rhs = self.closure_convert(*rhs);
                let body = self.closure_convert(*body);
                Expr::Let {
                    id,
                    ty,
                    rhs: Box::new(rhs),
                    body: Box::new(body),
                }
            }
            knormal::Expr::ExtApp(fun, args) => Expr::ExtApp(fun, args),
            knormal::Expr::Tuple(args) => Expr::Tuple(args),
            knormal::Expr::TupleIdx(id, idx) => Expr::TupleIdx(id, idx),
            knormal::Expr::Get(e1, e2) => Expr::Get(e1, e2),
            knormal::Expr::Put(e1, e2, e3) => Expr::Put(e1, e2, e3),

            //
            // Interesting parts
            //
            knormal::Expr::Var(id) => Expr::Var(id),

            knormal::Expr::App(fun, mut args) => {
                let fun_binder = self.new_tmp();
                args.insert(0, fun.clone());
                Expr::Let {
                    id: fun_binder.clone(),
                    ty: Type::Int, // FIXME TODO
                    rhs: Box::new(Expr::TupleIdx(fun, 0)),
                    body: Box::new(Expr::App(fun_binder, args)),
                }
            }

            knormal::Expr::LetRec {
                name,
                ty: _,
                mut args,
                rhs,
                body,
            } => {
                let mut fvs = HashSet::new();
                anormal::fvs(&rhs, &mut fvs);
                for arg in &args {
                    match arg {
                        BinderOrUnit::Binder(Binder { binder, .. }) => {
                            fvs.remove(binder);
                        }
                        BinderOrUnit::Unit => {}
                    }
                }

                // println!("fvs: {:?}", fvs);

                // Emit function
                let closure_arg = self.new_tmp();
                args.insert(
                    0,
                    BinderOrUnit::Binder(Binder {
                        binder: closure_arg.clone(),
                        ty: Type::Int, // FIXME
                    }),
                );

                let rhs = self.closure_convert(*rhs);
                let rhs = fvs.iter().enumerate().fold(rhs, |e, (fv_idx, fv)| {
                    Expr::Let {
                        id: fv.clone(),
                        ty: Type::Int, /* FIXME */
                        rhs: Box::new(Expr::TupleIdx(closure_arg.clone(), fv_idx + 1)),
                        body: Box::new(e),
                    }
                });

                self.funs.push(Fun {
                    name: name.clone(),
                    ty: Type::Int, // FIXME
                    args,
                    body: rhs,
                });

                // Allocate closure
                let body = self.closure_convert(*body);
                let mut closure_tuple = vec![name.clone()];
                for fv in fvs.into_iter() {
                    closure_tuple.push(fv);
                }
                let closure_tuple = Expr::Tuple(closure_tuple);
                Expr::Let {
                    id: name,
                    ty: Type::Int,
                    rhs: Box::new(closure_tuple),
                    body: Box::new(body),
                }
            }
        }
    }
}

use pretty::RcDoc;

impl Expr {
    pub fn pprint(&self) -> RcDoc<()> {
        use Expr::*;
        match self {
            Unit => RcDoc::text("()"),

            Int(i) => RcDoc::text(format!("{}", i)),

            Float(f) => RcDoc::text(format!("{}", f)),

            IBinOp(BinOp { op, arg1, arg2 }) => ppr_id(arg1)
                .append(RcDoc::space())
                .append(op.pprint())
                .append(RcDoc::space())
                .append(ppr_id(arg2)),

            FBinOp(BinOp { op, arg1, arg2 }) => ppr_id(arg1)
                .append(RcDoc::space())
                .append(op.pprint())
                .append(RcDoc::space())
                .append(ppr_id(arg2)),

            Neg(arg) => RcDoc::text("-").append(ppr_id(arg)),

            FNeg(arg) => RcDoc::text("-.").append(ppr_id(arg)),

            IfEq(arg1, arg2, then_, else_) => RcDoc::text("if")
                .append(RcDoc::space())
                .append(ppr_id(arg1))
                .append(RcDoc::space())
                .append(RcDoc::text("="))
                .append(RcDoc::space())
                .append(ppr_id(arg2))
                .append(RcDoc::space())
                .append(RcDoc::text("then"))
                .append(RcDoc::line().append(then_.pprint()).nest(4))
                .append(RcDoc::line())
                .append(RcDoc::text("else"))
                .append(RcDoc::line().append(else_.pprint()).nest(4)),

            IfLE(arg1, arg2, then_, else_) => RcDoc::text("if")
                .append(RcDoc::space())
                .append(ppr_id(arg1))
                .append(RcDoc::space())
                .append(RcDoc::text("<="))
                .append(RcDoc::space())
                .append(ppr_id(arg2))
                .append(RcDoc::space())
                .append(RcDoc::text("then"))
                .append(RcDoc::line().append(then_.pprint()).nest(4))
                .append(RcDoc::line())
                .append(RcDoc::text("else"))
                .append(RcDoc::line().append(else_.pprint()).nest(4)),

            Let {
                id,
                ty: _,
                rhs,
                body,
            } => RcDoc::text("let")
                .append(RcDoc::space())
                .append(ppr_id(id))
                .append(RcDoc::space())
                .append(RcDoc::text("="))
                .append(RcDoc::line().append(rhs.pprint()).nest(4))
                .append(RcDoc::line())
                .append(RcDoc::text("in"))
                .append(RcDoc::line().append(body.pprint()).nest(4)),

            Var(id) => ppr_id(id),

            App(fun, args) | ExtApp(fun, args) => ppr_id(fun)
                .append(RcDoc::space())
                .append(RcDoc::intersperse(args.iter().map(ppr_id), RcDoc::space())),

            Tuple(args) => RcDoc::text("(")
                .append(RcDoc::intersperse(
                    args.iter().map(ppr_id),
                    RcDoc::text(", "),
                ))
                .append(")"),

            TupleIdx(tup, idx) => ppr_id(tup).append(RcDoc::text(format!(".({})", idx))),

            Get(arg1, arg2) => ppr_id(arg1)
                .append(RcDoc::text(".("))
                .append(ppr_id(arg2))
                .append(RcDoc::text(")")),

            Put(arg1, arg2, arg3) => ppr_id(arg1)
                .append(RcDoc::text(".("))
                .append(ppr_id(arg2))
                .append(RcDoc::text(")"))
                .append(RcDoc::space())
                .append(RcDoc::text("<-"))
                .append(RcDoc::space())
                .append(ppr_id(arg3)),
        }
    }
}

impl Fun {
    pub fn pprint(&self) -> RcDoc<()> {
        let Fun {
            name,
            ty: _,
            args,
            body,
        } = self;
        RcDoc::text("let rec")
            .append(RcDoc::space())
            .append(ppr_id(name))
            .append(RcDoc::space())
            .append(RcDoc::intersperse(
                args.iter().map(BinderOrUnit::pprint),
                RcDoc::space(),
            ))
            .append(RcDoc::space())
            .append(RcDoc::text("="))
            .append(RcDoc::hardline().append(body.pprint()).nest(4))
    }
}
