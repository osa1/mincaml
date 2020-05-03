use crate::ctx::{Ctx, VarId};
use crate::knormal;
use crate::knormal::{BinOp, FloatBinOp, IntBinOp};
use crate::type_check::Type;
use crate::var::CompilerPhase::ClosureConvert;

use fxhash::FxHashSet;

// Same with knormal's Expr type, except this doesn't have LetRec
#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    Int(u64),
    Float(f64),
    IBinOp(BinOp<IntBinOp>),
    FBinOp(BinOp<FloatBinOp>),
    Neg(VarId),
    FNeg(VarId),
    IfEq(VarId, VarId, Box<Expr>, Box<Expr>),
    IfLE(VarId, VarId, Box<Expr>, Box<Expr>),
    Let {
        id: VarId,
        ty: Type,
        rhs: Box<Expr>,
        body: Box<Expr>,
    },
    Var(VarId),
    App(VarId, Vec<VarId>),
    // A C call
    ExtApp(String, Vec<VarId>),
    Tuple(Vec<VarId>),
    TupleIdx(VarId, usize),
    Get(VarId, VarId),
    Put(VarId, VarId, VarId),
}

// A function. No free variables.
#[derive(Debug)]
pub struct Fun {
    pub name: VarId,
    pub ty: Type,
    pub args: Vec<VarId>,
    pub body: Expr,
}

pub fn closure_convert(ctx: &mut Ctx, expr: knormal::Expr) -> (Vec<Fun>, Expr) {
    let mut funs = vec![];
    let e = closure_convert_(ctx, &mut funs, expr);
    (funs, e)
}

fn closure_convert_(ctx: &mut Ctx, funs: &mut Vec<Fun>, expr: knormal::Expr) -> Expr {
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
            let then_ = closure_convert_(ctx, funs, *then_);
            let else_ = closure_convert_(ctx, funs, *else_);
            Expr::IfEq(e1, e2, Box::new(then_), Box::new(else_))
        }
        knormal::Expr::IfLE(e1, e2, then_, else_) => {
            let then_ = closure_convert_(ctx, funs, *then_);
            let else_ = closure_convert_(ctx, funs, *else_);
            Expr::IfLE(e1, e2, Box::new(then_), Box::new(else_))
        }
        knormal::Expr::Let { id, ty, rhs, body } => {
            let rhs = closure_convert_(ctx, funs, *rhs);
            let body = closure_convert_(ctx, funs, *body);
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
            let fun_binder = ctx.fresh_generated_var(ClosureConvert);
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
            let mut rhs_fvs = Default::default();
            fvs(&rhs, &mut rhs_fvs);
            for arg in &args {
                rhs_fvs.remove(arg);
            }

            // println!("fvs: {:?}", fvs);

            // Emit function
            let closure_arg = ctx.fresh_generated_var(ClosureConvert);
            args.insert(0, closure_arg.clone());

            let rhs = closure_convert_(ctx, funs, *rhs);
            let rhs = rhs_fvs.iter().enumerate().fold(rhs, |e, (fv_idx, fv)| {
                Expr::Let {
                    id: fv.clone(),
                    ty: Type::Int, /* FIXME */
                    rhs: Box::new(Expr::TupleIdx(closure_arg.clone(), fv_idx + 1)),
                    body: Box::new(e),
                }
            });

            funs.push(Fun {
                name: name.clone(),
                ty: Type::Int, // FIXME
                args,
                body: rhs,
            });

            // Allocate closure
            let body = closure_convert_(ctx, funs, *body);
            let mut closure_tuple = vec![name.clone()];
            for fv in rhs_fvs.into_iter() {
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

use pretty::RcDoc;

// TODO: Overly-restrictive lifetimes below. RcDoc shouldn't really borrow anything from Ctx

fn ppr_id(ctx: &Ctx, id: VarId) -> RcDoc<()> {
    RcDoc::text(format!("{}", ctx.get_var(id)))
}

impl Expr {
    pub fn pprint<'a>(&'a self, ctx: &'a Ctx) -> RcDoc<'a, ()> {
        use Expr::*;
        match self {
            Unit => RcDoc::text("()"),

            Int(i) => RcDoc::text(format!("{}", i)),

            Float(f) => RcDoc::text(format!("{}", f)),

            IBinOp(BinOp { op, arg1, arg2 }) => ppr_id(ctx, *arg1)
                .append(RcDoc::space())
                .append(op.pprint())
                .append(RcDoc::space())
                .append(ppr_id(ctx, *arg2)),

            FBinOp(BinOp { op, arg1, arg2 }) => ppr_id(ctx, *arg1)
                .append(RcDoc::space())
                .append(op.pprint())
                .append(RcDoc::space())
                .append(ppr_id(ctx, *arg2)),

            Neg(arg) => RcDoc::text("-").append(ppr_id(ctx, *arg)),

            FNeg(arg) => RcDoc::text("-.").append(ppr_id(ctx, *arg)),

            IfEq(arg1, arg2, then_, else_) => RcDoc::text("if")
                .append(RcDoc::space())
                .append(ppr_id(ctx, *arg1))
                .append(RcDoc::space())
                .append(RcDoc::text("="))
                .append(RcDoc::space())
                .append(ppr_id(ctx, *arg2))
                .append(RcDoc::space())
                .append(RcDoc::text("then"))
                .append(RcDoc::line().append(then_.pprint(ctx)).nest(4))
                .append(RcDoc::line())
                .append(RcDoc::text("else"))
                .append(RcDoc::line().append(else_.pprint(ctx)).nest(4)),

            IfLE(arg1, arg2, then_, else_) => RcDoc::text("if")
                .append(RcDoc::space())
                .append(ppr_id(ctx, *arg1))
                .append(RcDoc::space())
                .append(RcDoc::text("<="))
                .append(RcDoc::space())
                .append(ppr_id(ctx, *arg2))
                .append(RcDoc::space())
                .append(RcDoc::text("then"))
                .append(RcDoc::line().append(then_.pprint(ctx)).nest(4))
                .append(RcDoc::line())
                .append(RcDoc::text("else"))
                .append(RcDoc::line().append(else_.pprint(ctx)).nest(4)),

            Let {
                id,
                ty: _,
                rhs,
                body,
            } => RcDoc::text("let")
                .append(RcDoc::space())
                .append(ppr_id(ctx, *id))
                .append(RcDoc::space())
                .append(RcDoc::text("="))
                .append(RcDoc::line().append(rhs.pprint(ctx)).nest(4))
                .append(RcDoc::line())
                .append(RcDoc::text("in"))
                .append(RcDoc::line().append(body.pprint(ctx)).nest(4)),

            Var(id) => ppr_id(ctx, *id),

            ExtApp(_fun, _args) => todo!(),

            App(fun, args) => ppr_id(ctx, *fun)
                .append(RcDoc::space())
                .append(RcDoc::intersperse(
                    args.iter().map(|i| ppr_id(ctx, *i)),
                    RcDoc::space(),
                )),

            Tuple(args) => RcDoc::text("(")
                .append(RcDoc::intersperse(
                    args.iter().map(|i| ppr_id(ctx, *i)),
                    RcDoc::text(", "),
                ))
                .append(")"),

            TupleIdx(tup, idx) => ppr_id(ctx, *tup).append(RcDoc::text(format!(".({})", idx))),

            Get(arg1, arg2) => ppr_id(ctx, *arg1)
                .append(RcDoc::text(".("))
                .append(ppr_id(ctx, *arg2))
                .append(RcDoc::text(")")),

            Put(arg1, arg2, arg3) => ppr_id(ctx, *arg1)
                .append(RcDoc::text(".("))
                .append(ppr_id(ctx, *arg2))
                .append(RcDoc::text(")"))
                .append(RcDoc::space())
                .append(RcDoc::text("<-"))
                .append(RcDoc::space())
                .append(ppr_id(ctx, *arg3)),
        }
    }
}

impl Fun {
    pub fn pprint<'a>(&'a self, ctx: &'a Ctx) -> RcDoc<'a, ()> {
        let Fun {
            name,
            ty: _,
            args,
            body,
        } = self;
        RcDoc::text("let rec")
            .append(RcDoc::space())
            .append(ppr_id(ctx, *name))
            .append(RcDoc::space())
            .append(RcDoc::intersperse(
                args.iter().map(|i| ppr_id(ctx, *i)),
                RcDoc::space(),
            ))
            .append(RcDoc::space())
            .append(RcDoc::text("="))
            .append(RcDoc::hardline().append(body.pprint(ctx)).nest(4))
    }
}

fn fvs(e: &knormal::Expr, acc: &mut FxHashSet<VarId>) {
    use knormal::Expr::*;
    match e {
        Unit | Int(_) | Float(_) => {}
        IBinOp(BinOp { arg1, arg2, op: _ }) => {
            acc.insert(arg1.clone());
            acc.insert(arg2.clone());
        }
        FBinOp(BinOp { arg1, arg2, op: _ }) => {
            acc.insert(arg1.clone());
            acc.insert(arg2.clone());
        }
        Neg(arg) | FNeg(arg) => {
            acc.insert(arg.clone());
        }
        IfEq(arg1, arg2, e1, e2) | IfLE(arg1, arg2, e1, e2) => {
            acc.insert(arg1.clone());
            acc.insert(arg2.clone());
            fvs(e1, acc);
            fvs(e2, acc);
        }
        Let {
            id,
            ty: _,
            rhs,
            body,
        } => {
            fvs(rhs, acc);
            fvs(body, acc);
            acc.remove(id);
        }
        Var(id) => {
            acc.insert(id.clone());
        }
        LetRec {
            name,
            ty: _,
            args,
            rhs,
            body,
        } => {
            fvs(rhs, acc);
            fvs(body, acc);
            acc.remove(name);
            for arg in args {
                acc.remove(arg);
            }
        }
        App(fun, args) => {
            acc.insert(fun.clone());
            for arg in args {
                acc.insert(arg.clone());
            }
        }
        ExtApp(_, args) | Tuple(args) => {
            for arg in args {
                acc.insert(arg.clone());
            }
        }
        TupleIdx(arg, _) => {
            acc.insert(arg.clone());
        }
        Get(arg1, arg2) => {
            acc.insert(arg1.clone());
            acc.insert(arg2.clone());
        }
        Put(arg1, arg2, arg3) => {
            acc.insert(arg1.clone());
            acc.insert(arg2.clone());
            acc.insert(arg3.clone());
        }
    }
}
