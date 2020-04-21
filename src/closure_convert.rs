// Closure conversion
//
// The idea is to turn a function definition like
//
//     let rec f x = x + y (* y is free in f *)
//
// into
//
//     let rec f_fun fvs x = x + fvs.(1)
//     let f_clo = (f_fun, y,)
//
// and any applications of f
//
//     f 5
//
// into
//
//    f_clo.(0) f_clo 5
//
// For this we first do one pass over the AST and annotate functions with free variables.

use crate::knormal;
use crate::knormal::{BinOp, BinderOrUnit, FloatBinOp, Id, IntBinOp};
use crate::type_check::Type;

// Same with knormal's Expr type, except this doesn't have LetRec
#[derive(Debug)]
enum Expr {
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
    LetTuple(Vec<(Id, Type)>, Id, Box<Expr>),
    Get(Id, Id),
    Put(Id, Id, Id),
}

// A function. No free variables.
#[derive(Debug)]
struct Fun {
    name: Id,
    ty: Type,
    args: Vec<BinderOrUnit>,
    body: Expr,
}

struct ClosureConvert {
    // Top-level function definitions without free variables
    funs: Vec<Fun>,
}

impl ClosureConvert {
    fn new() -> ClosureConvert {
        ClosureConvert { funs: vec![] }
    }

    fn is_closure(&self, id: &Id) -> bool {
        todo!()
    }

    fn closure_convert(&mut self, expr: knormal::Expr) -> Expr {
        match expr {
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
            knormal::Expr::Var(id) => Expr::Var(id),

            knormal::Expr::LetRec {
                name,
                ty,
                args,
                rhs,
                body,
            } => todo!(),

            knormal::Expr::App(fun, args) => todo!(),

            knormal::Expr::ExtApp(fun, args) => Expr::ExtApp(fun, args),
            knormal::Expr::Tuple(args) => Expr::Tuple(args),
            knormal::Expr::LetTuple(bndrs, id, body) => todo!(),
            knormal::Expr::Get(e1, e2) => Expr::Get(e1, e2),
            knormal::Expr::Put(e1, e2, e3) => Expr::Put(e1, e2, e3),
        }
    }
}
