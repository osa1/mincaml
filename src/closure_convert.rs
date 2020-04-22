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
use crate::knormal::{BinOp, Binder, BinderOrUnit, FloatBinOp, Id, IntBinOp};
use crate::locals::Locals;
use crate::type_check::Type;

use std::collections::{HashMap, HashSet};

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
    LetTuple(Vec<(Id, Type)>, Id, Box<Expr>),
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
}

fn fvs(e: &knormal::Expr, acc: &mut HashSet<Id>) {
    use knormal::Expr::*;
    match e {
        Unit | Int(_) | Float(_) => {}
        IBinOp(BinOp { arg1, arg2, .. }) | FBinOp(BinOp { arg1, arg2, .. }) | Get(arg1, arg2) => {
            acc.insert(arg1.clone());
            acc.insert(arg2.clone());
        }
        Neg(arg) | FNeg(arg) => {
            acc.insert(arg.clone());
        }
        IfEq(arg1, arg2, expr1, expr2) | IfLE(arg1, arg2, expr1, expr2) => {
            acc.insert(arg1.clone());
            acc.insert(arg2.clone());
            fvs(expr1, acc);
            fvs(expr2, acc);
        }
        Let {
            id,
            ty: _,
            rhs,
            body,
        } => {
            fvs(body, acc);
            acc.remove(id);
            fvs(rhs, acc);
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
            letrec_fvs(name, args, &**rhs, &**body, acc);
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
        LetTuple(binders, rhs, body) => {
            acc.insert(rhs.clone());
            fvs(body, acc);
            for (binder, _) in binders {
                acc.remove(binder);
            }
        }
        Put(arg1, arg2, arg3) => {
            acc.insert(arg1.clone());
            acc.insert(arg2.clone());
            acc.insert(arg3.clone());
        }
    }
}

fn letrec_fvs(
    name: &Id,
    args: &[BinderOrUnit],
    rhs: &knormal::Expr,
    body: &knormal::Expr,
    acc: &mut HashSet<Id>,
) {
    fvs(rhs, acc);
    for arg in args {
        match arg {
            BinderOrUnit::Unit => {}
            BinderOrUnit::Binder(Binder { binder, ty: _ }) => {
                acc.remove(binder);
            }
        }
    }
    fvs(body, acc);
    acc.remove(name);
}

fn mk_closure_id(id: &Id) -> Id {
    format!("{}.closure", id)
}

fn mk_arg_expr(idx: usize) -> Expr {
    todo!()
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

            knormal::Expr::Var(id) => {
                // If this variable for a closure argument or free variable then we replace it with
                // tuple access
                todo!()
                // match self.exprs.get(&id) {
                //     None => Expr::Var(id),
                //     Some(expr) => expr.clone(),
                // }
            }

            knormal::Expr::LetRec {
                name,
                ty,
                args,
                rhs,
                body,
            } => {
                let mut closure_fvs = HashSet::new();
                letrec_fvs(&name, &args, &*rhs, &*body, &mut closure_fvs);

                // * Allocate closure object (tuple) in a let in `body`, bind it to the name of the
                //   letrec.
                // * Introduce a LetTuple in `rhs` to bind captured variables.
                // * Emit a Fun binding for the function
                //
                // In application site we'll replace `f x` with `f[0] f x`

                todo!()
            }

            knormal::Expr::App(fun, args) => todo!(),

            knormal::Expr::ExtApp(fun, args) => Expr::ExtApp(fun, args),
            knormal::Expr::Tuple(args) => Expr::Tuple(args),
            knormal::Expr::LetTuple(bndrs, id, body) => todo!(),
            knormal::Expr::Get(e1, e2) => Expr::Get(e1, e2),
            knormal::Expr::Put(e1, e2, e3) => Expr::Put(e1, e2, e3),
        }
    }
}
