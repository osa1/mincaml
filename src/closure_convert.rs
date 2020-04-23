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
    // Variables local to LetRecs. On entering a LetRec we push a new entry. On exit we pop.
    locals: Vec<Locals<()>>,
    // Variables free in LetRecs. On entering a LetRec we push a new entry. On exit we pop.
    fvs: Vec<HashSet<Id>>,
    // Used to generate temporaries
    tmp_count: usize,
}

impl ClosureConvert {
    fn new() -> ClosureConvert {
        ClosureConvert {
            funs: vec![],
            locals: vec![],
            fvs: vec![],
            tmp_count: 0,
        }
    }

    fn new_tmp(&mut self) -> Id {
        let tmp = self.tmp_count;
        self.tmp_count += 1;
        format!("__cc.{}", tmp)
    }

    fn new_scope(&mut self) {
        if let Some(locals) = self.locals.last_mut() {
            locals.new_scope();
        }
    }

    fn pop_scope(&mut self) {
        if let Some(locals) = self.locals.last_mut() {
            locals.pop_scope();
        }
    }

    fn enter_letrec(&mut self) {
        self.locals.push(Locals::new(HashMap::new()));
        self.fvs.push(HashSet::new());
    }

    fn exit_letrec(&mut self) -> HashSet<Id> {
        self.locals.pop();
        self.fvs.pop().unwrap()
    }

    fn add_local(&mut self, id: Id) {
        if let Some(locals) = self.locals.last_mut() {
            locals.add(id, ());
        }
    }

    fn is_free(&self, id: &Id) -> bool {
        match self.locals.last() {
            Some(scope) => scope.get(id).is_some(),
            None =>
            // Not in a LetRec
            {
                false
            }
        }
    }

    fn id(&mut self, id: &Id) {
        if self.is_free(id) {
            if let Some(fvs) = self.fvs.last_mut() {
                fvs.insert(id.clone());
            }
        }
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
                self.new_scope();
                self.add_local(id.clone());
                let body = self.closure_convert(*body);
                self.pop_scope();
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
            knormal::Expr::Var(id) => {
                self.id(&id);
                Expr::Var(id)
            }

            knormal::Expr::App(fun, mut args) => {
                self.id(&fun);
                for arg in &args {
                    self.id(arg);
                }

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
                ty,
                args,
                rhs,
                body,
            } => {
                self.enter_letrec();
                self.add_local(name.clone());
                for arg in &args {
                    match arg {
                        BinderOrUnit::Unit => {}
                        BinderOrUnit::Binder(Binder { binder, ty: _ }) => {
                            self.add_local(binder.clone());
                        }
                    }
                }

                let rhs = self.closure_convert(*rhs);
                let fvs = self.exit_letrec();

                todo!()
            }
        }
    }
}
