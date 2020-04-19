use crate::locals::Locals;
use crate::parser;
use crate::parser::Binder;
use crate::type_check::Type;

use std::collections::HashMap;

pub type Id = String;

// TODO: Should we use parsre::Binder for the binders below?

#[derive(Debug)]
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
    LetRec {
        name: Id,
        ty: Type,
        args: Vec<(Id, Type)>,
        rhs: Box<Expr>,
        body: Box<Expr>,
    },
    App(Id, Vec<Id>),
    Tuple(Vec<Id>),
    LetTuple(Vec<(Id, Type)>, Id, Box<Expr>),
    Get(Id, Id),
    Put(Id, Id, Id),
}

#[derive(Debug)]
pub struct BinOp<A> {
    op: A,
    arg1: Id,
    arg2: Id,
}

#[derive(Debug)]
pub enum FloatBinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum IntBinOp {
    Add,
    Sub,
}

struct TmpLet {
    id: Id,
    ty: Type,
    rhs: Box<Expr>,
}

impl TmpLet {
    fn finish(self, body: Expr) -> Expr {
        Expr::Let {
            id: self.id,
            ty: self.ty,
            rhs: self.rhs,
            body: Box::new(body),
        }
    }
}

pub struct KNormal<'a> {
    tmp_count: u64,
    bndr_tys: &'a [Option<Type>],
    locals: Locals<Type>,
}

impl<'a> KNormal<'a> {
    pub fn new(bndr_tys: &'a [Option<Type>]) -> KNormal {
        KNormal {
            tmp_count: 0,
            bndr_tys,
            locals: Locals::new(HashMap::new()),
        }
    }

    // Creates a let with body initialized as `()` (Unit). Make sure to update it after normalizing
    // the body!
    fn insert_let(&mut self, rhs: Expr, ty: Type) -> (TmpLet, String) {
        let tmp = self.tmp_count;
        self.tmp_count += 1;
        let id = format!("__t{}", tmp);
        (
            TmpLet {
                id: id.clone(),
                ty: ty,
                rhs: Box::new(rhs),
            },
            id,
        )
    }

    pub fn knormal_(&mut self, expr: parser::Expr) -> Expr {
        self.knormal(expr).0
    }

    pub fn knormal(&mut self, expr: parser::Expr) -> (Expr, Type) {
        match expr {
            parser::Expr::Unit => (Expr::Unit, Type::Unit),
            parser::Expr::Bool(b) => (Expr::Int(if b { 1 } else { 0 }), Type::Int),
            parser::Expr::Int(i) => (Expr::Int(i), Type::Int),
            parser::Expr::Float(f) => (Expr::Float(f), Type::Float),
            parser::Expr::Not(e) => self.knormal(parser::Expr::If(
                e,
                Box::new(parser::Expr::Bool(false)),
                Box::new(parser::Expr::Bool(true)),
            )),
            parser::Expr::Neg(e) => {
                let e = self.knormal_(*e);
                let (tmp, var) = self.insert_let(e, Type::Int);
                (tmp.finish(Expr::Neg(var)), Type::Int)
            }
            parser::Expr::Add(e1, e2) => {
                let e1 = self.knormal_(*e1);
                let (tmp1, var1) = self.insert_let(e1, Type::Int);
                let e2 = self.knormal_(*e2);
                let (tmp2, var2) = self.insert_let(e2, Type::Int);
                let e = tmp1.finish(tmp2.finish(Expr::IBinOp(BinOp {
                    op: IntBinOp::Add,
                    arg1: var1,
                    arg2: var2,
                })));
                (e, Type::Int)
            }
            parser::Expr::Sub(e1, e2) => {
                let e1 = self.knormal_(*e1);
                let (tmp1, var1) = self.insert_let(e1, Type::Int);
                let e2 = self.knormal_(*e2);
                let (tmp2, var2) = self.insert_let(e2, Type::Int);
                let e = tmp1.finish(tmp2.finish(Expr::IBinOp(BinOp {
                    op: IntBinOp::Sub,
                    arg1: var1,
                    arg2: var2,
                })));
                (e, Type::Int)
            }
            parser::Expr::FNeg(e) => {
                let e = self.knormal_(*e);
                let (tmp, var) = self.insert_let(e, Type::Float);
                let e = tmp.finish(Expr::FNeg(var));
                (e, Type::Float)
            }
            parser::Expr::FAdd(e1, e2) => {
                let e1 = self.knormal_(*e1);
                let (tmp1, var1) = self.insert_let(e1, Type::Float);
                let e2 = self.knormal_(*e2);
                let (tmp2, var2) = self.insert_let(e2, Type::Float);
                let e = tmp1.finish(tmp2.finish(Expr::FBinOp(BinOp {
                    op: FloatBinOp::Add,
                    arg1: var1,
                    arg2: var2,
                })));
                (e, Type::Float)
            }
            parser::Expr::FSub(e1, e2) => {
                let e1 = self.knormal_(*e1);
                let (tmp1, var1) = self.insert_let(e1, Type::Float);
                let e2 = self.knormal_(*e2);
                let (tmp2, var2) = self.insert_let(e2, Type::Float);
                let e = tmp1.finish(tmp2.finish(Expr::FBinOp(BinOp {
                    op: FloatBinOp::Sub,
                    arg1: var1,
                    arg2: var2,
                })));
                (e, Type::Float)
            }
            parser::Expr::FMul(e1, e2) => {
                let e1 = self.knormal_(*e1);
                let (tmp1, var1) = self.insert_let(e1, Type::Float);
                let e2 = self.knormal_(*e2);
                let (tmp2, var2) = self.insert_let(e2, Type::Float);
                let e = tmp1.finish(tmp2.finish(Expr::FBinOp(BinOp {
                    op: FloatBinOp::Mul,
                    arg1: var1,
                    arg2: var2,
                })));
                (e, Type::Float)
            }
            parser::Expr::FDiv(e1, e2) => {
                let e1 = self.knormal_(*e1);
                let (tmp1, var1) = self.insert_let(e1, Type::Float);
                let e2 = self.knormal_(*e2);
                let (tmp2, var2) = self.insert_let(e2, Type::Float);
                let e = tmp1.finish(tmp2.finish(Expr::FBinOp(BinOp {
                    op: FloatBinOp::Div,
                    arg1: var1,
                    arg2: var2,
                })));
                (e, Type::Float)
            }
            parser::Expr::Eq(e1, e2) => {
                let e1 = self.knormal_(*e1);
                // TODO: Eq arg types?
                let (tmp1, var1) = self.insert_let(e1, Type::Int);
                let e1 = self.knormal_(*e2);
                let (tmp2, var2) = self.insert_let(e1, Type::Int);
                let e = tmp1.finish(tmp2.finish(Expr::IfEq(
                    var1,
                    var2,
                    Box::new(Expr::Int(1)),
                    Box::new(Expr::Int(0)),
                )));
                (e, Type::Int)
            }
            parser::Expr::Le(e1, e2) => {
                let e1 = self.knormal_(*e1);
                // TODO: Eq arg types?
                let (tmp1, var1) = self.insert_let(e1, Type::Int);
                let e1 = self.knormal_(*e2);
                let (tmp2, var2) = self.insert_let(e1, Type::Int);
                let e = tmp1.finish(tmp2.finish(Expr::IfLE(
                    var1,
                    var2,
                    Box::new(Expr::Int(1)),
                    Box::new(Expr::Int(0)),
                )));
                (e, Type::Int)
            }
            parser::Expr::If(cond, then_, else_) => {
                let cond = self.knormal_(*cond);
                let (cond_tmp, cond_var) = self.insert_let(cond, Type::Int);
                let true_ = self.knormal_(parser::Expr::Bool(true));
                let (true_tmp, true_var) = self.insert_let(true_, Type::Int);
                let (then_, ty) = self.knormal(*then_);
                let else_ = self.knormal_(*else_);
                let e = cond_tmp.finish(true_tmp.finish(Expr::IfEq(
                    cond_var,
                    true_var,
                    Box::new(then_),
                    Box::new(else_),
                )));
                (e, ty)
            }
            parser::Expr::Let { bndr, rhs, body } => {
                let Binder { binder, id } = bndr;
                let bndr_ty = self.bndr_tys[id].as_ref().cloned().unwrap();
                let rhs = self.knormal_(*rhs);
                self.locals.new_scope();
                self.locals.add(binder.clone(), bndr_ty.clone());
                let (body, body_ty) = self.knormal(*body);
                self.locals.pop_scope();
                let e = Expr::Let {
                    id: binder,
                    ty: bndr_ty,
                    rhs: Box::new(rhs),
                    body: Box::new(body),
                };
                (e, body_ty)
            }
            parser::Expr::Var(var) => {
                let var_ty = match self.locals.get(&var) {
                    None => panic!("Unbound variable: {}", var),
                    Some(var_ty) => var_ty,
                };
                (Expr::Var(var), var_ty.clone())
            }
            parser::Expr::LetRec {
                bndr: _,
                args: _,
                rhs: _,
                body: _,
            } => todo!(),
            parser::Expr::App { fun, args } => {
                let (fun, fun_ty) = self.knormal(*fun);
                let ret_ty: Type = match &fun_ty {
                    Type::Fun { args: _, ret } => (**ret).clone(),
                    other => panic!("Non-function in function position: {:?} : {:?}", fun, other),
                };

                let (fun_tmp, fun_id) = self.insert_let(fun, fun_ty);

                let mut arg_ids: Vec<Id> = Vec::with_capacity(args.len());
                let mut arg_tmps: Vec<TmpLet> = Vec::with_capacity(args.len());
                for arg in args {
                    let (arg, arg_ty) = self.knormal(arg);
                    let (arg_tmp, arg_id) = self.insert_let(arg, arg_ty);
                    arg_ids.push(arg_id);
                    arg_tmps.push(arg_tmp);
                }

                let fun_call = fun_tmp.finish(Expr::App(fun_id, arg_ids));

                let e = arg_tmps
                    .into_iter()
                    .rev()
                    .fold(fun_call, |acc, arg_tmp| arg_tmp.finish(acc));

                (e, ret_ty)
            }
            parser::Expr::Tuple(args) => {
                let mut arg_ids: Vec<Id> = Vec::with_capacity(args.len());
                let mut arg_tmps: Vec<TmpLet> = Vec::with_capacity(args.len());
                let mut arg_tys: Vec<Type> = Vec::with_capacity(args.len());

                for arg in args {
                    let (arg, arg_ty) = self.knormal(arg);
                    let (arg_tmp, arg_id) = self.insert_let(arg, arg_ty.clone());
                    arg_ids.push(arg_id);
                    arg_tmps.push(arg_tmp);
                    arg_tys.push(arg_ty);
                }

                let tuple = Expr::Tuple(arg_ids);

                let e = arg_tmps
                    .into_iter()
                    .rev()
                    .fold(tuple, |acc, arg_tmp| arg_tmp.finish(acc));

                (e, Type::Tuple(arg_tys))
            }
            parser::Expr::LetTuple {
                bndrs: _,
                rhs: _,
                body: _,
            } => todo!(),
            parser::Expr::Array(_e1, _e2) => todo!(),
            parser::Expr::Get(_e1, _e2) => todo!(),
            parser::Expr::Put(_e1, _e2, _e3) => todo!(),
        }
    }
}
