use crate::locals::Locals;
use crate::parser;
use crate::type_check::{mk_type_env, Type};

pub type Id = String;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Binder {
    pub binder: String,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinderOrUnit {
    Binder(Binder),
    Unit,
}

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
    LetRec {
        name: Id,
        ty: Type,
        args: Vec<BinderOrUnit>,
        rhs: Box<Expr>,
        body: Box<Expr>,
    },
    App(Id, Vec<Id>),
    // A C call
    ExtApp(Id, Vec<Id>),
    Tuple(Vec<Id>),
    LetTuple(Vec<(Id, Type)>, Id, Box<Expr>),
    Get(Id, Id),
    Put(Id, Id, Id),
}

#[derive(Debug, Clone)]
pub struct BinOp<A> {
    pub op: A,
    pub arg1: Id,
    pub arg2: Id,
}

#[derive(Debug, Clone, Copy)]
pub enum FloatBinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy)]
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
            locals: Locals::new(mk_type_env()),
        }
    }

    fn id_type(&self, id: usize) -> Type {
        match self.bndr_tys.get(id) {
            None => {
                panic!("Unknown id: {}", id);
            }
            Some(None) => {
                panic!("Id type unknown: {}", id);
            }
            Some(Some(ty)) => ty.clone(),
        }
    }

    // Creates a let with body initialized as `()` (Unit). Make sure to update it after normalizing
    // the body!
    fn mk_let(&mut self, rhs: Expr, ty: Type) -> (TmpLet, String) {
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
                let (tmp, var) = self.mk_let(e, Type::Int);
                (tmp.finish(Expr::Neg(var)), Type::Int)
            }
            parser::Expr::Add(e1, e2) => {
                let e1 = self.knormal_(*e1);
                let (tmp1, var1) = self.mk_let(e1, Type::Int);
                let e2 = self.knormal_(*e2);
                let (tmp2, var2) = self.mk_let(e2, Type::Int);
                let e = tmp1.finish(tmp2.finish(Expr::IBinOp(BinOp {
                    op: IntBinOp::Add,
                    arg1: var1,
                    arg2: var2,
                })));
                (e, Type::Int)
            }
            parser::Expr::Sub(e1, e2) => {
                let e1 = self.knormal_(*e1);
                let (tmp1, var1) = self.mk_let(e1, Type::Int);
                let e2 = self.knormal_(*e2);
                let (tmp2, var2) = self.mk_let(e2, Type::Int);
                let e = tmp1.finish(tmp2.finish(Expr::IBinOp(BinOp {
                    op: IntBinOp::Sub,
                    arg1: var1,
                    arg2: var2,
                })));
                (e, Type::Int)
            }
            parser::Expr::FNeg(e) => {
                let e = self.knormal_(*e);
                let (tmp, var) = self.mk_let(e, Type::Float);
                let e = tmp.finish(Expr::FNeg(var));
                (e, Type::Float)
            }
            parser::Expr::FAdd(e1, e2) => {
                let e1 = self.knormal_(*e1);
                let (tmp1, var1) = self.mk_let(e1, Type::Float);
                let e2 = self.knormal_(*e2);
                let (tmp2, var2) = self.mk_let(e2, Type::Float);
                let e = tmp1.finish(tmp2.finish(Expr::FBinOp(BinOp {
                    op: FloatBinOp::Add,
                    arg1: var1,
                    arg2: var2,
                })));
                (e, Type::Float)
            }
            parser::Expr::FSub(e1, e2) => {
                let e1 = self.knormal_(*e1);
                let (tmp1, var1) = self.mk_let(e1, Type::Float);
                let e2 = self.knormal_(*e2);
                let (tmp2, var2) = self.mk_let(e2, Type::Float);
                let e = tmp1.finish(tmp2.finish(Expr::FBinOp(BinOp {
                    op: FloatBinOp::Sub,
                    arg1: var1,
                    arg2: var2,
                })));
                (e, Type::Float)
            }
            parser::Expr::FMul(e1, e2) => {
                let e1 = self.knormal_(*e1);
                let (tmp1, var1) = self.mk_let(e1, Type::Float);
                let e2 = self.knormal_(*e2);
                let (tmp2, var2) = self.mk_let(e2, Type::Float);
                let e = tmp1.finish(tmp2.finish(Expr::FBinOp(BinOp {
                    op: FloatBinOp::Mul,
                    arg1: var1,
                    arg2: var2,
                })));
                (e, Type::Float)
            }
            parser::Expr::FDiv(e1, e2) => {
                let e1 = self.knormal_(*e1);
                let (tmp1, var1) = self.mk_let(e1, Type::Float);
                let e2 = self.knormal_(*e2);
                let (tmp2, var2) = self.mk_let(e2, Type::Float);
                let e = tmp1.finish(tmp2.finish(Expr::FBinOp(BinOp {
                    op: FloatBinOp::Div,
                    arg1: var1,
                    arg2: var2,
                })));
                (e, Type::Float)
            }
            parser::Expr::Eq(e1, e2) => {
                let (e1, e1_ty) = self.knormal(*e1);
                let (tmp1, var1) = self.mk_let(e1, e1_ty);
                let (e2, e2_ty) = self.knormal(*e2);
                let (tmp2, var2) = self.mk_let(e2, e2_ty);
                let e = tmp1.finish(tmp2.finish(Expr::IfEq(
                    var1,
                    var2,
                    Box::new(Expr::Int(1)),
                    Box::new(Expr::Int(0)),
                )));
                (e, Type::Int)
            }
            parser::Expr::Le(e1, e2) => {
                let (e1, e1_ty) = self.knormal(*e1);
                let (tmp1, var1) = self.mk_let(e1, e1_ty);
                let (e2, e2_ty) = self.knormal(*e2);
                let (tmp2, var2) = self.mk_let(e2, e2_ty);
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
                let (cond_tmp, cond_var) = self.mk_let(cond, Type::Int);
                let true_ = self.knormal_(parser::Expr::Bool(true));
                let (true_tmp, true_var) = self.mk_let(true_, Type::Int);
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
                let parser::Binder { binder, id } = bndr;
                let bndr_ty = self.id_type(id);
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
                bndr,
                args,
                rhs,
                body,
            } => {
                let parser::Binder { binder, id } = bndr;
                let bndr_ty = self.id_type(id);

                // First scope for the binder
                self.locals.new_scope();
                self.locals.add(binder.clone(), bndr_ty.clone());
                // Second scope for the args
                self.locals.new_scope();
                let mut kargs: Vec<BinderOrUnit> = Vec::with_capacity(args.len());
                for arg in args {
                    match arg {
                        parser::BinderOrUnit::Binder(parser::Binder { binder, id }) => {
                            let arg_ty = self.id_type(id);
                            self.locals.add(binder.clone(), arg_ty.clone());
                            kargs.push(BinderOrUnit::Binder(Binder {
                                binder: binder,
                                ty: arg_ty,
                            }));
                        }
                        parser::BinderOrUnit::Unit => {
                            kargs.push(BinderOrUnit::Unit);
                        }
                    }
                }

                let rhs = self.knormal_(*rhs);

                self.locals.pop_scope();

                let (body, body_ty) = self.knormal(*body);

                let e = Expr::LetRec {
                    name: binder,
                    ty: bndr_ty,
                    args: kargs,
                    rhs: Box::new(rhs),
                    body: Box::new(body),
                };

                (e, body_ty)
            }

            parser::Expr::App { fun, args } => {
                let (fun, fun_ty) = self.knormal(*fun);
                let ret_ty: Type = match &fun_ty {
                    Type::Fun { args: _, ret } => (**ret).clone(),
                    other => panic!("Non-function in function position: {:?} : {:?}", fun, other),
                };

                let (fun_tmp, fun_id) = self.mk_let(fun, fun_ty);

                let mut arg_ids: Vec<Id> = Vec::with_capacity(args.len());
                let mut arg_tmps: Vec<TmpLet> = Vec::with_capacity(args.len());
                for arg in args {
                    let (arg, arg_ty) = self.knormal(arg);
                    let (arg_tmp, arg_id) = self.mk_let(arg, arg_ty);
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
                    let (arg_tmp, arg_id) = self.mk_let(arg, arg_ty.clone());
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

            parser::Expr::LetTuple { bndrs, rhs, body } => {
                let mut kbndrs: Vec<(Id, Type)> = Vec::with_capacity(bndrs.len());

                for bndr in bndrs {
                    let parser::Binder { binder, id } = bndr;
                    let bndr_ty = self.id_type(id);
                    kbndrs.push((binder, bndr_ty));
                }

                let (rhs, rhs_ty) = self.knormal(*rhs);
                let (rhs_tmp, rhs_id) = self.mk_let(rhs, rhs_ty);

                self.locals.new_scope();
                for (bndr, bndr_ty) in &kbndrs {
                    self.locals.add(bndr.clone(), bndr_ty.clone());
                }

                let (body, body_ty) = self.knormal(*body);

                self.locals.pop_scope();

                let e = rhs_tmp.finish(Expr::LetTuple(kbndrs, rhs_id, Box::new(body)));

                (e, body_ty)
            }

            parser::Expr::Array(e1, e2) => {
                let (e1, e1_ty) = self.knormal(*e1);
                assert_eq!(e1_ty, Type::Int);
                let (e2, e2_ty) = self.knormal(*e2);
                let (e1_tmp, e1_id) = self.mk_let(e1, e1_ty);
                let (e2_tmp, e2_id) = self.mk_let(e2, e2_ty.clone());

                let array_fun = match &e2_ty {
                    Type::Float => "create_float_array".to_string(),
                    _ => "create_array".to_string(),
                };

                let app = Expr::ExtApp(array_fun, vec![e1_id, e2_id]);

                (
                    e1_tmp.finish(e2_tmp.finish(app)),
                    Type::Array(Box::new(e2_ty)),
                )
            }

            parser::Expr::Get(e1, e2) => {
                let (e1, e1_ty) = self.knormal(*e1);
                let elem_ty = match &e1_ty {
                    Type::Array(elem) => (**elem).clone(),
                    _ => panic!("Non-array type in Get: {:?}", e1_ty),
                };
                let (e2, e2_ty) = self.knormal(*e2);
                assert_eq!(e2_ty, Type::Int);
                let (e1_tmp, e1_id) = self.mk_let(e1, e1_ty);
                let (e2_tmp, e2_id) = self.mk_let(e2, e2_ty);

                let e = e1_tmp.finish(e2_tmp.finish(Expr::Get(e1_id, e2_id)));

                (e, elem_ty)
            }

            parser::Expr::Put(e1, e2, e3) => {
                let (e1, e1_ty) = self.knormal(*e1);
                assert!(e1_ty.is_array());
                let (e2, e2_ty) = self.knormal(*e2);
                assert_eq!(e2_ty, Type::Int);
                let (e3, e3_ty) = self.knormal(*e3);

                let (e1_tmp, e1_id) = self.mk_let(e1, e1_ty);
                let (e2_tmp, e2_id) = self.mk_let(e2, e2_ty);
                let (e3_tmp, e3_id) = self.mk_let(e3, e3_ty);

                let e = e1_tmp.finish(e2_tmp.finish(e3_tmp.finish(Expr::Put(e1_id, e2_id, e3_id))));

                (e, Type::Unit)
            }
        }
    }
}
