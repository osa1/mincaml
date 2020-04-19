use crate::parser;
use crate::type_check::Type;

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
}

impl<'a> KNormal<'a> {
    pub fn new(bndr_tys: &'a [Option<Type>]) -> KNormal {
        KNormal {
            tmp_count: 0,
            bndr_tys,
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

    pub fn knormal(&mut self, expr: parser::Expr) -> Expr {
        match expr {
            parser::Expr::Unit => Expr::Unit,
            parser::Expr::Bool(b) => Expr::Int(if b { 1 } else { 0 }),
            parser::Expr::Int(i) => Expr::Int(i),
            parser::Expr::Float(f) => Expr::Float(f),
            parser::Expr::Not(e) => self.knormal(parser::Expr::If(
                e,
                Box::new(parser::Expr::Bool(false)),
                Box::new(parser::Expr::Bool(true)),
            )),
            parser::Expr::Neg(e) => {
                let e = self.knormal(*e);
                let (tmp, var) = self.insert_let(e, Type::Int);
                tmp.finish(Expr::Neg(var))
            }
            parser::Expr::Add(e1, e2) => {
                let e1 = self.knormal(*e1);
                let (tmp1, var1) = self.insert_let(e1, Type::Int);
                let e2 = self.knormal(*e2);
                let (tmp2, var2) = self.insert_let(e2, Type::Int);
                tmp1.finish(tmp2.finish(Expr::IBinOp(BinOp {
                    op: IntBinOp::Add,
                    arg1: var1,
                    arg2: var2,
                })))
            }
            parser::Expr::Sub(e1, e2) => {
                let e1 = self.knormal(*e1);
                let (tmp1, var1) = self.insert_let(e1, Type::Int);
                let e2 = self.knormal(*e2);
                let (tmp2, var2) = self.insert_let(e2, Type::Int);
                tmp1.finish(tmp2.finish(Expr::IBinOp(BinOp {
                    op: IntBinOp::Sub,
                    arg1: var1,
                    arg2: var2,
                })))
            }
            parser::Expr::FNeg(e) => {
                let e = self.knormal(*e);
                let (tmp, var) = self.insert_let(e, Type::Float);
                tmp.finish(Expr::FNeg(var))
            }
            parser::Expr::FAdd(e1, e2) => {
                let e1 = self.knormal(*e1);
                let (tmp1, var1) = self.insert_let(e1, Type::Float);
                let e2 = self.knormal(*e2);
                let (tmp2, var2) = self.insert_let(e2, Type::Float);
                tmp1.finish(tmp2.finish(Expr::FBinOp(BinOp {
                    op: FloatBinOp::Add,
                    arg1: var1,
                    arg2: var2,
                })))
            }
            parser::Expr::FSub(e1, e2) => {
                let e1 = self.knormal(*e1);
                let (tmp1, var1) = self.insert_let(e1, Type::Float);
                let e2 = self.knormal(*e2);
                let (tmp2, var2) = self.insert_let(e2, Type::Float);
                tmp1.finish(tmp2.finish(Expr::FBinOp(BinOp {
                    op: FloatBinOp::Sub,
                    arg1: var1,
                    arg2: var2,
                })))
            }
            parser::Expr::FMul(e1, e2) => {
                let e1 = self.knormal(*e1);
                let (tmp1, var1) = self.insert_let(e1, Type::Float);
                let e2 = self.knormal(*e2);
                let (tmp2, var2) = self.insert_let(e2, Type::Float);
                tmp1.finish(tmp2.finish(Expr::FBinOp(BinOp {
                    op: FloatBinOp::Mul,
                    arg1: var1,
                    arg2: var2,
                })))
            }
            parser::Expr::FDiv(e1, e2) => {
                let e1 = self.knormal(*e1);
                let (tmp1, var1) = self.insert_let(e1, Type::Float);
                let e2 = self.knormal(*e2);
                let (tmp2, var2) = self.insert_let(e2, Type::Float);
                tmp1.finish(tmp2.finish(Expr::FBinOp(BinOp {
                    op: FloatBinOp::Div,
                    arg1: var1,
                    arg2: var2,
                })))
            }
            parser::Expr::Eq(e1, e2) => {
                let e1 = self.knormal(*e1);
                // TODO: Eq arg types?
                let (tmp1, var1) = self.insert_let(e1, Type::Int);
                let e1 = self.knormal(*e2);
                let (tmp2, var2) = self.insert_let(e1, Type::Int);
                tmp1.finish(tmp2.finish(Expr::IfEq(
                    var1,
                    var2,
                    Box::new(Expr::Int(1)),
                    Box::new(Expr::Int(0)),
                )))
            }
            parser::Expr::Le(e1, e2) => {
                let e1 = self.knormal(*e1);
                // TODO: Eq arg types?
                let (tmp1, var1) = self.insert_let(e1, Type::Int);
                let e1 = self.knormal(*e2);
                let (tmp2, var2) = self.insert_let(e1, Type::Int);
                tmp1.finish(tmp2.finish(Expr::IfLE(
                    var1,
                    var2,
                    Box::new(Expr::Int(1)),
                    Box::new(Expr::Int(0)),
                )))
            }
            parser::Expr::If(cond, then_, else_) => {
                let cond = self.knormal(*cond);
                let (cond_tmp, cond_var) = self.insert_let(cond, Type::Int);
                let true_ = self.knormal(parser::Expr::Bool(true));
                let (true_tmp, true_var) = self.insert_let(true_, Type::Int);
                let then_ = self.knormal(*then_);
                let else_ = self.knormal(*else_);
                cond_tmp.finish(true_tmp.finish(Expr::IfEq(
                    cond_var,
                    true_var,
                    Box::new(then_),
                    Box::new(else_),
                )))
            }
            parser::Expr::Let { bndr, rhs, body } => {
                todo!()
            }
            parser::Expr::Var(var) => {
                todo!()
            }
            parser::Expr::LetRec { bndr, args, rhs, body } => {
                todo!()
            }
            parser::Expr::App { fun, args } => {
                todo!()
            }
            parser::Expr::Tuple(args) => {
                todo!()
            }
            parser::Expr::LetTuple { bndrs, rhs, body } => {
                todo!()
            }
            parser::Expr::Array(e1, e2) => {
                todo!()
            }
            parser::Expr::Get(e1, e2) => {
                todo!()
            }
            parser::Expr::Put(e1, e2, e3) => {
                todo!()
            }
        }
    }
}
