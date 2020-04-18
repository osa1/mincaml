use crate::type_check::Type;
use crate::parser;

pub type Id = String;

#[derive(Debug)]
pub enum Expr {
    Unit,
    Int(i64),
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
        body: Box<Expr>
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
    Add, Sub, Mul, Div
}

#[derive(Debug)]
pub enum IntBinOp {
    Add, Sub,
}

pub fn knormal(expr: &parser::Expr) -> Expr {
    todo!()
}
