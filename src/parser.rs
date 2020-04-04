use crate::lexer::Token;

#[derive(Debug)]
pub enum Expr {
    // ()
    Unit,
    // true, false
    Bool(bool),
    Int(u64),
    Float(f64),
    // not <expr>
    Not(Box<Expr>),
    // - <expr>
    Neg(Box<Expr>),
    // <expr> + <expr>
    Add(Box<Expr>, Box<Expr>),
    // <expr> - <expr>
    Sub(Box<Expr>, Box<Expr>),
    // -. <expr>
    FNeg(Box<Expr>),
    // <expr> +. <expr>
    FAdd(Box<Expr>, Box<Expr>),
    // <expr> -. <expr>
    FSub(Box<Expr>, Box<Expr>),
    // <expr> *. <expr>
    FMul(Box<Expr>, Box<Expr>),
    // <expr> /. <expr>
    FDiv(Box<Expr>, Box<Expr>),
    // <expr> = <expr>
    Eq(Box<Expr>, Box<Expr>),
    // <expr> <= <expr>
    Le(Box<Expr>, Box<Expr>),
    // if <expr> then <expr> else <expr>
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    // let <ident> = <expr> in <expr>
    Let {
        id: String,
        rhs: Box<Expr>,
        body: Box<Expr>,
    },
    // <ident>
    Var(String),
    // let rec <ident> <ident>+ = <expr> in <expr>
    LetRec {
        name: String,
        args: Vec<String>,
        rhs: Box<Expr>,
        body: Box<Expr>,
    },
    // <expr> <expr>+
    App {
        fun: Box<Expr>,
        args: Vec<Expr>,
    },
    // <expr> (, <expr>)+
    Tuple(Vec<Expr>),
    // let ( <ident> (, <ident>)+ ) = <expr> in <expr>
    LetTuple {
        bndrs: Vec<String>,
        rhs: Box<Expr>,
        body: Box<Expr>,
    },
    // Array.create <expr> <expr>
    Array(Box<Expr>, Box<Expr>),
    // <expr> . ( <expr> )
    Get(Box<Expr>, Box<Expr>),
    // <expr> . ( <expr> ) <- <expr>
    Put(Box<Expr>, Box<Expr>, Box<Expr>),
}
