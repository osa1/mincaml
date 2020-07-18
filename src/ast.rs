use crate::{
    common::{Cmp, FloatBinOp, IntBinOp},
    ctx::VarId,
};

use cranelift_entity::{entity_impl, PrimaryMap};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExprIdx(u32);
entity_impl!(ExprIdx, "e");

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub children: Vec<ExprIdx>,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    // (), no children
    Unit,
    // true, false, no children
    Bool(bool),
    // no children
    Int(i64),
    // no children
    Float(f64),
    // not <expr>, one child
    Not,
    // - <expr>, one child
    Neg,
    // <expr> <op> <expr>, two children
    IntBinOp(IntBinOp),
    // -. <expr>, one child
    FNeg,
    // <expr> <op> <expr>, two children
    FloatBinOp(FloatBinOp),
    // <expr> <op> <expr>, two children
    Cmp(Cmp),
    // if <expr> then <expr> else <expr>, three children
    If,
    // let <bndr> = <expr> in <expr>, two children
    Let { bndr: VarId },
    // no children
    Var(VarId),
    // letrec <bndr> <args> = <expr> in <expr>, two children
    LetRec { bndr: VarId, args: Vec<VarId> },
    // <expr> <expr>*, first child is the function, rest are args
    App,
    // <expr> (,<expr>)+, many children
    Tuple,
    // let (<bndr> (,<bndr>)+) = <expr> in <expr>, two children
    LetTuple { bndrs: Vec<VarId> },
    // Array.create <expr> <expr>, two children
    Array,
    // <expr> . ( <expr> ), two children
    Get,
    // <expr> . ( <expr> ) <- <expr>, three children
    Put,
}

// We don't intern expressions as each will have different associated data, like the source
// location (and type, when we introduce polymorphism)
pub type ExprArena = PrimaryMap<ExprIdx, Expr>;

// TODO: Maybe these should be `ExprArena` methods?
impl Expr {
    pub fn unit(arena: &mut ExprArena) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::Unit,
            children: vec![],
        })
    }

    pub fn bool(b: bool, arena: &mut ExprArena) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::Bool(b),
            children: vec![],
        })
    }

    pub fn int(i: i64, arena: &mut ExprArena) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::Int(i),
            children: vec![],
        })
    }

    pub fn float(f: f64, arena: &mut ExprArena) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::Float(f),
            children: vec![],
        })
    }

    pub fn not(e: ExprIdx, arena: &mut ExprArena) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::Not,
            children: vec![e],
        })
    }

    pub fn neg(e: ExprIdx, arena: &mut ExprArena) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::Neg,
            children: vec![e],
        })
    }

    pub fn int_binop(e1: ExprIdx, op: IntBinOp, e2: ExprIdx, arena: &mut ExprArena) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::IntBinOp(op),
            children: vec![e1, e2],
        })
    }

    pub fn fneg(e: ExprIdx, arena: &mut ExprArena) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::FNeg,
            children: vec![e],
        })
    }

    pub fn float_binop(e1: ExprIdx, op: FloatBinOp, e2: ExprIdx, arena: &mut ExprArena) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::FloatBinOp(op),
            children: vec![e1, e2],
        })
    }

    pub fn cmp(e1: ExprIdx, op: Cmp, e2: ExprIdx, arena: &mut ExprArena) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::Cmp(op),
            children: vec![e1, e2],
        })
    }

    pub fn if_(e1: ExprIdx, e2: ExprIdx, e3: ExprIdx, arena: &mut ExprArena) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::If,
            children: vec![e1, e2, e3],
        })
    }

    pub fn let_(bndr: VarId, e1: ExprIdx, e2: ExprIdx, arena: &mut ExprArena) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::Let { bndr },
            children: vec![e1, e2],
        })
    }

    pub fn var(var: VarId, arena: &mut ExprArena) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::Var(var),
            children: vec![],
        })
    }

    pub fn letrec(
        bndr: VarId, args: Vec<VarId>, e1: ExprIdx, e2: ExprIdx, arena: &mut ExprArena,
    ) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::LetRec { bndr, args },
            children: vec![e1, e2],
        })
    }

    pub fn app(f: ExprIdx, mut args: Vec<ExprIdx>, arena: &mut ExprArena) -> ExprIdx {
        args.insert(0, f);
        arena.push(Expr {
            kind: ExprKind::App,
            children: args,
        })
    }

    pub fn tuple(args: Vec<ExprIdx>, arena: &mut ExprArena) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::Tuple,
            children: args,
        })
    }

    pub fn let_tuple(
        bndrs: Vec<VarId>, rhs: ExprIdx, body: ExprIdx, arena: &mut ExprArena,
    ) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::LetTuple { bndrs },
            children: vec![rhs, body],
        })
    }

    pub fn array(len: ExprIdx, elem: ExprIdx, arena: &mut ExprArena) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::Array,
            children: vec![len, elem],
        })
    }

    pub fn get(array: ExprIdx, idx: ExprIdx, arena: &mut ExprArena) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::Array,
            children: vec![array, idx],
        })
    }

    pub fn put(array: ExprIdx, idx: ExprIdx, value: ExprIdx, arena: &mut ExprArena) -> ExprIdx {
        arena.push(Expr {
            kind: ExprKind::Put,
            children: vec![array, idx, value],
        })
    }
}
