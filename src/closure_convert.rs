//! Closure conversion: converts `let rec`s to functions.
//!
//! Generated functions take 'self' as the first argument. 'self' is the representation of the
//! closure itself: a tuple with the function as the first element, and captured variables as rest
//! of the elements.

use crate::anormal;
use crate::cg_types::RepType;
use crate::common::*;
use crate::ctx::{Ctx, TypeId, VarId};

use fxhash::{FxHashMap, FxHashSet};

/// Post-closure-conversion expressions. Only difference from [anormal::Expr] is this one doesn't
/// have `LetRec`.
#[derive(Debug)]
pub enum Expr {
    Unit,

    Int(i64),

    Float(f64),

    /// Integer binary operation
    IBinOp(BinOp<IntBinOp>),

    /// Float binary operation
    FBinOp(BinOp<FloatBinOp>),

    /// Integer negation
    Neg(VarId),

    /// Float negation
    FNeg(VarId),

    /// Conditional
    If(VarId, VarId, Cmp, Box<Expr>, Box<Expr>),

    /// Let binding
    Let {
        id: VarId,
        ty_id: TypeId,
        rhs: Box<Expr>,
        body: Box<Expr>,
    },

    Var(VarId),

    /// Function application
    App(VarId, Vec<VarId>),

    /// Tuple allocation
    Tuple(Vec<VarId>),

    /// Tuple field read
    TupleGet(VarId, usize),

    /// Array allocation
    ArrayAlloc {
        len: VarId,
        elem: VarId,
    },

    /// Array field read
    ArrayGet(VarId, VarId),

    /// Array field write
    ArrayPut(VarId, VarId, VarId),
}

#[derive(Debug)]
pub struct Fun {
    /// Name of the function
    pub name: VarId,

    /// Argument names of the function
    pub args: Vec<VarId>,

    /// Body of the function
    pub body: Expr,

    /// Return type of the function
    pub return_type: RepType,
}

/// Closure conversion, see module documentation for details.
///
/// Returns functions of the program and the id for the main function.
pub fn closure_convert(ctx: &mut Ctx, expr: anormal::Expr) -> (Vec<Fun>, VarId) {
    todo!()
}

/// Collect free variables. Returns a map from closure ids to their free variables.
fn collect_fvs(expr: &anormal::Expr) -> FxHashMap<VarId, FxHashSet<VarId>> {
    todo!()
}

fn collect_fvs_(
    ctx: &Ctx, expr: &anormal::Expr, bounds: &mut FxHashSet<VarId>, fvs: &mut FxHashSet<VarId>,
    acc: &mut FxHashMap<VarId, FxHashSet<VarId>>,
) {
    use anormal::Expr::*;

    match expr {
        Unit | Int(_) | Float(_) => {}

        IBinOp(BinOp { arg1, arg2, .. })
        | FBinOp(BinOp { arg1, arg2, .. })
        | ArrayGet(arg1, arg2) => {
            collect_fv(ctx, *arg1, bounds, fvs);
            collect_fv(ctx, *arg2, bounds, fvs);
        }

        Neg(arg) | FNeg(arg) | TupleGet(arg, _) => {
            collect_fv(ctx, *arg, bounds, fvs);
        }

        If(arg1, arg2, _, e1, e2) => {
            collect_fv(ctx, *arg1, bounds, fvs);
            collect_fv(ctx, *arg2, bounds, fvs);
            collect_fvs_(ctx, e1, bounds, fvs, acc);
            collect_fvs_(ctx, e2, bounds, fvs, acc);
        }

        Let { id, ty_id: _, rhs, body } => {
            // TODO: We don't have shadowing at this point so we don't need to remove stuff from
            // `bounds`?
            bounds.insert(*id);
            collect_fvs_(ctx, body, bounds, fvs, acc);
            bounds.remove(id);
            collect_fvs_(ctx, rhs, bounds, fvs, acc);
        }

        Var(id) => collect_fv(ctx, *id, bounds, fvs),

        LetRec { name, ty_id: _, args, rhs, body } => {
            bounds.insert(*name);
            collect_fvs_(ctx, body, bounds, fvs, acc);
            let mut letrec_fvs: FxHashSet<VarId> = Default::default();
            for arg in args {
                bounds.insert(*arg);
            }
            collect_fvs_(ctx, rhs, bounds, &mut letrec_fvs, acc);
            fvs.extend(letrec_fvs.iter().copied());
            let old = acc.insert(*name, letrec_fvs);
            debug_assert_eq!(old, None);
            for arg in args {
                bounds.remove(arg);
            }
            bounds.remove(name);
        }

        App(fun, args) => {
            collect_fv(ctx, *fun, bounds, fvs);
            for arg in args {
                collect_fv(ctx, *arg, bounds, fvs);
            }
        }

        Tuple(args) => {
            for arg in args {
                collect_fv(ctx, *arg, bounds, fvs);
            }
        }

        ArrayAlloc { len, elem } => {
            collect_fv(ctx, *len, bounds, fvs);
            collect_fv(ctx, *elem, bounds, fvs);
        }

        ArrayPut(arg1, arg2, arg3) => {
            collect_fv(ctx, *arg1, bounds, fvs);
            collect_fv(ctx, *arg2, bounds, fvs);
            collect_fv(ctx, *arg3, bounds, fvs);
        }
    }
}

fn collect_fv(ctx: &Ctx, var: VarId, bounds: &FxHashSet<VarId>, fvs: &mut FxHashSet<VarId>) {
    if !ctx.is_builtin_var(var) && !bounds.contains(&var) {
        fvs.insert(var);
    }
}
