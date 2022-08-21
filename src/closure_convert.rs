//! Closure conversion: converts `let rec`s to functions, converts function applications to closure
//! applications.
//!
//! Generated functions take 'self' as the first argument. 'self' is the representation of the
//! closure itself: a tuple with the function as the first element, and captured variables as rest
//! of the elements.

mod print;

use crate::anormal;
use crate::cg_types::RepType;
use crate::common::*;
use crate::ctx::{Ctx, VarId};
use crate::type_check::Type;
use crate::var::CompilerPhase::ClosureConvert;

use fxhash::{FxHashMap, FxHashSet};

/// Post-closure-conversion expressions. Differences from [anormal::Expr] are:
///
/// - No `LetRec`
/// - `App`s and `TupleGet`s are annotated with `RepType`s for the values of the expressions
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
        rhs: Box<Expr>,
        body: Box<Expr>,
    },

    Var(VarId),

    /// Function application
    App(VarId, Vec<VarId>, RepType),

    /// Tuple allocation
    Tuple(Vec<VarId>),

    /// Tuple field read
    TupleGet(VarId, usize, RepType),

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

/// Closure conversion state
struct CcCtx<'ctx> {
    ctx: &'ctx mut Ctx,

    /// Functions generated so far
    funs: Vec<Fun>,
}

impl<'ctx> CcCtx<'ctx> {
    fn new(ctx: &'ctx mut Ctx) -> Self {
        Self { ctx, funs: vec![] }
    }

    fn fresh_var(&mut self, rep_type: RepType) -> VarId {
        self.ctx.fresh_codegen_var(ClosureConvert, rep_type)
    }

    fn fork_fun<F: FnOnce(&mut CcCtx) -> Fun>(&mut self, fork: F) {
        let fun = fork(self);
        self.funs.push(fun);
    }
}

/// Closure conversion, see module documentation for details.
///
/// Returns functions of the program and the id for the main function.
pub fn closure_convert(ctx: &mut Ctx, expr: anormal::Expr) -> (Vec<Fun>, VarId) {
    let closure_fvs = collect_fvs(ctx, &expr);
    let mut ctx = CcCtx::new(ctx);

    let main_name = ctx.fresh_var(RepType::Word);
    let main_expr = cc_expr(&mut ctx, expr, &closure_fvs);

    let CcCtx { ctx: _, mut funs } = ctx;
    funs.push(Fun {
        name: main_name,
        args: Vec::new(),
        body: main_expr,
        return_type: RepType::Word,
    });

    (funs, main_name)
}

fn cc_expr(ctx: &mut CcCtx, expr: anormal::Expr, fvs: &FxHashMap<VarId, FxHashSet<VarId>>) -> Expr {
    match expr {
        anormal::Expr::Unit => Expr::Unit,

        anormal::Expr::Int(i) => Expr::Int(i),

        anormal::Expr::Float(f) => Expr::Float(f),

        anormal::Expr::IBinOp(op) => Expr::IBinOp(op),

        anormal::Expr::FBinOp(op) => Expr::FBinOp(op),

        anormal::Expr::Neg(arg) => Expr::Neg(arg),

        anormal::Expr::FNeg(arg) => Expr::FNeg(arg),

        anormal::Expr::If(v1, v2, cmp, then_, else_) => Expr::If(
            v1,
            v2,
            cmp,
            Box::new(cc_expr(ctx, *then_, fvs)),
            Box::new(cc_expr(ctx, *else_, fvs)),
        ),

        anormal::Expr::Let { id, ty_id: _, rhs, body } => Expr::Let {
            id,
            rhs: Box::new(cc_expr(ctx, *rhs, fvs)),
            body: Box::new(cc_expr(ctx, *body, fvs)),
        },

        anormal::Expr::Var(var) => Expr::Var(var),

        anormal::Expr::LetRec { name, ty_id, args, rhs, body } => {
            let (fun_arg_tys, fun_ret_ty) = match &*ctx.ctx.var_type(name) {
                Type::Fun { args, ret } => (args.clone(), ret.clone()),
                _ => panic!(),
            };

            // After cc 'name' will refer to the closure tuple. For the function we use a fresh
            // variable.
            let fun_var = ctx.fresh_var(RepType::Word);
            {
                let mut fun_var_arg_tys = fun_arg_tys;
                // TODO: We can't have recursive types (not supported), so for the 'self' argument
                // we use 'int'. This is fine as rest of the passes do not care about parameter
                // types of functions being called; they use types of arguments being passed.
                fun_var_arg_tys.insert(0, Type::Int);
                let fun_var_ty = Type::Fun { args: fun_var_arg_tys, ret: fun_ret_ty.clone() };
                let fun_var_ty_interned = ctx.ctx.intern_type(fun_var_ty);
                ctx.ctx.set_var_type(fun_var, fun_var_ty_interned);
            }

            // Free variables of the closure will be moved to tuple payload
            let closure_fvs: Vec<VarId> = fvs.get(&name).unwrap().iter().copied().collect();

            {
                // Original identifier for the function will be used in tuple position in
                // `TupleGet` expressions, so we update its type to reflect that it's not a tuple.
                //
                // TODO: This is hacky for two reasons:
                //
                // - We can't have recursive types (not supported), so the function type in the
                //   tuple is not accurate
                //
                // - Code generator needs to handle the case where tuple in a `TupleGet` is
                //   function, not tuple, because we don't update built-in function types after
                //   closure conversion.
                //
                // Not sure how to best deal with this..
                let mut tuple_field_tys: Vec<Type> =
                    // arg types not used in codegen
                    vec![Type::Fun { args: vec![], ret: fun_ret_ty }];
                tuple_field_tys.extend(
                    closure_fvs
                        .iter()
                        .map(|fv| (*ctx.ctx.var_type(*fv)).clone()),
                );
                let tuple_ty = Type::Tuple(tuple_field_tys);
                let tuple_ty_interned = ctx.ctx.intern_type(tuple_ty);
                ctx.ctx.set_var_type(name, tuple_ty_interned);
            }

            // In the RHS and the body, 'name' will refer to the tuple. In the RHS the tuple will
            // be the first argument of the function, in the body we'll allocate a tuple.

            let rhs = cc_expr(ctx, *rhs, fvs);
            ctx.fork_fun(|ctx| {
                // Bind captured variables in function body
                let fun_body: Expr =
                    closure_fvs
                        .iter()
                        .copied()
                        .enumerate()
                        .rfold(rhs, |body, (fv_idx, fv)| Expr::Let {
                            id: fv,
                            rhs: Box::new(Expr::TupleGet(
                                name,
                                fv_idx + 1,
                                RepType::from(&*ctx.ctx.var_type(fv)),
                            )),
                            body: Box::new(body),
                        });

                let mut args = args;
                args.insert(0, name); // first argument will be 'self'

                let fun_type = ctx.ctx.get_type(ty_id);
                let fun_return_type = match &*fun_type {
                    Type::Fun { ret, .. } => RepType::from(&**ret),
                    _ => panic!("Non-function in function position"),
                };

                Fun { name: fun_var, args, body: fun_body, return_type: fun_return_type }
            });

            // Body
            let mut closure_tuple_args = closure_fvs;
            closure_tuple_args.insert(0, fun_var);
            let body = cc_expr(ctx, *body, fvs);
            Expr::Let {
                id: name,
                rhs: Box::new(Expr::Tuple(closure_tuple_args)),
                body: Box::new(body),
            }
        }

        anormal::Expr::App(fun, mut args) => {
            // f(x) -> f.0(f, x)

            // TODO FIXME: We update types of closures converted to tuples, but there are two cases
            // where an id in function position in an application will still have a function type
            // (instead of a tuple type):
            //
            // - Built-in functions: we don't update types of these
            // - `let x = y` where `y` is a function. Since technically `x` isn't a closure (`y`
            //   is), its type doesn't get updated to a tuple.
            let ret_ty = match &*ctx.ctx.var_type(fun) {
                Type::Fun { args: _, ret } => RepType::from(&**ret),
                Type::Tuple(tuple) => {
                    // Must be a user-defined closure
                    debug_assert!(!ctx.ctx.get_var(fun).is_builtin());
                    match &tuple[0] {
                        Type::Fun { args: _, ret } => RepType::from(&**ret),
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            };

            let fun_tmp = ctx.fresh_var(RepType::Word);
            args.insert(0, fun);
            Expr::Let {
                id: fun_tmp,
                rhs: Box::new(Expr::TupleGet(fun, 0, RepType::Word)),
                body: Box::new(Expr::App(fun_tmp, args, ret_ty)),
            }
        }

        anormal::Expr::Tuple(vars) => Expr::Tuple(vars),

        anormal::Expr::TupleGet(var, idx) => {
            let elem_ty = match &*ctx.ctx.var_type(var) {
                Type::Tuple(tuple) => RepType::from(&tuple[idx]),
                _ => panic!(),
            };
            Expr::TupleGet(var, idx, elem_ty)
        }

        anormal::Expr::ArrayAlloc { len, elem } => Expr::ArrayAlloc { len, elem },

        anormal::Expr::ArrayGet(var, idx) => Expr::ArrayGet(var, idx),

        anormal::Expr::ArrayPut(var, idx, elem) => Expr::ArrayPut(var, idx, elem),
    }
}

/// Collect free variables. Returns a map from closure ids to their free variables.
fn collect_fvs(ctx: &Ctx, expr: &anormal::Expr) -> FxHashMap<VarId, FxHashSet<VarId>> {
    let mut closure_fvs = Default::default();
    collect_fvs_(ctx, expr, &mut Default::default(), &mut closure_fvs);

    // for (var, fvs) in closure_fvs.iter() {
    //     let fvs_vec: Vec<_> = fvs.iter().copied().map(|fv| ctx.get_var(fv)).collect();
    //     println!("{}: {:?}", ctx.get_var(*var), fvs_vec);
    // }

    closure_fvs
}

fn collect_fvs_(
    ctx: &Ctx, expr: &anormal::Expr, fvs: &mut FxHashSet<VarId>,
    acc: &mut FxHashMap<VarId, FxHashSet<VarId>>,
) {
    use anormal::Expr::*;

    match expr {
        Unit | Int(_) | Float(_) => {}

        IBinOp(BinOp { arg1, arg2, .. })
        | FBinOp(BinOp { arg1, arg2, .. })
        | ArrayGet(arg1, arg2) => {
            collect_fv(ctx, *arg1, fvs);
            collect_fv(ctx, *arg2, fvs);
        }

        Neg(arg) | FNeg(arg) | TupleGet(arg, _) => {
            collect_fv(ctx, *arg, fvs);
        }

        If(arg1, arg2, _, e1, e2) => {
            collect_fv(ctx, *arg1, fvs);
            collect_fv(ctx, *arg2, fvs);
            collect_fvs_(ctx, e1, fvs, acc);
            collect_fvs_(ctx, e2, fvs, acc);
        }

        Let { id, ty_id: _, rhs, body } => {
            collect_fvs_(ctx, body, fvs, acc);
            collect_fvs_(ctx, rhs, fvs, acc);
            fvs.remove(id);
        }

        Var(id) => collect_fv(ctx, *id, fvs),

        LetRec { name, ty_id: _, args, rhs, body } => {
            collect_fvs_(ctx, body, fvs, acc);
            fvs.remove(name);

            let mut letrec_fvs: FxHashSet<VarId> = Default::default();
            collect_fvs_(ctx, rhs, &mut letrec_fvs, acc);
            for arg in args {
                letrec_fvs.remove(arg);
            }
            letrec_fvs.remove(name);

            fvs.extend(letrec_fvs.iter().copied());
            let old = acc.insert(*name, letrec_fvs);
            debug_assert_eq!(old, None);
        }

        App(fun, args) => {
            collect_fv(ctx, *fun, fvs);
            for arg in args {
                collect_fv(ctx, *arg, fvs);
            }
        }

        Tuple(args) => {
            for arg in args {
                collect_fv(ctx, *arg, fvs);
            }
        }

        ArrayAlloc { len, elem } => {
            collect_fv(ctx, *len, fvs);
            collect_fv(ctx, *elem, fvs);
        }

        ArrayPut(arg1, arg2, arg3) => {
            collect_fv(ctx, *arg1, fvs);
            collect_fv(ctx, *arg2, fvs);
            collect_fv(ctx, *arg3, fvs);
        }
    }
}

fn collect_fv(ctx: &Ctx, var: VarId, fvs: &mut FxHashSet<VarId>) {
    if !ctx.is_builtin_var(var) {
        fvs.insert(var);
    }
}
