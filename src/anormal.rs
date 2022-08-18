use crate::ast;
use crate::common::*;
use crate::ctx::{Ctx, TypeId, VarId};
use crate::type_check::Type;
use crate::var::CompilerPhase;

#[derive(Debug)]
pub enum Expr {
    Unit,
    Int(i64),
    Float(f64),
    IBinOp(BinOp<IntBinOp>),
    FBinOp(BinOp<FloatBinOp>),
    Neg(VarId),
    FNeg(VarId),
    If(VarId, VarId, Cmp, Box<Expr>, Box<Expr>),
    Let { id: VarId, ty_id: TypeId, rhs: Box<Expr>, body: Box<Expr> },
    Var(VarId),
    LetRec { name: VarId, ty_id: TypeId, args: Vec<VarId>, rhs: Box<Expr>, body: Box<Expr> },
    App(VarId, Vec<VarId>),
    // Tuple allocation
    Tuple(Vec<VarId>),
    // Tuple field read
    TupleGet(VarId, usize),
    // Array allocation
    ArrayAlloc { len: VarId, elem: VarId },
    // Array field read
    ArrayGet(VarId, VarId),
    // Array field write
    ArrayPut(VarId, VarId, VarId),
}

enum TmpLet {
    TmpLet { id: VarId, ty_id: TypeId, rhs: Box<Expr> },
    NoNeed,
}

impl TmpLet {
    fn finish(self, body: Expr) -> Expr {
        match self {
            TmpLet::NoNeed => body,
            TmpLet::TmpLet { id, ty_id, rhs } => Expr::Let { id, ty_id, rhs, body: Box::new(body) },
        }
    }
}

fn mk_let(ctx: &mut Ctx, e: Expr, ty_id: TypeId) -> (TmpLet, VarId) {
    match e {
        Expr::Var(var) => (TmpLet::NoNeed, var),
        _ => {
            let id = ctx.fresh_generated_var(CompilerPhase::ANormal);
            ctx.set_var_type(id, ty_id);
            (TmpLet::TmpLet { id, ty_id, rhs: Box::new(e) }, id)
        }
    }
}

pub fn anormal(ctx: &mut Ctx, expr: ast::Expr) -> Expr {
    anormal_(ctx, expr).0
}

pub fn anormal_(ctx: &mut Ctx, expr: ast::Expr) -> (Expr, TypeId) {
    let int = ctx.int_type_id();
    let float = ctx.float_type_id();
    let unit = ctx.unit_type_id();

    match expr {
        ast::Expr::Unit => (Expr::Unit, unit),
        ast::Expr::Bool(b) => (Expr::Int(if b { 1 } else { 0 }), int),
        ast::Expr::Int(i) => (Expr::Int(i), int),
        ast::Expr::Float(f) => (Expr::Float(f), float),

        ast::Expr::Not(e) => anormal_(
            ctx,
            ast::Expr::If(
                e,
                Box::new(ast::Expr::Bool(false)),
                Box::new(ast::Expr::Bool(true)),
            ),
        ),

        ast::Expr::Neg(e) => {
            let e = anormal(ctx, *e);
            let (tmp, var) = mk_let(ctx, e, int);
            (tmp.finish(Expr::Neg(var)), int)
        }

        ast::Expr::FNeg(e) => {
            let e = anormal(ctx, *e);
            let (tmp, var) = mk_let(ctx, e, float);
            (tmp.finish(Expr::FNeg(var)), float)
        }

        ast::Expr::IntBinOp(e1, op, e2) => {
            let e1 = anormal(ctx, *e1);
            let (tmp1, arg1) = mk_let(ctx, e1, int);
            let e2 = anormal(ctx, *e2);
            let (tmp2, arg2) = mk_let(ctx, e2, int);
            let e = tmp1.finish(tmp2.finish(Expr::IBinOp(BinOp { op, arg1, arg2 })));
            (e, int)
        }

        ast::Expr::FloatBinOp(e1, op, e2) => {
            let e1 = anormal(ctx, *e1);
            let (tmp1, arg1) = mk_let(ctx, e1, float);
            let e2 = anormal(ctx, *e2);
            let (tmp2, arg2) = mk_let(ctx, e2, float);
            let e = tmp1.finish(tmp2.finish(Expr::FBinOp(BinOp { op, arg1, arg2 })));
            (e, float)
        }

        ast::Expr::Cmp(e1, cmp, e2) => {
            let (e1, e1_ty) = anormal_(ctx, *e1);
            let (tmp1, var1) = mk_let(ctx, e1, e1_ty);
            let (e2, e2_ty) = anormal_(ctx, *e2);
            // assert_eq!(e1_ty, e2_ty);
            let (tmp2, var2) = mk_let(ctx, e2, e2_ty);
            let e = tmp1.finish(tmp2.finish(Expr::If(
                var1,
                var2,
                cmp,
                Box::new(Expr::Int(1)),
                Box::new(Expr::Int(0)),
            )));
            (e, int)
        }

        ast::Expr::If(box ast::Expr::Cmp(e1, cmp, e2), then_, else_) => {
            let (e1, e1_ty) = anormal_(ctx, *e1);
            let (tmp1, var1) = mk_let(ctx, e1, e1_ty);
            let (e2, e2_ty) = anormal_(ctx, *e2);
            // assert_eq!(e1_ty, e2_ty);
            let (tmp2, var2) = mk_let(ctx, e2, e2_ty);
            let (then_, ty) = anormal_(ctx, *then_);
            let else_ = anormal(ctx, *else_);
            let e = tmp1.finish(tmp2.finish(Expr::If(
                var1,
                var2,
                cmp,
                Box::new(then_),
                Box::new(else_),
            )));
            (e, ty)
        }

        ast::Expr::If(cond, then_, else_) => {
            let cond = anormal(ctx, *cond);
            let (cond_tmp, cond_var) = mk_let(ctx, cond, int);
            let true_ = anormal(ctx, ast::Expr::Bool(true));
            let (true_tmp, true_var) = mk_let(ctx, true_, int);
            let (then_, ty) = anormal_(ctx, *then_);
            let else_ = anormal(ctx, *else_);
            let e = cond_tmp.finish(true_tmp.finish(Expr::If(
                cond_var,
                true_var,
                Cmp::Equal,
                Box::new(then_),
                Box::new(else_),
            )));
            (e, ty)
        }

        ast::Expr::Let { bndr, rhs, body } => {
            let (rhs, rhs_ty) = anormal_(ctx, *rhs);
            let (body, body_ty) = anormal_(ctx, *body);
            let e = Expr::Let { id: bndr, ty_id: rhs_ty, rhs: Box::new(rhs), body: Box::new(body) };
            (e, body_ty)
        }

        ast::Expr::Var(var) => (Expr::Var(var), ctx.var_type_id(var)),

        ast::Expr::LetRec { bndr, args, rhs, body } => {
            let mut arg_tys: Vec<Type> = Vec::with_capacity(args.len());
            for arg in &args {
                arg_tys.push((*ctx.var_type(*arg)).clone());
            }

            let (rhs, rhs_ty_id) = anormal_(ctx, *rhs);
            let rhs_ty = (*ctx.get_type(rhs_ty_id)).clone();
            let fun_ty = Type::Fun { args: arg_tys, ret: Box::new(rhs_ty) };
            let (body, body_ty) = anormal_(ctx, *body);

            let e = Expr::LetRec {
                name: bndr,
                ty_id: ctx.intern_type(fun_ty),
                args,
                rhs: Box::new(rhs),
                body: Box::new(body),
            };

            (e, body_ty)
        }

        ast::Expr::App { fun, args } => {
            let (fun, fun_ty_id) = anormal_(ctx, *fun);
            let fun_ty = &*ctx.get_type(fun_ty_id);
            let ret_ty: Type = match &fun_ty {
                Type::Fun { args: _, ret } => (**ret).clone(),
                other => panic!("Non-function in function position: {:?} : {:?}", fun, other),
            };
            let ret_ty_id = ctx.intern_type(ret_ty);

            let (fun_tmp, fun_id) = mk_let(ctx, fun, fun_ty_id);

            let mut arg_ids: Vec<VarId> = Vec::with_capacity(args.len());
            let mut arg_tmps: Vec<TmpLet> = Vec::with_capacity(args.len());
            for arg in args {
                let (arg, arg_ty) = anormal_(ctx, arg);
                let (arg_tmp, arg_id) = mk_let(ctx, arg, arg_ty);
                arg_ids.push(arg_id);
                arg_tmps.push(arg_tmp);
            }

            let fun_call = fun_tmp.finish(Expr::App(fun_id, arg_ids));

            let e = arg_tmps
                .into_iter()
                .rev()
                .fold(fun_call, |acc, arg_tmp| arg_tmp.finish(acc));

            (e, ret_ty_id)
        }

        ast::Expr::Tuple(args) => {
            let mut arg_ids: Vec<VarId> = Vec::with_capacity(args.len());
            let mut arg_tmps: Vec<TmpLet> = Vec::with_capacity(args.len());
            let mut arg_tys: Vec<Type> = Vec::with_capacity(args.len());

            for arg in args {
                let (arg, arg_ty_id) = anormal_(ctx, arg);
                let arg_ty = (*ctx.get_type(arg_ty_id)).clone();
                let (arg_tmp, arg_id) = mk_let(ctx, arg, arg_ty_id);
                arg_ids.push(arg_id);
                arg_tmps.push(arg_tmp);
                arg_tys.push(arg_ty);
            }

            let tuple = Expr::Tuple(arg_ids);

            let e = arg_tmps
                .into_iter()
                .rev()
                .fold(tuple, |acc, arg_tmp| arg_tmp.finish(acc));

            (e, ctx.intern_type(Type::Tuple(arg_tys)))
        }

        ast::Expr::LetTuple { bndrs, rhs, body } => {
            let (body, body_ty) = anormal_(ctx, *body);

            let (rhs, rhs_ty) = anormal_(ctx, *rhs);
            let (rhs_tmp, rhs_id) = mk_let(ctx, rhs, rhs_ty);

            let e = bndrs
                .into_iter()
                .enumerate()
                .rev()
                .fold(body, |expr, (bndr_idx, bndr)| {
                    let ty_id = ctx.var_type_id(bndr);
                    Expr::Let {
                        id: bndr,
                        ty_id,
                        rhs: Box::new(Expr::TupleGet(rhs_id, bndr_idx)),
                        body: Box::new(expr),
                    }
                });

            (rhs_tmp.finish(e), body_ty)
        }

        ast::Expr::Array { len, elem } => {
            let (len, len_ty_id) = anormal_(ctx, *len);
            assert_eq!(len_ty_id, int);
            let (elem, elem_ty_id) = anormal_(ctx, *elem);
            let elem_ty = (*ctx.get_type(elem_ty_id)).clone();
            let (len_tmp, len_id) = mk_let(ctx, len, len_ty_id);
            let (elem_tmp, elem_id) = mk_let(ctx, elem, elem_ty_id);

            (
                len_tmp.finish(elem_tmp.finish(Expr::ArrayAlloc { len: len_id, elem: elem_id })),
                ctx.intern_type(Type::Array(Box::new(elem_ty))),
            )
        }

        ast::Expr::Get(e1, e2) => {
            let (e1, e1_ty_id) = anormal_(ctx, *e1);
            let elem_ty = match &*ctx.get_type(e1_ty_id) {
                Type::Array(elem) => (**elem).clone(),
                other => panic!("Non-array type in Get: {:?}", other),
            };
            let (e2, e2_ty_id) = anormal_(ctx, *e2);
            assert_eq!(e2_ty_id, int);
            let (e1_tmp, e1_id) = mk_let(ctx, e1, e1_ty_id);
            let (e2_tmp, e2_id) = mk_let(ctx, e2, e2_ty_id);

            let e = e1_tmp.finish(e2_tmp.finish(Expr::ArrayGet(e1_id, e2_id)));

            (e, ctx.intern_type(elem_ty))
        }

        ast::Expr::Put(e1, e2, e3) => {
            let (e1, e1_ty_id) = anormal_(ctx, *e1);
            // assert!(e1_ty.is_array());
            let (e2, e2_ty_id) = anormal_(ctx, *e2);
            assert_eq!(e2_ty_id, int);
            let (e3, e3_ty_id) = anormal_(ctx, *e3);

            let (e1_tmp, e1_id) = mk_let(ctx, e1, e1_ty_id);
            let (e2_tmp, e2_id) = mk_let(ctx, e2, e2_ty_id);
            let (e3_tmp, e3_id) = mk_let(ctx, e3, e3_ty_id);

            let e =
                e1_tmp.finish(e2_tmp.finish(e3_tmp.finish(Expr::ArrayPut(e1_id, e2_id, e3_id))));

            (e, unit)
        }
    }
}
