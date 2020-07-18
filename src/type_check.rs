// NOTE: This pass also renames binders: after this pass there's no shadowing in the IR.

use crate::ast::*;
use crate::ctx::{Ctx, VarId};
use crate::locals::Locals;
use crate::utils::take;
use crate::var::Uniq;

use cranelift_entity::{entity_impl, PrimaryMap};

use std::ops::{Index, IndexMut};
use std::rc::Rc;

use fxhash::FxHashMap;

pub type TypeEnv = FxHashMap<VarId, TypeIdx>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeIdx(u32);
entity_impl!(TypeIdx, "ty");

pub type TyVar = Uniq;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Float,
    Fun { args: Vec<TypeIdx>, ret: TypeIdx },
    Tuple(Vec<TypeIdx>),
    Array(TypeIdx),
    Var(TyVar),
}

#[derive(Debug)]
pub struct TypeArena {
    arena: PrimaryMap<TypeIdx, Type>,
}

impl Index<TypeIdx> for TypeArena {
    type Output = Type;

    fn index(&self, k: TypeIdx) -> &Type {
        &self.arena[k]
    }
}

impl IndexMut<TypeIdx> for TypeArena {
    fn index_mut(&mut self, k: TypeIdx) -> &mut Type {
        &mut self.arena[k]
    }
}

pub const UNIT_IDX: TypeIdx = TypeIdx(0);
pub const BOOL_IDX: TypeIdx = TypeIdx(1);
pub const INT_IDX: TypeIdx = TypeIdx(2);
pub const FLOAT_IDX: TypeIdx = TypeIdx(3);

impl TypeArena {
    pub fn new() -> Self {
        let mut arena = PrimaryMap::new();
        let unit_idx = arena.push(Type::Unit);
        let bool_idx = arena.push(Type::Bool);
        let int_idx = arena.push(Type::Int);
        let float_idx = arena.push(Type::Float);

        assert_eq!(unit_idx, UNIT_IDX);
        assert_eq!(bool_idx, BOOL_IDX);
        assert_eq!(int_idx, INT_IDX);
        assert_eq!(float_idx, FLOAT_IDX);

        TypeArena { arena }
    }

    pub fn unit(&self) -> TypeIdx {
        UNIT_IDX
    }

    pub fn bool(&self) -> TypeIdx {
        BOOL_IDX
    }

    pub fn int(&self) -> TypeIdx {
        INT_IDX
    }

    pub fn float(&self) -> TypeIdx {
        FLOAT_IDX
    }

    pub fn fun(&mut self, args: Vec<TypeIdx>, ret: TypeIdx) -> TypeIdx {
        self.arena.push(Type::Fun { args, ret })
    }

    pub fn tuple(&mut self, args: Vec<TypeIdx>) -> TypeIdx {
        self.arena.push(Type::Tuple(args))
    }

    pub fn array(&mut self, arg: TypeIdx) -> TypeIdx {
        self.arena.push(Type::Array(arg))
    }

    pub fn var(&mut self, var: TyVar) -> TypeIdx {
        self.arena.push(Type::Var(var))
    }
}

#[derive(Debug)]
pub enum TypeErr {
    /// Can't unify these two types
    UnifyError(Type, Type),
    /// Occurs check failed
    InfiniteType(Type, Type),
    /// Unbound variable
    UnboundVar(VarId),
}

type SubstEnv = FxHashMap<TyVar, Type>;

#[derive(Debug, Clone)]
struct Binder {
    binder: VarId,
    ty: TypeIdx,
}

type Scope = Locals<Rc<str>, Binder>;

pub fn type_check_pgm(
    ctx: &mut Ctx,
    expr: ExprIdx,
    expr_arena: &mut ExprArena,
    type_arena: &mut TypeArena,
    // expr_tys: &mut SecondaryMap<ExprIdx, Option<TypeIdx>>,
) -> Result<(), TypeErr> {
    let mut global_scope: FxHashMap<Rc<str>, Binder> = Default::default();

    for (var_id, ty) in ctx.builtins() {
        let var = ctx.get_var(*var_id);
        let var_name = var.name();
        global_scope.insert(
            var_name,
            Binder {
                binder: *var_id,
                ty: *ty,
            },
        );
    }

    let mut scope: Scope = Locals::new(global_scope);
    let mut subst_env: SubstEnv = Default::default();
    let mut ty_env: TypeEnv = Default::default();
    let ty = type_check(
        ctx,
        &mut ty_env,
        &mut subst_env,
        &mut scope,
        type_arena,
        expr_arena,
        expr,
    )?;
    unify(type_arena, &mut subst_env, UNIT_IDX, ty)?;

    for ty in ty_env.values() {
        norm_ty(type_arena, &subst_env, *ty);
    }

    ctx.extend_type_env(ty_env.into_iter());

    Ok(())
}

fn norm_ty(ty_arena: &mut TypeArena, substs: &SubstEnv, ty_idx: TypeIdx) {
    let ty = ty_arena[ty_idx].clone();
    // deref loop
    loop {
        match ty {
            Type::Unit | Type::Bool | Type::Int | Type::Float => {
                break;
            }
            Type::Fun { args, ret } => {
                for arg in args {
                    norm_ty(ty_arena, substs, arg);
                }
                norm_ty(ty_arena, substs, ret);
                break;
            }
            Type::Tuple(args) => {
                for arg in args {
                    norm_ty(ty_arena, substs, arg);
                }
                break;
            }
            Type::Array(arg) => {
                norm_ty(ty_arena, substs, arg);
                break;
            }
            Type::Var(_) => {
                let deref = deref_ty(ty_arena, substs, ty_idx);
                if deref == ty_idx {
                    break;
                }
            }
        }
    }
}

fn type_check(
    ctx: &mut Ctx, ty_env: &mut TypeEnv, subst_env: &mut SubstEnv, scope: &mut Scope,
    ty_arena: &mut TypeArena, expr_arena: &mut ExprArena, expr: ExprIdx,
) -> Result<TypeIdx, TypeErr> {
    let expr_kind = expr_arena[expr].kind.clone();
    match expr_kind {
        ExprKind::Unit => Ok(UNIT_IDX),
        ExprKind::Bool(_) => Ok(BOOL_IDX),
        ExprKind::Int(_) => Ok(INT_IDX),
        ExprKind::Float(_) => Ok(FLOAT_IDX),

        ExprKind::Not => {
            let e = expr_arena[expr].children[0];
            let e_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, e)?;
            unify(ty_arena, subst_env, BOOL_IDX, e_ty)?;
            Ok(BOOL_IDX)
        }

        ExprKind::Neg => {
            let e = expr_arena[expr].children[0];
            let e_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, e)?;
            unify(ty_arena, subst_env, INT_IDX, e_ty)?;
            Ok(INT_IDX)
        }

        ExprKind::IntBinOp(_op) => {
            let e1 = expr_arena[expr].children[0];
            let e2 = expr_arena[expr].children[1];
            let e1_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, e1)?;
            let e2_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, e2)?;
            unify(ty_arena, subst_env, INT_IDX, e1_ty)?;
            unify(ty_arena, subst_env, INT_IDX, e2_ty)?;
            Ok(INT_IDX)
        }

        ExprKind::FNeg => {
            let e = expr_arena[expr].children[0];
            let e_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, e)?;
            unify(ty_arena, subst_env, FLOAT_IDX, e_ty)?;
            Ok(FLOAT_IDX)
        }

        ExprKind::FloatBinOp(_op) => {
            let e1 = expr_arena[expr].children[0];
            let e2 = expr_arena[expr].children[1];
            let e1_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, e1)?;
            let e2_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, e2)?;
            unify(ty_arena, subst_env, FLOAT_IDX, e1_ty)?;
            unify(ty_arena, subst_env, FLOAT_IDX, e2_ty)?;
            Ok(FLOAT_IDX)
        }

        ExprKind::Cmp(_op) => {
            let e1 = expr_arena[expr].children[0];
            let e2 = expr_arena[expr].children[1];
            let e1_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, e1)?;
            let e2_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, e2)?;
            unify(ty_arena, subst_env, e1_ty, e2_ty)?;
            Ok(BOOL_IDX)
        }

        ExprKind::If => {
            let e1 = expr_arena[expr].children[0];
            let e2 = expr_arena[expr].children[1];
            let e3 = expr_arena[expr].children[2];
            let e1_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, e1)?;
            let e2_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, e2)?;
            let e3_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, e3)?;
            unify(ty_arena, subst_env, e1_ty, BOOL_IDX)?;
            unify(ty_arena, subst_env, e2_ty, e3_ty)?;
            Ok(e2_ty)
        }

        ExprKind::Let { bndr } => {
            let rhs = expr_arena[expr].children[0];
            let body = expr_arena[expr].children[1];
            let bndr_ty = ty_arena.var(ctx.fresh_tyvar());
            ty_env.insert(bndr, bndr_ty.clone());
            let rhs_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, rhs)?;
            unify(ty_arena, subst_env, bndr_ty, rhs_ty)?;
            scope.new_scope();
            scope.add(
                ctx.var_name(bndr),
                Binder {
                    binder: bndr,
                    ty: bndr_ty,
                },
            );
            let ret = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, body);
            scope.pop_scope();
            ret
        }

        ExprKind::Var(var) => match scope.get(&ctx.var_name(var)) {
            Some(Binder { binder, ty }) => {
                expr_arena[expr].kind = ExprKind::Var(*binder);
                Ok(*ty)
            }
            None => Err(TypeErr::UnboundVar(var)),
        },

        ExprKind::LetRec { bndr, ref args } => {
            let rhs = expr_arena[expr].children[0];
            let body = expr_arena[expr].children[1];

            // Type variables for the arguments
            let mut arg_tys: Vec<TypeIdx> = Vec::with_capacity(args.len());
            for arg in args {
                let arg_ty = ty_arena.var(ctx.fresh_tyvar());
                arg_tys.push(arg_ty);
                ty_env.insert(*arg, arg_ty);
            }

            // Type variable for the RHS
            let rhs_ty = ty_arena.var(ctx.fresh_tyvar());

            // We can now give type to the recursive function
            let fun_ty = ty_arena.fun(arg_tys.clone(), rhs_ty);

            ty_env.insert(bndr, fun_ty);

            // RHS and body will be type checked with `name` and args in scope
            scope.new_scope(); // new scope for function
            scope.add(
                ctx.var_name(bndr),
                Binder {
                    binder: bndr,
                    ty: fun_ty,
                },
            );
            scope.new_scope(); // new scope for args

            for (binder, arg_ty) in args.iter().zip(arg_tys.iter()) {
                scope.add(
                    ctx.var_name(*binder),
                    Binder {
                        binder: *binder,
                        ty: arg_ty.clone(),
                    },
                );
            }

            // Type check RHS with fun and args in scope
            let rhs_ty_ = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, rhs)?;
            unify(ty_arena, subst_env, rhs_ty, rhs_ty_)?;
            // Type check body with just the fun in scope
            scope.pop_scope();
            let ret = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, body);
            // Reset environment
            scope.pop_scope();
            ret
        }

        ExprKind::App => {
            let children = expr_arena[expr].children.clone();
            let ret_ty = ty_arena.var(ctx.fresh_tyvar());
            let mut arg_tys: Vec<TypeIdx> = Vec::with_capacity(children.len() - 1);
            for arg in &children[1..] {
                arg_tys.push(type_check(
                    ctx, ty_env, subst_env, scope, ty_arena, expr_arena, *arg,
                )?);
            }
            let fun_ty = ty_arena.fun(arg_tys, ret_ty);
            let fun_ty_ = type_check(
                ctx,
                ty_env,
                subst_env,
                scope,
                ty_arena,
                expr_arena,
                expr_arena[expr].children[0],
            )?;
            unify(ty_arena, subst_env, fun_ty, fun_ty_)?;
            Ok(ret_ty)
        }

        ExprKind::Tuple => {
            let args = expr_arena[expr].children.clone();
            let mut arg_tys: Vec<TypeIdx> = Vec::with_capacity(args.len());
            for arg in args {
                arg_tys.push(type_check(
                    ctx, ty_env, subst_env, scope, ty_arena, expr_arena, arg,
                )?);
            }
            Ok(ty_arena.tuple(arg_tys))
        }

        ExprKind::LetTuple { ref bndrs } => {
            let rhs = expr_arena[expr].children[0];
            let body = expr_arena[expr].children[1];

            let mut arg_tys: Vec<TypeIdx> = Vec::with_capacity(bndrs.len());
            for bndr in bndrs {
                let bndr_ty = ty_arena.var(ctx.fresh_tyvar());
                ty_env.insert(*bndr, bndr_ty);
                arg_tys.push(bndr_ty);
            }
            let tuple_ty = ty_arena.tuple(arg_tys.clone());
            let rhs_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, rhs)?;
            unify(ty_arena, subst_env, rhs_ty, tuple_ty)?;
            scope.new_scope();
            for (bndr, bndr_type) in bndrs.iter().zip(arg_tys.into_iter()) {
                scope.add(
                    ctx.var_name(*bndr),
                    Binder {
                        binder: *bndr,
                        ty: bndr_type,
                    },
                );
            }
            let ret = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, body);
            scope.pop_scope();
            ret
        }

        ExprKind::Array => {
            let len = expr_arena[expr].children[0];
            let elem = expr_arena[expr].children[1];
            let len_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, len)?;
            unify(ty_arena, subst_env, len_ty, INT_IDX)?;
            let elem_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, elem)?;
            Ok(ty_arena.array(elem_ty))
        }

        ExprKind::Get => {
            let e1 = expr_arena[expr].children[0];
            let e2 = expr_arena[expr].children[1];
            let array_elem_ty = ty_arena.var(ctx.fresh_tyvar());
            let array_ty = ty_arena.array(array_elem_ty);
            let e1_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, e1)?;
            unify(ty_arena, subst_env, e1_ty, array_ty)?;
            let e2_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, e2)?;
            unify(ty_arena, subst_env, e2_ty, INT_IDX)?;
            Ok(array_elem_ty)
        }

        ExprKind::Put => {
            let e1 = expr_arena[expr].children[0];
            let e2 = expr_arena[expr].children[1];
            let e3 = expr_arena[expr].children[2];
            let array_elem_ty = ty_arena.var(ctx.fresh_tyvar());
            let array_ty = ty_arena.array(array_elem_ty);
            let e1_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, e1)?;
            unify(ty_arena, subst_env, e1_ty, array_ty)?;
            let e2_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, e2)?;
            unify(ty_arena, subst_env, e2_ty, INT_IDX)?;
            let e3_ty = type_check(ctx, ty_env, subst_env, scope, ty_arena, expr_arena, e3)?;
            unify(ty_arena, subst_env, e3_ty, array_elem_ty)?;
            Ok(UNIT_IDX)
        }
    }
}

fn unify(
    arena: &mut TypeArena, subst_env: &mut SubstEnv, ty1: TypeIdx, ty2: TypeIdx,
) -> Result<(), TypeErr> {
    let ty1 = deref_ty(arena, subst_env, ty1).clone();
    let ty2 = deref_ty(arena, subst_env, ty2).clone();

    // println!("substs: {:?}", substs);
    // println!("unify {:?} ~ {:?}", ty1, ty2);

    let ty1_ = arena[ty1].clone();
    let ty2_ = arena[ty2].clone();

    match (&ty1_, &ty2_) {
        (Type::Unit, Type::Unit)
        | (Type::Bool, Type::Bool)
        | (Type::Int, Type::Int)
        | (Type::Float, Type::Float) => Ok(()),
        (
            Type::Fun {
                args: args1,
                ret: ret1,
            },
            Type::Fun {
                args: args2,
                ret: ret2,
            },
        ) => {
            if args1.len() != args2.len() {
                return Err(TypeErr::UnifyError(ty1_.clone(), ty2_.clone()));
            }
            for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                unify(arena, subst_env, *arg1, *arg2)?;
            }
            unify(arena, subst_env, *ret1, *ret2)
        }

        (Type::Var(var1), Type::Var(var2)) if var1 == var2 => Ok(()),

        (Type::Var(var), ty) => {
            if occurs_check(arena, subst_env, *var, ty2) {
                return Err(TypeErr::InfiniteType(ty1_.clone(), ty2_.clone()));
            }
            // println!("unify {:?} ~ {:?}", var, ty);
            subst_env.insert(*var, ty.clone());
            Ok(())
        }

        (ty, Type::Var(var)) => {
            if occurs_check(arena, subst_env, *var, ty1) {
                return Err(TypeErr::InfiniteType(ty1_.clone(), ty2_.clone()));
            }
            // println!("unify {:?} ~ {:?}", var, ty);
            subst_env.insert(*var, ty.clone());
            Ok(())
        }

        (Type::Tuple(args1), Type::Tuple(args2)) => {
            if args1.len() != args2.len() {
                return Err(TypeErr::UnifyError(ty1_.clone(), ty2_.clone()));
            }
            for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                unify(arena, subst_env, *arg1, *arg2)?;
            }
            Ok(())
        }

        (Type::Array(ty1), Type::Array(ty2)) => unify(arena, subst_env, *ty1, *ty2),

        _ => Err(TypeErr::UnifyError(ty1_, ty2_)),
    }
}

fn occurs_check(arena: &mut TypeArena, subst: &SubstEnv, var: TyVar, ty: TypeIdx) -> bool {
    let ty_ = deref_ty(arena, subst, ty);
    match arena[ty_].clone() {
        Type::Unit | Type::Bool | Type::Int | Type::Float => false,
        Type::Fun { args, ret } => {
            args.iter().any(|ty| occurs_check(arena, subst, var, *ty))
                || occurs_check(arena, subst, var, ret)
        }
        Type::Tuple(args) => args.iter().any(|ty| occurs_check(arena, subst, var, *ty)),
        Type::Array(ty) => occurs_check(arena, subst, var, ty),
        Type::Var(var_) => var == var_,
    }
}

fn deref_ty(arena: &mut TypeArena, subst: &SubstEnv, ty: TypeIdx) -> TypeIdx {
    loop {
        match &arena[ty] {
            Type::Var(tyvar) => match subst.get(tyvar) {
                None => {
                    return ty;
                }
                Some(ty_) => {
                    arena[ty] = ty_.clone();
                }
            },
            _ => {
                return ty;
            }
        }
    }
}

use std::fmt;

impl Type {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        use Type::*;
        match self {
            Unit => w.write_str("()"),
            Bool => w.write_str("bool"),
            Int => w.write_str("int"),
            Float => w.write_str("float"),
            Fun { args, ret } => {
                for arg in args {
                    arg.pp(ctx, w)?;
                    w.write_str(" -> ")?;
                }
                ret.pp(ctx, w)
            }
            Tuple(args) => {
                assert!(!args.is_empty());
                args[0].pp(ctx, w)?;
                for arg in &args[1..] {
                    w.write_str(" * ")?;
                    arg.pp(ctx, w)?;
                }
                Ok(())
            }
            Array(ty) => {
                w.write_str("[")?;
                ty.pp(ctx, w)?;
                w.write_str("]")
            }
            Var(var) => write!(w, "{}", var),
        }
    }
}

impl TypeIdx {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        ctx.get_type(*self).pp(ctx, w)
    }
}
