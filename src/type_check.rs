use fxhash::FxHashMap;
use std::rc::Rc;

use crate::ctx::{Ctx, VarId};
use crate::locals::Locals;
use crate::parser::Expr;
use crate::utils::take;
use crate::var::Uniq;

pub type TyVar = Uniq;

pub type TypeEnv = FxHashMap<VarId, Type>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Float,
    Fun { args: Vec<Type>, ret: Box<Type> },
    Tuple(Vec<Type>),
    Array(Box<Type>),
    Var(TyVar),
}

impl Type {
    pub fn is_array(&self) -> bool {
        match self {
            Type::Array(_) => true,
            _ => false,
        }
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
    ty: Type,
}

type Scope = Locals<Rc<str>, Binder>;

pub fn type_check_pgm(ctx: &mut Ctx, expr: &mut Expr) -> Result<(), TypeErr> {
    let mut global_scope: FxHashMap<Rc<str>, Binder> = Default::default();

    for (var_id, ty_id) in ctx.builtins() {
        let var = ctx.get_var(*var_id);
        let var_name = var.name();
        let ty = ctx.get_type(*ty_id);
        global_scope.insert(var_name, Binder { binder: *var_id, ty: (&*ty).clone() });
    }

    let mut scope: Scope = Locals::new(global_scope);
    let mut subst_env: SubstEnv = Default::default();
    let mut ty_env: TypeEnv = Default::default();
    let ty = type_check(ctx, &mut ty_env, &mut subst_env, &mut scope, expr)?;
    unify(&mut subst_env, &Type::Unit, &ty)?;

    for ty in ty_env.values_mut() {
        take(ty, |ty| norm_ty(&subst_env, ty));
    }

    ctx.extend_type_env(ty_env.into_iter());

    Ok(())
}

fn norm_ty(substs: &SubstEnv, ty: Type) -> Type {
    match ty {
        Type::Unit | Type::Bool | Type::Int | Type::Float => ty,
        Type::Fun { args, ret } => Type::Fun {
            args: args.into_iter().map(|ty| norm_ty(substs, ty)).collect(),
            ret: Box::new(norm_ty(substs, *ret)),
        },
        Type::Tuple(args) => Type::Tuple(args.into_iter().map(|ty| norm_ty(substs, ty)).collect()),
        Type::Array(ty) => Type::Array(Box::new(norm_ty(substs, *ty))),
        Type::Var(_) => norm_ty(substs, deref_ty(substs, &ty).clone()),
    }
}

fn deref_ty<'a>(subst: &'a SubstEnv, mut ty: &'a Type) -> &'a Type {
    loop {
        match ty {
            Type::Var(tyvar) => match subst.get(tyvar) {
                None => {
                    return ty;
                }
                Some(ty_) => {
                    ty = ty_;
                }
            },
            _ => {
                return ty;
            }
        }
    }
}

fn occurs_check(subst: &SubstEnv, var: TyVar, ty: &Type) -> bool {
    match deref_ty(subst, ty) {
        Type::Unit | Type::Bool | Type::Int | Type::Float => false,
        Type::Fun { args, ret } => {
            args.iter().any(|ty| occurs_check(subst, var, ty)) || occurs_check(subst, var, ret)
        }
        Type::Tuple(args) => args.iter().any(|ty| occurs_check(subst, var, ty)),
        Type::Array(ty) => occurs_check(subst, var, ty),
        Type::Var(var_) => var == *var_,
    }
}

fn type_check(
    ctx: &mut Ctx, ty_env: &mut TypeEnv, subst_env: &mut SubstEnv, scope: &mut Scope,
    expr: &mut Expr,
) -> Result<Type, TypeErr> {
    match expr {
        Expr::Unit => Ok(Type::Unit),
        Expr::Bool(_) => Ok(Type::Bool),
        Expr::Int(_) => Ok(Type::Int),
        Expr::Float(_) => Ok(Type::Float),

        Expr::Not(e) => {
            let e_ty = type_check(ctx, ty_env, subst_env, scope, e)?;
            unify(subst_env, &Type::Bool, &e_ty)?;
            Ok(Type::Bool)
        }

        Expr::Neg(e) => {
            let e_ty = type_check(ctx, ty_env, subst_env, scope, e)?;
            unify(subst_env, &Type::Int, &e_ty)?;
            Ok(Type::Int)
        }

        Expr::IntBinOp(e1, _, e2) => {
            let e1_ty = type_check(ctx, ty_env, subst_env, scope, e1)?;
            let e2_ty = type_check(ctx, ty_env, subst_env, scope, e2)?;
            unify(subst_env, &Type::Int, &e1_ty)?;
            unify(subst_env, &Type::Int, &e2_ty)?;
            Ok(Type::Int)
        }

        Expr::FNeg(e) => {
            let e_ty = type_check(ctx, ty_env, subst_env, scope, e)?;
            unify(subst_env, &Type::Float, &e_ty)?;
            Ok(Type::Float)
        }

        Expr::FloatBinOp(e1, _, e2) => {
            let e1_ty = type_check(ctx, ty_env, subst_env, scope, e1)?;
            let e2_ty = type_check(ctx, ty_env, subst_env, scope, e2)?;
            unify(subst_env, &Type::Float, &e1_ty)?;
            unify(subst_env, &Type::Float, &e2_ty)?;
            Ok(Type::Float)
        }

        Expr::Cmp(e1, _, e2) => {
            let e1_ty = type_check(ctx, ty_env, subst_env, scope, e1)?;
            let e2_ty = type_check(ctx, ty_env, subst_env, scope, e2)?;
            unify(subst_env, &e1_ty, &e2_ty)?;
            Ok(Type::Bool)
        }

        Expr::If(e1, e2, e3) => {
            let e1_ty = type_check(ctx, ty_env, subst_env, scope, e1)?;
            let e2_ty = type_check(ctx, ty_env, subst_env, scope, e2)?;
            let e3_ty = type_check(ctx, ty_env, subst_env, scope, e3)?;
            unify(subst_env, &e1_ty, &Type::Bool)?;
            unify(subst_env, &e2_ty, &e3_ty)?;
            Ok(e2_ty)
        }

        Expr::Let { bndr, ref mut rhs, body } => {
            let bndr_ty = Type::Var(ctx.fresh_tyvar());
            ty_env.insert(*bndr, bndr_ty.clone());
            let rhs_ty = type_check(ctx, ty_env, subst_env, scope, rhs)?;
            unify(subst_env, &bndr_ty, &rhs_ty)?;
            scope.new_scope();
            scope.add(ctx.var_name(*bndr), Binder { binder: *bndr, ty: bndr_ty });
            let ret = type_check(ctx, ty_env, subst_env, scope, body);
            scope.pop_scope();
            ret
        }

        Expr::Var(ref mut var) => match scope.get(&ctx.var_name(*var)) {
            Some(Binder { binder, ty }) => {
                *var = *binder;
                Ok(ty.clone())
            }
            None => Err(TypeErr::UnboundVar(*var)),
        },

        Expr::LetRec { bndr, ref args, rhs, body } => {
            // Type variables for the arguments
            let mut arg_tys: Vec<Type> = Vec::with_capacity(args.len());
            for arg in args {
                let arg_ty = Type::Var(ctx.fresh_tyvar());
                arg_tys.push(arg_ty.clone());
                ty_env.insert(*arg, arg_ty);
            }

            // Type variable for the RHS
            let rhs_ty = Type::Var(ctx.fresh_tyvar());

            // We can now give type to the recursive function
            let fun_ty = Type::Fun { args: arg_tys.clone(), ret: Box::new(rhs_ty.clone()) };

            ty_env.insert(*bndr, fun_ty.clone());

            // RHS and body will be type checked with `name` and args in scope
            scope.new_scope(); // new scope for function
            scope.add(ctx.var_name(*bndr), Binder { binder: *bndr, ty: fun_ty });
            scope.new_scope(); // new scope for args

            for (binder, arg_ty) in args.iter().zip(arg_tys.iter()) {
                scope.add(
                    ctx.var_name(*binder),
                    Binder { binder: *binder, ty: arg_ty.clone() },
                );
            }

            // Type check RHS with fun and args in scope
            let rhs_ty_ = type_check(ctx, ty_env, subst_env, scope, rhs)?;
            unify(subst_env, &rhs_ty, &rhs_ty_)?;
            // Type check body with just the fun in scope
            scope.pop_scope();
            let ret = type_check(ctx, ty_env, subst_env, scope, body);
            // Reset environment
            scope.pop_scope();
            ret
        }

        Expr::App { fun, args } => {
            let ret_ty = Type::Var(ctx.fresh_tyvar());
            let mut arg_tys: Vec<Type> = Vec::with_capacity(args.len());
            for arg in args {
                arg_tys.push(type_check(ctx, ty_env, subst_env, scope, arg)?);
            }
            let fun_ty = Type::Fun { args: arg_tys, ret: Box::new(ret_ty.clone()) };
            let fun_ty_ = type_check(ctx, ty_env, subst_env, scope, fun)?;
            unify(subst_env, &fun_ty, &fun_ty_)?;
            Ok(ret_ty)
        }

        Expr::Tuple(args) => {
            let mut arg_tys: Vec<Type> = Vec::with_capacity(args.len());
            for arg in args {
                arg_tys.push(type_check(ctx, ty_env, subst_env, scope, arg)?);
            }
            Ok(Type::Tuple(arg_tys))
        }

        Expr::LetTuple { ref bndrs, rhs, body } => {
            let mut arg_tys: Vec<Type> = Vec::with_capacity(bndrs.len());
            for bndr in bndrs {
                let bndr_ty = Type::Var(ctx.fresh_tyvar());
                ty_env.insert(*bndr, bndr_ty.clone());
                arg_tys.push(bndr_ty);
            }
            let tuple_ty = Type::Tuple(arg_tys.clone());
            let rhs_ty = type_check(ctx, ty_env, subst_env, scope, rhs)?;
            unify(subst_env, &rhs_ty, &tuple_ty)?;
            scope.new_scope();
            for (bndr, bndr_type) in bndrs.iter().zip(arg_tys.into_iter()) {
                scope.add(ctx.var_name(*bndr), Binder { binder: *bndr, ty: bndr_type });
            }
            let ret = type_check(ctx, ty_env, subst_env, scope, body);
            scope.pop_scope();
            ret
        }

        Expr::Array(e1, e2) => {
            let e1_ty = type_check(ctx, ty_env, subst_env, scope, e1)?;
            unify(subst_env, &e1_ty, &Type::Int)?;
            let e2_ty = type_check(ctx, ty_env, subst_env, scope, e2)?;
            Ok(Type::Array(Box::new(e2_ty)))
        }

        Expr::Get(e1, e2) => {
            let array_elem_ty = Type::Var(ctx.fresh_tyvar());
            let array_ty = Type::Array(Box::new(array_elem_ty.clone()));
            let e1_ty = type_check(ctx, ty_env, subst_env, scope, e1)?;
            unify(subst_env, &e1_ty, &array_ty)?;
            let e2_ty = type_check(ctx, ty_env, subst_env, scope, e2)?;
            unify(subst_env, &e2_ty, &Type::Int)?;
            Ok(array_elem_ty)
        }

        Expr::Put(e1, e2, e3) => {
            let array_elem_ty = Type::Var(ctx.fresh_tyvar());
            let array_ty = Type::Array(Box::new(array_elem_ty.clone()));
            let e1_ty = type_check(ctx, ty_env, subst_env, scope, e1)?;
            unify(subst_env, &e1_ty, &array_ty)?;
            let e2_ty = type_check(ctx, ty_env, subst_env, scope, e2)?;
            unify(subst_env, &e2_ty, &Type::Int)?;
            let e3_ty = type_check(ctx, ty_env, subst_env, scope, e3)?;
            unify(subst_env, &e3_ty, &array_elem_ty)?;
            Ok(Type::Unit)
        }
    }
}

fn unify(subst_env: &mut SubstEnv, ty1: &Type, ty2: &Type) -> Result<(), TypeErr> {
    let ty1 = deref_ty(subst_env, ty1).clone();
    let ty2 = deref_ty(subst_env, ty2).clone();

    // println!("substs: {:?}", substs);
    // println!("unify {:?} ~ {:?}", ty1, ty2);

    match (&ty1, &ty2) {
        (Type::Unit, Type::Unit)
        | (Type::Bool, Type::Bool)
        | (Type::Int, Type::Int)
        | (Type::Float, Type::Float) => Ok(()),
        (Type::Fun { args: args1, ret: ret1 }, Type::Fun { args: args2, ret: ret2 }) => {
            if args1.len() != args2.len() {
                return Err(TypeErr::UnifyError(ty1.clone(), ty2.clone()));
            }
            for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                unify(subst_env, arg1, arg2)?;
            }
            unify(subst_env, &*ret1, &*ret2)
        }

        (Type::Var(var1), Type::Var(var2)) if var1 == var2 => Ok(()),

        (Type::Var(var), ty) | (ty, Type::Var(var)) => {
            if occurs_check(subst_env, *var, ty) {
                return Err(TypeErr::InfiniteType(ty1, ty2));
            }
            subst_env.insert(*var, ty.clone());
            Ok(())
        }

        (Type::Tuple(args1), Type::Tuple(args2)) => {
            if args1.len() != args2.len() {
                return Err(TypeErr::UnifyError(ty1.clone(), ty2.clone()));
            }
            for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                unify(subst_env, arg1, arg2)?;
            }
            Ok(())
        }

        (Type::Array(ty1), Type::Array(ty2)) => unify(subst_env, ty1, ty2),

        _ => Err(TypeErr::UnifyError(ty1.clone(), ty2.clone())),
    }
}

/*
#[test]
fn unify_test_1() {
    let mut tyvar_cnt = 0;
    let mut substs = HashMap::new();

    let ty1 = Type::Int;
    let ty2 = new_tyvar(&mut tyvar_cnt);
    unify(&mut substs, &ty1, &ty2).unwrap();
    assert_eq!(deref_ty(&substs, &ty2), &Type::Int);
    assert_eq!(deref_ty(&substs, &ty1), &Type::Int);

    let ty3 = new_tyvar(&mut tyvar_cnt);
    unify(&mut substs, &ty2, &ty3).unwrap();
    assert_eq!(deref_ty(&substs, &ty2), &Type::Int);
    assert_eq!(deref_ty(&substs, &ty3), &Type::Int);
}

#[test]
fn unify_test_2() {
    let mut tyvar_cnt = 0;
    let mut substs = HashMap::new();

    let ty1 = Type::Int;
    let ty2 = new_tyvar(&mut tyvar_cnt);
    let ty3 = new_tyvar(&mut tyvar_cnt);
    let ty4 = new_tyvar(&mut tyvar_cnt);
    let ty5 = new_tyvar(&mut tyvar_cnt);

    unify(&mut substs, &ty2, &ty3).unwrap();
    unify(&mut substs, &ty2, &ty4).unwrap();
    unify(&mut substs, &ty2, &ty5).unwrap();
    unify(&mut substs, &ty5, &ty1).unwrap();

    assert_eq!(deref_ty(&substs, &ty1), &Type::Int);
    assert_eq!(deref_ty(&substs, &ty2), &Type::Int);
    assert_eq!(deref_ty(&substs, &ty3), &Type::Int);
    assert_eq!(deref_ty(&substs, &ty4), &Type::Int);
    assert_eq!(deref_ty(&substs, &ty5), &Type::Int);
}
*/
