/*

Unification
-----------

Find substitutions that make two types the same.

 */

use std::collections::HashMap;

use crate::parser::{Binder, Expr};
use crate::type_check_env::Locals;

/// Type variables are represented as unique integers.
pub type TyVar = u64;

#[derive(Debug, Clone, PartialEq, Eq)]
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

/// Create initial type environment with built-is stuff.
fn mk_type_env() -> HashMap<String, Type> {
    let mut env = HashMap::new();

    // float -> float
    let float_float = Type::Fun {
        args: vec![Type::Float],
        ret: Box::new(Type::Float),
    };

    // float -> int
    let float_int = Type::Fun {
        args: vec![Type::Float],
        ret: Box::new(Type::Int),
    };

    env.insert(
        "print_int".to_owned(),
        Type::Fun {
            args: vec![Type::Int],
            ret: Box::new(Type::Unit),
        },
    );
    env.insert(
        "print_newline".to_owned(),
        Type::Fun {
            args: vec![Type::Unit],
            ret: Box::new(Type::Unit),
        },
    );
    env.insert(
        "float_of_int".to_owned(),
        Type::Fun {
            args: vec![Type::Int],
            ret: Box::new(Type::Float),
        },
    );
    env.insert("int_of_float".to_owned(), float_int.clone());
    env.insert("truncate".to_owned(), float_int.clone());
    env.insert("abs_float".to_owned(), float_float.clone());
    env.insert("sqrt".to_owned(), float_float.clone());
    env.insert("sin".to_owned(), float_float.clone());
    env.insert("cos".to_owned(), float_float.clone());
    env
}

fn new_tyvar(tyvar_cnt: &mut u64) -> Type {
    let tyvar = *tyvar_cnt;
    *tyvar_cnt = *tyvar_cnt + 1;
    Type::Var(tyvar)
}

#[derive(Debug)]
pub enum TypeErr {
    /// Can't unify these two types
    UnifyError(Type, Type),
    /// Occurs check failed
    InfiniteType(Type, Type),
    /// Unbound variable
    UnboundVar(String),
}

/*
pub fn type_check(expr: &Expr) -> Result<Type, TypeErr> {
    let mut tyvar_cnt = 0;
    let mut env = Locals::new(mk_type_env());
    let mut substs = HashMap::new();
    type_check_(&mut tyvar_cnt, &mut substs, &mut env, expr).map(|ty| norm_ty(&substs, ty))
}
*/

pub fn type_check_pgm(expr: &Expr) -> Result<(), TypeErr> {
    let mut tyvar_cnt = 0;
    let mut env = Locals::new(mk_type_env());
    let mut substs = HashMap::new();
    let ty =
        type_check_(&mut tyvar_cnt, &mut substs, &mut env, expr).map(|ty| norm_ty(&substs, ty))?;
    unify(&mut substs, &Type::Unit, &ty)
}

fn norm_ty(substs: &HashMap<TyVar, Type>, ty: Type) -> Type {
    match ty {
        Type::Unit | Type::Bool | Type::Int | Type::Float => ty.clone(),
        Type::Fun { args, ret } => Type::Fun {
            args: args.into_iter().map(|ty| norm_ty(substs, ty)).collect(),
            ret: Box::new(norm_ty(substs, *ret)),
        },
        Type::Tuple(args) => Type::Tuple(args.into_iter().map(|ty| norm_ty(substs, ty)).collect()),
        Type::Array(ty) => Type::Array(Box::new(norm_ty(substs, *ty))),
        Type::Var(_) => deref_ty(substs, &ty).clone(),
    }
}

fn deref_ty<'a>(subst: &'a HashMap<TyVar, Type>, mut ty: &'a Type) -> &'a Type {
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

fn occurs_check(subst: &HashMap<TyVar, Type>, var: TyVar, ty: &Type) -> bool {
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

fn type_check_(
    tyvar_cnt: &mut u64,
    substs: &mut HashMap<TyVar, Type>,
    env: &mut Locals,
    expr: &Expr,
) -> Result<Type, TypeErr> {
    match expr {
        Expr::Unit => Ok(Type::Unit),
        Expr::Bool(_) => Ok(Type::Bool),
        Expr::Int(_) => Ok(Type::Int),
        Expr::Float(_) => Ok(Type::Float),
        Expr::Not(e) => {
            let e_ty = type_check_(tyvar_cnt, substs, env, e)?;
            unify(substs, &Type::Bool, &e_ty)?;
            Ok(Type::Bool)
        }
        Expr::Neg(e) => {
            let e_ty = type_check_(tyvar_cnt, substs, env, e)?;
            unify(substs, &Type::Int, &e_ty)?;
            Ok(Type::Int)
        }
        Expr::Add(e1, e2) | Expr::Sub(e1, e2) => {
            let e1_ty = type_check_(tyvar_cnt, substs, env, e1)?;
            let e2_ty = type_check_(tyvar_cnt, substs, env, e2)?;
            unify(substs, &Type::Int, &e1_ty)?;
            unify(substs, &Type::Int, &e2_ty)?;
            Ok(Type::Int)
        }
        Expr::FNeg(e) => {
            let e_ty = type_check_(tyvar_cnt, substs, env, e)?;
            unify(substs, &Type::Float, &e_ty)?;
            Ok(Type::Float)
        }
        Expr::FAdd(e1, e2) | Expr::FSub(e1, e2) | Expr::FMul(e1, e2) | Expr::FDiv(e1, e2) => {
            let e1_ty = type_check_(tyvar_cnt, substs, env, e1)?;
            let e2_ty = type_check_(tyvar_cnt, substs, env, e2)?;
            unify(substs, &Type::Float, &e1_ty)?;
            unify(substs, &Type::Float, &e2_ty)?;
            Ok(Type::Float)
        }
        Expr::Eq(e1, e2) | Expr::Le(e1, e2) => {
            let e1_ty = type_check_(tyvar_cnt, substs, env, e1)?;
            let e2_ty = type_check_(tyvar_cnt, substs, env, e2)?;
            unify(substs, &e1_ty, &e2_ty)?;
            Ok(Type::Bool)
        }
        Expr::If(e1, e2, e3) => {
            let e1_ty = type_check_(tyvar_cnt, substs, env, e1)?;
            let e2_ty = type_check_(tyvar_cnt, substs, env, e2)?;
            let e3_ty = type_check_(tyvar_cnt, substs, env, e3)?;
            unify(substs, &e1_ty, &Type::Bool)?;
            unify(substs, &e2_ty, &e3_ty)?;
            Ok(e2_ty)
        }
        Expr::Let {
            ref id,
            ref rhs,
            body,
        } => {
            let bndr_type = new_tyvar(tyvar_cnt);
            let rhs_type = type_check_(tyvar_cnt, substs, env, rhs)?;
            unify(substs, &bndr_type, &rhs_type)?;
            // FIXME: string clone
            env.new_scope();
            env.add(id.clone(), bndr_type);
            let ret = type_check_(tyvar_cnt, substs, env, body);
            env.pop_scope();
            ret
        }
        Expr::Var(var) => match env.get(var) {
            Some(ty) => Ok(ty.clone()),
            None => Err(TypeErr::UnboundVar(var.clone())),
        },
        Expr::LetRec {
            name,
            args,
            rhs,
            body,
        } => {
            // Type variables for the arguments
            let mut arg_tys: Vec<Type> = Vec::with_capacity(args.len());
            for binder in args {
                arg_tys.push(match binder {
                    Binder::Id(_) => new_tyvar(tyvar_cnt),
                    Binder::Unit => Type::Unit,
                });
            }

            // println!("name={}, args={:?}, arg_tys={:?}", name, args, arg_tys);

            // Type variable for the RHS
            let rhs_ty = new_tyvar(tyvar_cnt);
            // We can now give type to the recursive function
            let fun_ty = Type::Fun {
                args: arg_tys.clone(),
                ret: Box::new(rhs_ty.clone()),
            };
            // RHS and body will be type checked with `name` and args in scope
            env.new_scope(); // new scope for function
            env.add(name.clone(), fun_ty.clone());
            env.new_scope(); // new scope for args
            for (binder, arg_ty) in args.iter().zip(arg_tys.iter()) {
                match binder {
                    Binder::Unit => {}
                    Binder::Id(id) => {
                        env.add(id.clone(), arg_ty.clone());
                    }
                }
            }
            // Type check RHS with fun and args in scope
            let rhs_ty_ = type_check_(tyvar_cnt, substs, env, rhs)?;
            unify(substs, &rhs_ty, &rhs_ty_)?;
            // Type check body with just the fun in scope
            env.pop_scope();
            let ret = type_check_(tyvar_cnt, substs, env, body);
            // Reset environment
            env.pop_scope();
            ret
        }
        Expr::App { fun, args } => {
            let ret_ty = new_tyvar(tyvar_cnt);
            let mut arg_tys: Vec<Type> = Vec::with_capacity(args.len());
            for arg in args {
                arg_tys.push(type_check_(tyvar_cnt, substs, env, arg)?);
            }
            let fun_ty = Type::Fun {
                args: arg_tys,
                ret: Box::new(ret_ty.clone()),
            };
            let fun_ty_ = type_check_(tyvar_cnt, substs, env, fun)?;
            unify(substs, &fun_ty, &fun_ty_)?;
            Ok(ret_ty)
        }
        Expr::Tuple(args) => {
            let mut arg_tys: Vec<Type> = Vec::with_capacity(args.len());
            for arg in args {
                arg_tys.push(type_check_(tyvar_cnt, substs, env, arg)?);
            }
            Ok(Type::Tuple(arg_tys))
        }
        Expr::LetTuple { bndrs, rhs, body } => {
            let mut bndr_tys: Vec<Type> = Vec::with_capacity(bndrs.len());
            for _ in bndrs {
                bndr_tys.push(new_tyvar(tyvar_cnt));
            }
            let tuple_ty = Type::Tuple(bndr_tys.clone());
            let rhs_ty = type_check_(tyvar_cnt, substs, env, rhs)?;
            unify(substs, &rhs_ty, &tuple_ty)?;
            env.new_scope();
            for (bndr, bndr_type) in bndrs.iter().zip(bndr_tys.into_iter()) {
                env.add(bndr.clone(), bndr_type);
            }
            let ret = type_check_(tyvar_cnt, substs, env, body);
            env.pop_scope();
            ret
        }
        Expr::Array(e1, e2) => {
            let e1_ty = type_check_(tyvar_cnt, substs, env, e1)?;
            unify(substs, &e1_ty, &Type::Int)?;
            let e2_ty = type_check_(tyvar_cnt, substs, env, e2)?;
            Ok(Type::Array(Box::new(e2_ty)))
        }
        Expr::Get(e1, e2) => {
            let array_elem_ty = new_tyvar(tyvar_cnt);
            let array_ty = Type::Array(Box::new(array_elem_ty.clone()));
            let e1_ty = type_check_(tyvar_cnt, substs, env, e1)?;
            unify(substs, &e1_ty, &array_ty)?;
            let e2_ty = type_check_(tyvar_cnt, substs, env, e2)?;
            unify(substs, &e2_ty, &Type::Int)?;
            Ok(array_elem_ty)
        }
        Expr::Put(e1, e2, e3) => {
            let array_elem_ty = new_tyvar(tyvar_cnt);
            let array_ty = Type::Array(Box::new(array_elem_ty.clone()));
            let e1_ty = type_check_(tyvar_cnt, substs, env, e1)?;
            unify(substs, &e1_ty, &array_ty)?;
            let e2_ty = type_check_(tyvar_cnt, substs, env, e2)?;
            unify(substs, &e2_ty, &Type::Int)?;
            let e3_ty = type_check_(tyvar_cnt, substs, env, e3)?;
            unify(substs, &e3_ty, &array_elem_ty)?;
            Ok(Type::Unit)
        }
    }
}

fn unify(substs: &mut HashMap<TyVar, Type>, ty1: &Type, ty2: &Type) -> Result<(), TypeErr> {
    let ty1 = deref_ty(substs, ty1).clone();
    let ty2 = deref_ty(substs, ty2).clone();

    // println!("substs: {:?}", substs);
    // println!("unify {:?} ~ {:?}", ty1, ty2);

    match (&ty1, &ty2) {
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
                return Err(TypeErr::UnifyError(ty1.clone(), ty2.clone()));
            }
            for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                unify(substs, arg1, arg2)?;
            }
            unify(substs, &*ret1, &*ret2)
        }

        (Type::Var(var1), Type::Var(var2)) if var1 == var2 => Ok(()),

        (Type::Var(var), ty) | (ty, Type::Var(var)) => {
            if occurs_check(substs, *var, ty) {
                return Err(TypeErr::InfiniteType(ty1, ty2));
            }
            substs.insert(*var, ty.clone());
            Ok(())
        }

        (Type::Tuple(args1), Type::Tuple(args2)) => {
            if args1.len() != args2.len() {
                return Err(TypeErr::UnifyError(ty1.clone(), ty2.clone()));
            }
            for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                unify(substs, arg1, arg2)?;
            }
            Ok(())
        }
        (Type::Array(ty1), Type::Array(ty2)) => unify(substs, ty1, ty2),
        _ => Err(TypeErr::UnifyError(ty1.clone(), ty2.clone())),
    }
}

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
