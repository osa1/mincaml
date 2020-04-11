use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::Expr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Float,
    Fun { args: Vec<Type>, ret: Box<Type> },
    Tuple(Vec<Type>),
    Array(Box<Type>),
    Var(Rc<RefCell<Option<Type>>>),
}

fn mk_type_env() -> HashMap<String, Type> {
    let mut env = HashMap::new();
    env.insert("print_int".to_owned(), Type::Fun { args: vec![Type::Int], ret: Box::new(Type::Unit) });
    env
}

fn new_tyvar() -> Type {
    Type::Var(Rc::new(RefCell::new(None)))
}

#[derive(Debug)]
pub enum TypeErr {
    UnifyError(Type, Type),
    UnboundVar(String),
}

impl From<UnifyError> for TypeErr {
    fn from(err: UnifyError) -> TypeErr {
        TypeErr::UnifyError(err.ty1, err.ty2)
    }
}

pub fn type_check(expr: &Expr) -> Result<Type, TypeErr> {
    type_check_(&mut mk_type_env(), expr)
}

fn type_check_(env: &mut HashMap<String, Type>, expr: &Expr) -> Result<Type, TypeErr> {
    match expr {
        Expr::Unit => Ok(Type::Unit),
        Expr::Bool(_) => Ok(Type::Bool),
        Expr::Int(_) => Ok(Type::Int),
        Expr::Float(_) => Ok(Type::Float),
        Expr::Not(e) => {
            unify(&Type::Bool, &type_check_(env, e)?)?;
            Ok(Type::Bool)
        }
        Expr::Neg(e) => {
            unify(&Type::Int, &type_check_(env, e)?)?;
            Ok(Type::Int)
        }
        Expr::Add(e1, e2) | Expr::Sub(e1, e2) => {
            unify(&Type::Int, &type_check_(env, e1)?)?;
            unify(&Type::Int, &type_check_(env, e2)?)?;
            Ok(Type::Int)
        }
        Expr::FNeg(e) => {
            unify(&Type::Float, &type_check_(env, e)?)?;
            Ok(Type::Float)
        }
        Expr::FAdd(e1, e2) | Expr::FSub(e1, e2) | Expr::FMul(e1, e2) | Expr::FDiv(e1, e2) => {
            unify(&Type::Float, &type_check_(env, e1)?)?;
            unify(&Type::Float, &type_check_(env, e2)?)?;
            Ok(Type::Float)
        }
        Expr::Eq(e1, e2) | Expr::Le(e1, e2) => {
            let e1_ty = type_check_(env, e1)?;
            let e2_ty = type_check_(env, e2)?;
            unify(&e1_ty, &e2_ty)?;
            Ok(Type::Bool)
        }
        Expr::If(e1, e2, e3) => {
            let e1_ty = type_check_(env, e1)?;
            let e2_ty = type_check_(env, e2)?;
            let e3_ty = type_check_(env, e3)?;
            unify(&e1_ty, &Type::Bool)?;
            unify(&e2_ty, &e3_ty)?;
            Ok(e2_ty)
        }
        Expr::Let {
            ref id,
            ref rhs,
            body,
        } => {
            let bndr_type = Type::Var(Rc::new(RefCell::new(None)));
            let rhs_type = type_check_(env, rhs)?;
            unify(&bndr_type, &rhs_type)?;
            // FIXME: string clone
            env.insert(id.clone(), bndr_type);
            let ret = type_check_(env, body);
            env.remove(id);
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
            for _ in args {
                arg_tys.push(new_tyvar());
            }
            // Type variable for the RHS
            let rhs_ty = new_tyvar();
            // We can now give type to the recursive function
            let fun_ty = Type::Fun {
                args: arg_tys.clone(),
                ret: Box::new(rhs_ty.clone()),
            };
            // RHS and body will be type checked with `name` and args in scope
            env.insert(name.clone(), fun_ty.clone());
            for (arg, arg_ty) in args.iter().zip(arg_tys.iter()) {
                env.insert(arg.clone(), arg_ty.clone());
            }
            // Type check RHS
            let rhs_ty_ = type_check_(env, rhs)?;
            unify(&rhs_ty, &rhs_ty_)?;
            // Type check body
            let ret = type_check_(env, body);
            // Reset environment
            env.remove(name);
            for arg in args.iter() {
                env.remove(arg);
            }
            ret
        }
        Expr::App { fun, args } => {
            let ret_ty = new_tyvar();
            let mut arg_tys: Vec<Type> = Vec::with_capacity(args.len());
            for arg in args {
                arg_tys.push(type_check_(env, arg)?);
            }
            let fun_ty = Type::Fun {
                args: arg_tys,
                ret: Box::new(ret_ty.clone()),
            };
            let fun_ty_ = type_check_(env, fun)?;
            unify(&fun_ty, &fun_ty_)?;
            Ok(ret_ty)
        }
        Expr::Tuple(args) => {
            let mut arg_tys: Vec<Type> = Vec::with_capacity(args.len());
            for arg in args {
                arg_tys.push(type_check_(env, arg)?);
            }
            Ok(Type::Tuple(arg_tys))
        }
        Expr::LetTuple { bndrs, rhs, body } => {
            let mut bndr_tys: Vec<Type> = Vec::with_capacity(bndrs.len());
            for _ in bndrs {
                bndr_tys.push(new_tyvar());
            }
            let tuple_ty = Type::Tuple(bndr_tys.clone());
            let rhs_ty = type_check_(env, rhs)?;
            unify(&rhs_ty, &tuple_ty)?;
            for (bndr, bndr_type) in bndrs.iter().zip(bndr_tys.into_iter()) {
                env.insert(bndr.clone(), bndr_type);
            }
            let ret = type_check_(env, body);
            for bndr in bndrs.iter() {
                env.remove(bndr);
            }
            ret
        }
        Expr::Array(e1, e2) => {
            let e1_ty = type_check_(env, e1)?;
            unify(&e1_ty, &Type::Int)?;
            let e2_ty = type_check_(env, e2)?;
            Ok(Type::Array(Box::new(e2_ty)))
        }
        Expr::Get(e1, e2) => {
            let array_elem_ty = new_tyvar();
            let array_ty = Type::Array(Box::new(array_elem_ty.clone()));
            let e1_ty = type_check_(env, e1)?;
            unify(&e1_ty, &array_ty)?;
            let e2_ty = type_check_(env, e2)?;
            unify(&e2_ty, &Type::Int)?;
            Ok(array_elem_ty)
        }
        Expr::Put(e1, e2, e3) => {
            let array_elem_ty = new_tyvar();
            let array_ty = Type::Array(Box::new(array_elem_ty.clone()));
            let e1_ty = type_check_(env, e1)?;
            unify(&e1_ty, &array_ty)?;
            let e2_ty = type_check_(env, e2)?;
            unify(&e2_ty, &Type::Int)?;
            let e3_ty = type_check_(env, e3)?;
            unify(&e3_ty, &array_elem_ty)?;
            Ok(Type::Unit)
        }
    }
}

#[derive(Debug)]
pub struct UnifyError {
    pub ty1: Type,
    pub ty2: Type,
}

fn unify(ty1: &Type, ty2: &Type) -> Result<(), UnifyError> {
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
                return Err(UnifyError {
                    ty1: ty1.clone(),
                    ty2: ty2.clone(),
                });
            }
            for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                unify(arg1, arg2)?;
            }
            unify(ret1, ret2)
        }
        (Type::Tuple(args1), Type::Tuple(args2)) => {
            if args1.len() != args2.len() {
                return Err(UnifyError {
                    ty1: ty1.clone(),
                    ty2: ty2.clone(),
                });
            }
            for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                unify(arg1, arg2)?;
            }
            Ok(())
        }
        (Type::Array(ty1), Type::Array(ty2)) => unify(ty1, ty2),
        (Type::Var(var), _) => {
            // TODO: Occurrence check
            // Work around borrow RefCell borrow checking
            if let Some(ref ty1) = *var.borrow() {
                return unify(ty1, ty2);
            }
            *var.borrow_mut() = Some(ty2.clone());
            Ok(())
        }
        (_, Type::Var(var)) => {
            // TODO: Occurrence check
            // Work around borrow RefCell borrow checking
            if let Some(ref ty2) = *var.borrow() {
                return unify(ty1, ty2);
            }
            *var.borrow_mut() = Some(ty1.clone());
            Ok(())
        }
        _ => Err(UnifyError {
            ty1: ty1.clone(),
            ty2: ty2.clone(),
        }),
    }
}

fn deref_type(ty: &Type) -> Type {
    match ty {
        Type::Unit | Type::Bool | Type::Int | Type::Float => ty.clone(),
        Type::Fun { ref args, ref ret } => {
            let args = args.iter().map(deref_type).collect();
            let ret = Box::new(deref_type(ret));
            Type::Fun { args, ret }
        }
        Type::Tuple(ref tys) => Type::Tuple(tys.iter().map(deref_type).collect()),
        Type::Array(ref ty) => Type::Array(Box::new(deref_type(ty))),
        Type::Var(var) => match *var.borrow() {
            None => ty.clone(),
            Some(ref ty) => deref_type(ty),
        },
    }
}

#[test]
fn unify_test_1() {
    let ty1 = Type::Int;
    let ty2 = new_tyvar();
    unify(&ty1, &ty2).unwrap();
    assert_eq!(deref_type(&ty2), Type::Int);
    assert_eq!(deref_type(&ty1), Type::Int);

    let ty3 = new_tyvar();
    unify(&ty2, &ty3).unwrap();
    assert_eq!(deref_type(&ty2), Type::Int);
    assert_eq!(deref_type(&ty3), Type::Int);
}
