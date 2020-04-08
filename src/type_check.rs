use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::Expr;

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Float,
    Fun { args: Vec<Type>, ret: Box<Type> },
    Tuple(Vec<Type>),
    Array(Box<Type>),
    Var(Rc<RefCell<Option<Box<Type>>>>),
}

pub fn type_check(env: &mut HashMap<String, Type>, expr: &Expr) -> Type {
    match expr {
        Expr::Unit => Type::Unit,
        Expr::Bool(_) => Type::Bool,
        Expr::Int(_) => Type::Int,
        Expr::Float(_) => Type::Float,
        Expr::Not(e) => {
            unify(&Type::Bool, &type_check(env, e));
            Type::Bool
        }
        Expr::Neg(e) => {
            unify(&Type::Int, &type_check(env, e));
            Type::Int
        }
        Expr::Add(e1, e2) | Expr::Sub(e1, e2) => {
            unify(&Type::Int, &type_check(env, e1));
            unify(&Type::Int, &type_check(env, e2));
            Type::Int
        }
        Expr::FNeg(e) => {
            unify(&Type::Float, &type_check(env, e));
            Type::Float
        }
        Expr::FAdd(e1, e2) | Expr::FSub(e1, e2) | Expr::FMul(e1, e2) | Expr::FDiv(e1, e2) => {
            unify(&Type::Float, &type_check(env, e1));
            unify(&Type::Float, &type_check(env, e2));
            Type::Float
        }
        _ => todo!(),
    }
}

#[derive(Debug)]
struct UnifyError {
    ty1: Type,
    ty2: Type,
}

fn unify(ty1: &Type, ty2: &Type) -> Result<(), UnifyError> {
    match (&ty1, &ty2) {
        (Type::Unit, Type::Unit)
        | (Type::Bool, Type::Bool)
        | (Type::Int, Type::Int)
        | (Type::Float, Type::Float) => Ok(()),
        (Type::Fun { args: args1, ret: ret1 }, Type::Fun { args: args2, ret: ret2 }) => {
            if args1.len() != args2.len() {
                return Err(UnifyError { ty1: ty1.clone(), ty2: ty2.clone() });
            }
            for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                unify(arg1, arg2)?;
            }
            unify(ret1, ret2)
        }
        (Type::Tuple(args1), Type::Tuple(args2)) => {
            if args1.len() != args2.len() {
                return Err(UnifyError { ty1: ty1.clone(), ty2: ty2.clone() });
            }
            for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                unify(arg1, arg2)?;
            }
            Ok(())
        }
        (Type::Array(ty1), Type::Array(ty2)) => {
            unify(ty1, ty2)
        }
        (Type::Var(var1), Type::Var(var2)) => {
            todo!()
        }
        (Type::Var(var), _) => {
            todo!()
        }
        (_, Type::Var(var)) => {
            todo!()
        }
        _ => Err(UnifyError { ty1: ty1.clone(), ty2: ty2.clone() }),
    }
}
