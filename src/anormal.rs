//! A-normalization: rename binders to eliminate shadowing.

use crate::knormal::{BinOp, Binder, BinderOrUnit, Expr, Id};
use crate::locals::Locals;

use std::collections::HashMap;

pub fn anormal(expr: &mut Expr) {
    ANormal::new().anormal(expr)
}

struct ANormal {
    tmp_count: u64,
    locals: Locals<String>,
}

impl ANormal {
    fn new() -> ANormal {
        ANormal {
            tmp_count: 0,
            locals: Locals::new(HashMap::new()),
        }
    }

    fn new_scope(&mut self) {
        self.locals.new_scope();
    }

    fn pop_scope(&mut self) {
        self.locals.pop_scope();
    }

    fn update_binder(&mut self, bndr: &mut Id) {
        let c = self.tmp_count;
        self.tmp_count += 1;
        let new = format!("{}.{}", bndr, c);
        self.locals.add(bndr.to_string(), new.clone());
        *bndr = new;
    }

    fn update_use(&mut self, id: &mut Id) {
        match self.locals.get(id) {
            None => {
                // Should be an external Id
            }
            Some(id_) => {
                *id = id_.clone();
            }
        }
    }

    fn anormal(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Unit | Expr::Int(_) | Expr::Float(_) => {}

            Expr::IBinOp(BinOp {
                arg1: var1,
                arg2: var2,
                op: _,
            })
            | Expr::FBinOp(BinOp {
                arg1: var1,
                arg2: var2,
                op: _,
            })
            | Expr::Get(var1, var2) => {
                self.update_use(var1);
                self.update_use(var2);
            }
            Expr::Neg(var) | Expr::FNeg(var) | Expr::Var(var) => {
                self.update_use(var);
            }

            Expr::IfEq(v1, v2, e1, e2) | Expr::IfLE(v1, v2, e1, e2) => {
                self.update_use(v1);
                self.update_use(v2);
                self.anormal(e1);
                self.anormal(e2);
            }

            Expr::Let {
                id,
                rhs,
                body,
                ty: _,
            } => {
                self.anormal(rhs);
                self.new_scope();
                self.update_binder(id);
                self.anormal(body);
                self.pop_scope();
            }

            Expr::LetRec {
                name,
                ty: _,
                args,
                rhs,
                body,
            } => {
                self.new_scope();
                self.update_binder(name);
                self.new_scope();
                for arg in args {
                    match arg {
                        BinderOrUnit::Binder(Binder { binder, ty: _ }) => {
                            self.update_binder(binder);
                        }
                        BinderOrUnit::Unit => {}
                    }
                }
                self.anormal(rhs);
                self.pop_scope();
                self.anormal(body);
                self.pop_scope();
            }

            Expr::App(fun, args) => {
                self.update_use(fun);
                for arg in args {
                    self.update_use(arg);
                }
            }

            Expr::ExtApp(_, args) | Expr::Tuple(args) => {
                for arg in args {
                    self.update_use(arg);
                }
            }

            Expr::LetTuple(bndrs, var, expr) => {
                self.update_use(var);
                self.new_scope();
                for (bndr, _) in bndrs {
                    self.update_binder(bndr);
                }
                self.anormal(expr);
                self.pop_scope();
            }

            Expr::Put(var1, var2, var3) => {
                self.update_use(var1);
                self.update_use(var2);
                self.update_use(var3);
            }
        }
    }
}
