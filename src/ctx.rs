use crate::interner::{InternId, InternTable};
use crate::type_check::{TyVar, Type};
use crate::var::{CompilerPhase, Uniq, Var};
use crate::closure_convert::Label;

use fxhash::FxHashMap;
use std::num::NonZeroU32;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(InternId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(InternId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LableId(InternId);

pub struct Ctx {
    next_uniq: Uniq,
    tys: InternTable<Type>,
    vars: InternTable<Var>,
    ty_env: FxHashMap<VarId, TypeId>,
    builtins: Vec<(VarId, TypeId)>,
}

impl Default for Ctx {
    fn default() -> Self {
        let mut ctx = Ctx {
            next_uniq: Uniq(unsafe { NonZeroU32::new_unchecked(1) }),
            tys: Default::default(),
            vars: Default::default(),
            ty_env: Default::default(),
            builtins: vec![],
        };
        ctx.add_builtin_vars();
        ctx
    }
}

impl Ctx {
    fn fresh_uniq(&mut self) -> Uniq {
        let uniq = self.next_uniq;
        self.next_uniq.0 = unsafe { NonZeroU32::new_unchecked(self.next_uniq.0.get() + 1) };
        uniq
    }

    pub fn fresh_user_var(&mut self, name: &str) -> VarId {
        let uniq = self.fresh_uniq();
        self.intern_var(Var::new_user(name, uniq))
    }

    pub fn fresh_generated_var(&mut self, phase: CompilerPhase) -> VarId {
        let uniq = self.fresh_uniq();
        self.intern_var(Var::new_generated(phase, uniq))
    }

    pub fn fresh_builtin_var(&mut self, name: &str) -> VarId {
        let uniq = self.fresh_uniq();
        self.intern_var(Var::new_builtin(name, uniq))
    }

    pub fn fresh_label(&mut self) -> Label {
        self.fresh_uniq()
    }

    pub fn get_var(&self, id: VarId) -> Rc<Var> {
        self.vars.get(id.0)
    }

    pub fn get_type(&self, id: TypeId) -> Rc<Type> {
        self.tys.get(id.0)
    }

    pub fn var_name(&self, var: VarId) -> Rc<str> {
        self.vars.get(var.0).name()
    }

    pub fn var_type(&self, var: VarId) -> Option<Rc<Type>> {
        self.ty_env.get(&var).map(|ty_id| self.get_type(*ty_id))
    }

    pub fn fresh_tyvar(&mut self) -> TyVar {
        self.fresh_uniq()
    }

    pub fn extend_type_env(&mut self, types: impl Iterator<Item = (VarId, Type)>) {
        for (var, ty) in types {
            let ty = self.intern_type(ty);
            self.add_type(var, ty);
        }
    }

    pub fn add_type(&mut self, var: VarId, ty: TypeId) {
        self.ty_env.insert(var, ty);
    }

    pub fn builtins(&self) -> impl Iterator<Item = &(VarId, TypeId)> {
        self.builtins.iter()
    }

    fn add_builtin_type(&mut self, var: VarId, ty: TypeId) {
        self.ty_env.insert(var, ty);
        self.builtins.push((var, ty));
    }

    fn intern_var(&mut self, var: Var) -> VarId {
        VarId(self.vars.intern(var))
    }

    fn intern_type(&mut self, ty: Type) -> TypeId {
        TypeId(self.tys.intern(ty))
    }

    fn add_builtin_vars(&mut self) {
        // float -> float
        let float_float = self.intern_type(Type::Fun {
            args: vec![Type::Float],
            ret: Box::new(Type::Float),
        });

        // float -> int
        let float_int = self.intern_type(Type::Fun {
            args: vec![Type::Float],
            ret: Box::new(Type::Int),
        });

        let print_int_var = self.fresh_builtin_var("print_int");
        let print_int_ty = self.intern_type(Type::Fun {
            args: vec![Type::Int],
            ret: Box::new(Type::Unit),
        });
        self.add_builtin_type(print_int_var, print_int_ty);

        let print_newline_var = self.fresh_builtin_var("print_newline");
        let print_newline_ty = self.intern_type(Type::Fun {
            args: vec![Type::Unit],
            ret: Box::new(Type::Unit),
        });
        self.add_builtin_type(print_newline_var, print_newline_ty);

        let float_of_int_var = self.fresh_builtin_var("float_of_int");
        let float_of_int_ty = self.intern_type(Type::Fun {
            args: vec![Type::Int],
            ret: Box::new(Type::Float),
        });
        self.add_builtin_type(float_of_int_var, float_of_int_ty);

        let int_of_float_var = self.fresh_builtin_var("int_of_float");
        self.add_builtin_type(int_of_float_var, float_int);

        let truncate_var = self.fresh_builtin_var("truncate");
        self.add_builtin_type(truncate_var, float_int);

        let abs_float_var = self.fresh_builtin_var("abs_float");
        self.add_builtin_type(abs_float_var, float_float);

        let sqrt_var = self.fresh_builtin_var("sqrt");
        self.add_builtin_type(sqrt_var, float_float);

        let sin_var = self.fresh_builtin_var("sin");
        self.add_builtin_type(sin_var, float_float);

        let cos_var = self.fresh_builtin_var("cos");
        self.add_builtin_type(cos_var, float_float);
    }
}
