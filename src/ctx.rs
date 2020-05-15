use crate::cg_types::RepType;
use crate::closure_convert::Label;
use crate::interner::{InternId, InternTable};
use crate::type_check::{TyVar, Type};
use crate::var::{CompilerPhase, Uniq, Var};

use fxhash::FxHashMap;
use std::num::NonZeroU32;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(InternId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(InternId);

pub struct Ctx {
    next_uniq: Uniq,
    tys: InternTable<Type>,
    vars: InternTable<Var>,
    ty_env: FxHashMap<VarId, TypeId>,
    rep_ty_env: FxHashMap<VarId, RepType>,
    builtins: Vec<(VarId, TypeId)>,
    // Ids for widely used types
    int_id: TypeId,
    float_id: TypeId,
    unit_id: TypeId,
    // Built-in var ids
    print_int_var: Option<VarId>,
}

impl Default for Ctx {
    fn default() -> Self {
        let mut tys: InternTable<Type> = Default::default();
        let int_id = TypeId(tys.intern(Type::Int));
        let float_id = TypeId(tys.intern(Type::Float));
        let unit_id = TypeId(tys.intern(Type::Unit));
        let mut ctx = Ctx {
            next_uniq: Uniq(unsafe { NonZeroU32::new_unchecked(1) }),
            tys: Default::default(),
            vars: Default::default(),
            ty_env: Default::default(),
            rep_ty_env: Default::default(),
            builtins: vec![],
            int_id,
            float_id,
            unit_id,
            print_int_var: None,
        };
        ctx.add_builtin_vars();
        ctx
    }
}

impl Ctx {
    pub fn int_type_id(&self) -> TypeId {
        self.int_id
    }

    pub fn float_type_id(&self) -> TypeId {
        self.float_id
    }

    pub fn unit_type_id(&self) -> TypeId {
        self.unit_id
    }

    pub fn print_int_var(&self) -> VarId {
        self.print_int_var.unwrap()
    }

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

    pub fn set_var_type(&mut self, var: VarId, ty: TypeId) {
        self.ty_env.insert(var, ty);
    }

    pub fn fresh_codegen_var(&mut self, phase: CompilerPhase, rep_type: RepType) -> VarId {
        let uniq = self.fresh_uniq();
        let var_id = self.intern_var(Var::new_generated(phase, uniq));
        self.rep_ty_env.insert(var_id, rep_type);
        var_id
    }

    pub fn var_rep_type(&mut self, var: VarId) -> RepType {
        match self.rep_ty_env.get(&var) {
            None => match self.ty_env.get(&var).map(|ty_id| self.get_type(*ty_id)) {
                None => {
                    let var = self.get_var(var);
                    panic!("RepType of variable unknown: {} ({:?})", var, var);
                }
                Some(ty) => {
                    let rep_ty = RepType::from(&*ty);
                    self.rep_ty_env.insert(var, rep_ty);
                    rep_ty
                }
            },
            Some(rep_ty) => *rep_ty,
        }
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

    pub fn var_type_id(&self, var: VarId) -> TypeId {
        match self.ty_env.get(&var) {
            None => {
                let var = self.get_var(var);
                panic!("Type of variable unknown: {} ({:?})", var, var);
            }
            Some(ty_id) => *ty_id,
        }
    }

    pub fn var_type(&self, var: VarId) -> Rc<Type> {
        let ty_id = self.var_type_id(var);
        self.get_type(ty_id)
    }

    pub fn var_type_(&self, var: VarId) -> Option<Rc<Type>> {
        self.ty_env
            .get(&var)
            .map(|ty_id| self.tys.get(ty_id.0))
    }

    pub fn fresh_tyvar(&mut self) -> TyVar {
        self.fresh_uniq()
    }

    pub fn extend_type_env(&mut self, types: impl Iterator<Item = (VarId, Type)>) {
        for (var, ty) in types {
            let ty = self.intern_type(ty);
            self.ty_env.insert(var, ty);
        }
    }

    pub fn builtins(&self) -> impl Iterator<Item = &(VarId, TypeId)> {
        self.builtins.iter()
    }

    pub fn is_builtin_var(&self, id: VarId) -> bool {
        self.get_var(id).is_builtin()
    }

    fn add_builtin_type(&mut self, var: VarId, ty: TypeId) {
        self.ty_env.insert(var, ty);
        self.builtins.push((var, ty));
    }

    fn intern_var(&mut self, var: Var) -> VarId {
        VarId(self.vars.intern(var))
    }

    pub fn intern_type(&mut self, ty: Type) -> TypeId {
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
        self.print_int_var = Some(print_int_var);
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
