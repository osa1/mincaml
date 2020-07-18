use crate::cg_types::RepType;
use crate::interner::{InternId, InternTable};
use crate::type_check::{TyVar, Type, TypeArena, TypeIdx, FLOAT_IDX, INT_IDX, UNIT_IDX};
use crate::var::{CompilerPhase, Uniq, Var};

use fxhash::FxHashMap;
use std::num::NonZeroU32;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(InternId);

pub struct Ctx {
    /// Next unique Id, for unique variable generation
    next_uniq: Uniq,
    vars: InternTable<Var>,
    /// Maps variables to their types. Note that not all types have `Type`s, some only have
    /// `RepType`s. See below.
    ty_env: FxHashMap<VarId, TypeIdx>,
    /// Some variables are generated during code generation and they only have lower-level
    /// RepTypes. Those variables are mapped to their RepTypes via this map.
    rep_ty_env: FxHashMap<VarId, RepType>,
    /// Where types live
    ty_arena: TypeArena,
    /// Built-in (RTS) variables mapped to their types
    builtins: Vec<(VarId, TypeIdx)>,
}

impl Default for Ctx {
    fn default() -> Self {
        let mut ctx = Ctx {
            next_uniq: Uniq(unsafe { NonZeroU32::new_unchecked(1) }),
            vars: Default::default(),
            ty_env: Default::default(),
            rep_ty_env: Default::default(),
            ty_arena: TypeArena::new(),
            builtins: vec![],
        };
        ctx.add_builtin_vars();
        ctx
    }
}

impl Ctx {
    pub fn fresh_uniq(&mut self) -> Uniq {
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

    pub fn set_var_type(&mut self, var: VarId, ty: TypeIdx) {
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
                Some(ty) => RepType::from(&ty),
            },
            Some(rep_ty) => *rep_ty,
        }
    }

    fn fresh_builtin_var(&mut self, user_name: &str, symbol_name: &str) -> VarId {
        let uniq = self.fresh_uniq();
        self.intern_var(Var::new_builtin(user_name, symbol_name, uniq))
    }

    pub fn get_var(&self, id: VarId) -> Rc<Var> {
        self.vars.get(id.0)
    }

    // TODO: This copies the type!
    pub fn get_type(&self, id: TypeIdx) -> Type {
        self.ty_arena[id].clone()
    }

    pub fn var_name(&self, var: VarId) -> Rc<str> {
        self.vars.get(var.0).name()
    }

    pub fn var_type_id(&self, var: VarId) -> TypeIdx {
        match self.ty_env.get(&var) {
            None => {
                let var = self.get_var(var);
                panic!("Type of variable unknown: {} ({:?})", var, var);
            }
            Some(ty_id) => *ty_id,
        }
    }

    // TODO: This copies the type!
    pub fn var_type(&self, var: VarId) -> Type {
        let ty_id = self.var_type_id(var);
        self.get_type(ty_id)
    }

    pub fn fresh_tyvar(&mut self) -> TyVar {
        self.fresh_uniq()
    }

    pub fn extend_type_env(&mut self, types: impl Iterator<Item = (VarId, TypeIdx)>) {
        for (var, ty) in types {
            self.ty_env.insert(var, ty);
        }
    }

    pub fn builtins(&self) -> impl ExactSizeIterator<Item = &(VarId, TypeIdx)> {
        self.builtins.iter()
    }

    pub fn is_builtin_var(&self, id: VarId) -> bool {
        self.get_var(id).is_builtin()
    }

    fn add_builtin(&mut self, var: VarId, ty: TypeIdx) {
        self.ty_env.insert(var, ty);
        self.builtins.push((var, ty));
    }

    fn intern_var(&mut self, var: Var) -> VarId {
        VarId(self.vars.intern(var))
    }

    fn add_builtin_vars(&mut self) {
        // float -> float
        let float_float = self.ty_arena.fun(vec![FLOAT_IDX], FLOAT_IDX);

        // float -> int
        let float_int = self.ty_arena.fun(vec![FLOAT_IDX], INT_IDX);

        let print_int_var = self.fresh_builtin_var("print_int", "mc_print_int");
        let print_int_ty = self.ty_arena.fun(vec![INT_IDX], UNIT_IDX);
        self.add_builtin(print_int_var, print_int_ty);

        let print_newline_var = self.fresh_builtin_var("print_newline", "mc_print_newline");
        let print_newline_ty = self.ty_arena.fun(vec![UNIT_IDX], UNIT_IDX);
        self.add_builtin(print_newline_var, print_newline_ty);

        let float_of_int_var = self.fresh_builtin_var("float_of_int", "mc_float_of_int");
        let float_of_int_ty = self.ty_arena.fun(vec![INT_IDX], FLOAT_IDX);
        self.add_builtin(float_of_int_var, float_of_int_ty);

        let int_of_float_var = self.fresh_builtin_var("int_of_float", "mc_int_of_float");
        self.add_builtin(int_of_float_var, float_int);

        let truncate_var = self.fresh_builtin_var("truncate", "mc_truncate");
        self.add_builtin(truncate_var, float_int);

        let abs_float_var = self.fresh_builtin_var("abs_float", "mc_abs_float");
        self.add_builtin(abs_float_var, float_float);

        let sqrt_var = self.fresh_builtin_var("sqrt", "mc_sqrt");
        self.add_builtin(sqrt_var, float_float);

        let sin_var = self.fresh_builtin_var("sin", "mc_sin");
        self.add_builtin(sin_var, float_float);

        let cos_var = self.fresh_builtin_var("cos", "mc_cos");
        self.add_builtin(cos_var, float_float);
    }
}
