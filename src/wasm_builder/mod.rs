use crate::cg_types::RepType;
use crate::ctx::VarId;

use fxhash::FxHashMap;

/// A Wasm module builder
pub struct ModuleBuilder {}

/// A Wasm function builder
pub struct FunctionBuilder<'a> {
    module_builder: &'a mut ModuleBuilder,

    /// Locals in the function. Includes function arguments. Indexed by [FunctionLocalId].
    locals: Vec<ValType>,

    /// Maps ids to local indices, to index [locals].
    local_indices: FxHashMap<VarId, FunctionLocalId>,

    /// Wasm code for the function body, encoded directly in Wasm binary format.
    code: Vec<u8>,
}

/// A Wasm function local variable
struct Local {
    /// Index of the local in the function
    idx: u32,

    /// Wasm type of the local
    ty: ValType,
}

/// A Wasm value type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValType {
    I64,
    F64,
}

impl ValType {
    fn from_rep_type(rep_ty: RepType) -> Self {
        match rep_ty {
            RepType::Word => ValType::I64,
            RepType::Float => ValType::F64,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionLocalId(u32);

impl ModuleBuilder {
    pub fn new() -> Self {
        ModuleBuilder {}
    }

    pub fn new_function(&mut self, args: Vec<(VarId, RepType)>) -> FunctionBuilder {
        FunctionBuilder {
            module_builder: self,

            locals: args
                .iter()
                .map(|(_, v)| (ValType::from_rep_type(*v)))
                .collect(),

            local_indices: args
                .iter()
                .enumerate()
                .map(|(idx, (id, _))| (*id, FunctionLocalId(idx.try_into().unwrap())))
                .collect(),

            code: Vec::new(),
        }
    }
}

impl<'a> FunctionBuilder<'a> {
    /// Create a new Wasm local in the function for [id].
    pub fn new_local(&mut self, id: VarId, ty: RepType) -> FunctionLocalId {
        let id = FunctionLocalId(self.locals.len().try_into().unwrap());
        self.locals.push(ValType::from_rep_type(ty));
        id
    }

    /// Get [FunctionLocalId] for a local variable.
    pub fn id_wasm_local(&self, id: VarId) -> FunctionLocalId {
        *self
            .local_indices
            .get(&id)
            .unwrap_or_else(|| panic!("Wasm local not defined for id {:?}", id))
    }

    /// Read a local variable. Pushes the value to Wasm stack.
    pub fn get_local(&mut self, id: FunctionLocalId) {
        self.code.push(0x20);
        leb128::write::unsigned(&mut self.code, id.0.into()).unwrap();
    }

    /// Set a local variable. Value is popped from the Wasm stack.
    pub fn set_local(&mut self, id: FunctionLocalId) {
        self.code.push(0x21);
        leb128::write::unsigned(&mut self.code, id.0.into()).unwrap();
    }

    pub fn i64_const(&mut self, val: i64) {
        self.code.push(0x42);
        leb128::write::signed(&mut self.code, val).unwrap();
    }

    pub fn f64_const(&mut self, val: f64) {
        self.code.push(0x44);
        self.code.extend_from_slice(&val.to_le_bytes());
    }

    pub fn i64_add(&mut self) {
        self.code.push(0x7C);
    }

    pub fn i64_sub(&mut self) {
        self.code.push(0x7D);
    }

    pub fn f64_add(&mut self) {
        self.code.push(0xA0);
    }

    pub fn f64_sub(&mut self) {
        self.code.push(0xA1);
    }

    pub fn f64_mul(&mut self) {
        self.code.push(0xA2);
    }

    pub fn f64_div(&mut self) {
        self.code.push(0xA3);
    }

    /// Generate a `call_indirect` instruction. Table index assumed to be 0.
    ///
    /// TODO: type index?
    pub fn call_indirect(&mut self) {
        todo!()
    }
}
