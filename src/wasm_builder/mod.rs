use crate::cg_types::RepType;
use crate::ctx::VarId;

use fxhash::FxHashMap;

/// A Wasm module builder
#[derive(Debug)]
pub struct ModuleBuilder {
    /// Maps function types in the module to their type indices.
    ///
    /// Type indices are used in `call_indirect` instructions for the type of the function being
    /// called.
    func_types: FxHashMap<FuncType, u32>,

    /// Maps ids for functions to their indices in [functions]
    func_idxs: FxHashMap<VarId, usize>,

    /// Function codes in the module.
    functions: Vec<Function>,

    /// Glboals in the module
    globals: Vec<Global>,
}

/// A Wasm function builder
#[derive(Debug)]
pub struct FunctionBuilder<'a> {
    module_builder: &'a mut ModuleBuilder,

    /// Type of the function being built
    ty: FuncType,

    /// Id of the function being built
    id: VarId,

    /// Locals in the function. Includes function arguments. Indexed by [FunctionLocalId].
    locals: Vec<ValType>,

    /// Maps ids to local indices, to index [locals].
    local_indices: FxHashMap<VarId, FunctionLocalId>,

    /// Wasm code for the function body, encoded directly in Wasm binary format.
    code: Vec<u8>,
}

#[derive(Debug)]
struct Function {
    ty: FuncType,
    locals: Vec<ValType>,
    code: Vec<u8>,
}

/// A Wasm function local variable
#[derive(Debug)]
struct Local {
    /// Index of the local in the function
    idx: u32,

    /// Wasm type of the local
    ty: ValType,
}

/// A Wasm global. Currently we only generate mutable globals, and globals are initialized as 0.
#[derive(Debug, Clone, Copy)]
struct Global {
    ty: ValType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalId(u32);

/// A Wasm value type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValType {
    I64,
    F64,
}

impl ValType {
    fn binary(&self) -> u8 {
        match self {
            ValType::I64 => 0x7E,
            ValType::F64 => 0x7C,
        }
    }
}

/// A Wasm function type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncType {
    /// Argument types
    // NB. Can be represented as a bit vector since `ValType` is one bit of information
    pub args: Vec<ValType>,

    /// Return type
    pub ret: ValType,
}

impl ValType {
    pub fn from_rep_type(rep_ty: RepType) -> Self {
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
        ModuleBuilder {
            func_types: Default::default(),
            func_idxs: Default::default(),
            functions: Vec::new(),
            globals: Vec::new(),
        }
    }

    pub fn new_function(
        &mut self,
        id: VarId,
        args: Vec<(VarId, RepType)>,
        ret_ty: RepType,
    ) -> FunctionBuilder {
        let func_ty = FuncType {
            args: args
                .iter()
                .map(|(_, ty)| ValType::from_rep_type(*ty))
                .collect(),
            ret: ValType::from_rep_type(ret_ty),
        };

        self.add_func_type(func_ty.clone());

        FunctionBuilder {
            module_builder: self,

            ty: func_ty,

            id,

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

    pub fn new_global(&mut self, ty: ValType) -> GlobalId {
        let global_id = self.globals.len().try_into().unwrap();
        self.globals.push(Global { ty });
        GlobalId(global_id)
    }

    pub fn encode(self, main_fn_id: VarId) -> Vec<u8> {
        let mut encoded = Vec::new();

        encoded.extend_from_slice(&[0x00, 0x61, 0x73, 0x6D]); // Wasm magic number
        encoded.extend_from_slice(&[0x01, 0x00, 0x00, 0x00]); // Wasm version number

        // Type section
        {
            encoded.push(1); // type section id

            let mut type_section_body: Vec<u8> = Vec::new();

            // Type vector length
            leb128::write::unsigned(
                &mut type_section_body,
                self.func_types.len().try_into().unwrap(),
            )
            .unwrap();

            let mut func_types: Vec<(FuncType, u32)> = self
                .func_types
                .iter()
                .map(|(k, v)| (k.clone(), *v))
                .collect();

            func_types.sort_by_key(|(_, key)| *key);

            for (func_ty, _) in func_types {
                type_section_body.push(0x60);

                // Argument `resulttype` vector length
                leb128::write::unsigned(
                    &mut type_section_body,
                    func_ty.args.len().try_into().unwrap(),
                )
                .unwrap();

                for arg in func_ty.args {
                    type_section_body.push(arg.binary());
                }

                // Return `resulttype` vector length
                type_section_body.push(1);
                type_section_body.push(func_ty.ret.binary());
            }

            leb128::write::unsigned(&mut encoded, type_section_body.len().try_into().unwrap())
                .unwrap();
            encoded.extend_from_slice(&type_section_body);
        }

        // Function section
        {
            encoded.push(3); // function section id

            let mut function_section_body: Vec<u8> = Vec::new();

            leb128::write::unsigned(
                &mut function_section_body,
                self.functions.len().try_into().unwrap(),
            )
            .unwrap();

            for f in &self.functions {
                let f_ty_idx = self.func_types.get(&f.ty).unwrap();
                leb128::write::unsigned(
                    &mut function_section_body,
                    (*f_ty_idx).try_into().unwrap(),
                )
                .unwrap();
            }

            leb128::write::unsigned(
                &mut encoded,
                function_section_body.len().try_into().unwrap(),
            )
            .unwrap();
            encoded.extend_from_slice(&function_section_body);
        }

        // Table section
        {
            encoded.push(4); // table section id

            let mut table_section_body: Vec<u8> = Vec::new();

            // Table vector length
            table_section_body.push(1);

            // reftype
            table_section_body.push(0x70); // funcref

            // limits
            table_section_body.push(0x1); // min and max

            let n_functions: u64 = self.functions.len().try_into().unwrap();

            leb128::write::unsigned(&mut table_section_body, n_functions).unwrap(); // min
            leb128::write::unsigned(&mut table_section_body, n_functions).unwrap(); // max

            leb128::write::unsigned(&mut encoded, table_section_body.len().try_into().unwrap())
                .unwrap();

            encoded.extend_from_slice(&table_section_body);
        }

        // TODO: Do we need memory section? (5)

        // Global section
        {
            encoded.push(6);

            let mut global_section_body: Vec<u8> = Vec::new();

            // Global vector length
            leb128::write::unsigned(
                &mut global_section_body,
                self.globals.len().try_into().unwrap(),
            )
            .unwrap();

            for global in self.globals {
                global_section_body.push(global.ty.binary());
                global_section_body.push(0x01); // mut
            }

            leb128::write::unsigned(&mut encoded, global_section_body.len().try_into().unwrap())
                .unwrap();

            encoded.extend_from_slice(&global_section_body);
        }

        // Start section
        {
            encoded.push(8);

            let mut start_section_body: Vec<u8> = Vec::new();

            let main_fn_id = self
                .func_idxs
                .get(&main_fn_id)
                .unwrap_or_else(|| panic!("Unknown function {:?}", main_fn_id));

            leb128::write::unsigned(&mut start_section_body, (*main_fn_id).try_into().unwrap())
                .unwrap();

            leb128::write::unsigned(&mut encoded, start_section_body.len().try_into().unwrap())
                .unwrap();

            encoded.extend_from_slice(&start_section_body);
        }

        // Element section
        {
            encoded.push(9);

            let mut element_section_body: Vec<u8> = Vec::new();

            // element kind 0: expr for start offset, a vector of func indices
            element_section_body.push(0);

            // expr for start offset
            element_section_body.push(0x41); // i32.const
            element_section_body.push(0);
            element_section_body.push(0x0B); // end

            // Func index vec length
            leb128::write::unsigned(
                &mut element_section_body,
                self.functions.len().try_into().unwrap(),
            )
            .unwrap();

            for i in 0..self.functions.len() {
                leb128::write::unsigned(&mut element_section_body, i.try_into().unwrap()).unwrap();
            }

            leb128::write::unsigned(&mut encoded, element_section_body.len().try_into().unwrap())
                .unwrap();

            encoded.extend_from_slice(&element_section_body);
        }

        // Code section
        {
            encoded.push(10);

            let mut code_section_body: Vec<u8> = Vec::new();

            leb128::write::unsigned(
                &mut code_section_body,
                self.functions.len().try_into().unwrap(),
            )
            .unwrap();

            for Function {
                ty: _,
                locals,
                code,
            } in self.functions
            {
                let mut func_encoding: Vec<u8> = Vec::new();

                leb128::write::unsigned(&mut func_encoding, locals.len().try_into().unwrap())
                    .unwrap();

                for local in locals {
                    func_encoding.push(1); // number of locals with the type
                    func_encoding.push(local.binary());
                }

                func_encoding.extend_from_slice(&code);

                leb128::write::unsigned(
                    &mut code_section_body,
                    func_encoding.len().try_into().unwrap(),
                )
                .unwrap();

                code_section_body.extend_from_slice(&func_encoding);
            }

            leb128::write::unsigned(&mut encoded, code_section_body.len().try_into().unwrap())
                .unwrap();

            encoded.extend_from_slice(&code_section_body);
        }

        encoded
    }

    fn add_func_type(&mut self, func_type: FuncType) -> u32 {
        let next_idx: u32 = self.func_types.len().try_into().unwrap();
        match self.func_types.entry(func_type) {
            std::collections::hash_map::Entry::Occupied(entry) => *entry.get(),
            std::collections::hash_map::Entry::Vacant(entry) => *entry.insert(next_idx),
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

    pub fn block<F: FnOnce(&mut Self)>(&mut self, ty: RepType, f: F) {
        self.code.push(0x02);
        self.code.push(ValType::from_rep_type(ty).binary());
        f(self);
        self.code.push(0x0B);
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

    pub fn i64_eq(&mut self) {
        self.code.push(0x51);
    }

    pub fn i64_ne(&mut self) {
        self.code.push(0x52);
    }

    pub fn i64_lt_s(&mut self) {
        self.code.push(0x53);
    }

    pub fn i64_le_s(&mut self) {
        self.code.push(0x57);
    }

    pub fn i64_gt_s(&mut self) {
        self.code.push(0x55);
    }

    pub fn i64_ge_s(&mut self) {
        self.code.push(0x59);
    }

    pub fn f64_eq(&mut self) {
        self.code.push(0x61);
    }

    pub fn f64_ne(&mut self) {
        self.code.push(0x62);
    }

    pub fn f64_lt(&mut self) {
        self.code.push(0x63);
    }

    pub fn f64_le(&mut self) {
        self.code.push(0x65);
    }

    pub fn f64_gt(&mut self) {
        self.code.push(0x64);
    }

    pub fn f64_ge(&mut self) {
        self.code.push(0x66);
    }

    pub fn br(&mut self, i: u32) {
        self.code.push(0x0C);
        leb128::write::unsigned(&mut self.code, i.into()).unwrap();
    }

    pub fn br_if(&mut self, i: u32) {
        self.code.push(0x0D);
        leb128::write::unsigned(&mut self.code, i.into()).unwrap();
    }

    /// Generate a `call_indirect` instruction. Table index assumed to be 0.
    pub fn call_indirect(&mut self, func_type: FuncType) {
        let type_idx = self.module_builder.add_func_type(func_type);
        self.code.push(0x11);
        leb128::write::unsigned(&mut self.code, type_idx.into()).unwrap();
        self.code.push(0); // table index
    }

    pub fn call(&mut self, idx: usize) {
        self.code.push(0x10);
        leb128::write::unsigned(&mut self.code, idx.try_into().unwrap()).unwrap();
    }

    pub fn global_get(&mut self, global: &GlobalId) {
        self.code.push(0x23);
        leb128::write::unsigned(&mut self.code, global.0.into()).unwrap();
    }

    pub fn global_set(&mut self, global: &GlobalId) {
        self.code.push(0x24);
        leb128::write::unsigned(&mut self.code, global.0.into()).unwrap();
    }

    pub fn finish(self) {
        let FunctionBuilder {
            module_builder,
            id,
            locals,
            code,
            ty,
            local_indices: _,
        } = self;
        let idx = module_builder.functions.len();
        module_builder.functions.push(Function { ty, locals, code });
        let old_idx = module_builder.func_idxs.insert(id, idx);
        assert_eq!(old_idx, None);
    }
}
