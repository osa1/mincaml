use crate::cg_types::RepType;
use crate::ctx::VarId;

use std::collections::hash_map::Entry;

use fxhash::FxHashMap;

/// A Wasm module builder
#[derive(Debug)]
pub struct ModuleBuilder {
    /// Maps function types in the module to their type indices.
    ///
    /// Type indices are used in `call_indirect` instructions for the type of the function being
    /// called.
    func_types: FxHashMap<FuncType, u32>,

    /// Function imports: (module name, function name, function type index)
    func_imports: Vec<(String, String, u32)>,

    /// Global imports: (module name, global name, global type)
    global_imports: Vec<(String, String, Global)>,

    /// Maps ids for functions to their indices in the module.
    func_idxs: FxHashMap<VarId, u32>,

    /// Function codes in the module.
    functions: Vec<Function>,

    /// Glboals in the module
    globals: Vec<Global>,

    /// Data section contents. Currently we generate one data segment.
    data: Vec<u8>,
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
    locals: Vec<NumType>,

    /// Maps ids to local indices, to index [locals].
    local_indices: FxHashMap<VarId, FunctionLocalId>,

    /// Wasm code for the function body, encoded directly in Wasm binary format.
    code: Vec<u8>,
}

#[derive(Debug)]
struct Function {
    ty: FuncType,
    locals: Vec<NumType>,
    code: Vec<u8>,
}

/// A Wasm global
#[derive(Debug, Clone, Copy)]
pub struct Global {
    pub ty: ValType,
    pub mutbl: Mutability,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    Mut,
    Immut,
}

impl Mutability {
    fn binary(&self) -> u8 {
        match self {
            Mutability::Mut => 0x01,
            Mutability::Immut => 0x00,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalId(u32);

/// A Wasm numeric type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NumType {
    I32,
    #[allow(unused)]
    I64,
    F32,
    #[allow(unused)]
    F64,
}

impl NumType {
    fn binary(&self) -> u8 {
        match self {
            NumType::I32 => 0x7F,
            NumType::I64 => 0x7E,
            NumType::F32 => 0x7D,
            NumType::F64 => 0x7C,
        }
    }
}

/// A Wasm value type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValType {
    Num(NumType),
    Ref(RefType),
}

impl ValType {
    fn binary(&self) -> u8 {
        match self {
            ValType::Num(num_type) => num_type.binary(),
            ValType::Ref(ref_type) => ref_type.binary(),
        }
    }
}

/// A Wasm reference type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefType {
    FuncRef,
}

impl RefType {
    fn binary(&self) -> u8 {
        match self {
            RefType::FuncRef => 0x70,
        }
    }
}

/// A Wasm function type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncType {
    /// Argument types
    // NB. Can be represented as a bit vector since `ValType` is one bit of information
    pub args: Vec<NumType>,

    /// Return type
    pub ret: Vec<NumType>,
}

impl NumType {
    pub fn from_rep_type(rep_ty: RepType) -> Self {
        match rep_ty {
            RepType::Word => NumType::I32,
            RepType::Float => NumType::F32,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionLocalId(u32);

impl ModuleBuilder {
    pub fn new() -> Self {
        ModuleBuilder {
            func_types: Default::default(),
            func_imports: Vec::new(),
            global_imports: Vec::new(),
            func_idxs: Default::default(),
            functions: Vec::new(),
            globals: Vec::new(),
            data: Vec::new(),
        }
    }

    /// Add a function import. Returns function index.
    // TODO: Somehow make sure that we won't add more imports after adding a function
    pub fn new_func_import(
        &mut self,
        module_name: &str,
        import_name: &str,
        var: &VarId,
        ty: FuncType,
    ) -> u32 {
        let n_func_tys: u32 = self.func_types.len().try_into().unwrap();

        let ty_idx: u32 = match self.func_types.entry(ty) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                entry.insert(n_func_tys);
                n_func_tys
            }
        };

        let fun_id = self.func_imports.len() as u32;

        self.func_imports
            .push((module_name.to_owned(), import_name.to_owned(), ty_idx));

        let old = self.func_idxs.insert(*var, fun_id);
        debug_assert_eq!(old, None);

        fun_id
    }

    pub fn new_global_import(
        &mut self,
        module_name: &str,
        import_name: &str,
        global_ty: Global,
    ) -> u32 {
        let global_idx = self.global_imports.len() as u32;

        self.global_imports
            .push((module_name.to_owned(), import_name.to_owned(), global_ty));

        global_idx
    }

    pub fn allocate_function_idx(&mut self, var: VarId) -> u32 {
        let fun_id = self.func_idxs.len() as u32;
        let old = self.func_idxs.insert(var, fun_id);
        debug_assert_eq!(old, None);
        fun_id
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
                .map(|(_, ty)| NumType::from_rep_type(*ty))
                .collect(),
            ret: vec![NumType::from_rep_type(ret_ty)],
        };

        self.add_func_type(func_ty.clone());

        FunctionBuilder {
            module_builder: self,

            ty: func_ty,

            id,

            locals: args
                .iter()
                .map(|(_, v)| (NumType::from_rep_type(*v)))
                .collect(),

            local_indices: args
                .iter()
                .enumerate()
                .map(|(idx, (id, _))| (*id, FunctionLocalId(idx.try_into().unwrap())))
                .collect(),

            code: Vec::new(),
        }
    }

    pub fn new_global(&mut self, ty: ValType, mutbl: Mutability) -> GlobalId {
        let global_id = (self.global_imports.len() + self.globals.len()) as u32;
        self.globals.push(Global { ty, mutbl });
        GlobalId(global_id)
    }

    pub fn set_data(&mut self, data: Vec<u8>) {
        self.data = data;
    }

    pub fn encode(mut self, main_fn_id: VarId) -> Vec<u8> {
        // HACK: Wasm start function needs to have type `[] -> []`, but the mincaml expression type
        // will be returning `()`, which we represent as `0`.
        //
        // To make our main function have the right type, we find the main function, update its
        // type, and add a `drop` at the end of it.
        let wasm_main_fn_type = FuncType {
            args: vec![],
            ret: vec![],
        };

        // Register the type
        self.add_func_type(wasm_main_fn_type.clone());

        let main_fn_idx = self.func_idxs.get(&main_fn_id).unwrap();
        let main_fn = &mut self.functions[(*main_fn_idx) as usize - self.func_imports.len()];
        main_fn.ty = wasm_main_fn_type;
        let n_instructions = main_fn.code.len();
        main_fn.code[n_instructions - 1] = 0x1A; // convert 'end' into 'drop'
        main_fn.code.push(0x0B); // end

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
                leb128::write::unsigned(&mut type_section_body, func_ty.args.len() as u64).unwrap();

                for arg in func_ty.args {
                    type_section_body.push(arg.binary());
                }

                // Return `resulttype` vector length
                leb128::write::unsigned(&mut type_section_body, func_ty.ret.len() as u64).unwrap();

                for ret in func_ty.ret {
                    type_section_body.push(ret.binary());
                }
            }

            leb128::write::unsigned(&mut encoded, type_section_body.len().try_into().unwrap())
                .unwrap();
            encoded.extend_from_slice(&type_section_body);
        }

        // Import section
        {
            encoded.push(2); // import section id

            let mut import_section_body: Vec<u8> = Vec::new();

            leb128::write::unsigned(
                &mut import_section_body,
                self.func_imports.len().try_into().unwrap(),
            )
            .unwrap();

            for (module_name, fun_name, ty_idx) in &self.func_imports {
                leb128::write::unsigned(&mut import_section_body, module_name.len() as u64)
                    .unwrap();

                import_section_body.extend_from_slice(module_name.as_bytes());

                leb128::write::unsigned(&mut import_section_body, fun_name.len() as u64).unwrap();

                import_section_body.extend_from_slice(fun_name.as_bytes());

                import_section_body.push(0x00); // function
                leb128::write::unsigned(&mut import_section_body, (*ty_idx).into()).unwrap();
            }

            for (module_name, global_name, global_ty) in &self.global_imports {
                leb128::write::unsigned(&mut import_section_body, module_name.len() as u64)
                    .unwrap();

                import_section_body.extend_from_slice(module_name.as_bytes());

                leb128::write::unsigned(&mut import_section_body, global_name.len() as u64)
                    .unwrap();

                import_section_body.extend_from_slice(global_name.as_bytes());

                import_section_body.push(0x03); // global

                let Global { ty, mutbl } = global_ty;
                import_section_body.push(ty.binary());
                import_section_body.push(mutbl.binary());
            }

            leb128::write::unsigned(&mut encoded, import_section_body.len().try_into().unwrap())
                .unwrap();
            encoded.extend_from_slice(&import_section_body);
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

        // Memory section
        {
            encoded.push(5); // memory section id

            let mut memory_section_body: Vec<u8> = Vec::new();

            memory_section_body.push(1); // mem vector length
            memory_section_body.push(0x00); // limits, no max
            memory_section_body.push(0x00); // min = 0 (not sure if this is correct with the data section?)

            leb128::write::unsigned(&mut encoded, memory_section_body.len() as u64).unwrap();

            encoded.extend_from_slice(&memory_section_body);
        }

        // Global section
        {
            encoded.push(6); // global section id

            let mut global_section_body: Vec<u8> = Vec::new();

            // Global vector length
            leb128::write::unsigned(&mut global_section_body, self.globals.len() as u64).unwrap();

            for Global { ty, mutbl } in self.globals {
                global_section_body.push(ty.binary());
                global_section_body.push(mutbl.binary());
                // global_section_body.push(0x41); // i32.const
                // Initial value
                match ty {
                    ValType::Num(num_ty) => match num_ty {
                        NumType::I32 => {
                            global_section_body.push(0x41); // i32.const
                            global_section_body.push(0x00);
                        }
                        NumType::I64 => {
                            global_section_body.push(0x42); // i64.const
                            global_section_body.push(0x00);
                        }
                        NumType::F32 => {
                            global_section_body.push(0x43); // f32.const
                            global_section_body.extend_from_slice(&0f32.to_le_bytes());
                        }
                        NumType::F64 => {
                            global_section_body.push(0x44); // f64.const
                            global_section_body.extend_from_slice(&0f64.to_le_bytes());
                        }
                    },
                    ValType::Ref(ref_ty) => match ref_ty {
                        RefType::FuncRef => panic!("FuncRef globals not supported"),
                    },
                }
                global_section_body.push(0x0B); // end expr
            }

            leb128::write::unsigned(&mut encoded, global_section_body.len() as u64).unwrap();

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

            // vec length
            element_section_body.push(1);

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

        // Data count section
        encoded.push(12); // data count section id
        encoded.push(1); // section size
        encoded.push(1); // data count = 1

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

        // Data section
        {
            encoded.push(11); // data section id

            let mut data_section_body: Vec<u8> = Vec::new();

            data_section_body.push(1); // vec length = 1
            data_section_body.push(0); // active
            data_section_body.push(0x41); // i32.const
            data_section_body.push(0);
            data_section_body.push(0x0B); // end expr
            leb128::write::unsigned(&mut data_section_body, self.data.len() as u64).unwrap();
            data_section_body.extend_from_slice(&self.data);

            leb128::write::unsigned(&mut encoded, data_section_body.len().try_into().unwrap())
                .unwrap();

            encoded.extend_from_slice(&data_section_body);
        }

        encoded
    }

    fn add_func_type(&mut self, func_type: FuncType) -> u32 {
        let next_idx: u32 = self.func_types.len().try_into().unwrap();
        match self.func_types.entry(func_type) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => *entry.insert(next_idx),
        }
    }
}

impl<'a> FunctionBuilder<'a> {
    /// Create a new Wasm local in the function for [id]. The [FunctionLocalId] for the [id] can be
    /// later obtained with [id_wasm_local].
    pub fn new_local(&mut self, id: VarId, ty: RepType) -> FunctionLocalId {
        let local_id = FunctionLocalId(self.locals.len().try_into().unwrap());
        self.locals.push(NumType::from_rep_type(ty));
        let old = self.local_indices.insert(id, local_id);
        assert_eq!(old, None);
        local_id
    }

    /// Creates a new Wasm local in the function.
    pub fn new_local_(&mut self, ty: RepType) -> FunctionLocalId {
        let local_id = FunctionLocalId(self.locals.len().try_into().unwrap());
        self.locals.push(NumType::from_rep_type(ty));
        local_id
    }

    // Get [FunctionLocalId] for a local variable.
    // pub fn id_wasm_local(&self, ctx: &Ctx, id: VarId) -> FunctionLocalId {
    //     *self
    //         .local_indices
    //         .get(&id)
    //         .unwrap_or_else(|| panic!("Wasm local not defined for id {}", ctx.get_var(id)))
    // }

    /// `local.get`: read a local variable. Pushes the value to Wasm stack.
    pub fn local_get(&mut self, id: FunctionLocalId) {
        self.code.push(0x20);
        leb128::write::unsigned(&mut self.code, id.0.into()).unwrap();
    }

    // Read a local variable. Pushes the value to Wasm stack.
    // pub fn get_local_id(&mut self, ctx: &Ctx, id: &VarId) {
    //     let id = self.id_wasm_local(ctx, *id);
    //     self.code.push(0x20);
    //     leb128::write::unsigned(&mut self.code, id.0.into()).unwrap();
    // }

    /// Set a local variable. Value is popped from the Wasm stack.
    pub fn set_local(&mut self, id: FunctionLocalId) {
        self.code.push(0x21);
        leb128::write::unsigned(&mut self.code, id.0.into()).unwrap();
    }

    pub fn block<F: FnOnce(&mut Self)>(&mut self, ty: RepType, f: F) {
        self.code.push(0x02);
        self.code.push(NumType::from_rep_type(ty).binary());
        f(self);
        self.code.push(0x0B);
    }

    pub fn loop_<F: FnOnce(&mut Self)>(&mut self, ty: RepType, f: F) {
        self.code.push(0x03);
        self.code.push(NumType::from_rep_type(ty).binary());
        f(self);
        self.code.push(0x0B);
    }

    pub fn i32_const(&mut self, val: i32) {
        self.code.push(0x41);
        leb128::write::signed(&mut self.code, val.into()).unwrap();
    }

    #[allow(unused)]
    pub fn i64_const(&mut self, val: i64) {
        self.code.push(0x42);
        leb128::write::signed(&mut self.code, val).unwrap();
    }

    pub fn f32_const(&mut self, val: f32) {
        self.code.push(0x43);
        self.code.extend_from_slice(&val.to_le_bytes());
    }

    #[allow(unused)]
    pub fn f64_const(&mut self, val: f64) {
        self.code.push(0x44);
        self.code.extend_from_slice(&val.to_le_bytes());
    }

    pub fn i32_add(&mut self) {
        self.code.push(0x6A);
    }

    #[allow(unused)]
    pub fn i64_add(&mut self) {
        self.code.push(0x7C);
    }

    pub fn i32_sub(&mut self) {
        self.code.push(0x6B);
    }

    #[allow(unused)]
    pub fn i64_sub(&mut self) {
        self.code.push(0x7D);
    }

    pub fn i32_mul(&mut self) {
        self.code.push(0x6C);
    }

    pub fn f32_add(&mut self) {
        self.code.push(0x92);
    }

    #[allow(unused)]
    pub fn f64_add(&mut self) {
        self.code.push(0xA0);
    }

    pub fn f32_sub(&mut self) {
        self.code.push(0x93);
    }

    #[allow(unused)]
    pub fn f64_sub(&mut self) {
        self.code.push(0xA1);
    }

    pub fn f32_mul(&mut self) {
        self.code.push(0x94);
    }

    #[allow(unused)]
    pub fn f64_mul(&mut self) {
        self.code.push(0xA2);
    }

    pub fn f32_div(&mut self) {
        self.code.push(0x95);
    }

    #[allow(unused)]
    pub fn f64_div(&mut self) {
        self.code.push(0xA3);
    }

    pub fn i32_eq(&mut self) {
        self.code.push(0x5B);
    }

    #[allow(unused)]
    pub fn i64_eq(&mut self) {
        self.code.push(0x51);
    }

    pub fn i32_ne(&mut self) {
        self.code.push(0x5C);
    }

    #[allow(unused)]
    pub fn i64_ne(&mut self) {
        self.code.push(0x52);
    }

    pub fn i32_lt_s(&mut self) {
        self.code.push(0x48);
    }

    #[allow(unused)]
    pub fn i64_lt_s(&mut self) {
        self.code.push(0x53);
    }

    pub fn i32_le_s(&mut self) {
        self.code.push(0x4C);
    }

    #[allow(unused)]
    pub fn i64_le_s(&mut self) {
        self.code.push(0x57);
    }

    pub fn i32_gt_s(&mut self) {
        self.code.push(0x4A);
    }

    #[allow(unused)]
    pub fn i64_gt_s(&mut self) {
        self.code.push(0x55);
    }

    pub fn i32_ge_s(&mut self) {
        self.code.push(0x4E);
    }

    #[allow(unused)]
    pub fn i64_ge_s(&mut self) {
        self.code.push(0x59);
    }

    pub fn f32_eq(&mut self) {
        self.code.push(0x5B);
    }

    #[allow(unused)]
    pub fn f64_eq(&mut self) {
        self.code.push(0x61);
    }

    pub fn f32_ne(&mut self) {
        self.code.push(0x5C);
    }

    #[allow(unused)]
    pub fn f64_ne(&mut self) {
        self.code.push(0x62);
    }

    pub fn f32_lt(&mut self) {
        self.code.push(0x5D);
    }

    #[allow(unused)]
    pub fn f64_lt(&mut self) {
        self.code.push(0x63);
    }

    pub fn f32_le(&mut self) {
        self.code.push(0x5F);
    }

    #[allow(unused)]
    pub fn f64_le(&mut self) {
        self.code.push(0x65);
    }

    pub fn f32_gt(&mut self) {
        self.code.push(0x5E);
    }

    #[allow(unused)]
    pub fn f64_gt(&mut self) {
        self.code.push(0x64);
    }

    pub fn f32_ge(&mut self) {
        self.code.push(0x60);
    }

    #[allow(unused)]
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

    #[allow(unused)]
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

    pub fn memory_grow(&mut self) {
        self.code.push(0x40);
    }

    pub fn i32_load(&mut self, offset: u32) {
        self.code.push(0x28);
        self.code.push(0); // align
        leb128::write::unsigned(&mut self.code, offset.into()).unwrap();
    }

    #[allow(unused)]
    pub fn i64_load(&mut self, offset: u32) {
        self.code.push(0x29);
        self.code.push(0); // align
        leb128::write::unsigned(&mut self.code, offset.into()).unwrap();
    }

    pub fn f32_load(&mut self, offset: u32) {
        self.code.push(0x2A);
        self.code.push(0); // align
        leb128::write::unsigned(&mut self.code, offset.into()).unwrap();
    }

    #[allow(unused)]
    pub fn f64_load(&mut self, offset: u32) {
        self.code.push(0x2B);
        self.code.push(0); // align
        leb128::write::unsigned(&mut self.code, offset.into()).unwrap();
    }

    pub fn i32_store(&mut self, offset: u32) {
        self.code.push(0x36);
        self.code.push(0); // align
        leb128::write::unsigned(&mut self.code, offset.into()).unwrap();
    }

    #[allow(unused)]
    pub fn i64_store(&mut self, offset: u32) {
        self.code.push(0x37);
        self.code.push(0); // align
        leb128::write::unsigned(&mut self.code, offset.into()).unwrap();
    }

    pub fn f32_store(&mut self, offset: u32) {
        self.code.push(0x38);
        self.code.push(0); // align
        leb128::write::unsigned(&mut self.code, offset.into()).unwrap();
    }

    #[allow(unused)]
    pub fn f64_store(&mut self, offset: u32) {
        self.code.push(0x39);
        self.code.push(0); // align
        leb128::write::unsigned(&mut self.code, offset.into()).unwrap();
    }

    pub fn finish(self) {
        let FunctionBuilder {
            module_builder,
            id,
            locals,
            mut code,
            ty,
            local_indices: _,
        } = self;
        code.push(0x0B); // end expr
        debug_assert!(module_builder.func_idxs.contains_key(&id));
        module_builder.functions.push(Function { ty, locals, code });
    }
}
