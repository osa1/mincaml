use wasm_syntax::{self, Decode, Encode};

/// This function links mc-generated Wasm module with emcc-generated Wasm module for the runtime
/// system, resulting in a statically-linked Wasm module that contains the mc runtime system.
///
/// This function cannot link two arbitrary Wasm modules. It has assumptions about the kind of Wasm
/// emcc and mc generate.
///
/// Details on how linking works:
///
/// - `emcc_module` is only allowed to import "wasi_snapshot_preview" modules. The generated Wasm
///   module will only depend on WASI.
///
/// - Type sections: union type sections of both modules. Maintain a map from function types to
///   their indices in the linked module and update function types indices in the rest of the
///   linked module.
///
/// - Import sections:
///
///   - RTS is only allowed to have "wasi_snapshot_preview" function imports. These
///     are not modified.
///
///   - mc-generated Wasm only imports RTS globals (closures). Those imports are re-numbered in the
///     rest of the program with the imported globals' indices in the RTS module.
///
/// - Function sections: function sections are concatenated and type indices are updated using the
///   map from type sections step above.
///
/// - Table sections: there needs to be one table section in each of the modules and they need to
///   be funcref tables. Merge both in a table section with size enough for both tables.
///
/// - Memory sections: Ignore memory section in mc module, we generate it to make sure the
///   generated Wasm is valid (so that e.g. we can inspect the code with wasm2wat). Use the memory
///   section in the emcc module (there needs to be only one memory), but remove the upper size
///   limit. I don't understand why emcc is limiting the max size. (maybe there is a command line
///   flag to override this?)
///
/// - Global sections: Globals in mc module appended to the globals in emcc module. Global indices
///   in mc module updated.
///
/// - Export sections: mc does not generate export sections. Exports in the RTS module are used to
///   resolve indices of imports in the mc module. Generated module does not have any exports.
///
/// - Start sections: emcc module does not have a start section. mc module start section updated
///   with the index of the new start function after including the functions in the emcc module.
///
/// - Element sections: TODO not sure how to update element indices in mc module
///
/// - Code sections: TODO
///
/// - Data sections: TODO
///
/// - Data count sections: TODO
pub fn link_rts(mc_module: &[u8], emcc_module: &[u8]) {
    let (mc_module, rest) = wasm_syntax::Module::decode(mc_module)
        .unwrap_or_else(|_| panic!("Unable to parse mc-generated Wasm with wasm_syntax"));

    if !rest.is_empty() {
        panic!(
            "wasm_syntax left some bytes unparsed after parsing mc-generated Wasm: {:?}",
            rest
        );
    }

    let (emcc_module, rest) = wasm_syntax::Module::decode(emcc_module)
        .unwrap_or_else(|_| panic!("Unable to parse emcc-generated Wasm with wasm_syntax"));

    if !rest.is_empty() {
        panic!(
            "wasm_syntax left some bytes unparsed after parsing emcc-generated Wasm: {:?}",
            rest
        );
    }
}
