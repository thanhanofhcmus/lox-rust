mod array;
mod helpers;
mod json;
mod map;
mod math;
pub(crate) mod registry;
mod string;

use crate::{
    interpret,
    module::{ModuleIdentity, ModuleStringInterner},
    typecheck,
    types::TypeInterner,
};

/// Returns pre-built typecheck modules for all known std modules.
pub fn create_typecheck_modules(
    msi: &mut ModuleStringInterner,
    type_interner: &mut TypeInterner,
) -> Vec<(ModuleIdentity, typecheck::Module)> {
    let modules = vec![
        array::create_typecheck_module(msi, type_interner),
        map::create_typecheck_module(msi, type_interner),
        json::create_typecheck_module(msi, type_interner),
        math::create_typecheck_module(msi, type_interner),
        string::create_typecheck_module(msi, type_interner),
    ];
    modules
}

/// Returns pre-built interpret modules for all known std modules.
pub fn create_interpret_modules(msi: &mut ModuleStringInterner) -> Vec<(ModuleIdentity, interpret::Module)> {
    let modules = vec![
        array::create_interpret_module(msi),
        map::create_interpret_module(msi),
        json::create_interpret_module(msi),
        math::create_interpret_module(msi),
        string::create_interpret_module(msi),
    ];
    modules
}
