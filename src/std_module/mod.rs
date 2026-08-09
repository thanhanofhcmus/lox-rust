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
        array::typecheck_module(msi, type_interner),
        map::typecheck_module(msi, type_interner),
        json::typecheck_module(msi, type_interner),
        math::typecheck_module(msi, type_interner),
        string::typecheck_module(msi, type_interner),
    ];
    modules
}

/// Returns pre-built interpret modules for all known std modules.
pub fn create_interpret_modules(msi: &mut ModuleStringInterner) -> Vec<(ModuleIdentity, interpret::Module)> {
    let modules = vec![
        array::interpret_module(msi),
        map::interpret_module(msi),
        json::interpret_module(msi),
        math::interpret_module(msi),
        string::interpret_module(msi),
    ];
    modules
}
