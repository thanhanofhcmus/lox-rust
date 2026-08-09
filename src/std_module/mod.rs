mod array;
mod helpers;
mod json;
mod map;
mod math;
mod string;

pub(crate) use self::json::json_interpret_module;
pub(crate) use self::json::json_typecheck_module;
pub(crate) use self::{array::array_interpret_module, array::array_typecheck_module};
pub(crate) use self::{map::map_interpret_module, map::map_typecheck_module};
pub(crate) use self::{math::math_interpret_module, math::math_typecheck_module};
pub(crate) use self::{string::string_interpret_module, string::string_typecheck_module};

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
        array_typecheck_module(msi, type_interner),
        map_typecheck_module(msi, type_interner),
        json_typecheck_module(msi, type_interner),
        math_typecheck_module(msi, type_interner),
        string_typecheck_module(msi, type_interner),
    ];
    modules
}

/// Returns pre-built interpret modules for all known std modules.
pub fn create_interpret_modules(msi: &mut ModuleStringInterner) -> Vec<(ModuleIdentity, interpret::Module)> {
    let modules = vec![
        array_interpret_module(msi),
        map_interpret_module(msi),
        json_interpret_module(msi),
        math_interpret_module(msi),
        string_interpret_module(msi),
    ];
    modules
}
