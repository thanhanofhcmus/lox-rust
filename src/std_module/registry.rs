use crate::{interpret::BuiltinFn, types::Type};

// ---------------------------------------------------------------------------
// StdFnEntry — metadata for one standard-library function
// ---------------------------------------------------------------------------

pub(crate) struct StdFnEntry {
    pub name: &'static str,
    pub type_: Type,
    pub fn_ptr: BuiltinFn,
}

/// This expands to:
/// 1. The `fn` item as written
/// 2. A `const` named `__std_fn_entry_<name>` (where `<name>` is the function ident)
#[macro_export]
macro_rules! std_fn {
    ($module:literal, $name:literal, $type_:expr, fn $fn_name:ident($($args:tt)*) $(-> $ret:ty)? { $($body:tt)* }) => {
        fn $fn_name($($args)*) $(-> $ret)? {
            $($body)*
        }

        paste::paste! {
            #[allow(non_upper_case_globals)]
            const [<__std_fn_entry_ $fn_name>]: $crate::std_module::registry::StdFnEntry =
                $crate::std_module::registry::StdFnEntry {
                    name: $name,
                    type_: $type_,
                    fn_ptr: $fn_name as $crate::interpret::BuiltinFn,
                };
        }
    };
}

// ---------------------------------------------------------------------------
// collect_std_fns! — gather per-module entries and emit builder functions
// ---------------------------------------------------------------------------
#[macro_export]
macro_rules! collect_std_fns {
    (
        $module_path:literal,
        functions: [$($fn_ident:ident),* $(,)?],
        constants: [$(( $const_name:literal, $const_type:expr, $const_value:expr )),* $(,)?]
    ) => {
        paste::paste! {
            const MODULE_FNS: &[$crate::std_module::registry::StdFnEntry] = &[
                $([<__std_fn_entry_ $fn_ident>]),*
            ];
        }

        fn identity(
            msi: &mut $crate::module::ModuleStringInterner,
        ) -> $crate::module::ModuleIdentity {
            $crate::module::ModuleIdentity {
                resolved_path: msi.intern($module_path),
                is_std: true,
            }
        }

        pub(crate) fn create_typecheck_module(
            msi: &mut $crate::module::ModuleStringInterner,
            type_interner: &mut $crate::types::TypeInterner,
        ) -> ($crate::module::ModuleIdentity, $crate::typecheck::Module) {
            let ident = identity(msi);

            let mut symbol_scope = $crate::types::TypeScope::new();

            for entry in MODULE_FNS {
                let (type_id, _) = type_interner.intern_type(&entry.type_);
                symbol_scope.associate($crate::id::Id::new(entry.name), type_id);
            }

            $(
                let __const_id = $crate::id::Id::new($const_name);
                let (__type_id, _) = type_interner.intern_type(&$const_type);
                symbol_scope.associate(__const_id, __type_id);
            )*

            let module = $crate::typecheck::Module::new(symbol_scope);

            (ident, module)
        }

        #[allow(unused_mut)]
        pub(crate) fn create_interpret_module(
            msi: &mut $crate::module::ModuleStringInterner,
        ) -> ($crate::module::ModuleIdentity, $crate::interpret::Module) {
            let ident = identity(msi);

            let mut variables = std::collections::HashMap::new();

            for entry in MODULE_FNS {
                variables.insert(
                    $crate::id::Id::new(entry.name),
                    $crate::interpret::Value::BuiltinFunction(entry.fn_ptr),
                );
            }

            $(
                variables.insert(
                    $crate::id::Id::new($const_name),
                    $crate::interpret::Value::from($const_value),
                );
            )*

            let module = $crate::interpret::Module::new(variables);

            (ident, module)
        }
    };
}
