use std::collections::HashMap;

use crate::{
    id::Id,
    identifier_registry::ComplexId,
    module::{ModuleIdentity, ModuleRegistry as GenericModuleRegistry},
    types::{SliceId, StructField, Type, TypeId, TypeInterner, TypeScope},
};

pub type ModuleRegistry = GenericModuleRegistry<Module>;

#[derive(Debug, Clone, Default)]
pub struct Module {
    pub(crate) symbol_scope: TypeScope,
    pub(crate) struct_scope: TypeScope,
}

/// Scoped symbol table mapping `Id` → `Type` for typechecking.
/// The outermost scope is seeded with interpreter builtins (see `BUILTIN_NAMES`).
pub struct Environment<'a> {
    scopes: Vec<TypeScope>,
    local_struct_scope: TypeScope,
    imported_modules: HashMap<Id, ModuleIdentity>,
    // TODO: move preludes to shared
    preludes: TypeScope,
    type_interner: &'a mut TypeInterner,
    module_registry: &'a ModuleRegistry,
}

impl<'a> Environment<'a> {
    pub fn new(type_interner: &'a mut TypeInterner, module_registry: &'a ModuleRegistry) -> Self {
        let mut preludes = TypeScope::new();

        for &name in BUILTIN_NAMES {
            let type_ = get_builtin_fn_type(name);
            let type_id = type_interner.intern_type(&type_).0;
            preludes.associate(Id::new(name), type_id);
        }

        Self {
            // Scope 0 is the user-global scope
            scopes: vec![TypeScope::new()],
            local_struct_scope: TypeScope::new(),
            imported_modules: HashMap::new(),
            preludes,
            type_interner,
            module_registry,
        }
    }

    pub fn from_module(
        module: Module,
        type_interner: &'a mut TypeInterner,
        module_registry: &'a ModuleRegistry,
    ) -> Self {
        let Module {
            symbol_scope,
            struct_scope,
        } = module;
        let mut env = Self::new(type_interner, module_registry);

        env.local_struct_scope = struct_scope;
        _ = std::mem::replace(env.scopes.get_mut(0).unwrap(), symbol_scope);

        env
    }

    pub fn add_module(&mut self, id: Id, identity: ModuleIdentity) {
        self.imported_modules.insert(id, identity);
    }

    pub fn make_module(&mut self) -> Module {
        let symbol_scope = std::mem::take(self.scopes.get_mut(0).unwrap());
        let struct_scope = std::mem::replace(&mut self.local_struct_scope, TypeScope::new());
        Module {
            symbol_scope,
            struct_scope,
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(TypeScope::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("scope underflow");
    }

    /// Declare `id` in the current (innermost) scope. Returns `Err` with the
    /// existing type if `id` is already bound *in the same scope*. Shadowing
    /// a binding from an outer scope is allowed and is not an error.
    pub fn declare_variable_id(&mut self, id: Id, type_id: TypeId) -> Result<(), TypeId> {
        if id == Id::UNDERSCORE {
            return Ok(());
        }
        let scope = self.scopes.last_mut().expect("must have at least one scope");
        match scope.get_type_id(id) {
            Some(type_id) => Err(type_id),
            None => {
                scope.associate(id, type_id);
                Ok(())
            }
        }
    }

    pub fn lookup_variable_id(&self, cid: ComplexId) -> Option<TypeId> {
        // Module-qualified name (e.g. `math::add`): ONLY search that module.
        if let Some(module_id) = cid.module {
            let identity = self.imported_modules.get(&module_id)?;
            let module = self.module_registry.get(identity)?;
            return module.symbol_scope.get_type_id(cid.name);
        }

        // Unqualified name: check local scopes, then preludes.
        self.scopes
            .iter()
            .rev()
            .find_map(|s| s.get_type_id(cid.name))
            .or_else(|| self.preludes.get_type_id(cid.name))
    }

    /// Return only the TypeId
    pub fn declare_type(&mut self, type_: &Type) -> TypeId {
        self.declare_type_with_check(type_).0
    }

    /// Return the TypeId and if the type has already been declared before
    pub fn declare_type_with_check(&mut self, type_: &Type) -> (TypeId, bool) {
        // if 2 modules declare 2 points type, they would be the same here
        // should we allow that or should we make the type_id unique per module?
        self.type_interner.intern_type(type_)
    }

    pub fn lookup_type(&self, type_id: TypeId) -> Option<Type> {
        self.type_interner.get_type(type_id)
    }

    pub fn declare_type_id_slice(&mut self, v: Vec<TypeId>) -> SliceId {
        self.type_interner.intern_type_id_slice(v)
    }

    pub fn declare_struct_field_slice(&mut self, v: Vec<StructField>) -> SliceId {
        self.type_interner.intern_struct_field_slice(v)
    }

    pub fn lookup_type_id_slice(&self, slice_id: SliceId) -> &[TypeId] {
        self.type_interner.lookup_type_id_slice(slice_id)
    }

    pub fn lookup_struct_field_slice(&self, slice_id: SliceId) -> &[StructField] {
        self.type_interner.lookup_struct_field_slice(slice_id)
    }

    pub fn associate_id_with_type(&mut self, id: Id, type_id: TypeId) {
        if id == Id::UNDERSCORE {
            return;
        }
        self.local_struct_scope.associate(id, type_id);
    }

    pub fn lookup_type_id(&self, cid: ComplexId) -> Option<TypeId> {
        let scope = if let Some(module_id) = cid.module {
            let module_identity = self.imported_modules.get(&module_id)?;
            let module = self.module_registry.get(module_identity)?;
            &module.struct_scope
        } else {
            &self.local_struct_scope
        };
        scope.get_type_id(cid.name)
    }
}

/// Keep in sync with `src/interpret/prelude.rs::create()`. Registered as
/// `Type::Any` — we don't model their signatures yet.
const BUILTIN_NAMES: &[&str] = &[
    "print",
    "assert",
    "_dbg_print",
    "_dbg_state",
    "_dbg_gc_mark",
    "_dbg_gc_sweep",
    "_dbg_gc_mark_sweep",
    "_dbg_heap_stats",
];

fn get_builtin_fn_type(name: &str) -> Type {
    match name {
        "print" => Type::FUNCTION_EMPTY_VARIADIC_STR_TO_NIL,
        "assert" => Type::FUNCTION_BOOL_STR_TO_NIL,
        "_dbg_print" => Type::FUNCTION_EMPTY_VARIADIC_ANY_TO_NIL,
        "_dbg_state" | "_dbg_gc_mark" | "_dbg_gc_sweep" | "_dbg_gc_mark_sweep" | "_dbg_heap_stats" => {
            Type::FUNCTION_EMPTY_TO_NIL
        }
        _ => Type::Any,
    }
}
