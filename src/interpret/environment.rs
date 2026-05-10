use std::collections::HashMap;

use super::values::Value;
use crate::{
    id::Id,
    identifier_registry::ComplexId,
    interpret::{
        debug_string::DebugString,
        error::InterpretError,
        heap::{GcHandle, GcKind, GcObject, Heap, StrId},
        prelude,
        values::{Array, Function, Map, Struct, Tuple},
    },
    module::{ModuleMetadata, ModuleRegistry as GenericModuleRegistry},
};

#[derive(Debug)]
struct Scope {
    variables: HashMap<Id, Value>,
    is_readonly: bool,
}

impl Scope {
    fn make_global() -> Self {
        Self::new(false)
    }

    fn new(is_readonly: bool) -> Self {
        Self {
            variables: HashMap::new(),
            is_readonly,
        }
    }

    fn from_map(variables: HashMap<Id, Value>, is_readonly: bool) -> Self {
        Self { variables, is_readonly }
    }

    fn get_variable(&self, id: Id) -> Option<Value> {
        self.variables.get(&id).copied()
    }

    fn insert_variable(&mut self, id: Id, value: Value) -> Option<Value> {
        self.variables.insert(id, value)
    }

    fn insert_multiple_variables(&mut self, variables: HashMap<Id, Value>) {
        self.variables.extend(variables)
    }
}

#[derive(Debug)]
struct ScopeStack(Vec<Scope>);

impl ScopeStack {
    fn new() -> Self {
        Self(vec![Scope::make_global()])
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn push(&mut self, is_readonly: bool) -> Result<(), InterpretError> {
        if self.0.len() > SCOPE_SIZE_LIMIT {
            return Err(InterpretError::ScopeOverflow(SCOPE_SIZE_LIMIT));
        }
        self.0.push(Scope::new(is_readonly));
        Ok(())
    }

    fn pop(&mut self) -> Result<Scope, InterpretError> {
        self.0.pop().ok_or(InterpretError::ScopeUnderflow)
    }

    fn get_current(&self) -> &Scope {
        self.0.last().expect("There must be at least one scope")
    }

    fn get_current_mut(&mut self) -> &mut Scope {
        self.0.last_mut().expect("There must be at least one scope")
    }

    fn iter_outward(&self) -> impl Iterator<Item = &Scope> {
        self.0.iter().rev()
    }

    fn iter_outward_mut(&mut self) -> impl Iterator<Item = &mut Scope> {
        self.0.iter_mut().rev()
    }

    fn iter_all(&self) -> impl Iterator<Item = &Scope> {
        self.0.iter()
    }
}

#[derive(Debug, Default)]
pub struct Module {
    variables: HashMap<Id, Value>,
}

pub type ModuleRegistry = GenericModuleRegistry<Module>;

const SCOPE_SIZE_LIMIT: usize = 100;

#[derive(derive_more::Debug)]
pub struct Environment<'a> {
    pub(super) heap: &'a mut Heap,
    scope_stack: ScopeStack,
    imported_modules: HashMap<Id, ModuleMetadata>,

    // TODO: move preludes to shared
    preludes: Scope,
    module_registry: &'a ModuleRegistry,
}

macro_rules! decl_gc_type_methods {
    ($name:ident, $variant:ident, $type:ty, $kind:expr, $val_variant:ident) => {
        paste::item! {
            // Generates get_array, get_function, etc.
            pub fn [<get_ $name>](&self, handle: GcHandle) -> Result<&$type, InterpretError> {
                let Some(obj) = self.heap.get_object(handle) else {
                    return Err(InterpretError::GcObjectNotFound(handle));
                };
                let GcObject::$variant(inner) = obj else {
                    return Err(InterpretError::GcObjectWrongType(
                        handle,
                        obj.get_kind(),
                        $kind,
                    ));
                };
                Ok(inner)
            }

            // Generates insert_array_variable, etc.
            pub fn [<insert_ $name _variable>](&mut self, data: $type) -> Value {
                let object = GcObject::$variant(data);
                let handle = self.heap.insert_object(object);
                Value::$val_variant(handle)
            }
        }
    };
}

impl<'a> Environment<'a> {
    pub fn new(heap: &'a mut Heap, module_registry: &'a ModuleRegistry) -> Self {
        Self {
            heap,
            scope_stack: ScopeStack::new(),
            imported_modules: HashMap::new(),
            preludes: Scope::from_map(prelude::create(), true),
            module_registry,
        }
    }

    pub fn from_module(Module { variables }: Module, heap: &'a mut Heap, module_registry: &'a ModuleRegistry) -> Self {
        let mut env = Self::new(heap, module_registry);
        assert!(env.scope_stack.len() == 1, "env should only have the global scope here");
        env.scope_stack.get_current_mut().insert_multiple_variables(variables);
        env
    }

    pub(super) fn collect_all_variables(&self) -> Vec<Value> {
        let mut variables = self
            .scope_stack
            .iter_all()
            .flat_map(|s| s.variables.values().copied())
            .collect::<Vec<_>>();

        // Collect variables from all imported modules so the GC sweep keeps
        // them alive.  TODO: this only covers modules that are *directly*
        // imported by the currently-running module.  If module A imports B
        // but the current module only imports A, B's exported variables are
        // not collected here.  We need a full transitive walk in the future.
        for metadata in self.imported_modules.values() {
            if let Some(module) = self.module_registry.get(metadata) {
                variables.extend(module.variables.values().copied());
            }
        }

        variables
    }

    // Module functions

    pub fn add_module(&mut self, id: Id, metadata: ModuleMetadata) {
        self.imported_modules.insert(id, metadata);
    }

    pub fn make_module(&mut self) -> Module {
        // Transfer variables to the module instead of disposing them
        // do NOT call pop_scope() here, which would shallow_dispose every value.
        // We use get_current_mut here because we assume the current scope is the last scope
        // i.e. The global scope
        let last_scope = std::mem::replace(self.scope_stack.get_current_mut(), Scope::make_global());
        let variables = last_scope.variables;

        Module { variables }
    }

    // Scope functions

    pub fn push_scope(&mut self, is_readonly: bool) -> Result<(), InterpretError> {
        self.scope_stack.push(is_readonly)
    }

    pub fn pop_scope(&mut self) -> Result<(), InterpretError> {
        let last_scope = self.scope_stack.pop()?;
        for (_, value) in last_scope.variables {
            self.heap.shallow_dispose_value(value);
        }
        Ok(())
    }

    // Variable functions

    pub fn get_variable_current_scope(&self, id: Id) -> Option<Value> {
        self.scope_stack.get_current().get_variable(id)
    }

    pub fn get_variable_all_scope(&self, cid: ComplexId) -> Option<(Value, bool /* is_readonly */)> {
        // check scopes/stacks
        for scope in self.scope_stack.iter_outward() {
            if let Some(value) = scope.get_variable(cid.name) {
                return Some((value, scope.is_readonly));
            }
        }

        // from now on, the variable is always readonly

        // get from built-in
        if let Some(value) = self.preludes.get_variable(cid.name) {
            return Some((value, true));
        }

        let module_id = cid.module?;
        let metadata = self.imported_modules.get(&module_id)?;
        let module = self.module_registry.get(metadata)?;
        module.variables.get(&cid.name).copied().map(|v| (v, true))
    }

    pub fn insert_variable(&mut self, id: Id, value: Value) -> Option<Value> {
        // underscore does not hold an old value
        if id == Id::UNDERSCORE {
            return None;
        }
        let old_value = self.scope_stack.get_current_mut().insert_variable(id, value);
        self.heap.shallow_copy_value(value);
        old_value
    }

    pub fn replace_variable(&mut self, id: Id, value: Value) -> bool {
        // underscore does not hold an old value
        if id == Id::UNDERSCORE {
            return false;
        }
        for scope in self.scope_stack.iter_outward_mut() {
            if scope.is_readonly {
                continue;
            }
            if let Some(old_value) = scope.get_variable(id) {
                // insert new value and return the old one
                // the return value should be the same as old_value
                // we can use either to dispose the actual value in the heap
                self.heap.shallow_copy_value(value);
                self.heap.shallow_dispose_value(old_value);
                scope.insert_variable(id, value);
                return true;
            }
        }
        false
    }

    pub fn get_string(&self, id: StrId) -> Result<&str, InterpretError> {
        self.heap.get_string_or_error(id)
    }

    pub fn insert_string_id(&mut self, s: String) -> StrId {
        self.heap.insert_string(s)
    }

    pub fn insert_string_variable(&mut self, s: String) -> Value {
        Value::Str(self.insert_string_id(s))
    }

    decl_gc_type_methods!(array, Array, Array, GcKind::Array, Array);
    decl_gc_type_methods!(map, Map, Map, GcKind::Map, Map);
    decl_gc_type_methods!(tuple, Tuple, Tuple, GcKind::Tuple, Tuple);
    decl_gc_type_methods!(struct, Struct, Struct, GcKind::Struct, Struct);
    decl_gc_type_methods!(function, Function, Function, GcKind::Function, Function);

    pub fn debug_state_string(&self) -> String {
        use std::fmt::Write as FmtWrite;
        let mut s = String::new();

        writeln!(s, "Scope Stack [depth={}]:", self.scope_stack.len()).unwrap();
        for (i, scope) in self.scope_stack.iter_all().enumerate() {
            let kind = if scope.is_readonly { "readonly" } else { "writable" };
            writeln!(s, "  [{i}] ({kind}):").unwrap();
            if scope.variables.is_empty() {
                writeln!(s, "    (empty)").unwrap();
            } else {
                for (id, value) in &scope.variables {
                    writeln!(s, "    {id:?} = {value:?}").unwrap();
                }
            }
        }
        writeln!(s).unwrap();
        writeln!(s).unwrap();
        writeln!(s, "Modules:").unwrap();
        for module_id in &self.imported_modules {
            writeln!(s, "  {:?}:", module_id).unwrap();
        }
        writeln!(s).unwrap();

        s += &self.heap.debug_string();
        writeln!(s).unwrap();

        s
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn push_and_pop_scope_ok() {
        let module_restritry = ModuleRegistry::default();
        let mut heap = Heap::new();
        let mut env = Environment::new(&mut heap, &module_restritry);
        env.push_scope(false).unwrap();
        env.pop_scope().unwrap();
    }

    #[test]
    fn pop_beyond_empty_returns_underflow() {
        let module_restritry = ModuleRegistry::default();
        let mut heap = Heap::new();
        let mut env = Environment::new(&mut heap, &module_restritry);
        // ScopeStack starts with one scope; first pop succeeds, second triggers underflow.
        env.pop_scope().unwrap();
        let err = env.pop_scope().unwrap_err();
        assert!(
            matches!(err, InterpretError::ScopeUnderflow),
            "expected ScopeUnderflow, got {err:?}"
        );
    }

    #[test]
    fn scope_overflow_returns_error() {
        let module_restritry = ModuleRegistry::default();
        let mut heap = Heap::new();
        let mut env = Environment::new(&mut heap, &module_restritry);
        // Push past SCOPE_SIZE_LIMIT (100) to trigger overflow.
        for _ in 0..SCOPE_SIZE_LIMIT {
            env.push_scope(false).unwrap();
        }
        let err = env.push_scope(false).unwrap_err();
        assert!(
            matches!(err, InterpretError::ScopeOverflow(_)),
            "expected ScopeOverflow, got {err:?}"
        );
    }

    #[test]
    fn variable_visible_in_inner_scope() {
        use crate::id::Id;

        let module_restritry = ModuleRegistry::default();
        let mut heap = Heap::new();
        let mut env = Environment::new(&mut heap, &module_restritry);
        let id = Id::new("x");
        env.insert_variable(id, Value::make_bool(true));
        env.push_scope(false).unwrap();

        // Variable is not in the inner scope's own slot.
        assert!(env.get_variable_current_scope(id).is_none());

        env.pop_scope().unwrap();
        assert!(env.get_variable_current_scope(id).is_some());
    }

    #[test]
    fn readonly_scope_skipped_on_replace() {
        use crate::id::Id;
        use crate::interpret::values::Scalar;

        let module_restritry = ModuleRegistry::default();
        let mut heap = Heap::new();
        let mut env = Environment::new(&mut heap, &module_restritry);
        env.push_scope(false).unwrap();
        let id = Id::new("x");
        env.insert_variable(id, Value::make_bool(false));

        // Push a readonly scope on top — replace must skip it and update the outer scope.
        env.push_scope(true).unwrap();
        let found = env.replace_variable(id, Value::make_bool(true));
        assert!(found);

        env.pop_scope().unwrap();
        let (val, is_readonly) = env
            .get_variable_all_scope(ComplexId { module: None, name: id })
            .unwrap();
        assert!(matches!(val, Value::Scalar(Scalar::Bool(true))));
        assert!(!is_readonly);
    }
}
