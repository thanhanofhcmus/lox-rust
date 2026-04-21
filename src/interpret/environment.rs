use std::{
    cell::{RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
};

use super::values::Value;
use crate::{
    ast::IdentifierNode,
    id::Id,
    interpret::{
        debug_string::DebugString,
        error::InterpretError,
        heap::{GcHandle, GcKind, GcObject, Heap},
        predule,
        string_interner::StrId,
        values::{Array, Function, Map},
    },
};

#[derive(Debug)]
struct Scope {
    variables: HashMap<Id, Value>,
    is_readonly: bool,
}

impl Scope {
    fn new(is_readonly: bool) -> Self {
        Self {
            variables: HashMap::new(),
            is_readonly,
        }
    }

    fn get_variable(&self, id: Id) -> Option<Value> {
        self.variables.get(&id).copied()
    }

    fn insert_variable(&mut self, id: Id, value: Value) -> Option<Value> {
        self.variables.insert(id, value)
    }
}

#[derive(Debug)]
struct ScopeStack(Vec<Scope>);

impl ScopeStack {
    fn new() -> Self {
        Self(vec![Scope::new(false)])
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

#[derive(Debug)]
struct Module {
    variables: HashMap<Id, Value>,
    #[allow(unused)]
    id: Id,
}

impl Module {
    fn new(id: Id) -> Self {
        Self {
            variables: HashMap::new(),
            id,
        }
    }
}

const CURRENT_MODULE_NAME: &str = "__current__";
const SCOPE_SIZE_LIMIT: usize = 100;

#[derive(derive_more::Debug)]
pub struct Environment {
    pub(super) heap: Heap,

    scope_stack: ScopeStack,

    modules: HashMap<Id, Module>,

    preludes: HashMap<Id, Value>,

    current_module_id: Id,

    #[debug("<writer>")]
    print_writer: Rc<RefCell<dyn std::io::Write>>,

    pub(super) strict_assert: bool,
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

impl Environment {
    pub fn new(
        print_writer: Rc<RefCell<dyn std::io::Write>>,
        strict_assert: bool,
    ) -> Self {
        let current_module_id = Id::new(CURRENT_MODULE_NAME);

        let modules = HashMap::from([(current_module_id, Module::new(current_module_id))]);

        Self {
            heap: Heap::new(),
            scope_stack: ScopeStack::new(),
            modules,
            preludes: predule::create(),
            current_module_id,

            print_writer,

            strict_assert,
        }
    }

    pub(super) fn get_print_writer(&'_ self) -> RefMut<'_, dyn std::io::Write> {
        self.print_writer.borrow_mut()
    }

    pub(super) fn collect_all_variables(&self) -> Vec<Value> {
        let mut variables = self
            .scope_stack
            .iter_all()
            .flat_map(|s| s.variables.values().copied())
            .collect::<Vec<_>>();
        let module_vars = self
            .modules
            .iter()
            .flat_map(|(_, v)| v.variables.values().copied());
        variables.extend(module_vars);

        variables
    }

    // Module functions

    pub fn contains_module(&self, id: Id) -> bool {
        self.modules.contains_key(&id)
    }

    pub fn init_module(&mut self, id: Id) {
        self.current_module_id = id;
        // if we were to allow import in any places, we need to stash the stack
        // of the current module and restore it later
    }

    pub fn deinit_module(&mut self) {
        let mut module = Module::new(self.current_module_id);
        // after a module is parsed, there should only be the "global" scope
        assert!(self.scope_stack.len() == 1);
        // Transfer variables to the module instead of disposing them —
        // do NOT call pop_scope() here, which would shallow_dispose every value.
        let last_scope = self
            .scope_stack
            .pop()
            .expect("scope underflow in deinit_module");
        module.variables = last_scope.variables;

        // TODO: handle heap

        self.modules.insert(self.current_module_id, module);
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

    pub fn get_variable_current_scope(&self, id: impl Into<Id>) -> Option<Value> {
        self.scope_stack.get_current().get_variable(id.into())
    }

    pub fn get_variable_all_scope(
        &self,
        node: &IdentifierNode,
    ) -> Option<(Value, bool /* is_readonly */)> {
        let id = node.get_id();
        // check scopes/stacks
        for scope in self.scope_stack.iter_outward() {
            if let Some(value) = scope.get_variable(id) {
                return Some((value, scope.is_readonly));
            }
        }

        // from now on, the variable is always readonly

        // get from built-in
        if let Some(value) = self.preludes.get(&id) {
            return Some((*value, true));
        }

        None
    }

    pub fn insert_variable_current_scope(
        &mut self,
        id: impl Into<Id>,
        value: Value,
    ) -> Option<Value> {
        let old_value = self
            .scope_stack
            .get_current_mut()
            .insert_variable(id.into(), value);
        self.heap.shallow_copy_value(value);
        old_value
    }

    pub fn replace_variable_function_scope(&mut self, id: impl Into<Id>, value: Value) -> bool {
        let id = id.into();
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

        writeln!(s, "Current module: {:?}", self.current_module_id).unwrap();
        writeln!(s).unwrap();
        writeln!(s, "Modules:").unwrap();
        for module in self.modules.values() {
            if module.variables.is_empty() {
                writeln!(s, "  {:?}: (empty)", module.id).unwrap();
            } else {
                writeln!(s, "  {:?}:", module.id).unwrap();
                for (id, value) in &module.variables {
                    writeln!(s, "    {id:?} = {value:?}").unwrap();
                }
            }
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
    use std::{cell::RefCell, rc::Rc};

    fn make_env() -> Environment {
        Environment::new(Rc::new(RefCell::new(std::io::sink())), false)
    }

    #[test]
    fn push_and_pop_scope_ok() {
        let mut env = make_env();
        env.push_scope(false).unwrap();
        env.pop_scope().unwrap();
    }

    #[test]
    fn pop_beyond_empty_returns_underflow() {
        let mut env = make_env();
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
        let mut env = make_env();
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
        use crate::interpret::values::Scalar;

        let mut env = make_env();
        let id = Id::new("x");
        env.insert_variable_current_scope(id, Value::Scalar(Scalar::Bool(true)));
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

        let mut env = make_env();
        let id = Id::new("x");
        env.insert_variable_current_scope(id, Value::Scalar(Scalar::Bool(false)));

        // Push a readonly scope on top — replace must skip it and update the outer scope.
        env.push_scope(true).unwrap();
        let found = env.replace_variable_function_scope(id, Value::Scalar(Scalar::Bool(true)));
        assert!(found);

        env.pop_scope().unwrap();
        let val = env.get_variable_current_scope(id).unwrap();
        assert!(matches!(val, Value::Scalar(Scalar::Bool(true))));
    }
}
