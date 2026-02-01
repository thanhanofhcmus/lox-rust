use std::{
    cell::{RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
};

use super::value::Value;
use crate::{
    ast::IdentifierNode,
    id::Id,
    interpret::{
        error::Error,
        heap::{GcHandle, GcKind, GcObject, Heap},
        predule,
        value::{Array, Function, VMap},
    },
};

#[derive(Debug)]
struct Scope {
    variables: HashMap<Id, Value>,
    index: usize,
}

impl Scope {
    fn new(index: usize) -> Self {
        Self {
            variables: HashMap::new(),
            index,
        }
    }

    fn get_variable(&self, id: Id) -> Option<Value> {
        self.variables.get(&id).copied()
    }

    fn get_variable_recursive<'a>(&'a self, id: Id, scopes: &'a Vec<Scope>) -> Option<Value> {
        if let Some(value) = self.variables.get(&id) {
            return Some(*value);
        }
        if let Some(parent_scope) = self.get_parent_scope(scopes) {
            return parent_scope.get_variable_recursive(id, scopes);
        }
        None
    }

    fn get_parent_scope<'a>(&self, scopes: &'a [Scope]) -> Option<&'a Scope> {
        if self.index == 0 {
            None
        } else {
            Some(&scopes[self.index - 1])
        }
    }

    fn insert_variable(&mut self, id: Id, value: Value) -> Option<Value> {
        self.variables.insert(id, value)
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
const SCOPE_SIZE_LIMIT: usize = 20;
const GC_TRIGGER_POINT: usize = 100;

macro_rules! decl_gc_type_methods {
    ($name:ident, $variant:ident, $type:ty, $kind:expr, $val_variant:ident) => {
        paste::item! {
            // Generates get_string, get_array, etc.
            pub fn [<get_ $name>](&self, handle: GcHandle) -> Result<&$type, Error> {
                let Some(obj) = self.get_gc_object(handle) else {
                    return Err(Error::GcObjectNotFound(handle));
                };
                let GcObject::$variant(inner) = obj else {
                    return Err(Error::GcObjectWrongType(
                        handle,
                        obj.get_kind(),
                        $kind,
                    ));
                };
                Ok(inner)
            }

            // Generates get_string_mut
            #[allow(dead_code)]
            pub fn [<get_ $name _mut>](&mut self, handle: GcHandle) -> Result<&mut $type, Error> {
                let Some(obj) = self.get_gc_object_mut(handle) else {
                    return Err(Error::GcObjectNotFound(handle));
                };
                let GcObject::$variant(inner) = obj else {
                    return Err(Error::GcObjectWrongType(
                        handle,
                        obj.get_kind(),
                        $kind,
                    ));
                };
                Ok(inner)
            }


            // Generates insert_string_variable, etc.
            pub fn [<insert_ $name _variable>](&mut self, data: $type) -> Value {
                let object = GcObject::$variant(data);
                let handle = self.insert_gc_object(object);
                Value::$val_variant(handle)
            }
        }
    };
}

#[derive(derive_more::Debug)]
pub struct Environment {
    pub(super) heap: Heap,

    scopes: Vec<Scope>,

    modules: HashMap<Id, Module>,

    #[debug(skip)]
    preludes: HashMap<Id, Value>,

    #[debug(skip)]
    current_module_id: Id,

    #[debug(skip)]
    print_writer: Rc<RefCell<dyn std::io::Write>>,
}

impl Environment {
    pub fn new(print_writer: Rc<RefCell<dyn std::io::Write>>) -> Self {
        let current_module_id = Id::new(CURRENT_MODULE_NAME);

        let scopes = vec![Scope::new(0)];
        let mut modules = HashMap::new();
        modules.insert(current_module_id, Module::new(current_module_id));

        Self {
            heap: Heap::new(),
            scopes,
            modules,
            preludes: predule::create(),

            current_module_id,

            print_writer,
        }
    }

    pub(super) fn get_print_writer(&'_ self) -> RefMut<'_, dyn std::io::Write> {
        self.print_writer.borrow_mut()
    }

    pub(super) fn collect_all_variables(&self) -> Vec<Value> {
        let mut variables = self
            .scopes
            .iter()
            .flat_map(|s| s.variables.values().copied())
            .collect::<Vec<_>>();
        let module_vars = self
            .modules
            .iter()
            .flat_map(|(_, v)| v.variables.values().copied());
        variables.extend(module_vars);

        variables
    }

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
        // after a module is parsed, the should only be the "global" scope
        assert!(self.scopes.len() == 1);
        module.variables = std::mem::take(&mut self.get_current_scope_mut().variables);

        // TODO: handle heap

        self.modules.insert(self.current_module_id, module);
    }

    pub fn push_scope(&mut self) {
        let new_scope_index = self.scopes.len();
        if new_scope_index > SCOPE_SIZE_LIMIT {
            panic!("Scope overflow");
        }
        self.scopes.push(Scope::new(new_scope_index));
    }

    pub fn pop_scope(&mut self) {
        let last_scope = self.scopes.pop().expect("Scope underflow");
        for (_, value) in last_scope.variables {
            self.heap.shallow_dispose_value(value);
        }
    }

    fn get_current_scope(&self) -> &Scope {
        self.scopes
            .last()
            .expect("There must be at least one scope")
    }

    fn get_current_scope_mut(&mut self) -> &mut Scope {
        self.scopes
            .last_mut()
            .expect("There must be at least one scope")
    }

    pub fn get_variable_current_scope(&self, id: impl Into<Id>) -> Option<Value> {
        self.get_current_scope().get_variable(id.into())
    }

    pub fn get_variable_all_scope(&self, node: &IdentifierNode) -> Option<Value> {
        self.get_variable_all_scope_by_id(node.get_id(), &node.prefix_ids)
    }

    pub fn get_variable_all_scope_by_id(&self, id: Id, prefix_ids: &[Id]) -> Option<Value> {
        // check scopes/stacks
        if let Some(value) = self
            .get_current_scope()
            .get_variable_recursive(id, &self.scopes)
        {
            return Some(value);
        }

        // get from built-in
        if let Some(value) = self.preludes.get(&id) {
            return Some(*value);
        }

        // No prefixes mean this code is trying to reference values from the trasparent scope only
        if prefix_ids.is_empty() {
            return None;
        }

        // For now, we expect a variable from another module is in the form
        // module_name.variable_name (1 dot, 2 parts)
        let module_id = prefix_ids[0];
        let module = self.modules.get(&module_id)?;

        // This code is allowing code to un-imported module
        // for example print(a.b) -> nil eventhoug we have not import a yet
        module.variables.get(&id).copied()
    }

    pub fn insert_variable_current_scope(
        &mut self,
        id: impl Into<Id>,
        value: Value,
    ) -> Option<Value> {
        let result = self
            .get_current_scope_mut()
            .insert_variable(id.into(), value);
        self.heap.shallow_copy_value(value);

        if self.heap.get_stats().number_of_used_objects >= GC_TRIGGER_POINT {
            let root_values = self.collect_all_variables();
            self.heap.mark(root_values);
            self.heap.sweep();
        }

        result
    }

    pub fn replace_variable_current_scope(
        &mut self,
        id: impl Into<Id>,
        value: Value,
    ) -> Option<Value> {
        let id = id.into();
        if let Some(old_value) = self.get_variable_current_scope(id) {
            self.heap.shallow_dispose_value(old_value);
        };
        self.insert_variable_current_scope(id, value)
    }

    fn insert_gc_object(&mut self, object: GcObject) -> GcHandle {
        self.heap.allocate(object)
    }

    pub fn get_gc_object(&self, handle: GcHandle) -> Option<&GcObject> {
        self.heap.get_object(handle)
    }

    pub fn get_gc_object_mut(&mut self, handle: GcHandle) -> Option<&mut GcObject> {
        self.heap.get_object_mut(handle)
    }

    decl_gc_type_methods!(string, Str, String, GcKind::String, Str);
    decl_gc_type_methods!(array, Array, Array, GcKind::Array, Array);
    decl_gc_type_methods!(map, Map, VMap, GcKind::Map, Map);
    decl_gc_type_methods!(function, Function, Function, GcKind::Function, Function);
}
