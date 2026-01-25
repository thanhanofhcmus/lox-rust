use std::{
    cell::{RefCell, RefMut},
    collections::HashMap,
    fmt,
    rc::Rc,
};

use super::value::Value;
use crate::{
    ast::IdentifierNode,
    id::Id,
    interpret::{
        error::Error,
        gc::{GcHandle, GcObject, Heap},
        predule,
        value::{Array, Map},
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
    // todo functions
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

pub struct Environment {
    heap: Heap,
    scopes: Vec<Scope>,
    modules: HashMap<Id, Module>,
    preludes: HashMap<Id, Value>,

    current_module_id: Id,
    print_writer: Rc<RefCell<dyn std::io::Write>>,
}

impl fmt::Debug for Environment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Environment")
            .field("heap", &self.heap)
            .field("scopes", &self.scopes)
            .field("modules", &self.modules)
            // .field("current_module_id", &self.current_module_id)
            // .field("preludes", &self.preludes)
            // .field("print_writer", &self.print_writer)
            .finish()
    }
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

        // TOOD: handle heap

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

    pub fn get_variable_current_scope(&self, node: &IdentifierNode) -> Option<Value> {
        self.get_current_scope().get_variable(node.get_id())
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
        node: &IdentifierNode,
        value: Value,
    ) -> Option<Value> {
        self.insert_variable_current_scope_by_id(node.get_id(), value)
    }

    pub fn insert_variable_current_scope_by_id(&mut self, id: Id, value: Value) -> Option<Value> {
        self.heap.shallow_copy_value(value);
        self.get_current_scope_mut().insert_variable(id, value)
    }

    pub fn replace_variable_current_scope(
        &mut self,
        node: &IdentifierNode,
        value: Value,
    ) -> Option<Value> {
        if let Some(old_value) = self.get_variable_current_scope(node) {
            self.heap.shallow_dispose_value(old_value);
        };
        self.insert_variable_current_scope(node, value)
    }

    pub fn insert_gc_object(&mut self, object: GcObject) -> GcHandle {
        self.heap.allocate(object)
    }

    pub fn get_gc_object(&self, handle: GcHandle) -> Option<&GcObject> {
        self.heap.get_object(handle)
    }

    pub fn get_string(&self, handle: GcHandle) -> Result<&String, Error> {
        let Some(obj) = self.get_gc_object(handle) else {
            return Err(Error::NotFoundGcObject(handle));
        };
        let GcObject::Str(str) = obj else {
            return Err(Error::WrongTypeGcObject(
                handle,
                obj.type_name().to_string(),
                // TODO: fix this when we split Kind and Value enum
                GcObject::Str(String::new()).type_name().to_string(),
            ));
        };
        Ok(str)
    }

    pub fn insert_string_variable(&mut self, s: String) -> Value {
        let object = GcObject::Str(s);
        let handle = self.insert_gc_object(object);
        Value::Str(handle)
    }

    pub fn get_array(&self, handle: GcHandle) -> Result<&Array, Error> {
        let Some(obj) = self.get_gc_object(handle) else {
            return Err(Error::NotFoundGcObject(handle));
        };
        let GcObject::Array(arr) = obj else {
            return Err(Error::WrongTypeGcObject(
                handle,
                obj.type_name().to_string(),
                // TODO: fix this when we split Kind and Value enum
                GcObject::Array(Array::new()).type_name().to_string(),
            ));
        };
        Ok(arr)
    }

    pub fn insert_array_variable(&mut self, arr: Array) -> Value {
        let object = GcObject::Array(arr);
        let handle = self.insert_gc_object(object);
        Value::Array(handle)
    }

    pub fn get_map(&self, handle: GcHandle) -> Result<&Map, Error> {
        let Some(obj) = self.get_gc_object(handle) else {
            return Err(Error::NotFoundGcObject(handle));
        };
        let GcObject::Map(map) = obj else {
            return Err(Error::WrongTypeGcObject(
                handle,
                obj.type_name().to_string(),
                // TODO: fix this when we split Kind and Value enum
                GcObject::Map(Map::new()).type_name().to_string(),
            ));
        };
        Ok(map)
    }

    pub fn insert_map_variable(&mut self, map: Map) -> Value {
        let object = GcObject::Map(map);
        let handle = self.insert_gc_object(object);
        Value::Map(handle)
    }
}
