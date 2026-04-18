use std::collections::HashMap;

use crate::{id::Id, types::Type};

/// Keep in sync with `src/interpret/predule.rs::create()`. Registered as
/// `Type::Any` — we don't model their signatures yet.
const BUILTIN_NAMES: &[&str] = &[
    "print",
    "assert",
    "from_json",
    "to_json",
    "array_len",
    "array_push",
    "array_pop",
    "array_insert",
    "map_length",
    "map_keys",
    "map_values",
    "map_insert",
    "map_remove",
    "_dbg_print",
    "_dbg_state",
    "_dbg_gc_mark",
    "_dbg_gc_sweep",
    "_dbg_gc_mark_sweep",
    "_dbg_heap_stats",
];

/// Scoped symbol table mapping `Id` → `Type` for typechecking.
/// The outermost scope is seeded with interpreter builtins (see `BUILTIN_NAMES`).
pub struct Environment {
    scopes: Vec<HashMap<Id, Type>>,
}

impl Environment {
    pub fn new() -> Self {
        let mut builtins = HashMap::new();
        for &name in BUILTIN_NAMES {
            builtins.insert(Id::new(name), Type::Any);
        }
        // Scope 0 holds builtins; scope 1 is the user-global scope. Keeping them
        // separate lets user code shadow builtins without a redeclaration error.
        Self {
            scopes: vec![builtins, HashMap::new()],
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("scope underflow");
    }

    /// Declare `id` in the current (innermost) scope. Returns `Err` with the
    /// existing type if `id` is already bound *in the same scope*. Shadowing
    /// a binding from an outer scope is allowed and is not an error.
    pub fn declare(&mut self, id: Id, ty: Type) -> Result<(), Type> {
        let scope = self.scopes.last_mut().expect("at least one scope");
        use std::collections::hash_map::Entry;
        match scope.entry(id) {
            Entry::Occupied(o) => Err(o.get().clone()),
            Entry::Vacant(v) => {
                v.insert(ty);
                Ok(())
            }
        }
    }

    pub fn lookup(&self, id: Id) -> Option<&Type> {
        self.scopes.iter().rev().find_map(|s| s.get(&id))
    }
}
