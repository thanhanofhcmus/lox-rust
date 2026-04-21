use std::collections::HashMap;

use crate::{
    ast::IdentifierNode,
    id::Id,
    types::{SymbolId, Type, TypeId, TypeInterner},
};

/// Keep in sync with `src/interpret/prelude.rs::create()`. Registered as
/// `Type::Any` — we don't model their signatures yet.
const BUILTIN_NAMES: &[&str] = &[
    "print",
    "assert",
    "from_json",
    "to_json",
    "array_length",
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
    scopes: Vec<HashMap<Id, TypeId>>,
    type_interner: TypeInterner,
}

fn get_builtin_fn_type(name: &str) -> Type {
    // TODO: Generate type parameters for arrays and maps function
    match name {
        "print" => Type::Function {
            params: vec![],
            variadic: Some(TypeId::STR),
            return_: TypeId::STR,
        },

        "assert" => Type::Function {
            params: vec![TypeId::BOOL, TypeId::STR],
            variadic: None,
            return_: TypeId::STR,
        },

        "from_json" => Type::Function {
            params: vec![TypeId::STR],
            variadic: None,
            return_: TypeId::STR,
        },
        "to_json" => Type::Function {
            params: vec![TypeId::STR],
            variadic: Some(TypeId::BOOL),
            return_: TypeId::STR,
        },

        "array_length" => Type::Function {
            params: vec![TypeId::ANY],
            variadic: None,
            return_: TypeId::NUMBER,
        },
        "array_push" => Type::Function {
            params: vec![TypeId::ANY],
            variadic: Some(TypeId::ANY),
            return_: TypeId::UNIT,
        },
        "array_pop" => Type::Function {
            params: vec![TypeId::ANY],
            variadic: None,
            // Nill  or T
            return_: TypeId::ANY,
        },
        "array_insert" => Type::Function {
            params: vec![TypeId::ANY],
            variadic: Some(TypeId::ANY),
            return_: TypeId::UNIT,
        },

        "map_length" => Type::Function {
            params: vec![TypeId::ANY],
            variadic: None,
            return_: TypeId::NUMBER,
        },
        "map_keys" => Type::Function {
            params: vec![TypeId::ANY],
            variadic: None,
            // array of strings
            return_: TypeId::ANY,
        },
        "map_values" => Type::Function {
            params: vec![TypeId::ANY],
            variadic: None,
            // array of value type
            return_: TypeId::ANY,
        },
        "map_insert" => Type::Function {
            params: vec![TypeId::ANY, TypeId::ANY, TypeId::ANY],
            variadic: None,
            return_: TypeId::NIL,
        },
        "map_remove" => Type::Function {
            params: vec![TypeId::ANY, TypeId::ANY],
            variadic: None,
            return_: TypeId::ANY,
        },

        "_dbg_print" | "_dbg_state" | "_dbg_gc_mark" | "_dbg_gc_sweep" | "_dbg_gc_mark_sweep"
        | "_dbg_heap_stats" => Type::Function {
            params: vec![],
            variadic: None,
            return_: TypeId::UNIT,
        },

        _ => Type::Any,
    }
}

impl Environment {
    pub fn new() -> Self {
        let mut builtins = HashMap::new();
        let mut type_interner = TypeInterner::new();

        for &name in BUILTIN_NAMES {
            let type_ = get_builtin_fn_type(name);
            let type_id = type_interner.intern_type(&type_);
            builtins.insert(Id::new(name), type_id);
        }

        // Scope 0 holds builtins; scope 1 is the user-global scope. Keeping them
        // separate lets user code shadow builtins without a re-declaration error.
        Self {
            scopes: vec![builtins, HashMap::new()],
            type_interner,
        }
    }

    pub fn get_type_interner(&self) -> &TypeInterner {
        &self.type_interner
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
    pub fn declare_id(&mut self, id: Id, type_id: TypeId) -> Result<(), TypeId> {
        let scope = self.scopes.last_mut().expect("at least one scope");
        use std::collections::hash_map::Entry;
        match scope.entry(id) {
            Entry::Occupied(o) => Err(*o.get()),
            Entry::Vacant(v) => {
                v.insert(type_id);
                Ok(())
            }
        }
    }

    pub fn lookup_id(&self, id: Id) -> Option<TypeId> {
        self.scopes.iter().rev().find_map(|s| s.get(&id)).copied()
    }

    pub fn declare_type(&mut self, type_: &Type) -> TypeId {
        self.type_interner.intern_type(type_)
    }

    pub fn lookup_type(&self, type_id: TypeId) -> Option<&Type> {
        self.type_interner.get_type(type_id)
    }

    pub fn declare_type_symbol(&mut self, node: IdentifierNode, input: &str) -> SymbolId {
        self.type_interner
            .insert_symbol(node.get_id(), node.create_name(input))
    }

    pub fn associate_id_with_type(&mut self, id: Id, type_id: TypeId) {
        self.type_interner
            .associate_symbol_with_type(SymbolId::from(id), type_id);
    }

    pub fn lookup_type_id_from_id(&self, id: Id) -> Option<TypeId> {
        self.type_interner
            .get_type_id_by_symbol_id(SymbolId::from(id))
    }
}
