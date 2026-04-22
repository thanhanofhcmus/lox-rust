use std::collections::HashMap;

use crate::{id::Id, identifier_registry::IdentifierRegistry};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

impl TypeId {
    // Using the top 4 bits for the category (allows 16 categories)
    const CATEGORY_SHIFT: usize = (std::mem::size_of::<usize>() * 8) - 4;
    const ID_MASK: usize = !(0xF << Self::CATEGORY_SHIFT);

    const CATEGORY_SCALAR: usize = 0 << Self::CATEGORY_SHIFT;
    const CATEGORY_ARRAY: usize = 1 << Self::CATEGORY_SHIFT;
    const CATEGORY_MAP: usize = 2 << Self::CATEGORY_SHIFT;
    const CATEGORY_FUNCTION: usize = 3 << Self::CATEGORY_SHIFT;
    const CATEGORY_STRUCT: usize = 4 << Self::CATEGORY_SHIFT;

    pub const ANY: Self = Self(Self::CATEGORY_SCALAR | 1);
    pub const BOOL: Self = Self(Self::CATEGORY_SCALAR | 2);
    pub const NUMBER: Self = Self(Self::CATEGORY_SCALAR | 3);
    pub const STR: Self = Self(Self::CATEGORY_SCALAR | 4);
    pub const UNIT: Self = Self(Self::CATEGORY_SCALAR | 5);
    pub const NIL: Self = Self(Self::CATEGORY_SCALAR | 6);

    pub const ARRAY_ANY: Self = Self(Self::CATEGORY_ARRAY | 1);
    pub const ARRAY_UNTYPED: Self = Self(Self::CATEGORY_ARRAY | 2);

    pub const MAP_UNTYPED: Self = Self(Self::CATEGORY_MAP | 1);
    pub const MAP_ANY_ANY: Self = Self(Self::CATEGORY_MAP | 2);

    pub const FUNCTION_ANY: Self = Self(Self::CATEGORY_FUNCTION | 1);

    const NEXT_IDS: [usize; 5] = [
        7, // Scalar
        3, // Array
        3, // Map
        2, // Function
        1, // Struct
    ];

    pub fn is_array(&self) -> bool {
        (self.0 & !Self::ID_MASK) == Self::CATEGORY_ARRAY
    }

    pub fn is_map(&self) -> bool {
        (self.0 & !Self::ID_MASK) == Self::CATEGORY_MAP
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub id: Id,
    pub type_: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructType {
    pub id: Id,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Any, // gradual typing, top type
    Bool,
    Number,
    Str,
    Unit, // the `()` value — bare `return`, statement-only blocks, void-ish calls
    Nil,

    Array {
        elem: TypeId,
    },

    Map {
        key: TypeId,
        value: TypeId,
    },

    Struct(StructType),

    // TODO: support generic types
    Function {
        params: Vec<TypeId>,
        /// If the function is variadic, this is the element type shared by
        /// every variadic argument (plain `...` of values beyond `params`).
        variadic: Option<TypeId>,
        return_: TypeId,
    },
}

impl Type {
    pub const ARRAY_ANY: Self = Self::Array { elem: TypeId::ANY };

    pub const MAP_ANY_ANY: Self = Self::Map {
        key: TypeId::ANY,
        value: TypeId::ANY,
    };

    pub const FUNCTION_ANY: Self = Self::Function {
        params: vec![],
        variadic: Some(TypeId::ANY),
        return_: TypeId::ANY,
    };
}

#[derive(Debug, Clone)]
pub struct TypeInterner {
    type_to_id: HashMap<Type, TypeId>,
    id_to_type: HashMap<TypeId, Type>,
    /// Store the name of the structs and their assoicated fields, maybe method later
    id_to_type_id: HashMap<Id, TypeId>,
    counters: [usize; 5],
}

impl TypeInterner {
    pub fn new() -> Self {
        // The TypeId::*_UNTYPED variants are deliberately left out.
        // They do not have a type representation and cannot be interned as an existing type.

        let id_to_type = HashMap::from([
            (TypeId::ANY, Type::Any),
            (TypeId::BOOL, Type::Bool),
            (TypeId::NUMBER, Type::Number),
            (TypeId::STR, Type::Str),
            (TypeId::UNIT, Type::Unit),
            (TypeId::NIL, Type::Nil),
            (TypeId::ARRAY_ANY, Type::ARRAY_ANY),
            (TypeId::MAP_ANY_ANY, Type::MAP_ANY_ANY),
            (TypeId::FUNCTION_ANY, Type::FUNCTION_ANY),
        ]);

        let mut type_to_id = HashMap::new();
        for (type_id, type_) in &id_to_type {
            type_to_id.insert(type_.clone(), *type_id);
        }

        Self {
            type_to_id,
            id_to_type,
            id_to_type_id: HashMap::new(),
            counters: TypeId::NEXT_IDS,
        }
    }

    pub fn intern_type(&mut self, type_: &Type) -> TypeId {
        if let Some(type_id) = self.type_to_id.get(type_) {
            return *type_id;
        }

        let (index, category) = match type_ {
            Type::Array { .. } => (1, TypeId::CATEGORY_ARRAY),
            Type::Map { .. } => (2, TypeId::CATEGORY_MAP),
            Type::Function { .. } => (3, TypeId::CATEGORY_FUNCTION),
            Type::Struct { .. } => (4, TypeId::CATEGORY_STRUCT),
            _ => (0, TypeId::CATEGORY_SCALAR),
        };

        let id_payload = self.counters[index];
        self.counters[index] += 1;

        let type_id = TypeId(category | (id_payload & TypeId::ID_MASK));

        self.type_to_id.insert(type_.clone(), type_id);
        self.id_to_type.insert(type_id, type_.clone());

        type_id
    }

    pub fn get_type(&self, type_id: TypeId) -> Option<&Type> {
        self.id_to_type.get(&type_id)
    }

    pub fn associate_id_with_type(&mut self, id: Id, type_id: TypeId) {
        _ = self.id_to_type_id.insert(id, type_id);
    }

    pub fn get_type_id_by_id(&self, id: Id) -> Option<TypeId> {
        self.id_to_type_id.get(&id).copied()
    }

    pub fn generate_readable_name(&self, sb: &IdentifierRegistry, id: TypeId) -> String {
        match self.get_type(id) {
            Some(Type::Any) => "any".into(),
            Some(Type::Bool) => "bool".into(),
            Some(Type::Number) => "number".into(),
            Some(Type::Str) => "string".into(),
            Some(Type::Unit) => "unit".into(),
            Some(Type::Nil) => "nil".into(),
            Some(Type::Array { elem }) => format!("[{}]", self.generate_readable_name(sb, *elem)),
            Some(Type::Map { key, value }) => {
                format!(
                    "%{{{} => {}}}",
                    self.generate_readable_name(sb, *key),
                    self.generate_readable_name(sb, *value)
                )
            }
            Some(Type::Struct(StructType { id, fields })) => {
                let name = sb.get_or_unknown(*id);
                let fields: Vec<String> = fields
                    .iter()
                    .map(|field| {
                        format!(
                            "{}: {}",
                            sb.get_or_unknown(field.id),
                            self.generate_readable_name(sb, field.type_)
                        )
                    })
                    .collect();
                format!("struct {} {{ {} }}", name, fields.join(", "))
            }
            Some(Type::Function {
                params,
                variadic,
                return_,
            }) => {
                let mut p: Vec<String> = params
                    .iter()
                    .map(|&i| self.generate_readable_name(sb, i))
                    .collect();
                if let Some(variadic_type) = variadic {
                    p.push(format!(
                        "{} ...",
                        self.generate_readable_name(sb, *variadic_type)
                    ))
                }
                format!(
                    "fn({}) -> {}",
                    p.join(", "),
                    self.generate_readable_name(sb, *return_)
                )
            }
            None => format!("Unknown(#{})", id.0),
        }
    }
}
