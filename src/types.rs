use std::collections::HashMap;

use crate::{id::Id, symbol_names::SymbolNames};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

impl TypeId {
    pub const ANY: Self = Self(0);
    pub const BOOL: Self = Self(1);
    pub const NUMBER: Self = Self(2);
    pub const STR: Self = Self(3);
    pub const UNIT: Self = Self(4);
    pub const NIL: Self = Self(5);
    pub const ANY_FUNCTION: Self = Self(6);

    pub const LAST_RESERVED_COUNTER: usize = 10;
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
    pub const ANY_FUNCTION: Self = Self::Function {
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
    next_id: usize,
}

impl TypeInterner {
    pub fn new() -> Self {
        let type_to_id = HashMap::from([
            (Type::Any, TypeId::ANY),
            (Type::Bool, TypeId::BOOL),
            (Type::Number, TypeId::NUMBER),
            (Type::Str, TypeId::STR),
            (Type::Unit, TypeId::UNIT),
            (Type::Nil, TypeId::NIL),
            (Type::ANY_FUNCTION, TypeId::ANY_FUNCTION),
        ]);
        let id_to_type = HashMap::from([
            (TypeId::ANY, Type::Any),
            (TypeId::BOOL, Type::Bool),
            (TypeId::NUMBER, Type::Number),
            (TypeId::STR, Type::Str),
            (TypeId::UNIT, Type::Unit),
            (TypeId::NIL, Type::Nil),
            (TypeId::ANY_FUNCTION, Type::ANY_FUNCTION),
        ]);

        Self {
            type_to_id,
            id_to_type,
            id_to_type_id: HashMap::new(),
            next_id: TypeId::LAST_RESERVED_COUNTER + 1,
        }
    }

    pub fn intern_type(&mut self, type_: &Type) -> TypeId {
        if let Some(type_id) = self.type_to_id.get(type_) {
            return *type_id;
        }
        let type_id = TypeId(self.next_id);
        self.next_id += 1;
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

    pub fn generate_readable_name(&self, sb: &SymbolNames, id: TypeId) -> String {
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
                    "%{{{}, {}}}",
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
