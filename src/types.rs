use std::collections::HashMap;

use crate::id::Id;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(Id);

impl From<Id> for SymbolId {
    fn from(value: Id) -> Self {
        Self(value)
    }
}

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
    pub symbol_id: SymbolId,
    pub type_: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructType {
    pub symbol_id: SymbolId,
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
    symbol_to_name: HashMap<SymbolId, String>,
    symbol_to_id: HashMap<SymbolId, TypeId>,
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
            symbol_to_name: HashMap::new(),
            symbol_to_id: HashMap::new(),
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

    pub fn insert_symbol(&mut self, id: Id, name: String) -> SymbolId {
        let id = SymbolId::from(id);
        self.symbol_to_name.insert(id, name);
        id
    }

    pub fn get_symbol(&self, id: SymbolId) -> Option<&str> {
        self.symbol_to_name.get(&id).map(|v| v.as_str())
    }

    pub fn associate_symbol_with_type(&mut self, symbol_id: SymbolId, type_id: TypeId) {
        _ = self.symbol_to_id.insert(symbol_id, type_id);
    }

    pub fn get_type_id_by_symbol_id(&self, symbol_id: SymbolId) -> Option<TypeId> {
        self.symbol_to_id.get(&symbol_id).copied()
    }

    pub fn generate_readable_name(&self, id: TypeId) -> String {
        match self.get_type(id) {
            Some(Type::Any) => "any".into(),
            Some(Type::Bool) => "bool".into(),
            Some(Type::Number) => "number".into(),
            Some(Type::Str) => "string".into(),
            Some(Type::Unit) => "unit".into(),
            Some(Type::Nil) => "nil".into(),
            Some(Type::Array { elem }) => format!("[{}]", self.generate_readable_name(*elem)),
            Some(Type::Map { key, value }) => {
                format!(
                    "%{{{}, {}}}",
                    self.generate_readable_name(*key),
                    self.generate_readable_name(*value)
                )
            }
            Some(Type::Struct(StructType {
                symbol_id: id,
                fields,
            })) => {
                let name = self.get_symbol(*id).unwrap_or("#Unknown");
                let fields: Vec<String> = fields
                    .iter()
                    .map(|field| {
                        format!(
                            "{}: {}",
                            self.get_symbol(field.symbol_id).unwrap_or("#Unknown"),
                            self.generate_readable_name(field.type_)
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
                    .map(|&i| self.generate_readable_name(i))
                    .collect();
                if let Some(variadic_type) = variadic {
                    p.push(format!(
                        "{} ...",
                        self.generate_readable_name(*variadic_type)
                    ))
                }
                format!(
                    "fn({}) -> {}",
                    p.join(", "),
                    self.generate_readable_name(*return_)
                )
            }
            None => format!("Unknown(#{})", id.0),
        }
    }
}
