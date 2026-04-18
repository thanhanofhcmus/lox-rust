use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

impl TypeId {
    pub const ANY: Self = Self(0);
    pub const BOOL: Self = Self(1);
    pub const NUMBER: Self = Self(2);
    pub const STR: Self = Self(3);
    pub const UNIT: Self = Self(4);
    pub const NIL: Self = Self(5);

    pub const LAST_RESVERED_COUNTER: usize = 10;
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
        // Key is always string
        value: TypeId,
    },

    Function {
        params: Vec<TypeId>,
        return_: TypeId,
    },
}

#[derive(Debug, Clone)]
pub struct TypeInterner {
    type_to_id: HashMap<Type, TypeId>,
    id_to_type: HashMap<TypeId, Type>,
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
        ]);
        let id_to_type = HashMap::from([
            (TypeId::ANY, Type::Any),
            (TypeId::BOOL, Type::Bool),
            (TypeId::NUMBER, Type::Number),
            (TypeId::STR, Type::Str),
            (TypeId::UNIT, Type::Unit),
            (TypeId::NIL, Type::Nil),
        ]);

        Self {
            type_to_id,
            id_to_type,
            next_id: TypeId::LAST_RESVERED_COUNTER + 1,
        }
    }

    pub fn intern(&mut self, type_: &Type) -> TypeId {
        if let Some(type_id) = self.type_to_id.get(type_) {
            return *type_id;
        }
        let type_id = TypeId(self.next_id);
        self.next_id += 1;
        self.type_to_id.insert(type_.clone(), type_id);
        self.id_to_type.insert(type_id, type_.clone());
        type_id
    }

    #[allow(unused)]
    pub fn get(&self, type_id: TypeId) -> Option<&Type> {
        self.id_to_type.get(&type_id)
    }
}
