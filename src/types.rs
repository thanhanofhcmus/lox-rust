use std::{collections::HashMap, hash::Hash};

use crate::{id::Id, identifier_registry::IdentifierRegistry};

// ---------------------------------------------------------------------------
// SliceId — handle to a &[TypeId] or &[StructField] stored in TypeInterner
// ---------------------------------------------------------------------------

/// Handle to a variable-length slice stored in [`TypeInterner`].
///
/// The top 2 bits encode the category; the lower bits are the index.
/// Static slices use indices into a fixed-size `&'static` table,
/// enabling `const`-constructible `Type` values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SliceId(usize);

impl SliceId {
    const CATEGORY_SHIFT: usize = (std::mem::size_of::<usize>() * 8) - 2;
    const ID_MASK: usize = !(0b11 << Self::CATEGORY_SHIFT);

    const CATEGORY_STATIC_TYPE_ID: usize = 0 << Self::CATEGORY_SHIFT;
    const CATEGORY_DYNAMIC_TYPE_ID: usize = 1 << Self::CATEGORY_SHIFT;
    const CATEGORY_STATIC_STRUCT_FIELD: usize = 2 << Self::CATEGORY_SHIFT;
    const CATEGORY_DYNAMIC_STRUCT_FIELD: usize = 3 << Self::CATEGORY_SHIFT;

    /// Number of static type-id slices we reserve.
    const STATIC_TYPE_ID_COUNT: usize = 64;

    /// Number of static struct-field slices we reserve.
    const STATIC_STRUCT_FIELD_COUNT: usize = 16;

    pub const fn new_static_type_id(index: usize) -> Self {
        Self(Self::CATEGORY_STATIC_TYPE_ID | index)
    }

    pub(crate) fn new_dynamic_type_id(index: usize) -> Self {
        Self(Self::CATEGORY_DYNAMIC_TYPE_ID | index)
    }

    pub const fn new_static_struct_field(index: usize) -> Self {
        Self(Self::CATEGORY_STATIC_STRUCT_FIELD | index)
    }

    pub(crate) fn new_dynamic_struct_field(index: usize) -> Self {
        Self(Self::CATEGORY_DYNAMIC_STRUCT_FIELD | index)
    }

    pub(crate) fn is_static_type_id(self) -> bool {
        (self.0 & !Self::ID_MASK) == Self::CATEGORY_STATIC_TYPE_ID
    }

    pub(crate) fn is_static_struct_field(self) -> bool {
        (self.0 & !Self::ID_MASK) == Self::CATEGORY_STATIC_STRUCT_FIELD
    }

    pub(crate) fn index(self) -> usize {
        self.0 & Self::ID_MASK
    }

    /// Empty param list.
    pub const EMPTY: Self = Self::new_static_type_id(0);
    /// `[STR]`
    pub const STR: Self = Self::new_static_type_id(1);
    /// `[NUMBER]`
    pub const NUMBER: Self = Self::new_static_type_id(2);
    /// `[ANY]`
    pub const ANY: Self = Self::new_static_type_id(3);
    /// `[BOOL, STR]`
    pub const BOOL_STR: Self = Self::new_static_type_id(4);
    /// `[STR, STR]`
    pub const STR_STR: Self = Self::new_static_type_id(5);
    /// `[NUMBER, NUMBER]`
    pub const NUMBER_NUMBER: Self = Self::new_static_type_id(6);
    /// `[ANY, STR]`
    pub const ANY_STR: Self = Self::new_static_type_id(7);
    /// `[STR, NUMBER]`
    pub const STR_NUMBER: Self = Self::new_static_type_id(8);
    /// `[ANY, NUMBER]`
    pub const ANY_NUMBER: Self = Self::new_static_type_id(9);
    /// `[ANY, ANY]`
    pub const ANY_ANY: Self = Self::new_static_type_id(10);
    /// `[ANY, ANY, ANY]`
    pub const ANY_ANY_ANY: Self = Self::new_static_type_id(11);
    /// `[STR, STR, STR]`
    pub const STR_STR_STR: Self = Self::new_static_type_id(12);

    /// Empty struct field list.
    pub const STRUCT_EMPTY: Self = Self::new_static_struct_field(0);
}

// `TypeId` is intentionally NOT defined via `define_type_index!` to let us control the underlying type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

impl TypeId {
    // Using the top 4 bits for the category (allows 16 categories)
    const CATEGORY_SHIFT: usize = (std::mem::size_of::<usize>() * 8) - 4;
    const ID_MASK: usize = !(0b1111 << Self::CATEGORY_SHIFT);

    const CATEGORY_SCALAR: usize = 0 << Self::CATEGORY_SHIFT;
    const CATEGORY_ARRAY: usize = 1 << Self::CATEGORY_SHIFT;
    const CATEGORY_MAP: usize = 2 << Self::CATEGORY_SHIFT;
    const CATEGORY_TUPLE: usize = 3 << Self::CATEGORY_SHIFT;
    const CATEGORY_FUNCTION: usize = 4 << Self::CATEGORY_SHIFT;
    const CATEGORY_STRUCT: usize = 5 << Self::CATEGORY_SHIFT;

    pub const ANY: Self = Self(Self::CATEGORY_SCALAR | 1);
    pub const BOOL: Self = Self(Self::CATEGORY_SCALAR | 2);
    pub const NUMBER: Self = Self(Self::CATEGORY_SCALAR | 3);
    pub const STR: Self = Self(Self::CATEGORY_SCALAR | 4);
    pub const UNIT: Self = Self(Self::CATEGORY_SCALAR | 5);
    pub const NIL: Self = Self(Self::CATEGORY_SCALAR | 6);

    pub const ARRAY_UNTYPED: Self = Self(Self::CATEGORY_ARRAY | 1);
    pub const ARRAY_ANY: Self = Self(Self::CATEGORY_ARRAY | 2);

    pub const MAP_ANY_ANY: Self = Self(Self::CATEGORY_MAP | 1);
    pub const MAP_UNTYPED: Self = Self(Self::CATEGORY_MAP | 2);

    pub const FUNCTION_ANY: Self = Self(Self::CATEGORY_FUNCTION | 1);

    const NEXT_IDS: [usize; 6] = [
        7, // Scalar
        3, // Array
        3, // Map
        1, // Tuple
        2, // Function
        1, // Struct
    ];

    pub fn is_array(self) -> bool {
        (self.0 & !Self::ID_MASK) == Self::CATEGORY_ARRAY
    }

    pub fn is_map(self) -> bool {
        (self.0 & !Self::ID_MASK) == Self::CATEGORY_MAP
    }

    // debug functions
    pub fn get_parts(self) -> (usize, usize) {
        (
            (self.0 & !Self::ID_MASK) >> Self::CATEGORY_SHIFT,
            (self.0 & Self::ID_MASK),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub id: Id,
    pub type_: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructType {
    pub id: Id,
    pub fields: SliceId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

    Tuple {
        members: SliceId,
    },

    Struct(StructType),

    // TODO: support generic types
    Function {
        params: SliceId,
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
        params: SliceId::EMPTY,
        variadic: Some(TypeId::ANY),
        return_: TypeId::ANY,
    };

    pub const FUNCTION_EMPTY_TO_NIL: Self = Self::Function {
        params: SliceId::EMPTY,
        variadic: None,
        return_: TypeId::NIL,
    };

    pub const FUNCTION_EMPTY_VARIADIC_STR_TO_NIL: Self = Self::Function {
        params: SliceId::EMPTY,
        variadic: Some(TypeId::STR),
        return_: TypeId::NIL,
    };

    pub const FUNCTION_EMPTY_VARIADIC_ANY_TO_NIL: Self = Self::Function {
        params: SliceId::EMPTY,
        variadic: Some(TypeId::ANY),
        return_: TypeId::NIL,
    };

    pub const FUNCTION_BOOL_STR_TO_NIL: Self = Self::Function {
        params: SliceId::BOOL_STR,
        variadic: None,
        return_: TypeId::NIL,
    };

    pub const FUNCTION_STR_TO_NUMBER: Self = Self::Function {
        params: SliceId::STR,
        variadic: None,
        return_: TypeId::NUMBER,
    };

    pub const FUNCTION_STR_TO_STR: Self = Self::Function {
        params: SliceId::STR,
        variadic: None,
        return_: TypeId::STR,
    };

    pub const FUNCTION_STR_TO_BOOL: Self = Self::Function {
        params: SliceId::STR,
        variadic: None,
        return_: TypeId::BOOL,
    };

    pub const FUNCTION_STR_TO_ANY: Self = Self::Function {
        params: SliceId::STR,
        variadic: None,
        return_: TypeId::ANY,
    };

    pub const FUNCTION_STR_STR_TO_BOOL: Self = Self::Function {
        params: SliceId::STR_STR,
        variadic: None,
        return_: TypeId::BOOL,
    };

    pub const FUNCTION_STR_STR_TO_NUMBER: Self = Self::Function {
        params: SliceId::STR_STR,
        variadic: None,
        return_: TypeId::NUMBER,
    };

    pub const FUNCTION_STR_STR_TO_ANY: Self = Self::Function {
        params: SliceId::STR_STR,
        variadic: None,
        return_: TypeId::ANY,
    };

    pub const FUNCTION_ANY_STR_TO_STR: Self = Self::Function {
        params: SliceId::ANY_STR,
        variadic: None,
        return_: TypeId::STR,
    };

    pub const FUNCTION_STR_NUMBER_TO_STR: Self = Self::Function {
        params: SliceId::STR_NUMBER,
        variadic: None,
        return_: TypeId::STR,
    };

    pub const FUNCTION_NUMBER_TO_NUMBER: Self = Self::Function {
        params: SliceId::NUMBER,
        variadic: None,
        return_: TypeId::NUMBER,
    };

    pub const FUNCTION_NUMBER_NUMBER_TO_NUMBER: Self = Self::Function {
        params: SliceId::NUMBER_NUMBER,
        variadic: None,
        return_: TypeId::NUMBER,
    };

    pub const FUNCTION_ANY_TO_NUMBER: Self = Self::Function {
        params: SliceId::ANY,
        variadic: None,
        return_: TypeId::NUMBER,
    };

    pub const FUNCTION_ANY_TO_ANY: Self = Self::Function {
        params: SliceId::ANY,
        variadic: None,
        return_: TypeId::ANY,
    };

    pub const FUNCTION_ANY_NUMBER_TO_UNIT: Self = Self::Function {
        params: SliceId::ANY_NUMBER,
        variadic: None,
        return_: TypeId::UNIT,
    };

    pub const FUNCTION_ANY_ANY_TO_ANY: Self = Self::Function {
        params: SliceId::ANY_ANY,
        variadic: None,
        return_: TypeId::ANY,
    };

    pub const FUNCTION_ANY_ANY_ANY_TO_NIL: Self = Self::Function {
        params: SliceId::ANY_ANY_ANY,
        variadic: None,
        return_: TypeId::NIL,
    };

    pub const FUNCTION_STR_STR_STR_TO_STR: Self = Self::Function {
        params: SliceId::STR_STR_STR,
        variadic: None,
        return_: TypeId::STR,
    };

    pub const FUNCTION_ANY_VARIADIC_ANY_TO_UNIT: Self = Self::Function {
        params: SliceId::ANY,
        variadic: Some(TypeId::ANY),
        return_: TypeId::UNIT,
    };

    pub const FUNCTION_ANY_NUMBER_VARIADIC_ANY_TO_UNIT: Self = Self::Function {
        params: SliceId::ANY_NUMBER,
        variadic: Some(TypeId::ANY),
        return_: TypeId::UNIT,
    };

    pub const FUNCTION_STR_NUMBER_VARIADIC_NUMBER_TO_STR: Self = Self::Function {
        params: SliceId::STR_NUMBER,
        variadic: Some(TypeId::NUMBER),
        return_: TypeId::STR,
    };

    pub const FUNCTION_ANY_VARIADIC_BOOL_TO_STR: Self = Self::Function {
        params: SliceId::ANY,
        variadic: Some(TypeId::BOOL),
        return_: TypeId::STR,
    };
}

#[derive(Debug, Clone, Default)]
pub struct TypeScope {
    symbols: HashMap<Id, TypeId>,
}

impl TypeScope {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    pub fn get_type_id(&self, id: Id) -> Option<TypeId> {
        self.symbols.get(&id).copied()
    }

    pub fn associate(&mut self, id: Id, type_id: TypeId) {
        _ = self.symbols.insert(id, type_id)
    }
}

#[derive(Debug, Clone)]
pub struct TypeInterner {
    type_to_id: HashMap<Type, TypeId>,
    id_to_type: HashMap<TypeId, Type>,
    counters: [usize; 6],

    // Slice storage
    static_type_id_slices: [&'static [TypeId]; SliceId::STATIC_TYPE_ID_COUNT],
    dynamic_type_id_slices: Vec<Vec<TypeId>>,
    static_struct_field_slices: [&'static [StructField]; SliceId::STATIC_STRUCT_FIELD_COUNT],
    dynamic_struct_field_slices: Vec<Vec<StructField>>,
}

impl TypeInterner {
    pub fn new() -> Self {
        // The TypeId::*_UNTYPED variants are deliberately left out.
        // They do not have a type representation and cannot be interned from an existing type.

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
            type_to_id.insert(*type_, *type_id);
        }

        const EMPTY_TYPE_ID_SLICE: &[TypeId] = &[];
        const EMPTY_STRUCT_FIELD_SLICE: &[StructField] = &[];

        Self {
            type_to_id,
            id_to_type,
            counters: TypeId::NEXT_IDS,
            static_type_id_slices: [EMPTY_TYPE_ID_SLICE; SliceId::STATIC_TYPE_ID_COUNT],
            dynamic_type_id_slices: Vec::new(),
            static_struct_field_slices: [EMPTY_STRUCT_FIELD_SLICE; SliceId::STATIC_STRUCT_FIELD_COUNT],
            dynamic_struct_field_slices: Vec::new(),
        }
    }

    /// Register all static slices referenced by the `SliceId::*` constants.
    /// Must be called after construction and before any interning of types
    /// that reference those constants.
    pub fn init_static_slices(&mut self) {
        self.register_static_type_id_slice(&[], SliceId::EMPTY.index());
        self.register_static_type_id_slice(&[TypeId::STR], SliceId::STR.index());
        self.register_static_type_id_slice(&[TypeId::NUMBER], SliceId::NUMBER.index());
        self.register_static_type_id_slice(&[TypeId::ANY], SliceId::ANY.index());
        self.register_static_type_id_slice(&[TypeId::BOOL, TypeId::STR], SliceId::BOOL_STR.index());
        self.register_static_type_id_slice(&[TypeId::STR, TypeId::STR], SliceId::STR_STR.index());
        self.register_static_type_id_slice(&[TypeId::NUMBER, TypeId::NUMBER], SliceId::NUMBER_NUMBER.index());
        self.register_static_type_id_slice(&[TypeId::ANY, TypeId::STR], SliceId::ANY_STR.index());
        self.register_static_type_id_slice(&[TypeId::STR, TypeId::NUMBER], SliceId::STR_NUMBER.index());
        self.register_static_type_id_slice(&[TypeId::ANY, TypeId::NUMBER], SliceId::ANY_NUMBER.index());
        self.register_static_type_id_slice(&[TypeId::ANY, TypeId::ANY], SliceId::ANY_ANY.index());
        self.register_static_type_id_slice(&[TypeId::ANY, TypeId::ANY, TypeId::ANY], SliceId::ANY_ANY_ANY.index());
        self.register_static_type_id_slice(&[TypeId::STR, TypeId::STR, TypeId::STR], SliceId::STR_STR_STR.index());
    }

    // -----------------------------------------------------------------------
    // Slice management
    // -----------------------------------------------------------------------

    /// Register a static `&[TypeId]` slice and return its [`SliceId`].
    pub fn register_static_type_id_slice(&mut self, slice: &'static [TypeId], index: usize) -> SliceId {
        assert!(index < SliceId::STATIC_TYPE_ID_COUNT);
        self.static_type_id_slices[index] = slice;
        SliceId::new_static_type_id(index)
    }

    /// Intern a dynamically-created `Vec<TypeId>` and return its [`SliceId`].
    /// Deduplicates: if an identical slice already exists, returns the existing handle.
    pub fn intern_type_id_slice(&mut self, v: Vec<TypeId>) -> SliceId {
        // Check static slices first
        for (i, slice) in self.static_type_id_slices.iter().enumerate() {
            if *slice == v.as_slice() {
                return SliceId::new_static_type_id(i);
            }
        }
        // Check dynamic slices
        for (i, slice) in self.dynamic_type_id_slices.iter().enumerate() {
            if slice.as_slice() == v.as_slice() {
                return SliceId::new_dynamic_type_id(i);
            }
        }
        let index = self.dynamic_type_id_slices.len();
        self.dynamic_type_id_slices.push(v);
        SliceId::new_dynamic_type_id(index)
    }

    /// Register a static `&[StructField]` slice and return its [`SliceId`].
    pub fn register_static_struct_field_slice(&mut self, slice: &'static [StructField], index: usize) -> SliceId {
        assert!(index < SliceId::STATIC_STRUCT_FIELD_COUNT);
        self.static_struct_field_slices[index] = slice;
        SliceId::new_static_struct_field(index)
    }

    /// Intern a dynamically-created `Vec<StructField>` and return its [`SliceId`].
    /// Deduplicates: if an identical slice already exists, returns the existing handle.
    pub fn intern_struct_field_slice(&mut self, v: Vec<StructField>) -> SliceId {
        // Check static slices first
        for (i, slice) in self.static_struct_field_slices.iter().enumerate() {
            if *slice == v.as_slice() {
                return SliceId::new_static_struct_field(i);
            }
        }
        // Check dynamic slices
        for (i, slice) in self.dynamic_struct_field_slices.iter().enumerate() {
            if slice.as_slice() == v.as_slice() {
                return SliceId::new_dynamic_struct_field(i);
            }
        }
        let index = self.dynamic_struct_field_slices.len();
        self.dynamic_struct_field_slices.push(v);
        SliceId::new_dynamic_struct_field(index)
    }

    /// Look up a type-id slice by its handle.
    pub fn lookup_type_id_slice(&self, id: SliceId) -> &[TypeId] {
        if id.is_static_type_id() {
            self.static_type_id_slices[id.index()]
        } else {
            &self.dynamic_type_id_slices[id.index()]
        }
    }

    /// Look up a struct-field slice by its handle.
    pub fn lookup_struct_field_slice(&self, id: SliceId) -> &[StructField] {
        if id.is_static_struct_field() {
            self.static_struct_field_slices[id.index()]
        } else {
            &self.dynamic_struct_field_slices[id.index()]
        }
    }

    pub fn intern_type(&mut self, type_: &Type) -> (TypeId, bool) {
        if let Some(type_id) = self.type_to_id.get(type_) {
            return (*type_id, true);
        }

        let (index, category) = match type_ {
            Type::Unit | Type::Nil | Type::Any | Type::Bool | Type::Number | Type::Str => (0, TypeId::CATEGORY_SCALAR),
            Type::Array { .. } => (1, TypeId::CATEGORY_ARRAY),
            Type::Map { .. } => (2, TypeId::CATEGORY_MAP),
            Type::Tuple { .. } => (3, TypeId::CATEGORY_TUPLE),
            Type::Function { .. } => (4, TypeId::CATEGORY_FUNCTION),
            Type::Struct { .. } => (5, TypeId::CATEGORY_STRUCT),
        };

        let id_payload = self.counters[index];
        self.counters[index] += 1;

        let type_id = TypeId(category | (id_payload & TypeId::ID_MASK));

        self.type_to_id.insert(*type_, type_id);
        self.id_to_type.insert(type_id, *type_);

        (type_id, false)
    }

    pub fn get_type(&self, type_id: TypeId) -> Option<Type> {
        self.id_to_type.get(&type_id).copied()
    }

    pub fn generate_readable_name(&self, ir: &IdentifierRegistry, id: TypeId) -> String {
        match self.get_type(id) {
            Some(Type::Any) => "any".into(),
            Some(Type::Bool) => "bool".into(),
            Some(Type::Number) => "number".into(),
            Some(Type::Str) => "string".into(),
            Some(Type::Unit) => "unit".into(),
            Some(Type::Nil) => "nil".into(),
            Some(Type::Array { elem }) => format!("[{}]", self.generate_readable_name(ir, elem)),
            Some(Type::Map { key, value }) => {
                format!(
                    "%{{{} => {}}}",
                    self.generate_readable_name(ir, key),
                    self.generate_readable_name(ir, value)
                )
            }
            Some(Type::Tuple { members }) => {
                let slice = self.lookup_type_id_slice(members);
                let names: Vec<String> = slice.iter().map(|m| self.generate_readable_name(ir, *m)).collect();
                format!("%({})", names.join(", "))
            }
            Some(Type::Struct(StructType { id, fields })) => {
                let name = ir.get_or_unknown(id);
                let slice = self.lookup_struct_field_slice(fields);
                let field_strs: Vec<String> = slice
                    .iter()
                    .map(|field| {
                        format!(
                            "{}: {}",
                            ir.get_or_unknown(field.id),
                            self.generate_readable_name(ir, field.type_)
                        )
                    })
                    .collect();
                format!("struct {} {{ {} }}", name, field_strs.join(", "))
            }
            Some(Type::Function {
                params,
                variadic,
                return_,
            }) => {
                let slice = self.lookup_type_id_slice(params);
                let mut p: Vec<String> = slice.iter().map(|&i| self.generate_readable_name(ir, i)).collect();
                if let Some(variadic_type) = variadic {
                    p.push(format!("{} ...", self.generate_readable_name(ir, variadic_type)))
                }
                format!("fn({}) -> {}", p.join(", "), self.generate_readable_name(ir, return_))
            }
            None => format!("Unknown(#{})", id.0),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_ti() -> TypeInterner {
        let mut ti = TypeInterner::new();
        ti.init_static_slices();
        ti
    }

    // -----------------------------------------------------------------------
    // TypeId
    // -----------------------------------------------------------------------

    #[test]
    fn type_id_scalar_constants_are_distinct() {
        assert_eq!(TypeId::ANY, TypeId::ANY);
        assert_ne!(TypeId::ANY, TypeId::BOOL);
        assert_ne!(TypeId::BOOL, TypeId::NUMBER);
        assert_ne!(TypeId::NUMBER, TypeId::STR);
        assert_ne!(TypeId::STR, TypeId::UNIT);
        assert_ne!(TypeId::UNIT, TypeId::NIL);
    }

    #[test]
    fn type_id_array_any_is_array() {
        assert!(TypeId::ARRAY_ANY.is_array());
        assert!(!TypeId::ARRAY_ANY.is_map());
    }

    #[test]
    fn type_id_map_is_map() {
        assert!(TypeId::MAP_ANY_ANY.is_map());
        assert!(!TypeId::MAP_ANY_ANY.is_array());
    }

    // -----------------------------------------------------------------------
    // SliceId static constants
    // -----------------------------------------------------------------------

    #[test]
    fn slice_id_static_constants_are_distinct() {
        assert_ne!(SliceId::EMPTY, SliceId::STR);
        assert_ne!(SliceId::STR, SliceId::NUMBER);
        assert_ne!(SliceId::NUMBER, SliceId::ANY);
        assert_ne!(SliceId::ANY, SliceId::BOOL_STR);
        assert_ne!(SliceId::BOOL_STR, SliceId::STR_STR);
        assert_ne!(SliceId::STR_STR, SliceId::NUMBER_NUMBER);
        assert_ne!(SliceId::NUMBER_NUMBER, SliceId::ANY_STR);
        assert_ne!(SliceId::ANY_STR, SliceId::STR_NUMBER);
        assert_ne!(SliceId::STR_NUMBER, SliceId::ANY_NUMBER);
        assert_ne!(SliceId::ANY_NUMBER, SliceId::ANY_ANY);
        assert_ne!(SliceId::ANY_ANY, SliceId::ANY_ANY_ANY);
        assert_ne!(SliceId::ANY_ANY_ANY, SliceId::STR_STR_STR);
    }

    #[test]
    fn slice_id_static_is_recognized() {
        assert!(SliceId::EMPTY.is_static_type_id());
        assert!(!SliceId::EMPTY.is_static_struct_field());
    }

    #[test]
    fn slice_id_struct_empty_is_struct_field() {
        assert!(SliceId::STRUCT_EMPTY.is_static_struct_field());
        assert!(!SliceId::STRUCT_EMPTY.is_static_type_id());
    }

    #[test]
    fn slice_id_dynamic_type_id_is_not_static() {
        let mut ti = make_ti();
        let sid = ti.intern_type_id_slice(vec![TypeId::NUMBER, TypeId::STR]);
        assert!(!sid.is_static_type_id());
        assert!(!sid.is_static_struct_field());
    }

    #[test]
    fn slice_id_dynamic_struct_field_is_not_static() {
        let mut ti = make_ti();
        let sid = ti.intern_struct_field_slice(vec![StructField {
            id: Id::new("x"),
            type_: TypeId::NUMBER,
        }]);
        assert!(!sid.is_static_type_id());
        assert!(!sid.is_static_struct_field());
    }

    // -----------------------------------------------------------------------
    // SliceId deduplication
    // -----------------------------------------------------------------------

    #[test]
    fn slice_id_same_static_type_id_returns_same_handle() {
        // The const itself is already the only handle
        assert_eq!(SliceId::STR, SliceId::STR);
    }

    #[test]
    fn slice_id_identical_dynamic_type_id_returns_same_handle() {
        let mut ti = make_ti();
        let a = ti.intern_type_id_slice(vec![TypeId::NUMBER, TypeId::BOOL]);
        let b = ti.intern_type_id_slice(vec![TypeId::NUMBER, TypeId::BOOL]);
        assert_eq!(a, b);
    }

    #[test]
    fn slice_id_different_dynamic_type_id_returns_different_handles() {
        let mut ti = make_ti();
        let a = ti.intern_type_id_slice(vec![TypeId::NUMBER]);
        let b = ti.intern_type_id_slice(vec![TypeId::BOOL]);
        assert_ne!(a, b);
    }

    #[test]
    fn slice_id_dynamic_matches_static_returns_static() {
        let mut ti = make_ti();
        let sid = ti.intern_type_id_slice(vec![TypeId::STR]);
        assert_eq!(sid, SliceId::STR);
        assert!(sid.is_static_type_id());
    }

    #[test]
    fn slice_id_dynamic_matches_static_multi_element() {
        let mut ti = make_ti();
        let sid = ti.intern_type_id_slice(vec![TypeId::STR, TypeId::STR]);
        assert_eq!(sid, SliceId::STR_STR);
    }

    #[test]
    fn slice_id_identical_dynamic_struct_field_returns_same_handle() {
        let mut ti = make_ti();
        let f = vec![StructField {
            id: Id::new("x"),
            type_: TypeId::NUMBER,
        }];
        let a = ti.intern_struct_field_slice(f.clone());
        let b = ti.intern_struct_field_slice(f);
        assert_eq!(a, b);
    }

    #[test]
    fn slice_id_different_dynamic_struct_field_returns_different_handles() {
        let mut ti = make_ti();
        let a = ti.intern_struct_field_slice(vec![StructField {
            id: Id::new("x"),
            type_: TypeId::NUMBER,
        }]);
        let b = ti.intern_struct_field_slice(vec![StructField {
            id: Id::new("y"),
            type_: TypeId::NUMBER,
        }]);
        assert_ne!(a, b);
    }

    // -----------------------------------------------------------------------
    // Type interning — identity
    // -----------------------------------------------------------------------

    #[test]
    fn intern_scalar_types_are_stable() {
        let mut ti = make_ti();
        assert_eq!(ti.intern_type(&Type::Any).0, TypeId::ANY);
        assert_eq!(ti.intern_type(&Type::Bool).0, TypeId::BOOL);
        assert_eq!(ti.intern_type(&Type::Number).0, TypeId::NUMBER);
        assert_eq!(ti.intern_type(&Type::Str).0, TypeId::STR);
        assert_eq!(ti.intern_type(&Type::Unit).0, TypeId::UNIT);
        assert_eq!(ti.intern_type(&Type::Nil).0, TypeId::NIL);
    }

    #[test]
    fn intern_array_any_is_stable() {
        let mut ti = make_ti();
        assert_eq!(ti.intern_type(&Type::ARRAY_ANY).0, TypeId::ARRAY_ANY);
    }

    #[test]
    fn intern_map_any_any_is_stable() {
        let mut ti = make_ti();
        assert_eq!(ti.intern_type(&Type::MAP_ANY_ANY).0, TypeId::MAP_ANY_ANY);
    }

    #[test]
    fn intern_function_any_is_stable() {
        let mut ti = make_ti();
        assert_eq!(ti.intern_type(&Type::FUNCTION_ANY).0, TypeId::FUNCTION_ANY);
    }

    // -----------------------------------------------------------------------
    // Type interning — structural equality
    // -----------------------------------------------------------------------

    #[test]
    fn intern_same_array_yields_same_id() {
        let mut ti = make_ti();
        let a = ti.intern_type(&Type::Array { elem: TypeId::NUMBER }).0;
        let b = ti.intern_type(&Type::Array { elem: TypeId::NUMBER }).0;
        assert_eq!(a, b);
    }

    #[test]
    fn intern_same_map_yields_same_id() {
        let mut ti = make_ti();
        let a = ti
            .intern_type(&Type::Map {
                key: TypeId::STR,
                value: TypeId::BOOL,
            })
            .0;
        let b = ti
            .intern_type(&Type::Map {
                key: TypeId::STR,
                value: TypeId::BOOL,
            })
            .0;
        assert_eq!(a, b);
    }

    #[test]
    fn intern_same_tuple_yields_same_id() {
        let mut ti = make_ti();
        let s1 = ti.intern_type_id_slice(vec![TypeId::NUMBER, TypeId::STR]);
        let s2 = ti.intern_type_id_slice(vec![TypeId::NUMBER, TypeId::STR]);
        // Same slice content → same SliceId
        assert_eq!(s1, s2);
        let a = ti.intern_type(&Type::Tuple { members: s1 }).0;
        let b = ti.intern_type(&Type::Tuple { members: s2 }).0;
        assert_eq!(a, b);
    }

    #[test]
    fn intern_different_tuples_yield_different_ids() {
        let mut ti = make_ti();
        let s1 = ti.intern_type_id_slice(vec![TypeId::NUMBER]);
        let s2 = ti.intern_type_id_slice(vec![TypeId::STR]);
        let a = ti.intern_type(&Type::Tuple { members: s1 }).0;
        let b = ti.intern_type(&Type::Tuple { members: s2 }).0;
        assert_ne!(a, b);
    }

    #[test]
    fn intern_same_function_yields_same_id() {
        let mut ti = make_ti();
        let params = ti.intern_type_id_slice(vec![TypeId::STR, TypeId::NUMBER]);
        let a = ti
            .intern_type(&Type::Function {
                params,
                variadic: None,
                return_: TypeId::BOOL,
            })
            .0;
        let b = ti
            .intern_type(&Type::Function {
                params,
                variadic: None,
                return_: TypeId::BOOL,
            })
            .0;
        assert_eq!(a, b);
    }

    #[test]
    fn intern_different_functions_yield_different_ids() {
        let mut ti = make_ti();
        let p1 = ti.intern_type_id_slice(vec![TypeId::STR]);
        let p2 = ti.intern_type_id_slice(vec![TypeId::NUMBER]);
        let a = ti
            .intern_type(&Type::Function {
                params: p1,
                variadic: None,
                return_: TypeId::BOOL,
            })
            .0;
        let b = ti
            .intern_type(&Type::Function {
                params: p2,
                variadic: None,
                return_: TypeId::BOOL,
            })
            .0;
        assert_ne!(a, b);
    }

    #[test]
    fn intern_same_struct_yields_same_id() {
        let mut ti = make_ti();
        let fields = ti.intern_struct_field_slice(vec![StructField {
            id: Id::new("x"),
            type_: TypeId::NUMBER,
        }]);
        let a = ti
            .intern_type(&Type::Struct(StructType {
                id: Id::new("Point"),
                fields,
            }))
            .0;
        let b = ti
            .intern_type(&Type::Struct(StructType {
                id: Id::new("Point"),
                fields,
            }))
            .0;
        assert_eq!(a, b);
    }

    #[test]
    fn intern_different_structs_yield_different_ids() {
        let mut ti = make_ti();
        let f1 = ti.intern_struct_field_slice(vec![StructField {
            id: Id::new("x"),
            type_: TypeId::NUMBER,
        }]);
        let f2 = ti.intern_struct_field_slice(vec![StructField {
            id: Id::new("y"),
            type_: TypeId::NUMBER,
        }]);
        let a = ti
            .intern_type(&Type::Struct(StructType {
                id: Id::new("Point"),
                fields: f1,
            }))
            .0;
        let b = ti
            .intern_type(&Type::Struct(StructType {
                id: Id::new("Point"),
                fields: f2,
            }))
            .0;
        assert_ne!(a, b);
    }

    // -----------------------------------------------------------------------
    // Type — Copy
    // -----------------------------------------------------------------------

    #[test]
    fn type_is_copy() {
        let t = Type::Any;
        let _t2 = t; // no move
        let _t3 = t; // still usable
    }

    #[test]
    fn struct_type_is_copy() {
        let mut ti = make_ti();
        let fields = ti.intern_struct_field_slice(vec![]);
        let st = StructType {
            id: Id::new("Foo"),
            fields,
        };
        let _st2 = st;
        let _st3 = st;
    }

    // -----------------------------------------------------------------------
    // Type — const construction
    // -----------------------------------------------------------------------

    #[test]
    fn const_type_function_uses_static_slices() {
        assert_eq!(Type::FUNCTION_STR_TO_NUMBER, Type::FUNCTION_STR_TO_NUMBER);
        assert_ne!(Type::FUNCTION_STR_TO_NUMBER, Type::FUNCTION_STR_TO_BOOL);
    }

    #[test]
    fn const_type_function_interns_as_single_id() {
        let mut ti = make_ti();
        let a = ti.intern_type(&Type::FUNCTION_STR_TO_NUMBER).0;
        let b = ti.intern_type(&Type::FUNCTION_STR_TO_NUMBER).0;
        assert_eq!(a, b);
    }

    // -----------------------------------------------------------------------
    // TypeInterner — round-trip
    // -----------------------------------------------------------------------

    #[test]
    fn intern_then_lookup_returns_same_type() {
        let mut ti = make_ti();
        let array_type = Type::Array { elem: TypeId::STR };
        let (id, _) = ti.intern_type(&array_type);
        let resolved = ti.get_type(id).unwrap();
        assert_eq!(resolved, array_type);
    }

    #[test]
    fn interned_function_round_trips() {
        let mut ti = make_ti();
        let params = ti.intern_type_id_slice(vec![TypeId::STR]);
        let fn_type = Type::Function {
            params,
            variadic: Some(TypeId::NUMBER),
            return_: TypeId::BOOL,
        };
        let (id, _) = ti.intern_type(&fn_type);
        let resolved = ti.get_type(id).unwrap();
        assert_eq!(resolved, fn_type);
    }

    #[test]
    fn interned_tuple_round_trips() {
        let mut ti = make_ti();
        let members = ti.intern_type_id_slice(vec![TypeId::NUMBER, TypeId::STR, TypeId::BOOL]);
        let tuple_type = Type::Tuple { members };
        let (id, _) = ti.intern_type(&tuple_type);
        let resolved = ti.get_type(id).unwrap();
        assert_eq!(resolved, tuple_type);
    }

    #[test]
    fn interned_struct_round_trips() {
        let mut ti = make_ti();
        let fields = ti.intern_struct_field_slice(vec![
            StructField {
                id: Id::new("x"),
                type_: TypeId::NUMBER,
            },
            StructField {
                id: Id::new("y"),
                type_: TypeId::STR,
            },
        ]);
        let struct_type = Type::Struct(StructType {
            id: Id::new("Point"),
            fields,
        });
        let (id, _) = ti.intern_type(&struct_type);
        let resolved = ti.get_type(id).unwrap();
        assert_eq!(resolved, struct_type);
    }

    // -----------------------------------------------------------------------
    // SliceId — lookup returns correct data
    // -----------------------------------------------------------------------

    #[test]
    fn lookup_static_type_id_slice_returns_data() {
        let ti = make_ti();
        let slice = ti.lookup_type_id_slice(SliceId::STR);
        assert_eq!(slice, &[TypeId::STR]);
    }

    #[test]
    fn lookup_static_multi_element_slice() {
        let ti = make_ti();
        let slice = ti.lookup_type_id_slice(SliceId::STR_STR);
        assert_eq!(slice, &[TypeId::STR, TypeId::STR]);
    }

    #[test]
    fn lookup_dynamic_type_id_slice() {
        let mut ti = make_ti();
        let sid = ti.intern_type_id_slice(vec![TypeId::NUMBER, TypeId::BOOL, TypeId::STR]);
        let slice = ti.lookup_type_id_slice(sid);
        assert_eq!(slice, &[TypeId::NUMBER, TypeId::BOOL, TypeId::STR]);
    }

    #[test]
    fn lookup_struct_field_slice() {
        let mut ti = make_ti();
        let fields = vec![
            StructField {
                id: Id::new("x"),
                type_: TypeId::NUMBER,
            },
            StructField {
                id: Id::new("y"),
                type_: TypeId::STR,
            },
        ];
        let sid = ti.intern_struct_field_slice(fields.clone());
        let slice = ti.lookup_struct_field_slice(sid);
        assert_eq!(slice, fields.as_slice());
    }

    // -----------------------------------------------------------------------
    // Type — variadic functions
    // -----------------------------------------------------------------------

    #[test]
    fn variadic_function_types_are_distinct() {
        assert_ne!(Type::FUNCTION_ANY_VARIADIC_ANY_TO_UNIT, Type::FUNCTION_ANY_TO_ANY);
    }

    #[test]
    fn variadic_and_non_variadic_same_params_are_distinct() {
        let mut ti = make_ti();
        let a = ti
            .intern_type(&Type::Function {
                params: SliceId::STR,
                variadic: None,
                return_: TypeId::NIL,
            })
            .0;
        let b = ti
            .intern_type(&Type::Function {
                params: SliceId::STR,
                variadic: Some(TypeId::NUMBER),
                return_: TypeId::NIL,
            })
            .0;
        assert_ne!(a, b);
    }

    // -----------------------------------------------------------------------
    // TypeInterner — already-interned detection
    // -----------------------------------------------------------------------

    #[test]
    fn intern_returns_true_for_already_interned() {
        let mut ti = make_ti();
        let (_, first) = ti.intern_type(&Type::Number);
        assert!(first); // predefined, so already there
        let (_, second) = ti.intern_type(&Type::Number);
        assert!(second);
    }

    #[test]
    fn intern_returns_false_for_new_type() {
        let mut ti = make_ti();
        let members = ti.intern_type_id_slice(vec![TypeId::NUMBER, TypeId::NUMBER, TypeId::NUMBER]);
        let new_type = Type::Tuple { members };
        let (_, is_existing) = ti.intern_type(&new_type);
        assert!(!is_existing);
        let (_, is_existing2) = ti.intern_type(&new_type);
        assert!(is_existing2);
    }
}
