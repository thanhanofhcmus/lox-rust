// We might want a 2 phase type system
// A heavy Type: Carry Type info enough to resolve the type
//
// A small Type (TypeId): Only have the Id to the type. Cheap to copy around

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Any, // gradual typing, top type
    Bool,
    Number,
    Str,
    Array(Box<Type>),
    Map {
        // Key is always string
        value: Box<Type>,
    },
    Function {
        params: Vec<Type>,
        return_: Box<Type>,
    },
    Unit, // the `()` value — bare `return`, statement-only blocks, void-ish calls
    Nil,
}
