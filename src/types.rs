#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Any, // gradual typing
    Bool,
    Number,
    Str,
    Array(Box<Type>),
    Map {
        key: Box<Type>,
        value: Box<Type>,
    },
    Function {
        params: Vec<Type>,
        return_: Box<Type>,
    },
    Never, // internal bottom type: expressions that never produce a value (bare return)
    Nil,
}
