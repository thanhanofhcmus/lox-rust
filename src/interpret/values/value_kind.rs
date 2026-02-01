#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum ValueKind {
    Nil,
    Integer,
    Floating,
    Bool,
    Unit,
    Str,
    Array,
    Map,
    Function,
    BuiltinFunction,
}

pub trait GetValueKind {
    fn get_kind(&self) -> ValueKind;
}
