use std::collections::BTreeMap;

use derive_more::Display;

use crate::{
    ast::BlockNode,
    id::Id,
    interpret::{
        error::Error,
        heap::GcHandle,
        string_interner::StrId,
        values::{display_writer::DisplayWriter, number::Number, scalar::Scalar},
        Environment, Interpreter,
    },
};

pub type BuiltinFn = fn(&mut Interpreter, Vec<Value>) -> Result<Value, Error>;

#[derive(Debug, Clone)]
pub struct Function {
    pub arg_ids: Vec<Id>,
    pub body: BlockNode,
}

pub type Array = Vec<Value>;

pub type Map = BTreeMap<MapKey, Value>;

#[derive(Display, derive_more::Debug, Clone, Copy)]
pub enum Value {
    #[display("()")]
    Unit,

    Scalar(Scalar),

    // TODO: Error crate needs display method and is using this format
    // we need to make error print the actual value or maybe keep printing String like this
    #[display("string({:?})", _0)]
    #[debug("String({:?})", _0)]
    Str(StrId),

    #[display("array({:?})", _0)]
    #[debug("Array({:?})", _0)]
    Array(GcHandle),

    #[display("map({:?})", _0)]
    #[debug("Map({:?})", _0)]
    Map(GcHandle),

    #[display("function({:?})", _0)]
    #[debug("Function({:?})", _0)]
    Function(GcHandle),

    #[display("builtin_function")]
    #[debug("BuitinFunction({:?})", _0)]
    BuiltinFunction(BuiltinFn),
}

impl Value {
    pub fn make_nil() -> Value {
        Value::Scalar(Scalar::Nil)
    }

    pub fn make_bool(v: bool) -> Value {
        Value::Scalar(Scalar::Bool(v))
    }

    pub fn make_number(v: Number) -> Value {
        Value::Scalar(Scalar::Number(v))
    }

    pub fn get_bool(self) -> Option<bool> {
        match self {
            Value::Scalar(Scalar::Bool(v)) => Some(v),
            _ => None,
        }
    }

    pub fn get_handle(self) -> Option<GcHandle> {
        match self {
            Value::Array(handle) | Value::Map(handle) | Value::Function(handle) => Some(handle),
            _ => None,
        }
    }

    pub fn replace_handle(&mut self, new_handle: GcHandle) -> Option<GcHandle> {
        match self {
            Value::Array(handle) | Value::Map(handle) | Value::Function(handle) => {
                Some(std::mem::replace(handle, new_handle))
            }
            _ => None,
        }
    }

    pub fn deep_eq(&self, other: &Self, env: &Environment) -> Result<bool, Error> {
        use Value::*;
        match (self, other) {
            // TODO
            (Unit, _) => panic!("compare where lhs is unit"),
            (_, Unit) => panic!("compare where rhs is unit"),

            (Scalar(l), Scalar(r)) => Ok(l == r),

            (Str(l), Str(r)) => Ok(l == r),

            // 2 functions are considered equals if they point to the same `blueprint`
            (Value::Function(l), Value::Function(r)) => Ok(l == r),
            (Value::BuiltinFunction(l), Value::BuiltinFunction(r)) => {
                Ok(std::ptr::fn_addr_eq(*l, *r))
            }

            (Array(l_handle), Array(r_handle)) => {
                if l_handle == r_handle {
                    return Ok(true);
                }
                let l_arr = env.get_array(*l_handle)?;
                let r_arr = env.get_array(*r_handle)?;
                if l_arr.len() != r_arr.len() {
                    return Ok(false);
                }
                for (l_value, r_value) in l_arr.iter().zip(r_arr) {
                    if !(l_value.deep_eq(r_value, env)?) {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (Map(l_handle), Map(r_handle)) => {
                if l_handle == r_handle {
                    return Ok(true);
                }
                let l_map = env.get_map(*l_handle)?;
                let r_map = env.get_map(*r_handle)?;
                for (l_key, l_value) in l_map.iter() {
                    let Some(r_value) = r_map.get(l_key) else {
                        return Ok(false);
                    };
                    if !(l_value.deep_eq(r_value, env)?) {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    pub fn get_at_index(&self, indexee: Value, env: &Environment) -> Result<Value, Error> {
        let Some(handle) = self.get_handle() else {
            return Err(Error::ValueUnIndexable(*self));
        };
        env.get_gc_object(handle)
            .ok_or(Error::GcObjectNotFound(handle))?
            .get_at_index(indexee)
    }

    pub fn to_index(self) -> Result<usize, Error> {
        let Value::Scalar(Scalar::Number(v)) = self else {
            return Err(Error::ValueMustBeUsize(self));
        };
        v.try_into()
    }
}

impl DisplayWriter for Value {
    fn write_display(self, env: &Environment, w: &mut dyn std::io::Write) -> Result<(), Error> {
        let convert = |e| Error::WriteValueFailed(self, e);

        match self {
            Value::Scalar(v) => v.write_display(env, w),
            Value::Unit => write!(w, "()").map_err(convert),
            Value::Str(handle) => {
                let s = env.get_string(handle)?;
                w.write_all(s.as_bytes()).map_err(convert)
            }
            Value::Array(handle) => {
                let arr = env.get_array(handle)?;
                w.write_all(b"[").map_err(convert)?;

                if let Some((last, rest)) = arr.split_last() {
                    for value in rest {
                        value.write_display(env, w)?;
                        w.write_all(b", ").map_err(convert)?;
                    }
                    last.write_display(env, w)?;
                }

                w.write_all(b"]").map_err(convert)?;

                Ok(())
            }
            Value::Map(handle) => {
                let map: &Map = env.get_map(handle)?;

                let mut entries = map.iter();

                w.write_all(b"%{").map_err(convert)?;
                if let Some((first_key, first_value)) = entries.next() {
                    first_key.write_display(env, w)?;
                    write!(w, " => ").map_err(convert)?;
                    first_value.write_display(env, w)?;

                    for (k, v) in entries {
                        write!(w, ", ").map_err(convert)?;
                        k.write_display(env, w)?;
                        write!(w, " => ").map_err(convert)?;
                        v.write_display(env, w)?;
                    }
                }
                w.write_all(b"}").map_err(convert)?;

                Ok(())
            }
            Value::Function(_) => write!(w, "function").map_err(convert),
            Value::BuiltinFunction(_) => write!(w, "builtin_function").map_err(convert),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MapKey {
    Scalar(Scalar),
    Str(StrId),
}

impl MapKey {
    pub fn convert_from_value(value: Value) -> Result<Self, Error> {
        match value {
            Value::Scalar(v) => Ok(Self::Scalar(v)),
            Value::Str(id) => Ok(MapKey::Str(id)),
            Value::Array(_)
            | Value::Map(_)
            | Value::Unit
            | Value::Function(_)
            | Value::BuiltinFunction(_) => Err(Error::ValueCannotBeUsedAsKey(value)),
        }
    }
}

impl DisplayWriter for MapKey {
    fn write_display(self, env: &Environment, w: &mut dyn std::io::Write) -> Result<(), Error> {
        match self {
            MapKey::Scalar(v) => v.write_display(env, w),
            MapKey::Str(handle) => {
                let s = env.get_string(handle)?;
                w.write_all(s.as_bytes())
                    .map_err(|e| Error::WriteValueFailed(Value::Str(handle), e))
            }
        }
    }
}
