use std::collections::BTreeMap;

use derive_more::Display;

use super::{error::Error, interpreter::Interpreter};

use crate::{
    ast::BlockNode,
    id::Id,
    interpret::{gc::GcHandle, helper_values::MapKey, Environment},
};

pub type BuiltinFn = fn(&mut Interpreter, Vec<Value>) -> Result<Value, Error>;

#[derive(Debug, Clone)]
pub struct Function {
    pub arg_ids: Vec<Id>,
    pub body: BlockNode,
}

pub type Array = Vec<Value>;

pub type Map = BTreeMap<MapKey, Value>;

#[derive(Display, Debug, Clone, Copy)]
pub enum Value {
    #[display(fmt = "nil")]
    Nil,

    #[display(fmt = "()")]
    Unit,

    #[display(fmt = "{}", _0)]
    Integer(i64),

    #[display(fmt = "{}", _0)]
    Floating(f64),

    #[display(fmt = "{}", _0)]
    Bool(bool),

    // TODO: Error crate needs display method and is using this format
    // we need to make error print the actual value or maybe keep printing String like this
    #[display(fmt = "String({:?})", _0)] // Quotes for strings
    Str(GcHandle),

    #[display(fmt = "Array({:?})", _0)]
    Array(GcHandle),

    #[display(fmt = "Map({:?})", _0)] // Call helper
    Map(GcHandle),

    #[display(fmt = "function")]
    Function(GcHandle),

    #[display(fmt = "builtin_function")]
    BuiltinFunction(BuiltinFn),
}

impl Value {
    pub fn get_handle(&self) -> Option<GcHandle> {
        match self {
            Value::Str(handle) => Some(*handle),
            Value::Array(handle) => Some(*handle),
            Value::Function(handle) => Some(*handle),
            _ => None,
        }
    }

    pub fn is_scalar_type(&self) -> bool {
        matches!(self, Value::Nil | Value::Unit | Value::Bool(_) | Value::Integer(_) | Value::Floating(_) if {
            true
        })
    }

    pub fn is_function_type(&self) -> bool {
        matches!(self, Value::Function(_) | Value::BuiltinFunction(_) if {
            true
        })
    }

    pub fn function_eq(&self, other: &Self) -> bool {
        match (self, other) {
            // 2 functions are considireded equals if they point to the same `blueprint`
            (Value::Function(l), Value::Function(r)) => l == r,
            (Value::BuiltinFunction(l), Value::BuiltinFunction(r)) => std::ptr::fn_addr_eq(*l, *r),
            _ => false,
        }
    }

    pub fn deep_eq(&self, other: &Self, env: &Environment) -> Result<bool, Error> {
        if self.is_scalar_type() && other.is_scalar_type() {
            return Ok(self.shallow_eq(other));
        }
        if self.is_function_type() && other.is_function_type() {
            return Ok(self.function_eq(other));
        }
        use Value::*;
        match (self, other) {
            (Str(l_handle), Str(r_handle)) => {
                if l_handle == r_handle {
                    return Ok(true);
                }
                let l_str = env.get_string(*l_handle)?;
                let r_str = env.get_string(*r_handle)?;
                Ok(l_str == r_str)
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

    pub fn shallow_eq(&self, other: &Self) -> bool {
        if self.is_function_type() && other.is_function_type() {
            return self.function_eq(other);
        }

        use Value::*;
        const NUMBER_DELTA: f64 = 1e-10;

        match (self, other) {
            // TODO
            (Unit, _) => panic!("compare where lhs is unit"),
            (_, Unit) => panic!("compare where rhs is unit"),

            (Nil, Nil) => true,
            (Bool(l), Bool(r)) => l == r,

            (Integer(l), Integer(r)) => l == r,
            (Floating(l), Integer(r)) => (l - (*r as f64)).abs() < NUMBER_DELTA,
            (Integer(l), Floating(r)) => ((*l as f64) - r).abs() < NUMBER_DELTA,
            (Floating(l), Floating(r)) => (*l - *r).abs() < NUMBER_DELTA,

            (Str(l), Str(r)) => l == r,

            (Array(l), Array(r)) => l == r,
            // (Map(l), Map(r)) => l.len() == r.len() && l.iter().all(|(k, v)| r.get(k) == Some(v)),
            _ => false,
        }
    }

    pub fn write_display(
        &self,
        env: &Environment,
        w: &mut dyn std::io::Write,
    ) -> Result<(), Error> {
        let convert = |e| Error::WriteFailed(self.clone(), e);

        match self {
            Value::Nil => write!(w, "nil").map_err(convert),
            Value::Unit => write!(w, "()").map_err(convert),
            Value::Integer(v) => write!(w, "{}", *v).map_err(convert),
            Value::Floating(v) => write!(w, "{}", *v).map_err(convert),
            Value::Bool(v) => write!(w, "{}", *v).map_err(convert),
            Value::Str(handle) => {
                let s = env.get_string(*handle)?;
                w.write_all(s.as_bytes()).map_err(convert)
            }
            Value::Array(handle) => {
                let arr = env.get_array(*handle)?;
                w.write_all(b"[").map_err(convert)?;

                for value in arr {
                    value.write_display(env, w)?;
                    // TODO: for the last value, do not write `,`
                    w.write_all(b", ").map_err(convert)?;
                }
                w.write_all(b"]").map_err(convert)?;

                Ok(())
            }
            Value::Map(handle) => {
                let map = env.get_map(*handle)?;
                w.write_all(b"%{").map_err(convert)?;
                for (k, v) in map {
                    write!(w, "{}", k).map_err(convert)?;
                    // TODO: for the last value, do not write `,`
                    w.write_all(b" => ").map_err(convert)?;
                    v.write_display(env, w)?;
                    w.write_all(b", ").map_err(convert)?;
                }
                w.write_all(b"}").map_err(convert)?;

                Ok(())
            }
            Value::Function(_) => write!(w, "function").map_err(convert),
            Value::BuiltinFunction(_) => write!(w, "builtin_function").map_err(convert),
        }
    }
}
