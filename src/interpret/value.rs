use std::collections::BTreeMap;

use derive_more::Display;

use super::{error::Error, interpreter::Interpreter};

use crate::{
    ast::BlockNode,
    id::Id,
    interpret::{gc::GcHandle, hepler_values::MapKey, Environment},
};

pub type BuiltinFn = fn(&mut Interpreter, Vec<Value>) -> Result<Value, Error>;

#[derive(Debug, Clone)]
pub struct Function {
    pub arg_ids: Vec<Id>,
    pub body: BlockNode,
}

#[derive(Display, Debug, Clone)]
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

    // TODO: reimplement fmt, this display does not mean anything
    //  we have to check for special case when displaying strings
    #[display(fmt = "{:?}", _0)] // Quotes for strings
    Str(GcHandle),

    #[display(fmt = "{}", "format_array(_0)")] // Call helper
    Array(Vec<Value>),

    #[display(fmt = "%{}", "format_map(_0)")] // Call helper
    Map(BTreeMap<MapKey, Value>),

    #[display(fmt = "function")]
    Function(GcHandle),

    #[display(fmt = "builtin_function")]
    BuiltinFunction(BuiltinFn),
}

impl Value {
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
            (Array(l), Array(r)) => {
                if l.len() != r.len() {
                    return Ok(false);
                }
                for (l_value, r_value) in l.iter().zip(r) {
                    if !(l_value.deep_eq(r_value, env)?) {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (Map(l), Map(r)) => {
                if l.len() != r.len() {
                    return Ok(false);
                }
                for (l_key, l_value) in l.iter() {
                    let Some(r_value) = r.get(l_key) else {
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

            (Array(l), Array(r)) => l.len() == r.len() && l.iter().zip(r).all(|(a, b)| a == b),
            (Map(l), Map(r)) => l.len() == r.len() && l.iter().all(|(k, v)| r.get(k) == Some(v)),

            _ => false,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.shallow_eq(other)
    }
}

impl Eq for Value {}
// Helper function to simulate Python's list printing
fn format_array(values: &[Value]) -> String {
    let internal = values
        .iter()
        .map(|v| v.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{}]", internal)
}

// Helper function to simulate Python's list printing
fn format_map(values: &BTreeMap<MapKey, Value>) -> String {
    let internal = values
        .iter()
        //TODO: fix display
        .map(|(k, v)| format!("{} => {}", k, v))
        .collect::<Vec<_>>()
        .join(", ");
    format!("{{{}}}", internal)
}
