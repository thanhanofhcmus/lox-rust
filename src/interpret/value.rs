use std::{cmp::Ordering, collections::BTreeMap};

use derive_more::Display;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use super::{error::Error, interpreter::Interpreter};

use crate::{
    ast::BlockNode,
    id::Id,
    interpret::{gc::GcHandle, Environment},
};

pub type BuiltinFn = fn(&mut Interpreter, Vec<Value>) -> Result<Value, Error>;

#[derive(Debug, Clone)]
pub struct Function {
    pub arg_ids: Vec<Id>,
    pub body: BlockNode,
}

#[derive(Display, Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Value {
    #[display(fmt = "nil")]
    Nil,

    #[display(fmt = "()")]
    Unit,

    // The integer variant must be declared before the floating variant
    // for serde to try to parse it first
    #[display(fmt = "{}", _0)]
    Integer(i64),

    #[display(fmt = "{}", _0)]
    Floating(f64),

    #[display(fmt = "{}", _0)]
    Bool(bool),

    #[display(fmt = "{:?}", _0)] // Quotes for strings
    Str(String),

    #[display(fmt = "{}", "format_array(_0)")] // Call helper
    Array(Vec<Value>),

    #[display(fmt = "%{}", "format_map(_0)")] // Call helper
    #[serde(serialize_with = "serialize_map", deserialize_with = "deserialize_map")]
    Map(BTreeMap<Value, Value>),

    #[display(fmt = "function")]
    #[serde(skip)]
    Function(GcHandle),

    #[display(fmt = "builtin_function")]
    #[serde(skip)]
    BuiltinFunction(BuiltinFn),
}

impl Value {
    // does not mean anything, just for ordering in a btree
    fn type_rank(&self) -> u8 {
        match self {
            Value::Nil => 0,
            Value::Bool(_) => 1,
            Value::Integer(_) => 2,
            Value::Floating(_) => 3,
            Value::Str(_) => 4,
            Value::Array(_) => 5,
            Value::Map(_) => 6,
            Value::Function(_) => 7,
            Value::BuiltinFunction(_) => 8,
            Value::Unit => 10,
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

    pub fn deep_eq(&self, other: &Self, env: &Environment) -> bool {
        if self.is_scalar_type() && other.is_scalar_type() {
            return self.shallow_eq(other);
        }
        if self.is_function_type() && other.is_function_type() {
            return self.function_eq(other);
        }
        use Value::*;
        match (self, other) {
            // TODO: get value from the heap for GC equal
            (Str(l), Str(r)) => l == r,
            (Array(l), Array(r)) => {
                l.len() == r.len() && l.iter().zip(r).all(|(a, b)| a.deep_eq(b, env))
            }
            (Map(l), Map(r)) => {
                if l.len() != r.len() {
                    return false;
                }
                for (l_key, l_value) in l.iter() {
                    let Some(r_value) = r.get(l_key) else {
                        return false;
                    };
                    if !l_value.deep_eq(r_value, env) {
                        return false;
                    }
                }
                true
            }
            _ => false,
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

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use Value::*;

        let rank_cmp = self.type_rank().cmp(&other.type_rank());
        if rank_cmp != Ordering::Equal {
            return rank_cmp;
        }

        match (self, other) {
            // TODO
            (Unit, _) => panic!("ordering where lhs is unit"),
            (_, Unit) => panic!("ordering where rhs is unit"),

            (Nil, Nil) => Ordering::Equal,
            (Bool(a), Bool(b)) => a.cmp(b),
            (Str(a), Str(b)) => a.cmp(b),
            (Floating(a), Floating(b)) => {
                // Handle f64 comparison (ignoring NaN for simplicity, or treat NaN as the smallest number)
                a.partial_cmp(b).unwrap_or(Ordering::Less)
            }
            (Array(a), Array(b)) => a.cmp(b),
            (Map(a), Map(b)) => a.cmp(b),
            (Function(a), Function(b)) => a.cmp(b),
            (BuiltinFunction(a), BuiltinFunction(b)) => {
                (a as *const BuiltinFn).cmp(&(b as *const BuiltinFn))
            }
            _ => Ordering::Equal,
        }
    }
}

fn serialize_map<S: Serializer>(
    m: &BTreeMap<Value, Value>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    let string_map = m
        .iter()
        .filter_map(|(k, v)| {
            match v {
                Value::BuiltinFunction(_) | Value::Function(_) => return None,
                _ => {}
            };
            match k {
                Value::Str(s) => Some((s, v)),
                // We can allow other type to be key with to_string here
                _ => None,
            }
        })
        .collect::<BTreeMap<&String, &Value>>();

    string_map.serialize(serializer)
}

fn deserialize_map<'de, D: Deserializer<'de>>(
    deserializer: D,
) -> Result<BTreeMap<Value, Value>, D::Error> {
    // might have performance issue here if the json object is to big
    let string_map = BTreeMap::<String, Value>::deserialize(deserializer)?;
    let mut value_map = BTreeMap::new();
    for (k, v) in string_map {
        value_map.insert(Value::Str(k), v);
    }
    Ok(value_map)
}

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
fn format_map(values: &BTreeMap<Value, Value>) -> String {
    let internal = values
        .iter()
        .map(|(k, v)| format!("{} => {}", k, v))
        .collect::<Vec<_>>()
        .join(", ");
    format!("{{{}}}", internal)
}
