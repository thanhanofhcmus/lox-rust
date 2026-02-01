use std::{cmp::Ordering, collections::BTreeMap};

use derive_more::Display;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::interpret::{error::Error, value::Value, Environment};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SerialValue {
    Nil,

    // The integer variant must be declared before the floating variant
    // for serde to try to parse it first
    Integer(i64),

    Floating(f64),

    Bool(bool),

    Str(String),

    Array(Vec<SerialValue>),

    #[serde(serialize_with = "serialize_map", deserialize_with = "deserialize_map")]
    Map(BTreeMap<MapKey, SerialValue>),
}

impl SerialValue {
    pub fn convert_from_value(value: Value, env: &Environment) -> Result<Self, Error> {
        let v = match value {
            Value::Nil => SerialValue::Nil,
            Value::Integer(v) => SerialValue::Integer(v),
            Value::Floating(v) => SerialValue::Floating(v),
            Value::Bool(v) => SerialValue::Bool(v),

            Value::Str(handle) => {
                let s = env.get_string(handle)?;
                SerialValue::Str(s.clone())
            }

            Value::Array(handle) => {
                let arr = env.get_array(handle)?;
                let mut vs = Vec::with_capacity(arr.len());
                for v in arr {
                    let sv = SerialValue::convert_from_value(v.to_owned(), env)?;
                    vs.push(sv);
                }
                SerialValue::Array(vs)
            }
            Value::Map(handle) => {
                let map = env.get_map(handle)?;
                let mut vsm = BTreeMap::<MapKey, SerialValue>::new();
                for (k, v) in map {
                    let v_sv = SerialValue::convert_from_value(*v, env)?;
                    vsm.insert(k.clone(), v_sv);
                }
                SerialValue::Map(vsm)
            }

            Value::Unit | Value::Function(_) | Value::BuiltinFunction(_) => {
                return Err(Error::TypeIsNotSerializable(value))
            }
        };
        Ok(v)
    }

    pub fn hydrate(self, env: &mut Environment) -> Result<Value, Error> {
        let v = match self {
            SerialValue::Nil => Value::Nil,
            SerialValue::Integer(v) => Value::Integer(v),
            SerialValue::Floating(v) => Value::Floating(v),
            SerialValue::Bool(v) => Value::Bool(v),

            SerialValue::Str(v) => env.insert_string_variable(v),

            SerialValue::Array(values) => {
                let mut arr = Vec::with_capacity(values.len());
                for v in values {
                    let sv = SerialValue::hydrate(v, env)?;
                    arr.push(sv);
                }
                env.insert_array_variable(arr)
            }
            SerialValue::Map(m) => {
                let mut map = BTreeMap::new();
                for (k, v) in m {
                    let v_sv = SerialValue::hydrate(v, env)?;
                    map.insert(k, v_sv);
                }
                env.insert_map_variable(map)
            }
        };
        Ok(v)
    }
}

#[derive(Display, Debug, Clone)]
pub enum MapKey {
    #[display("nil")]
    Nil,

    // TODO: number will be considered the same if they are use in the same map
    #[display("{}", _0)]
    Integer(i64),
    #[display("{}", _0)]
    Floating(f64),

    #[display("{}", _0)]
    Bool(bool),

    #[display("{}", _0)]
    Str(String),
}

impl MapKey {
    fn type_rank(&self) -> u8 {
        match self {
            MapKey::Nil => 0,
            MapKey::Bool(_) => 1,
            MapKey::Integer(_) => 2,
            MapKey::Floating(_) => 3,
            MapKey::Str(_) => 4,
        }
    }

    pub fn convert_from_simple_value(value: Value) -> Result<Self, Error> {
        let v = match value {
            Value::Nil => MapKey::Nil,
            Value::Integer(v) => MapKey::Integer(v),
            Value::Floating(v) => MapKey::Floating(v),
            Value::Bool(v) => MapKey::Bool(v),
            _ => return Err(Error::ValueCannotBeUsedAsKey(value)),
        };
        Ok(v)
    }

    pub fn convert_from_value(value: Value, env: &Environment) -> Result<Self, Error> {
        match value {
            Value::Str(handle) => {
                let s = env.get_string(handle)?;
                Ok(MapKey::Str(s.clone()))
            }

            Value::Array(_)
            | Value::Map(_)
            | Value::Unit
            | Value::Function(_)
            | Value::BuiltinFunction(_) => Err(Error::ValueCannotBeUsedAsKey(value)),

            _ => Self::convert_from_simple_value(value),
        }
    }
}

impl PartialEq for MapKey {
    fn eq(&self, other: &Self) -> bool {
        use MapKey::*;
        const NUMBER_DELTA: f64 = 1e-10;

        match (self, other) {
            (Nil, Nil) => true,
            (Bool(l), Bool(r)) => l == r,

            (Integer(l), Integer(r)) => l == r,
            (Floating(l), Integer(r)) => (l - (*r as f64)).abs() < NUMBER_DELTA,
            (Integer(l), Floating(r)) => ((*l as f64) - r).abs() < NUMBER_DELTA,
            (Floating(l), Floating(r)) => (*l - *r).abs() < NUMBER_DELTA,

            (Str(l), Str(r)) => l == r,

            _ => false,
        }
    }
}

impl Eq for MapKey {}

impl PartialOrd for MapKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for MapKey {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use MapKey::*;

        let rank_cmp = self.type_rank().cmp(&other.type_rank());
        if rank_cmp != Ordering::Equal {
            return rank_cmp;
        }

        match (self, other) {
            (Nil, Nil) => Ordering::Equal,
            (Bool(a), Bool(b)) => a.cmp(b),
            (Str(a), Str(b)) => a.cmp(b),
            (Integer(a), Integer(b)) => a.cmp(b),
            (Floating(a), Floating(b)) => {
                // Handle f64 comparison (ignoring NaN for simplicity, or treat NaN as the smallest number)
                a.partial_cmp(b).unwrap_or(Ordering::Less)
            }
            _ => Ordering::Equal,
        }
    }
}

fn serialize_map<S: Serializer>(
    m: &BTreeMap<MapKey, SerialValue>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    let string_map = m
        .iter()
        .filter_map(|(k, v)| {
            match k {
                MapKey::Str(s) => Some((s, v)),
                // We can allow other type to be key with to_string here
                _ => None,
            }
        })
        .collect::<BTreeMap<&String, &SerialValue>>();

    string_map.serialize(serializer)
}

fn deserialize_map<'de, D: Deserializer<'de>>(
    deserializer: D,
) -> Result<BTreeMap<MapKey, SerialValue>, D::Error> {
    // might have performance issue here if the json object is to big
    let string_map = BTreeMap::<String, SerialValue>::deserialize(deserializer)?;
    let mut value_map = BTreeMap::new();
    for (k, v) in string_map {
        value_map.insert(MapKey::Str(k), v);
    }
    Ok(value_map)
}
