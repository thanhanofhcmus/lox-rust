use std::{cmp::Ordering, collections::BTreeMap};

use derive_more::Display;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::interpret::{error::Error, value::Value, Environment};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SerializableValue {
    Nil,

    // The integer variant must be declared before the floating variant
    // for serde to try to parse it first
    Integer(i64),

    Floating(f64),

    Bool(bool),

    Str(String),

    Array(Vec<SerializableValue>),

    #[serde(serialize_with = "serialize_map", deserialize_with = "deserialize_map")]
    Map(BTreeMap<MapKey, SerializableValue>),
}

impl SerializableValue {
    pub fn convert_from_value(value: Value, env: &Environment) -> Result<Self, Error> {
        let v = match value {
            Value::Nil => SerializableValue::Nil,
            Value::Integer(v) => SerializableValue::Integer(v),
            Value::Floating(v) => SerializableValue::Floating(v),
            Value::Bool(v) => SerializableValue::Bool(v),

            Value::Str(handle) => {
                let s = env.get_string(handle)?;
                SerializableValue::Str(s.clone())
            }

            Value::Array(values) => {
                let mut vs = Vec::with_capacity(values.len());
                for v in values {
                    let sv = SerializableValue::convert_from_value(v, env)?;
                    vs.push(sv);
                }
                SerializableValue::Array(vs)
            }
            Value::Map(m) => {
                let mut vsm = BTreeMap::new();
                for (k, v) in m {
                    let v_sv = SerializableValue::convert_from_value(v, env)?;
                    vsm.insert(k, v_sv);
                }
                SerializableValue::Map(vsm)
            }

            Value::Unit | Value::Function(_) | Value::BuiltinFunction(_) => {
                return Err(Error::TypeIsNotSerializable(value))
            }
        };
        Ok(v)
    }

    pub fn hydrate(self, env: &mut Environment) -> Result<Value, Error> {
        let v = match self {
            SerializableValue::Nil => Value::Nil,
            SerializableValue::Integer(v) => Value::Integer(v),
            SerializableValue::Floating(v) => Value::Floating(v),
            SerializableValue::Bool(v) => Value::Bool(v),

            SerializableValue::Str(v) => env.insert_string_variable(v),

            SerializableValue::Array(values) => {
                let mut vs = Vec::with_capacity(values.len());
                for v in values {
                    let sv = SerializableValue::hydrate(v, env)?;
                    vs.push(sv);
                }
                Value::Array(vs)
            }
            SerializableValue::Map(m) => {
                let mut vsm = BTreeMap::new();
                for (k, v) in m {
                    let v_sv = SerializableValue::hydrate(v, env)?;
                    vsm.insert(k, v_sv);
                }
                Value::Map(vsm)
            }
        };
        Ok(v)
    }
}

#[derive(Display, Debug, Clone)]
pub enum MapKey {
    #[display(fmt = "nil")]
    Nil,

    // TODO: number will be considered the same if they are use in the same map
    #[display(fmt = "{}", _0)]
    Integer(i64),
    #[display(fmt = "{}", _0)]
    Floating(f64),

    #[display(fmt = "{}", _0)]
    Bool(bool),

    #[display(fmt = "{}", _0)]
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

    pub fn convert_from_value(value: Value, env: &Environment) -> Result<Self, Error> {
        let v = match value {
            Value::Nil => MapKey::Nil,
            Value::Integer(v) => MapKey::Integer(v),
            Value::Floating(v) => MapKey::Floating(v),
            Value::Bool(v) => MapKey::Bool(v),

            Value::Str(handle) => {
                let s = env.get_string(handle)?;
                MapKey::Str(s.clone())
            }

            // TODO: return error
            Value::Array(_)
            | Value::Map(_)
            | Value::Unit
            | Value::Function(_)
            | Value::BuiltinFunction(_) => return Err(Error::ValueCannotBeUsedAsKey(value)),
        };
        Ok(v)
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
    m: &BTreeMap<MapKey, SerializableValue>,
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
        .collect::<BTreeMap<&String, &SerializableValue>>();

    string_map.serialize(serializer)
}

fn deserialize_map<'de, D: Deserializer<'de>>(
    deserializer: D,
) -> Result<BTreeMap<MapKey, SerializableValue>, D::Error> {
    // might have performance issue here if the json object is to big
    let string_map = BTreeMap::<String, SerializableValue>::deserialize(deserializer)?;
    let mut value_map = BTreeMap::new();
    for (k, v) in string_map {
        value_map.insert(MapKey::Str(k), v);
    }
    Ok(value_map)
}
