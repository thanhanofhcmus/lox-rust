use std::{cmp::Ordering, collections::BTreeMap};

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::interpret::{
    error::Error,
    values::{
        scalar::Scalar,
        value::{MapKey, Value},
    },
    Environment,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SerialMapKey {
    Scalar(Scalar),
    Str(String),
}

impl SerialMapKey {
    pub fn convert_from_map_key(value: &MapKey, env: &Environment) -> Result<Self, Error> {
        let v = match value {
            MapKey::Scalar(v) => Self::Scalar(*v),
            MapKey::Str(handle) => {
                let s = env.get_string(*handle)?;
                Self::Str(s.to_string())
            }
        };
        Ok(v)
    }

    pub fn hydrate_map_key(self, env: &mut Environment) -> Result<MapKey, Error> {
        let v = match self {
            SerialMapKey::Scalar(v) => MapKey::Scalar(v),
            SerialMapKey::Str(v) => MapKey::Str(env.insert_string_id(v)),
        };
        Ok(v)
    }
}

impl PartialEq for SerialMapKey {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (SerialMapKey::Str(l), SerialMapKey::Str(r)) => l == r,
            (SerialMapKey::Scalar(l), SerialMapKey::Scalar(r)) => l == r,
            _ => false,
        }
    }
}

impl Eq for SerialMapKey {}

impl PartialOrd for SerialMapKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SerialMapKey {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use SerialMapKey::*;

        match (self, other) {
            (Scalar(a), Scalar(b)) => a.cmp(b),
            (Str(a), Str(b)) => a.cmp(b),
            _ => Ordering::Equal,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SerialValue {
    Scalar(Scalar),

    Str(String),

    Array(Vec<SerialValue>),

    #[serde(serialize_with = "serialize_map", deserialize_with = "deserialize_map")]
    Map(BTreeMap<SerialMapKey, SerialValue>),
}

impl SerialValue {
    pub fn convert_from_value(value: Value, env: &Environment) -> Result<Self, Error> {
        let v = match value {
            Value::Scalar(v) => Self::Scalar(v),

            Value::Str(handle) => {
                let s = env.get_string(handle)?;
                SerialValue::Str(s.to_string())
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
                let mut vsm = BTreeMap::<SerialMapKey, SerialValue>::new();
                for (k, v) in map {
                    let k_sv = SerialMapKey::convert_from_map_key(k, env)?;
                    let v_sv = SerialValue::convert_from_value(*v, env)?;
                    vsm.insert(k_sv, v_sv);
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
            SerialValue::Scalar(v) => Value::Scalar(v),

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
                    let k_v = k.hydrate_map_key(env)?;
                    let v_v = v.hydrate(env)?;
                    map.insert(k_v, v_v);
                }
                env.insert_map_variable(map)
            }
        };
        Ok(v)
    }
}

fn serialize_map<S: Serializer>(
    m: &BTreeMap<SerialMapKey, SerialValue>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    let string_map = m
        .iter()
        .filter_map(|(k, v)| {
            match k {
                SerialMapKey::Str(s) => Some((s, v)),
                // We can allow other type to be key with to_string here
                _ => None,
            }
        })
        .collect::<BTreeMap<&String, &SerialValue>>();

    string_map.serialize(serializer)
}

fn deserialize_map<'de, D: Deserializer<'de>>(
    deserializer: D,
) -> Result<BTreeMap<SerialMapKey, SerialValue>, D::Error> {
    // might have performance issue here if the json object is to big
    let string_map = BTreeMap::<String, SerialValue>::deserialize(deserializer)?;
    let mut value_map = BTreeMap::new();
    for (k, v) in string_map {
        value_map.insert(SerialMapKey::Str(k), v);
    }
    Ok(value_map)
}
