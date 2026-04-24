use std::collections::BTreeMap;

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::identifier_registry::IdentifierRegistry;
use crate::interpret::{
    Environment,
    error::InterpretError,
    values::{
        scalar::Scalar,
        value::{MapKey, Value},
    },
};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(untagged)]
pub enum SerialMapKey {
    Scalar(Scalar),
    Str(String),
}

impl SerialMapKey {
    pub fn convert_from_map_key(value: &MapKey, env: &Environment) -> Result<Self, InterpretError> {
        let v = match value {
            MapKey::Scalar(v) => Self::Scalar(*v),
            MapKey::Str(str_id) => {
                let s = env.get_string(*str_id)?;
                Self::Str(s.to_string())
            }
        };
        Ok(v)
    }

    pub fn hydrate_map_key(self, env: &mut Environment) -> Result<MapKey, InterpretError> {
        let v = match self {
            Self::Scalar(v) => MapKey::Scalar(v),
            Self::Str(v) => MapKey::Str(env.insert_string_id(v)),
        };
        Ok(v)
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
    pub fn convert_from_value(
        value: Value,
        env: &Environment,
        sb: &IdentifierRegistry,
    ) -> Result<Self, InterpretError> {
        let v = match value {
            Value::Scalar(v) => Self::Scalar(v),

            Value::Str(handle) => {
                let s = env.get_string(handle)?;
                Self::Str(s.to_string())
            }

            Value::Array(handle) => {
                let arr = env.get_array(handle)?;
                let vs = arr
                    .iter()
                    .map(|v| Self::convert_from_value(*v, env, sb))
                    .collect::<Result<_, _>>()?;
                Self::Array(vs)
            }

            Value::Map(handle) => {
                let map = env.get_map(handle)?;
                let mut vsm = BTreeMap::new();
                for (k, v) in map {
                    let k_sv = SerialMapKey::convert_from_map_key(k, env)?;
                    let v_sv = SerialValue::convert_from_value(*v, env, sb)?;
                    vsm.insert(k_sv, v_sv);
                }
                Self::Map(vsm)
            }

            // Structs serialize as JSON objects. Field iteration order follows
            // the underlying HashMap; BTreeMap then sorts keys alphabetically
            // on output, giving a stable JSON shape even if insertion order is
            // not.
            Value::Struct(handle) => {
                let struct_ = env.get_struct(handle)?;
                let mut vsm = BTreeMap::new();
                for (field_id, field) in &struct_.fields {
                    let name = sb.get_or_unknown(*field_id).to_string();
                    let v_sv = SerialValue::convert_from_value(field.value, env, sb)?;
                    vsm.insert(SerialMapKey::Str(name), v_sv);
                }
                Self::Map(vsm)
            }

            // Tuples serialize as JSON arrays, preserving member order.
            Value::Tuple(handle) => {
                let tuple = env.get_tuple(handle)?;
                let vs = tuple
                    .members
                    .iter()
                    .map(|v| Self::convert_from_value(*v, env, sb))
                    .collect::<Result<_, _>>()?;
                Self::Array(vs)
            }

            Value::Unit | Value::Function(_) | Value::BuiltinFunction(_) => {
                return Err(InterpretError::TypeIsNotSerializable(value));
            }
        };
        Ok(v)
    }

    pub fn hydrate(self, env: &mut Environment) -> Result<Value, InterpretError> {
        let v = match self {
            Self::Scalar(v) => Value::Scalar(v),

            Self::Str(v) => env.insert_string_variable(v),

            Self::Array(values) => {
                let arr = values
                    .into_iter()
                    .map(|v| v.hydrate(env))
                    .collect::<Result<_, _>>()?;
                env.insert_array_variable(arr)
            }

            Self::Map(m) => {
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
