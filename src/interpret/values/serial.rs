use std::collections::BTreeMap;

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::interpret::{
    Environment,
    error::Error,
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
    pub fn convert_from_value(value: Value, env: &Environment) -> Result<Self, Error> {
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
                    .map(|v| Self::convert_from_value(*v, env))
                    .collect::<Result<_, _>>()?;
                Self::Array(vs)
            }

            Value::Map(handle) => {
                let map = env.get_map(handle)?;
                let mut vsm = BTreeMap::new();
                for (k, v) in map {
                    let k_sv = SerialMapKey::convert_from_map_key(k, env)?;
                    let v_sv = SerialValue::convert_from_value(*v, env)?;
                    vsm.insert(k_sv, v_sv);
                }
                Self::Map(vsm)
            }

            Value::Unit | Value::Function(_) | Value::BuiltinFunction(_) => {
                return Err(Error::TypeIsNotSerializable(value));
            }
        };
        Ok(v)
    }

    pub fn hydrate(self, env: &mut Environment) -> Result<Value, Error> {
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
