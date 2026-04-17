use std::collections::BTreeMap;

use crate::{
    ast::{FnParamNode, TypedBlockNode},
    interpret::{
        Environment, Interpreter,
        error::Error,
        heap::GcHandle,
        string_interner::StrId,
        values::{display_writer::DisplayWriter, number::Number, scalar::Scalar},
    },
};

pub type BuiltinFn = fn(&mut Interpreter, Vec<Value>) -> Result<Value, Error>;

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<FnParamNode>,
    pub body: TypedBlockNode,
}

pub type Array = Vec<Value>;

pub type Map = BTreeMap<MapKey, Value>;

#[derive(derive_more::Debug, Clone, Copy)]
pub enum Value {
    Unit,

    #[debug("Scalar({:?})", _0)]
    Scalar(Scalar),

    // TODO: Error crate needs display method and is using this format
    // we need to make error print the actual value or maybe keep printing String like this
    #[debug("String({:?})", _0)]
    Str(StrId),

    #[debug("Array({:?})", _0)]
    Array(GcHandle),

    #[debug("Map({:?})", _0)]
    Map(GcHandle),

    #[debug("Function({:?})", _0)]
    Function(GcHandle),

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

    pub fn get_number(self) -> Option<Number> {
        match self {
            Value::Scalar(Scalar::Number(v)) => Some(v),
            _ => None,
        }
    }

    pub fn get_str_id(self) -> Option<StrId> {
        match self {
            Value::Str(str_id) => Some(str_id),
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
            // TODO: Unit should not be comparable, make this a "friendly" error
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
        // all the 2 types array and map that have the index method also have to get the value through a handle
        let Some(handle) = self.get_handle() else {
            return Err(Error::ValueUnIndexable(*self));
        };
        env.heap
            .get_object(handle)
            .ok_or(Error::GcObjectNotFound(handle))?
            .get_at_index(indexee)
    }

    pub fn to_index(self) -> Result<usize, Error> {
        match self.get_number() {
            Some(v) => v.try_into(),
            None => Err(Error::ValueMustBeUsize(self)),
        }
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
                    // for type string, we want to add quotes ""
                    if let MapKey::Str(_) = first_key {
                        w.write_all(b"\"").map_err(convert)?;
                        first_key.write_display(env, w)?;
                        w.write_all(b"\"").map_err(convert)?;
                    } else {
                        // normal type, normal write
                        first_key.write_display(env, w)?;
                    }
                    write!(w, " => ").map_err(convert)?;
                    first_value.write_display(env, w)?;

                    for (k, v) in entries {
                        write!(w, ", ").map_err(convert)?;
                        if let MapKey::Str(_) = k {
                            w.write_all(b"\"").map_err(convert)?;
                            k.write_display(env, w)?;
                            w.write_all(b"\"").map_err(convert)?;
                        } else {
                            // normal type, normal write
                            k.write_display(env, w)?;
                        }
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
            _ => Err(Error::ValueCannotBeUsedAsKey(value)),
        }
    }

    pub fn get_str_id(self) -> Option<StrId> {
        match self {
            Self::Str(str_id) => Some(str_id),
            _ => None,
        }
    }
}

impl DisplayWriter for MapKey {
    fn write_display(self, env: &Environment, w: &mut dyn std::io::Write) -> Result<(), Error> {
        match self {
            MapKey::Scalar(v) => v.write_display(env, w),
            MapKey::Str(str_id) => {
                let s = env.get_string(str_id)?;
                w.write_all(s.as_bytes())
                    .map_err(|e| Error::WriteValueFailed(Value::Str(str_id), e))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpret::string_interner::StrId;
    use crate::interpret::values::{number::Number, scalar::Scalar};
    use pretty_assertions::assert_eq;

    // ---------- constructors ----------

    #[test]
    fn make_nil_builds_scalar_nil() {
        assert!(matches!(Value::make_nil(), Value::Scalar(Scalar::Nil)));
    }

    #[test]
    fn make_bool_builds_scalar_bool() {
        assert!(matches!(
            Value::make_bool(true),
            Value::Scalar(Scalar::Bool(true))
        ));
        assert!(matches!(
            Value::make_bool(false),
            Value::Scalar(Scalar::Bool(false))
        ));
    }

    #[test]
    fn make_number_builds_scalar_number() {
        let v = Value::make_number(Number::Integer(42));
        assert!(matches!(
            v,
            Value::Scalar(Scalar::Number(Number::Integer(42)))
        ));
    }

    // ---------- get_bool ----------

    #[test]
    fn get_bool_on_bool_value_returns_some() {
        assert_eq!(Value::make_bool(true).get_bool(), Some(true));
        assert_eq!(Value::make_bool(false).get_bool(), Some(false));
    }

    #[test]
    fn get_bool_on_non_bool_returns_none() {
        assert_eq!(Value::make_nil().get_bool(), None);
        assert_eq!(
            Value::make_number(Number::Integer(0)).get_bool(),
            None
        );
    }

    // ---------- get_number ----------

    #[test]
    fn get_number_on_number_value_returns_some() {
        let n = Number::Integer(7);
        assert_eq!(Value::make_number(n).get_number(), Some(n));
    }

    #[test]
    fn get_number_on_non_number_returns_none() {
        assert_eq!(Value::make_bool(true).get_number(), None);
        assert_eq!(Value::make_nil().get_number(), None);
    }

    // ---------- get_str_id ----------

    #[test]
    fn get_str_id_on_non_str_returns_none() {
        assert_eq!(Value::make_nil().get_str_id(), None);
        assert_eq!(Value::make_bool(true).get_str_id(), None);
    }

    // ---------- get_handle ----------

    #[test]
    fn get_handle_on_non_handle_returns_none() {
        assert!(Value::make_nil().get_handle().is_none());
        assert!(Value::make_bool(true).get_handle().is_none());
        assert!(Value::make_number(Number::Integer(0))
            .get_handle()
            .is_none());
        // Unit and BuiltinFunction are also non-handle
        assert!(Value::Unit.get_handle().is_none());
    }

    // ---------- to_index ----------

    #[test]
    fn to_index_non_negative_integer_ok() {
        assert_eq!(
            Value::make_number(Number::Integer(5)).to_index().unwrap(),
            5
        );
    }

    #[test]
    fn to_index_negative_integer_errors() {
        assert!(Value::make_number(Number::Integer(-1)).to_index().is_err());
    }

    #[test]
    fn to_index_non_integer_float_errors() {
        assert!(Value::make_number(Number::Floating(2.5))
            .to_index()
            .is_err());
    }

    #[test]
    fn to_index_integer_valued_float_ok() {
        assert_eq!(
            Value::make_number(Number::Floating(7.0)).to_index().unwrap(),
            7
        );
    }

    #[test]
    fn to_index_non_number_value_errors() {
        assert!(Value::make_bool(true).to_index().is_err());
        assert!(Value::make_nil().to_index().is_err());
    }

    // ---------- MapKey::convert_from_value ----------

    #[test]
    fn mapkey_from_scalar_ok() {
        let k = MapKey::convert_from_value(Value::make_bool(true)).unwrap();
        assert!(matches!(k, MapKey::Scalar(Scalar::Bool(true))));
    }

    #[test]
    fn mapkey_from_nil_ok() {
        let k = MapKey::convert_from_value(Value::make_nil()).unwrap();
        assert!(matches!(k, MapKey::Scalar(Scalar::Nil)));
    }

    #[test]
    fn mapkey_from_str_ok() {
        // We can't construct a StrId without going through an interner; use the
        // default value that `mem::zeroed`-style construction would give via
        // serde/Copy semantics is not safe. Instead, construct through our own
        // interner — intentionally inlined to avoid a heap dependency.
        use crate::interpret::string_interner::StringInterner;
        let mut interner = StringInterner::new();
        let id: StrId = interner.intern("hello");
        let k = MapKey::convert_from_value(Value::Str(id)).unwrap();
        assert_eq!(k.get_str_id(), Some(id));
    }

    #[test]
    fn mapkey_from_builtin_function_errors() {
        fn dummy_builtin(
            _: &mut crate::interpret::Interpreter,
            _: Vec<Value>,
        ) -> Result<Value, Error> {
            Ok(Value::Unit)
        }
        let v = Value::BuiltinFunction(dummy_builtin);
        let err = MapKey::convert_from_value(v).unwrap_err();
        assert!(matches!(err, Error::ValueCannotBeUsedAsKey(_)));
    }
}
