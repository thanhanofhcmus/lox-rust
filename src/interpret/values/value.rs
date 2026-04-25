use std::collections::{BTreeMap, HashMap};

use crate::{
    ast::{BlockNode, ChainStep, FnParamNode},
    id::Id,
    identifier_registry::IdentifierRegistry,
    interpret::{
        Environment,
        error::InterpretError,
        heap::{GcHandle, StrId},
        interpreter::BorrowContext,
        values::{display_writer::DisplayWriter, number::Number, scalar::Scalar},
    },
    types::TypeId,
};

pub type BuiltinFn = fn(&mut BorrowContext, Vec<Value>) -> Result<Value, InterpretError>;

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<FnParamNode>,
    pub body: BlockNode<TypeId>,
}

pub type Array = Vec<Value>;

pub type Map = BTreeMap<MapKey, Value>;

#[derive(Debug, Clone)]
pub struct Tuple {
    pub members: Vec<Value>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub value: Value,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub id: Id,
    pub fields: HashMap<Id, StructField>,
}

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

    #[debug("Tuple({:?})", _0)]
    Tuple(GcHandle),

    #[debug("Struct({:?})", _0)]
    Struct(GcHandle),

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
            Value::Array(handle)
            | Value::Map(handle)
            | Value::Function(handle)
            | Value::Tuple(handle)
            | Value::Struct(handle) => Some(handle),
            Value::Unit | Value::Scalar(_) | Value::Str(_) | Value::BuiltinFunction(_) => None,
        }
    }

    pub fn replace_handle(&mut self, new_handle: GcHandle) -> Option<GcHandle> {
        match self {
            Value::Array(handle)
            | Value::Map(handle)
            | Value::Function(handle)
            | Value::Tuple(handle)
            | Value::Struct(handle) => Some(std::mem::replace(handle, new_handle)),
            Value::Unit | Value::Scalar(_) | Value::Str(_) | Value::BuiltinFunction(_) => None,
        }
    }

    pub fn deep_eq(&self, other: &Self, env: &Environment) -> Result<bool, InterpretError> {
        use Value::*;
        match (self, other) {
            (Unit, _) | (_, Unit) => Err(InterpretError::UseUnitValue),

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

            (Tuple(l_handle), Tuple(r_handle)) => {
                if l_handle == r_handle {
                    return Ok(true);
                }
                let l_tuple = env.get_tuple(*l_handle)?;
                let r_tuple = env.get_tuple(*r_handle)?;
                if l_tuple.members.len() != r_tuple.members.len() {
                    return Ok(false);
                }
                for (l_value, r_value) in l_tuple.members.iter().zip(&r_tuple.members) {
                    if !(l_value.deep_eq(r_value, env)?) {
                        return Ok(false);
                    }
                }
                Ok(true)
            }

            (Struct(l_handle), Struct(r_handle)) => {
                if l_handle == r_handle {
                    return Ok(true);
                }
                let l_struct = env.get_struct(*l_handle)?;
                let r_struct = env.get_struct(*r_handle)?;
                // nominal typing: distinct struct types are never equal
                if l_struct.id != r_struct.id {
                    return Ok(false);
                }
                if l_struct.fields.len() != r_struct.fields.len() {
                    return Ok(false);
                }
                for (field_id, l_field) in l_struct.fields.iter() {
                    let Some(r_field) = r_struct.fields.get(field_id) else {
                        return Ok(false);
                    };
                    if !(l_field.value.deep_eq(&r_field.value, env)?) {
                        return Ok(false);
                    }
                }
                Ok(true)
            }

            _ => Ok(false),
        }
    }

    pub fn get_by_chain_step(
        &self,
        step: ChainStep<Value>,
        env: &Environment,
    ) -> Result<Value, InterpretError> {
        let Some(handle) = self.get_handle() else {
            return Err(match step {
                ChainStep::Member(member) => {
                    InterpretError::MemberAccessOnInvalidType(*self, member)
                }
                ChainStep::Subscription(_) => InterpretError::ValueUnIndexable(*self),
            });
        };
        env.heap
            .get_object(handle)
            .ok_or(InterpretError::GcObjectNotFound(handle))?
            .get_by_chain_step(step)
    }

    pub fn to_index(self) -> Result<usize, InterpretError> {
        match self.get_number() {
            Some(v) => v.try_into(),
            None => Err(InterpretError::ValueMustBeUsize(self)),
        }
    }
}

impl DisplayWriter for Value {
    fn write_display(
        self,
        env: &Environment,
        ir: &IdentifierRegistry,
        w: &mut dyn std::io::Write,
    ) -> Result<(), InterpretError> {
        let convert = |e| InterpretError::WriteValueFailed(self, e);

        match self {
            Value::Scalar(v) => v.write_display(env, ir, w),
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
                        value.write_display(env, ir, w)?;
                        w.write_all(b", ").map_err(convert)?;
                    }
                    last.write_display(env, ir, w)?;
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
                        first_key.write_display(env, ir, w)?;
                        w.write_all(b"\"").map_err(convert)?;
                    } else {
                        // normal type, normal write
                        first_key.write_display(env, ir, w)?;
                    }
                    write!(w, " => ").map_err(convert)?;
                    first_value.write_display(env, ir, w)?;

                    for (k, v) in entries {
                        write!(w, ", ").map_err(convert)?;
                        if let MapKey::Str(_) = k {
                            w.write_all(b"\"").map_err(convert)?;
                            k.write_display(env, ir, w)?;
                            w.write_all(b"\"").map_err(convert)?;
                        } else {
                            // normal type, normal write
                            k.write_display(env, ir, w)?;
                        }
                        write!(w, " => ").map_err(convert)?;
                        v.write_display(env, ir, w)?;
                    }
                }
                w.write_all(b"}").map_err(convert)?;

                Ok(())
            }
            Value::Struct(handle) => {
                let struct_ = env.get_struct(handle)?;
                write!(w, "{}{{", ir.get_or_unknown(struct_.id)).map_err(convert)?;
                let mut entries = struct_.fields.iter();
                if let Some((field_id, field_value)) = entries.next() {
                    let field_name = ir.get_or_unknown(*field_id);
                    write!(w, "{} = ", field_name).map_err(convert)?;
                    field_value.value.write_display(env, ir, w)?;
                    for (field_id, field_value) in entries {
                        let field_name = ir.get_or_unknown(*field_id);
                        write!(w, ", {} = ", field_name).map_err(convert)?;
                        field_value.value.write_display(env, ir, w)?;
                    }
                }
                write!(w, "}}").map_err(convert)?;
                Ok(())
            }
            Value::Tuple(handle) => {
                let tuple = env.get_tuple(handle)?;
                w.write_all(b"%(").map_err(convert)?;

                if let Some((last, rest)) = tuple.members.split_last() {
                    for value in rest {
                        value.write_display(env, ir, w)?;
                        w.write_all(b", ").map_err(convert)?;
                    }
                    last.write_display(env, ir, w)?;
                }

                w.write_all(b")").map_err(convert)?;

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
    pub fn convert_from_value(value: Value) -> Result<Self, InterpretError> {
        match value {
            Value::Scalar(v) => Ok(Self::Scalar(v)),
            Value::Str(id) => Ok(MapKey::Str(id)),
            _ => Err(InterpretError::ValueCannotBeUsedAsKey(value)),
        }
    }

    pub fn to_value(self) -> Value {
        match self {
            Self::Scalar(v) => Value::Scalar(v),
            Self::Str(str_id) => Value::Str(str_id),
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
    fn write_display(
        self,
        env: &Environment,
        ir: &IdentifierRegistry,
        w: &mut dyn std::io::Write,
    ) -> Result<(), InterpretError> {
        match self {
            MapKey::Scalar(v) => v.write_display(env, ir, w),
            MapKey::Str(str_id) => {
                let s = env.get_string(str_id)?;
                w.write_all(s.as_bytes())
                    .map_err(|e| InterpretError::WriteValueFailed(Value::Str(str_id), e))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
        assert_eq!(Value::make_number(Number::Integer(0)).get_bool(), None);
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
        assert!(
            Value::make_number(Number::Integer(0))
                .get_handle()
                .is_none()
        );
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
        assert!(
            Value::make_number(Number::Floating(2.5))
                .to_index()
                .is_err()
        );
    }

    #[test]
    fn to_index_integer_valued_float_ok() {
        assert_eq!(
            Value::make_number(Number::Floating(7.0))
                .to_index()
                .unwrap(),
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
        use crate::interpret::heap::HeapStringInterner;
        let mut interner = HeapStringInterner::new();
        let id: StrId = interner.intern("hello");
        let k = MapKey::convert_from_value(Value::Str(id)).unwrap();
        assert_eq!(k.get_str_id(), Some(id));
    }

    #[test]
    fn mapkey_from_builtin_function_errors() {
        fn dummy_builtin(_: &mut BorrowContext, _: Vec<Value>) -> Result<Value, InterpretError> {
            Ok(Value::Unit)
        }
        let v = Value::BuiltinFunction(dummy_builtin);
        let err = MapKey::convert_from_value(v).unwrap_err();
        assert!(matches!(err, InterpretError::ValueCannotBeUsedAsKey(_)));
    }
}
