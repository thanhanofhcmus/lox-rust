use serde::{Deserialize, Serialize};

use crate::interpret::{
    error::Error,
    values::{
        number::Number,
        value_kind::{GetValueKind, ValueKind},
        DisplayWriter, Value,
    },
};

#[derive(
    derive_more::Debug,
    derive_more::Display,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Serialize,
    Deserialize,
)]
#[serde(untagged)]
pub enum Scalar {
    #[display("nil")]
    Nil,

    #[display("{}", _0)]
    #[debug("Number({})", _0)]
    Number(Number),

    #[display("{}", _0)]
    #[debug("Bool({})", _0)]
    Bool(bool),
}

impl GetValueKind for Scalar {
    fn get_kind(&self) -> ValueKind {
        match self {
            Scalar::Nil => ValueKind::Nil,
            Scalar::Number(v) => v.get_kind(),
            Scalar::Bool(_) => ValueKind::Bool,
        }
    }
}

impl DisplayWriter for Scalar {
    fn write_display(
        self,
        env: &crate::interpret::Environment,
        w: &mut dyn std::io::Write,
    ) -> Result<(), Error> {
        let convert = |e| Error::WriteValueFailed(Value::Scalar(self), e);
        match self {
            Scalar::Nil => write!(w, "nil").map_err(convert),
            Scalar::Number(v) => v.write_display(env, w),
            Scalar::Bool(v) => write!(w, "{}", v).map_err(convert),
        }
    }
}
