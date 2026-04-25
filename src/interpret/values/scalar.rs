use serde::{Deserialize, Serialize};

use crate::identifier_registry::IdentifierRegistry;
use crate::interpret::{
    Environment,
    error::InterpretError,
    values::{DisplayWriter, Value, number::Number},
};

#[derive(
    derive_more::Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize,
)]
#[serde(untagged)]
pub enum Scalar {
    Nil,

    #[debug("Number({:?})", _0)]
    Number(Number),

    #[debug("Bool({})", _0)]
    Bool(bool),
}

impl DisplayWriter for Scalar {
    fn write_display(
        self,
        env: &Environment,
        ir: &IdentifierRegistry,
        w: &mut dyn std::io::Write,
    ) -> Result<(), InterpretError> {
        let convert = |e| InterpretError::WriteValueFailed(Value::Scalar(self), e);
        match self {
            Self::Nil => write!(w, "nil").map_err(convert),
            Self::Number(v) => v.write_display(env, ir, w),
            Self::Bool(v) => write!(w, "{}", v).map_err(convert),
        }
    }
}
