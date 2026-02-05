use serde::{Deserialize, Serialize};

use crate::interpret::{
    error::Error,
    values::{number::Number, DisplayWriter, Value},
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
        env: &crate::interpret::Environment,
        w: &mut dyn std::io::Write,
    ) -> Result<(), Error> {
        let convert = |e| Error::WriteValueFailed(Value::Scalar(self), e);
        match self {
            Self::Nil => write!(w, "nil").map_err(convert),
            Self::Number(v) => v.write_display(env, w),
            Self::Bool(v) => write!(w, "{}", v).map_err(convert),
        }
    }
}
