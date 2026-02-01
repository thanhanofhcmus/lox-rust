use serde::{Deserialize, Serialize};
use std::cmp::Ordering;

use crate::interpret::{
    error::Error,
    values::{
        display_writer::DisplayWriter,
        value_kind::{GetValueKind, ValueKind},
        Value,
    },
};

#[derive(derive_more::Debug, derive_more::Display, Clone, Copy, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Scalar {
    #[display("nil")]
    Nil,

    #[display("{}", _0)]
    #[debug("Integer({})", _0)]
    Integer(i64),

    #[display("{}", _0)]
    #[debug("Floating({})", _0)]
    Floating(f64),

    #[display("{}", _0)]
    #[debug("Bool({})", _0)]
    Bool(bool),
}

impl GetValueKind for Scalar {
    fn get_kind(&self) -> ValueKind {
        match self {
            Scalar::Nil => ValueKind::Nil,
            Scalar::Integer(_) => ValueKind::Integer,
            Scalar::Floating(_) => ValueKind::Floating,
            Scalar::Bool(_) => ValueKind::Bool,
        }
    }
}

impl DisplayWriter for Scalar {
    fn write_display(
        self,
        _: &crate::interpret::Environment,
        w: &mut dyn std::io::Write,
    ) -> Result<(), Error> {
        let result = match self {
            Scalar::Nil => write!(w, "nil"),
            Scalar::Integer(v) => write!(w, "{}", v),
            Scalar::Floating(v) => write!(w, "{}", v),
            Scalar::Bool(v) => write!(w, "{}", v),
        };

        result.map_err(|e| Error::WriteValueFailed(Value::Scalar(self), e))?;

        Ok(())
    }
}

impl PartialEq for Scalar {
    fn eq(&self, other: &Self) -> bool {
        use Scalar::*;
        const NUMBER_DELTA: f64 = 1e-10;

        match (self, other) {
            (Nil, Nil) => true,
            (Bool(l), Bool(r)) => l == r,

            (Integer(l), Integer(r)) => l == r,
            (Floating(l), Integer(r)) => (l - (*r as f64)).abs() < NUMBER_DELTA,
            (Integer(l), Floating(r)) => ((*l as f64) - r).abs() < NUMBER_DELTA,
            (Floating(l), Floating(r)) => (*l - *r).abs() < NUMBER_DELTA,
            _ => false,
        }
    }
}

impl Eq for Scalar {}

impl PartialOrd for Scalar {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Scalar {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use Scalar::*;

        let rank_cmp = self.get_kind().cmp(&other.get_kind());
        if rank_cmp != Ordering::Equal {
            return rank_cmp;
        }

        match (self, other) {
            (Nil, Nil) => Ordering::Equal,
            (Bool(a), Bool(b)) => a.cmp(b),
            (Integer(a), Integer(b)) => a.cmp(b),
            (Floating(a), Floating(b)) => {
                // Handle f64 comparison (ignoring NaN for simplicity, or treat NaN as the smallest number)
                a.partial_cmp(b).unwrap_or(Ordering::Less)
            }
            _ => Ordering::Equal,
        }
    }
}
