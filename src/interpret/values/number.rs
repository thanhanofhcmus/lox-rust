use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    ops::{Add, Div, Mul, Rem, Sub},
};

use crate::interpret::{
    error::Error,
    values::{display_writer::DisplayWriter, Value},
};

#[derive(derive_more::Debug, derive_more::Display, Clone, Copy, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Number {
    #[display("{}", _0)]
    #[debug("Integer({})", _0)]
    Integer(i64),

    #[display("{}", _0)]
    #[debug("Floating({})", _0)]
    Floating(f64),
}

impl Number {
    pub fn to_f64(self) -> f64 {
        match self {
            Self::Integer(v) => v as f64,
            Self::Floating(v) => v,
        }
    }

    pub fn is_zero(self) -> bool {
        match self {
            Number::Integer(v) => v == 0,
            Number::Floating(v) => v == 0.0,
        }
    }
}

macro_rules! impl_binary_op {
    ($trait:ident, $method:ident) => {
        impl $trait for Number {
            type Output = Self;
            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Self::Integer(l), Self::Integer(r)) => Self::Integer(l.$method(r)),
                    (l, r) => Self::Floating(l.to_f64().$method(r.to_f64())),
                }
            }
        }
    };
}

impl_binary_op!(Add, add);
impl_binary_op!(Sub, sub);
impl_binary_op!(Mul, mul);
impl_binary_op!(Rem, rem);

impl Div for Number {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(l), Self::Integer(r)) => {
                if l % r == 0 {
                    Self::Integer(l / r)
                } else {
                    Self::Floating(l as f64 / r as f64)
                }
            }
            // Mixed types or float types always result in Floating
            (l, r) => Self::Floating(l.to_f64() / r.to_f64()),
        }
    }
}

impl TryInto<usize> for Number {
    type Error = Error;

    fn try_into(self) -> Result<usize, Self::Error> {
        let err = Error::ValueMustBeUsize(Value::make_number(self));
        match self {
            Self::Integer(i) if i >= 0 => usize::try_from(i).map_err(|_| err),
            Self::Floating(v) if v >= 0.0 && v.fract() == 0.0 && v <= usize::MAX as f64 => {
                Ok(v as usize)
            }
            _ => Err(err),
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        use Number::*;
        const NUMBER_DELTA: f64 = 1e-10;

        match (*self, *other) {
            (Integer(l), Integer(r)) => l == r,
            (Floating(l), Integer(r)) => (l - (r as f64)).abs() < NUMBER_DELTA,
            (Integer(l), Floating(r)) => ((l as f64) - r).abs() < NUMBER_DELTA,
            (Floating(l), Floating(r)) => (l - r).abs() < NUMBER_DELTA,
        }
    }
}

impl Eq for Number {}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Number {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use Number::*;
        // For f64 comparison, ignoring NaN for simplicity
        match (*self, *other) {
            (Integer(l), Integer(r)) => l.cmp(&r),
            (Floating(l), Integer(r)) => l.partial_cmp(&(r as f64)).unwrap_or(Ordering::Less),
            (Integer(l), Floating(r)) => (l as f64).partial_cmp(&r).unwrap_or(Ordering::Less),
            (Floating(l), Floating(r)) => l.partial_cmp(&r).unwrap_or(Ordering::Less),
        }
    }
}

impl std::ops::Neg for Number {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Integer(v) => Self::Integer(-v),
            Self::Floating(v) => Self::Floating(-v),
        }
    }
}

impl DisplayWriter for Number {
    fn write_display(
        self,
        _: &crate::interpret::Environment,
        w: &mut dyn std::io::Write,
    ) -> Result<(), Error> {
        let result = match self {
            Self::Integer(v) => write!(w, "{}", v),
            Self::Floating(v) => write!(w, "{}", v),
        };
        result.map_err(|e| Error::WriteValueFailed(Value::make_number(self), e))?;
        Ok(())
    }
}
