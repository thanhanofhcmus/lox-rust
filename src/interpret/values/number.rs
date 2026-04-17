use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    ops::{Add, Div, Mul, Rem, Sub},
};

use crate::interpret::{
    error::Error,
    values::{Value, display_writer::DisplayWriter},
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

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    use Number::*;

    // ---------- to_f64 / is_zero ----------

    #[test]
    fn to_f64_integer() {
        assert_eq!(Integer(5).to_f64(), 5.0);
    }

    #[test]
    fn to_f64_floating() {
        assert_eq!(Floating(5.5).to_f64(), 5.5);
    }

    #[test]
    fn is_zero_integer() {
        assert!(Integer(0).is_zero());
        assert!(!Integer(1).is_zero());
    }

    #[test]
    fn is_zero_floating() {
        assert!(Floating(0.0).is_zero());
        assert!(!Floating(0.1).is_zero());
    }

    // ---------- arithmetic ----------

    #[test]
    fn add_int_int_stays_int() {
        assert_eq!(Integer(2) + Integer(3), Integer(5));
    }

    #[test]
    fn add_mixed_promotes_to_floating() {
        assert_eq!(Integer(2) + Floating(0.5), Floating(2.5));
        assert_eq!(Floating(0.5) + Integer(2), Floating(2.5));
    }

    #[test]
    fn sub_int_int_stays_int() {
        assert_eq!(Integer(10) - Integer(3), Integer(7));
    }

    #[test]
    fn mul_mixed_floats() {
        assert_eq!(Integer(3) * Floating(2.0), Floating(6.0));
    }

    #[test]
    fn rem_int_int_stays_int() {
        assert_eq!(Integer(10) % Integer(3), Integer(1));
    }

    #[test]
    fn div_int_int_exact_stays_int() {
        assert_eq!(Integer(6) / Integer(2), Integer(3));
        assert_eq!(Integer(-8) / Integer(4), Integer(-2));
    }

    #[test]
    fn div_int_int_inexact_promotes_to_floating() {
        assert_eq!(Integer(5) / Integer(2), Floating(2.5));
    }

    #[test]
    fn div_mixed_is_floating() {
        assert_eq!(Integer(5) / Floating(2.0), Floating(2.5));
        assert_eq!(Floating(5.0) / Integer(2), Floating(2.5));
    }

    #[test]
    fn neg_integer() {
        assert_eq!(-Integer(5), Integer(-5));
        assert_eq!(-Integer(0), Integer(0));
    }

    #[test]
    fn neg_floating() {
        assert_eq!(-Floating(1.5), Floating(-1.5));
    }

    // ---------- equality with delta tolerance ----------

    #[test]
    fn eq_same_type_exact() {
        assert_eq!(Integer(5), Integer(5));
        assert_eq!(Floating(1.5), Floating(1.5));
    }

    #[test]
    fn eq_floating_within_delta_is_equal() {
        // 1e-10 delta tolerance
        assert_eq!(Floating(1.0), Floating(1.0 + 1e-11));
    }

    #[test]
    fn eq_floating_outside_delta_is_not_equal() {
        assert_ne!(Floating(1.0), Floating(1.0 + 1e-9));
    }

    #[test]
    fn eq_cross_type_same_value() {
        assert_eq!(Integer(5), Floating(5.0));
        assert_eq!(Floating(5.0), Integer(5));
    }

    // ---------- ordering across types ----------

    #[test]
    fn cmp_integers() {
        assert!(Integer(3) < Integer(5));
        assert!(Integer(10) > Integer(5));
    }

    #[test]
    fn cmp_cross_type() {
        assert!(Integer(5) < Floating(5.5));
        assert!(Floating(5.5) > Integer(5));
    }

    // ---------- TryInto<usize> ----------

    #[test]
    fn try_into_usize_integer_non_negative() {
        let r: Result<usize, _> = Integer(42).try_into();
        assert_eq!(r.unwrap(), 42);
    }

    #[test]
    fn try_into_usize_integer_zero() {
        let r: Result<usize, _> = Integer(0).try_into();
        assert_eq!(r.unwrap(), 0);
    }

    #[test]
    fn try_into_usize_integer_negative_errors() {
        let r: Result<usize, _> = Integer(-1).try_into();
        assert!(r.is_err());
    }

    #[test]
    fn try_into_usize_floating_integer_valued_ok() {
        let r: Result<usize, _> = Floating(7.0).try_into();
        assert_eq!(r.unwrap(), 7);
    }

    #[test]
    fn try_into_usize_floating_non_integer_errors() {
        let r: Result<usize, _> = Floating(2.5).try_into();
        assert!(r.is_err());
    }

    #[test]
    fn try_into_usize_floating_negative_errors() {
        let r: Result<usize, _> = Floating(-1.0).try_into();
        assert!(r.is_err());
    }

    #[test]
    fn try_into_usize_floating_nan_errors() {
        let r: Result<usize, _> = Floating(f64::NAN).try_into();
        assert!(r.is_err());
    }
}
