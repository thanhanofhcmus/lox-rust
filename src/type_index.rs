use std::hash::Hash;

pub trait Index: Clone + Copy + PartialEq + Eq + PartialEq + Ord + Hash {
    fn from_value(value: usize) -> Self;
    fn to_value(&self) -> usize;
}

#[macro_export]
macro_rules! define_type_index {
    (
        $vis:vis struct $name:ident
    ) => {
        #[derive(
            Debug, Clone, Copy,
            PartialEq, Eq, PartialOrd, Ord,
            Hash, Default
        )]
        $vis struct $name(pub(self) usize);

        impl Index for $name {
            fn from_value(value: usize) -> Self { Self(value) }
            fn to_value(&self) -> usize { self.0 }
        }

    };
}
