use std::{
    hash::{Hash, Hasher},
    marker::PhantomData,
};

#[derive(derive_more::Debug)]
#[debug("{}({})", std::any::type_name::<Marker>(), _0)]
pub struct TypeIndex<Raw, Marker>(Raw, PhantomData<Marker>);

impl<Raw: Clone, Marker> Clone for TypeIndex<Raw, Marker> {
    fn clone(&self) -> Self {
        Self::new(self.0.clone())
    }
}

impl<Raw: Copy, Marker> Copy for TypeIndex<Raw, Marker> {}

impl<Raw: PartialOrd, Marker> PartialOrd for TypeIndex<Raw, Marker> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<Raw: Ord, Marker> Ord for TypeIndex<Raw, Marker> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<Raw: PartialEq, Marker> PartialEq for TypeIndex<Raw, Marker> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<Raw: Eq, Marker> Eq for TypeIndex<Raw, Marker> {}

impl<Raw: Hash, Marker> Hash for TypeIndex<Raw, Marker> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<Raw: Default, Marker> Default for TypeIndex<Raw, Marker> {
    fn default() -> Self {
        Self(Raw::default(), PhantomData)
    }
}

impl<Raw, Marker> TypeIndex<Raw, Marker> {
    pub fn new(value: Raw) -> Self {
        Self(value, PhantomData)
    }
}

impl<Raw: Copy, Marker> TypeIndex<Raw, Marker> {
    pub fn get(&self) -> Raw {
        self.0
    }
}
