use std::hash::{DefaultHasher, Hash, Hasher};

#[derive(derive_more::Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[debug("Id({_0:020})")]
pub struct Id(u64);

impl Id {
    pub fn new(name: &str) -> Self {
        let mut hasher = DefaultHasher::new();
        name.hash(&mut hasher);
        let full_hash = hasher.finish();

        Id(full_hash)
    }
}
