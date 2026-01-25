use std::{
    fmt,
    hash::{DefaultHasher, Hash, Hasher},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(u64);

impl Id {
    pub fn new(name: &str) -> Self {
        let mut hasher = DefaultHasher::new();
        name.hash(&mut hasher);
        let full_hash = hasher.finish();

        Id(full_hash)
    }
}

impl fmt::Debug for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("Id({})", self.0))
    }
}
