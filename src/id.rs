use std::hash::{DefaultHasher, Hash, Hasher};

use strum_macros::FromRepr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, FromRepr)]
#[repr(u8)]
pub enum Category {
    Module = 1,
    Value = 2,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(u64);

impl Id {
    const MASK_60_BITS: u64 = 0x0FFF_FFFF_FFFF_FFFF;

    pub fn new(cateory: Category, name: &str) -> Self {
        let mut hasher = DefaultHasher::new();
        name.hash(&mut hasher);
        let full_hash = hasher.finish();

        let id = ((cateory as u64) << 60 | (full_hash & Self::MASK_60_BITS));

        Id(id)
    }

    pub fn cateory(&self) -> Option<Category> {
        let raw = (self.0 >> 60) as u8;
        Category::from_repr(raw)
    }
}
