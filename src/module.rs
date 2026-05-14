use crate::{id::Id, string_interner::StringInterner};
use std::collections::HashMap;

crate::define_type_index!(pub struct StrId);
pub type ModuleStringInterner = StringInterner<StrId>;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ModuleMetadata {
    pub path: StrId,
    pub package: Id,
}

#[derive(Debug, Default)]
pub struct ModuleRegistry<T> {
    map: HashMap<ModuleMetadata, T>,
}

impl<T> ModuleRegistry<T> {
    pub fn insert(&mut self, metadata: ModuleMetadata, module: T) {
        self.map.insert(metadata, module);
    }

    pub fn get(&self, metadata: &ModuleMetadata) -> Option<&T> {
        self.map.get(metadata)
    }
}
