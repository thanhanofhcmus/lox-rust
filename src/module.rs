use crate::id::Id;
use std::collections::HashMap;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ModuleMetadata {
    pub path: String,
    pub package: Id,
}

#[derive(Debug)]
pub struct ModuleRegistry<T> {
    map: HashMap<ModuleMetadata, T>,
}

impl<T> ModuleRegistry<T> {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    pub fn insert(&mut self, metadata: ModuleMetadata, module: T) {
        self.map.insert(metadata, module);
    }

    pub fn get(&self, metadata: &ModuleMetadata) -> Option<&T> {
        self.map.get(metadata)
    }
}
