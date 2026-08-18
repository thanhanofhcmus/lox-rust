use crate::{id::Id, string_interner::StringInterner};
use std::collections::HashMap;

crate::define_type_index!(pub struct ModuleStrId);
pub type ModuleStringInterner = StringInterner<ModuleStrId>;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ModuleMetadata {
    pub path: ModuleStrId,
    pub package: Id,
}

/// Globally unique resolved module identity.
/// Used as the key for all module-level caches and registries.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Default)]
pub struct ModuleIdentity {
    pub resolved_path: ModuleStrId,
    pub is_std: bool,
}

#[derive(Debug, Default)]
pub struct ModuleRegistry<T, K = ModuleIdentity> {
    map: HashMap<K, T>,
}

impl<T, K: Eq + std::hash::Hash> ModuleRegistry<T, K> {
    pub fn insert(&mut self, key: K, module: T) {
        self.map.insert(key, module);
    }

    pub fn get(&self, key: &K) -> Option<&T> {
        self.map.get(key)
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }
}
