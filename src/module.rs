use crate::{id::Id, string_interner::StringInterner};
use std::collections::HashMap;
use std::path::PathBuf;

crate::define_type_index!(pub struct StrId);
pub type ModuleStringInterner = StringInterner<StrId>;

/// What an import statement looks like in source.
/// Lives in the AST — never contains a filesystem path.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ModuleMetadata {
    pub path: StrId,
    pub package: Id,
}

/// Canonical identity for a module — deduplicates imports and serves as the
/// key for all module caches / registries.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleIndex {
    pub package: Id,
    pub canonical_path: PathBuf,
}

#[derive(Debug, Default)]
pub struct ModuleRegistry<T> {
    map: HashMap<ModuleIndex, T>,
}

impl<T> ModuleRegistry<T> {
    pub fn insert(&mut self, index: ModuleIndex, module: T) {
        self.map.insert(index, module);
    }

    pub fn get(&self, index: &ModuleIndex) -> Option<&T> {
        self.map.get(index)
    }
}
