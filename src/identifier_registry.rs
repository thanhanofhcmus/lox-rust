use std::collections::HashMap;

use crate::{id::Id, span::Span};

pub struct ComplexId {
    pub module: Option<Id>,
    pub name: Id,
}

impl From<ComplexIdentifier> for ComplexId {
    fn from(value: ComplexIdentifier) -> Self {
        Self {
            module: value.module.map(|v| v.id),
            name: value.name.id,
        }
    }
}

impl From<Identifier> for ComplexId {
    fn from(value: Identifier) -> Self {
        Self {
            module: None,
            name: value.id,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ComplexIdentifier {
    pub module: Option<Identifier>,
    pub name: Identifier,
}

impl From<Identifier> for ComplexIdentifier {
    fn from(name: Identifier) -> Self {
        Self { module: None, name }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Identifier {
    pub span: Span,
    pub id: Id,
}

#[derive(Debug, Clone)]
pub struct IdentifierRegistry {
    map: HashMap<Id, String>,
}

impl IdentifierRegistry {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    pub fn insert(&mut self, id: Id, name: String) {
        match self.map.get(&id) {
            Some(existing) if existing == &name => {}
            Some(existing) => panic!(
                "Id collision: {:?} hashes to the same Id as {:?} (both produced {:?}). \
                 The 64-bit FNV-1a Id is not collision-free; rename one identifier or \
                 widen the Id hash.",
                name, existing, id
            ),
            None => {
                self.map.insert(id, name);
            }
        }
    }

    pub fn get(&self, id: Id) -> Option<&str> {
        self.map.get(&id).map(|v| v.as_str())
    }

    pub fn get_or_unknown(&self, id: Id) -> &str {
        const UNKNOWN: &str = "#Unknown";
        self.get(id).unwrap_or(UNKNOWN)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert_same_name_twice_is_idempotent() {
        let mut r = IdentifierRegistry::new();
        let id = Id::new("foo");
        r.insert(id, "foo".to_string());
        r.insert(id, "foo".to_string());
        assert_eq!(r.get(id), Some("foo"));
    }

    #[test]
    #[should_panic(expected = "Id collision")]
    fn insert_collision_panics() {
        // Construct two IDs that share a hash but originate from different
        // names. The public `Id::new` is collision-resistant for short
        // strings, so we forge a collision by reusing the hash byte-for-byte.
        let mut r = IdentifierRegistry::new();
        let original = Id::new("alpha");
        r.insert(original, "alpha".to_string());
        // Reinsert the same Id but claim it's "beta" — simulates a real hash
        // collision between two distinct source identifiers.
        r.insert(original, "beta".to_string());
    }
}
