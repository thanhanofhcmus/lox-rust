use std::collections::HashMap;

use crate::{id::Id, span::Span};

#[derive(Debug, Clone, Copy)]
pub struct Identifier {
    pub span: Span,
    pub id: Id,
}

#[derive(Debug, Clone)]
pub struct SymbolNames {
    map: HashMap<Id, String>,
}

impl SymbolNames {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn insert(&mut self, id: Id, name: String) {
        self.map.insert(id, name);
    }

    pub fn get(&self, id: Id) -> Option<&str> {
        self.map.get(&id).map(|v| v.as_str())
    }

    pub fn get_or_unknown(&self, id: Id) -> &str {
        const UNKNOWN: &str = "#Unknown";
        self.get(id).unwrap_or(UNKNOWN)
    }
}
