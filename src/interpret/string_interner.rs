use std::collections::{HashMap, HashSet};

use derive_more::Display;

/// The Ordering in this type does not mean anything, just to be used as map key
#[derive(Display, derive_more::Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[debug("StrId({_0})")]
pub struct StrId(usize);

#[derive(Debug, Clone)]
pub struct StringInterner {
    string_to_id: HashMap<String, StrId>,
    id_to_string: HashMap<StrId, String>,
    next_id: usize,
    mark_to_keep: HashSet<StrId>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            string_to_id: HashMap::new(),
            id_to_string: HashMap::new(),
            next_id: 0,
            mark_to_keep: HashSet::new(),
        }
    }

    pub fn intern(&mut self, s: &str) -> StrId {
        if let Some(id) = self.string_to_id.get(s) {
            return *id;
        }
        let id = StrId(self.next_id);
        self.next_id += 1;
        self.string_to_id.insert(s.into(), id);
        self.id_to_string.insert(id, s.into());
        id
    }

    pub fn get(&self, id: StrId) -> Option<&str> {
        self.id_to_string.get(&id).map(|s| s.as_str())
    }

    pub fn reset_marks(&mut self) {
        self.mark_to_keep.clear();
    }

    pub fn mark_to_keep(&mut self, id: StrId) {
        self.mark_to_keep.insert(id);
    }

    pub fn sweep(&mut self) {
        let dead = self
            .id_to_string
            .iter()
            .filter(|(id, _)| !self.mark_to_keep.contains(id))
            .map(|(id, s)| (*id, s.clone()))
            .collect::<Vec<_>>();
        for (id, s) in dead {
            self.id_to_string.remove(&id);
            self.string_to_id.remove(&s);
        }
    }
}
