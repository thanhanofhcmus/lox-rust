use std::collections::HashMap;

use derive_more::Display;

#[derive(Display, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct StrId(usize);

#[derive(Debug, Clone)]
pub struct StringInterner {
    map: HashMap<String, StrId>,
    vec: Vec<String>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            vec: Vec::new(),
        }
    }

    pub fn intern(&mut self, s: &str) -> StrId {
        if let Some(id) = self.map.get(s) {
            return *id;
        };

        let id = StrId(self.vec.len());
        self.map.insert(s.into(), id);
        self.vec.push(s.into());

        id
    }

    pub fn get(&self, id: StrId) -> Option<&str> {
        self.vec.get(id.0).map(|v| v.as_ref())
    }
}
