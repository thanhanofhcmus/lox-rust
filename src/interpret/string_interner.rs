use std::collections::{HashMap, HashSet};

use derive_more::Display;

use super::debug_string::DebugString;

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

impl DebugString for StringInterner {
    fn debug_string(&self) -> String {
        use std::fmt::Write as FmtWrite;
        let mut s = String::new();
        writeln!(
            s,
            "  String Interner (count={}, next_id={}):",
            self.id_to_string.len(),
            self.next_id
        )
        .unwrap();
        if self.id_to_string.is_empty() {
            writeln!(s, "    (empty)").unwrap();
        } else {
            let mut entries: Vec<_> = self.id_to_string.iter().collect();
            entries.sort_by_key(|(id, _)| id.0);
            for (id, string) in entries {
                writeln!(s, "    {id:?} = {string:?}").unwrap();
            }
        }
        s
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn interning_same_string_returns_same_id() {
        let mut interner = StringInterner::new();
        let a = interner.intern("hello");
        let b = interner.intern("hello");
        assert_eq!(a, b);
    }

    #[test]
    fn interning_different_strings_returns_different_ids() {
        let mut interner = StringInterner::new();
        let a = interner.intern("hello");
        let b = interner.intern("world");
        assert_ne!(a, b);
    }

    #[test]
    fn get_returns_interned_string() {
        let mut interner = StringInterner::new();
        let id = interner.intern("foo");
        assert_eq!(interner.get(id), Some("foo"));
    }

    #[test]
    fn get_unknown_id_returns_none() {
        let interner = StringInterner::new();
        // Construct an id that was never interned by fabricating via next_id behavior:
        // since StrId is not publicly constructible outside this module, we instead
        // intern once and then sweep it away, then try to get it.
        let mut interner = interner;
        let id = interner.intern("temp");
        interner.sweep(); // unmarked → purged
        assert_eq!(interner.get(id), None);
    }

    #[test]
    fn sweep_without_marks_clears_all() {
        let mut interner = StringInterner::new();
        let a = interner.intern("a");
        let b = interner.intern("b");
        interner.sweep();
        assert_eq!(interner.get(a), None);
        assert_eq!(interner.get(b), None);
    }

    #[test]
    fn sweep_preserves_marked_removes_unmarked() {
        let mut interner = StringInterner::new();
        let keep = interner.intern("keep");
        let drop = interner.intern("drop");
        interner.mark_to_keep(keep);
        interner.sweep();
        assert_eq!(interner.get(keep), Some("keep"));
        assert_eq!(interner.get(drop), None);
    }

    #[test]
    fn reset_marks_then_sweep_clears_everything() {
        let mut interner = StringInterner::new();
        let id = interner.intern("x");
        interner.mark_to_keep(id);
        interner.reset_marks();
        interner.sweep();
        assert_eq!(interner.get(id), None);
    }

    #[test]
    fn re_intern_after_sweep_can_reuse_or_get_new_id() {
        // The interner uses monotonic next_id — after sweep, re-interning the same
        // string produces a *new* id (not reuse of the old one).
        let mut interner = StringInterner::new();
        let first = interner.intern("foo");
        interner.sweep();
        let second = interner.intern("foo");
        assert_ne!(first, second);
    }
}
