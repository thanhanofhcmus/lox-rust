use std::{
    cmp,
    collections::{HashMap, HashSet},
    marker::PhantomData,
};

use derive_more::Debug;

#[derive(Debug)]
#[debug("{}({})", std::any::type_name::<T>(), _0)]
pub struct SymbolId<T>(usize, PhantomData<T>);

impl<T> Clone for SymbolId<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for SymbolId<T> {}

impl<T> PartialOrd for SymbolId<T> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for SymbolId<T> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> PartialEq for SymbolId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for SymbolId<T> {}

impl<T> std::hash::Hash for SymbolId<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T> SymbolId<T> {
    pub fn new(value: usize) -> Self {
        Self(value, PhantomData)
    }
}

#[derive(Debug, Clone)]
pub struct StringInterner<T> {
    string_to_id: HashMap<String, SymbolId<T>>,
    id_to_string: HashMap<SymbolId<T>, String>,
    next_id: usize,
    mark_to_keep: HashSet<SymbolId<T>>,
}

impl<T> StringInterner<T> {
    pub fn new() -> Self {
        Self {
            string_to_id: HashMap::new(),
            id_to_string: HashMap::new(),
            next_id: 0,
            mark_to_keep: HashSet::new(),
        }
    }

    pub fn intern(&mut self, s: &str) -> SymbolId<T> {
        if let Some(id) = self.string_to_id.get(s) {
            return *id;
        }
        let id = SymbolId::new(self.next_id);
        self.next_id += 1;
        self.string_to_id.insert(s.into(), id);
        self.id_to_string.insert(id, s.into());
        id
    }

    pub fn get(&self, id: SymbolId<T>) -> Option<&str> {
        self.id_to_string.get(&id).map(|s| s.as_str())
    }

    pub fn reset_marks(&mut self) {
        self.mark_to_keep.clear();
    }

    pub fn mark_to_keep(&mut self, id: SymbolId<T>) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    type SI = StringInterner<()>;

    #[test]
    fn interning_same_string_returns_same_id() {
        let mut interner = SI::new();
        let a = interner.intern("hello");
        let b = interner.intern("hello");
        assert_eq!(a, b);
    }

    #[test]
    fn interning_different_strings_returns_different_ids() {
        let mut interner = SI::new();
        let a = interner.intern("hello");
        let b = interner.intern("world");
        assert_ne!(a, b);
    }

    #[test]
    fn get_returns_interned_string() {
        let mut interner = SI::new();
        let id = interner.intern("foo");
        assert_eq!(interner.get(id), Some("foo"));
    }

    #[test]
    fn get_unknown_id_returns_none() {
        let interner = SI::new();
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
        let mut interner = SI::new();
        let a = interner.intern("a");
        let b = interner.intern("b");
        interner.sweep();
        assert_eq!(interner.get(a), None);
        assert_eq!(interner.get(b), None);
    }

    #[test]
    fn sweep_preserves_marked_removes_unmarked() {
        let mut interner = SI::new();
        let keep = interner.intern("keep");
        let drop = interner.intern("drop");
        interner.mark_to_keep(keep);
        interner.sweep();
        assert_eq!(interner.get(keep), Some("keep"));
        assert_eq!(interner.get(drop), None);
    }

    #[test]
    fn reset_marks_then_sweep_clears_everything() {
        let mut interner = SI::new();
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
        let mut interner = SI::new();
        let first = interner.intern("foo");
        interner.sweep();
        let second = interner.intern("foo");
        assert_ne!(first, second);
    }
}
