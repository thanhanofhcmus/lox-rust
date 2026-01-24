use std::collections::BTreeMap;

use crate::interpret::value::{Function, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GcHandle(usize);

#[derive(Debug, Clone)]
pub enum GcObject {
    Str(String),

    Array(Vec<Value>),

    // TODO: maybe make the key a gc entrance
    Map(BTreeMap<Value, Value>),

    Function(Function),
}

struct HeapEntry {
    object: GcObject,
    ref_count: usize,
    is_marked_for_delete: bool,
}

impl HeapEntry {
    fn new(object: GcObject) -> Self {
        Self {
            object,
            ref_count: 1,
            is_marked_for_delete: false,
        }
    }
}

pub struct Heap {
    slots: Vec<Option<HeapEntry>>,
    free_list: Vec<usize>,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            slots: vec![],
            free_list: vec![],
        }
    }

    /// Move the GcObject to the heap
    pub fn allocate(&mut self, object: GcObject) -> GcHandle {
        if let Some(index) = self.free_list.pop() {
            self.slots[index] = Some(HeapEntry::new(object));
            return GcHandle(index);
        }

        let index = self.slots.len();
        self.slots.push(Some(HeapEntry::new(object)));
        GcHandle(index)
    }

    pub fn get(&self, handle: GcHandle) -> Option<&GcObject> {
        match self.slots.get(handle.0) {
            Some(Some(v)) => Some(&v.object),
            _ => None,
        }
    }
}
