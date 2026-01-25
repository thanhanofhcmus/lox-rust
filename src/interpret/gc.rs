use std::{
    collections::BTreeMap,
    fmt,
    ops::{AddAssign, SubAssign},
};

use crate::interpret::{
    helper_values::MapKey,
    value::{Array, Function, Value},
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GcHandle(usize);

impl fmt::Debug for GcHandle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("GcHandle({})", self.0))
    }
}

#[derive(Debug, Clone)]
pub enum GcObject {
    Str(String),

    Array(Array),

    // TODO: maybe make the key a gc entrance
    Map(BTreeMap<MapKey, Value>),

    Function(Function),
}

impl GcObject {
    pub fn type_name(&self) -> &'static str {
        match self {
            GcObject::Str(_) => "String",
            GcObject::Array(_) => "Array",
            GcObject::Map(_) => "Map",
            GcObject::Function(_) => "Function",
        }
    }
}

#[derive(Debug)]
struct HeapEntry {
    object: GcObject,

    /// for CoW
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

#[derive(Debug)]
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

    pub fn shallow_copy_value(&mut self, value: &Value) -> Value {
        // check if this is an lvalue, if yes, then promote it to not be lvale anymore
        self.recursive_ref_update(value.clone(), true);
        value.clone()
    }

    pub fn shallow_dispose_value(&mut self, value: Value) {
        self.recursive_ref_update(value.clone(), false);
    }

    pub fn get_object(&self, handle: GcHandle) -> Option<&GcObject> {
        match self.slots.get(handle.0) {
            Some(Some(v)) => Some(&v.object),
            _ => None,
        }
    }

    fn recursive_ref_update(&mut self, value: Value, is_increase: bool) {
        let Some(handle) = value.get_handle() else {
            return;
        };

        let Some(Some(entry)) = self.slots.get_mut(handle.0) else {
            // TODO: maybe return error
            return;
        };

        if is_increase {
            entry.ref_count.add_assign(1);
        } else {
            entry.ref_count.sub_assign(1);
        }

        match &entry.object {
            GcObject::Str(_) | GcObject::Function(_) => { /* do nothing */ }
            GcObject::Array(arr) => {
                for value in arr.clone() {
                    self.recursive_ref_update(value, is_increase);
                }
            }
            GcObject::Map(map) => {
                // TODO: recursive update for map key as well
                for (_, value) in map.clone() {
                    self.recursive_ref_update(value, is_increase);
                }
            }
        };
    }
}
