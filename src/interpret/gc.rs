use std::{
    collections::{BTreeMap, HashMap},
    ops::{AddAssign, SubAssign},
};

use crate::interpret::{
    helper_values::MapKey,
    value::{Array, Function, Value},
};

#[derive(derive_more::Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[debug("GcHandle({_0})")]
pub struct GcHandle(usize);

#[derive(derive_more::Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum GcKind {
    String,
    Array,
    Map,
    Function,
}

impl GcKind {
    pub fn type_name(&self) -> &'static str {
        match self {
            GcKind::String => "String",
            GcKind::Array => "Array",
            GcKind::Map => "Map",
            GcKind::Function => "Function",
        }
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
    pub fn get_kind(&self) -> GcKind {
        match self {
            GcObject::Str(_) => GcKind::String,
            GcObject::Array(_) => GcKind::Array,
            GcObject::Map(_) => GcKind::Map,
            GcObject::Function(_) => GcKind::Function,
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

#[derive(derive_more::Debug)]
#[allow(dead_code)]
pub struct HeapStats {
    pub slots_size: usize,
    pub free_list_size: usize,
    pub number_of_total_objects: usize,
    pub number_of_objects_to_delete: usize,
    pub total_objects_stats: HashMap<GcKind, usize>,
    pub objects_to_delete_stats: HashMap<GcKind, usize>,
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

    pub fn get_stats(&self) -> HeapStats {
        let slots_size = self.slots.len();
        let free_list_size = self.free_list.len();

        let mut number_of_total_objects = 0;
        let mut number_of_objects_to_delete = 0;

        let mut total_objects_stats = HashMap::new();
        let mut objects_to_delete_stats = HashMap::new();

        for entry in self.slots.iter().flatten() {
            let kind = entry.object.get_kind();

            number_of_total_objects += 1;
            total_objects_stats.entry(kind).or_insert(0).add_assign(1);

            if entry.is_marked_for_delete {
                number_of_objects_to_delete += 1;
                objects_to_delete_stats
                    .entry(kind)
                    .or_insert(0)
                    .add_assign(1);
            }
        }

        HeapStats {
            slots_size,
            free_list_size,
            number_of_total_objects,
            number_of_objects_to_delete,
            total_objects_stats,
            objects_to_delete_stats,
        }
    }

    pub fn mark(&mut self, root_values: Vec<Value>) {
        for entry in self.slots.iter_mut().flatten() {
            entry.is_marked_for_delete = true;
        }

        let mut trace_list = root_values
            .iter()
            .filter_map(|v| v.get_handle())
            .collect::<Vec<_>>();

        while let Some(handle) = trace_list.pop() {
            let Some(Some(entry)) = self.slots.get_mut(handle.0) else {
                continue;
            };
            entry.is_marked_for_delete = false;

            match &mut entry.object {
                GcObject::Str(_) | GcObject::Function(_) => { /* do nothing */ }
                GcObject::Array(arr) => {
                    let it = arr.iter().filter_map(|v| v.get_handle());
                    trace_list.extend(it);
                }
                GcObject::Map(map) => {
                    // TODO: recursive update for map key as well
                    let it = map.iter().filter_map(|(_, v)| v.get_handle());
                    trace_list.extend(it);
                }
            }
        }
    }

    pub fn sweep(&mut self) {
        for entry in self.slots.iter_mut() {
            if entry
                .as_ref()
                .map(|e| e.is_marked_for_delete)
                .unwrap_or(false)
            {
                entry.take();
            }
        }
        // move all Some element to the top
        self.slots.sort_by_key(|e| e.is_none());
        let first_none_dex = self.slots.partition_point(|e| e.is_some());
        self.free_list = (first_none_dex..self.slots.len()).rev().collect();
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

    pub fn shallow_copy_value(&mut self, value: Value) {
        // check if this is an lvalue, if yes, then promote it to not be lvale anymore
        self.recursive_ref_update(value, true);
    }

    pub fn shallow_dispose_value(&mut self, value: Value) {
        self.recursive_ref_update(value, false);
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
