use std::{
    collections::HashMap,
    ops::{AddAssign, SubAssign},
};

use crate::interpret::{
    error::Error,
    string_interner::{StrId, StringInterner},
    values::{Array, Function, Map, MapKey, Value},
};

#[derive(derive_more::Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[debug("GcHandle({_0})")]
pub struct GcHandle(usize);

#[derive(derive_more::Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum GcKind {
    Array,
    Map,
    Function,
}

impl GcKind {
    pub fn type_name(&self) -> &'static str {
        match self {
            GcKind::Array => "Array",
            GcKind::Map => "Map",
            GcKind::Function => "Function",
        }
    }
}

#[derive(Debug, Clone)]
pub enum GcObject {
    Array(Array),
    Map(Map),
    Function(Function),
}

impl GcObject {
    pub fn get_kind(&self) -> GcKind {
        match self {
            GcObject::Array(_) => GcKind::Array,
            GcObject::Map(_) => GcKind::Map,
            GcObject::Function(_) => GcKind::Function,
        }
    }

    pub fn get_at_index(&self, indexee: Value) -> Result<Value, Error> {
        match self {
            GcObject::Array(arr) => {
                let idx = indexee.to_index()?;
                arr.get(idx)
                    .copied()
                    .ok_or(Error::ArrayOutOfBound(arr.len(), idx))
            }
            GcObject::Map(map) => {
                let map_key = MapKey::convert_from_value(indexee)?;
                Ok(map.get(&map_key).copied().unwrap_or(Value::make_nil()))
            }
            _ => Err(Error::GcObjectUnIndexable(self.get_kind())),
        }
    }

    pub fn replace_at_index(&mut self, indexee: Value, new_value: Value) -> Result<Value, Error> {
        match self {
            GcObject::Array(arr) => {
                let idx = indexee.to_index()?;
                match arr.get_mut(idx) {
                    Some(v) => Ok(std::mem::replace(v, new_value)),
                    None => Err(Error::ArrayOutOfBound(arr.len(), idx)),
                }
            }
            GcObject::Map(map) => {
                let map_key = MapKey::convert_from_value(indexee)?;
                Ok(map.insert(map_key, new_value).unwrap_or(Value::make_nil()))
            }
            _ => Err(Error::GcObjectUnIndexable(self.get_kind())),
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
    pub number_of_used_objects: usize,
    pub number_of_to_be_deleted_objects: usize,
    pub total_objects_stats: HashMap<GcKind, usize>,
    pub to_be_deleted_objects_stats: HashMap<GcKind, usize>,
}

#[derive(Debug)]
pub struct Heap {
    slots: Vec<Option<HeapEntry>>,
    free_list: Vec<usize>,
    string_interner: StringInterner,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            slots: vec![],
            free_list: vec![],
            string_interner: StringInterner::new(),
        }
    }

    pub fn get_stats(&self) -> HeapStats {
        let slots_size = self.slots.len();
        let free_list_size = self.free_list.len();

        let mut number_of_total_objects = 0;
        let mut number_of_used_objects = 0;
        let mut number_of_to_be_deleted_objects = 0;

        let mut total_objects_stats = HashMap::new();
        let mut to_be_deleted_objects_stats = HashMap::new();

        for entry in self.slots.iter().flatten() {
            let kind = entry.object.get_kind();

            number_of_total_objects += 1;
            total_objects_stats.entry(kind).or_insert(0).add_assign(1);

            if entry.is_marked_for_delete {
                number_of_to_be_deleted_objects += 1;
                to_be_deleted_objects_stats
                    .entry(kind)
                    .or_insert(0)
                    .add_assign(1);
            } else {
                number_of_used_objects += 1;
            }
        }

        HeapStats {
            slots_size,
            free_list_size,
            number_of_total_objects,
            number_of_used_objects,
            number_of_to_be_deleted_objects,
            total_objects_stats,
            to_be_deleted_objects_stats,
        }
    }

    pub fn mark(&mut self, root_values: Vec<Value>) {
        for entry in self.slots.iter_mut().flatten() {
            entry.is_marked_for_delete = true;
        }

        // TODO: mark for string_interner

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
                GcObject::Function(_) => { /* do nothing */ }
                GcObject::Array(arr) => {
                    let it = arr.iter().filter_map(|v| v.get_handle());
                    trace_list.extend(it);
                }
                GcObject::Map(map) => {
                    let value_it = map.iter().filter_map(|(_, v)| v.get_handle());
                    trace_list.extend(value_it);
                }
            }
        }
    }

    pub fn sweep(&mut self) {
        for (index, entry) in self.slots.iter_mut().enumerate() {
            if let Some(v) = entry && v.is_marked_for_delete {
                entry.take();
                self.free_list.push(index);
            }
        }
        self.free_list.sort();
        self.free_list.reverse();
    }

    pub fn shallow_copy_value(&mut self, value: Value) {
        // check if this is an lvalue, if yes, then promote it to not be lvalue anymore
        // TODO: recursively increase ref is wrong,
        // what we really want to say is we want the value increase ref
        // self.recursive_ref_count_update(value, true);
        self.update_ref_count(value, true);
    }

    pub fn shallow_dispose_value(&mut self, value: Value) {
        let some_entry = self.update_ref_count(value, false);
        let Some(entry) = some_entry else {
            return;
        };
        if entry.ref_count != 0 {
            return;
        }
        // this object does not have any backing any more, mark as dead and remove the count
        entry.is_marked_for_delete = true;
        match &entry.object {
            GcObject::Function(_) => { /* do nothing */ }
            GcObject::Array(arr) => {
                for value in arr.clone() {
                    self.shallow_dispose_value(value);
                }
            }
            GcObject::Map(map) => {
                for (_, value) in map.clone() {
                    self.shallow_dispose_value(value);
                }
            }
        };
    }

    fn copy_by_value(&mut self, value: Value) -> Result<(GcHandle, Value), Error> {
        let old_handle = value.get_handle().expect("Must be a handled value");
        let old_object = self.get_object_or_error(old_handle)?;
        let new_handle = self.insert_object(old_object.clone());
        let mut new_value = value;
        new_value.replace_handle(new_handle);
        Ok((new_handle, new_value))
    }

    pub fn deep_copy_reassign_object(
        &mut self,
        current_root_value: Value,
        last_value: Value,
        chain: &[Value],
        reassigning_value: Value,
    ) -> Result<Value, Error> {
        let (root_handle, root_value) = self.copy_by_value(current_root_value)?;

        let mut current_handle = root_handle;

        for indexee in chain.iter().copied() {
            let current_value = self
                .get_object_or_error(current_handle)?
                .get_at_index(indexee)?;

            let (new_handle, new_value) = self.copy_by_value(current_value)?;

            // TODO: dispose old value
            _ = self
                .get_object_mut_or_error(current_handle)?
                .replace_at_index(indexee, new_value)?;

            current_handle = new_handle;
        }

        _ = self
            .get_object_mut_or_error(current_handle)?
            .replace_at_index(last_value, reassigning_value)?;

        Ok(root_value)
    }

    pub fn insert_string(&mut self, s: String) -> StrId {
        self.string_interner.intern(&s)
    }

    pub fn get_string(&self, id: StrId) -> Option<&str> {
        self.string_interner.get(id)
    }

    pub fn get_string_or_error(&self, id: StrId) -> Result<&str, Error> {
        self.get_string(id).ok_or(Error::StringNotFoundOnHeap(id))
    }

    /// Move the GcObject to the heap
    pub fn insert_object(&mut self, object: GcObject) -> GcHandle {
        if let Some(index) = self.free_list.pop() {
            self.slots[index] = Some(HeapEntry::new(object));
            return GcHandle(index);
        }

        let index = self.slots.len();
        self.slots.push(Some(HeapEntry::new(object)));
        GcHandle(index)
    }

    pub fn get_object(&self, handle: GcHandle) -> Option<&GcObject> {
        match self.slots.get(handle.0) {
            Some(Some(v)) => Some(&v.object),
            _ => None,
        }
    }

    pub fn get_object_mut(&mut self, handle: GcHandle) -> Option<&mut GcObject> {
        match self.slots.get_mut(handle.0) {
            Some(Some(v)) => Some(&mut v.object),
            _ => None,
        }
    }

    pub fn get_object_or_error(&self, handle: GcHandle) -> Result<&GcObject, Error> {
        self.get_object(handle)
            .ok_or(Error::GcObjectNotFound(handle))
    }

    pub fn get_object_mut_or_error(&mut self, handle: GcHandle) -> Result<&mut GcObject, Error> {
        self.get_object_mut(handle)
            .ok_or(Error::GcObjectNotFound(handle))
    }

    fn update_ref_count(&mut self, value: Value, is_increase: bool) -> Option<&mut HeapEntry> {
        let handle = value.get_handle()?;

        let Some(Some(entry)) = self.slots.get_mut(handle.0) else {
            // TODO: maybe return error
            return None;
        };

        if is_increase {
            entry.ref_count.add_assign(1);
        } else {
            entry.ref_count.sub_assign(1);
        }

        Some(entry)
    }
}
