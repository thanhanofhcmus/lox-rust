use std::{
    collections::HashMap,
    ops::{AddAssign, SubAssign},
};

use crate::{
    interpret::{
        debug_string::DebugString,
        error::InterpretError,
        values::{Array, Function, Map, MapKey, Struct, Value},
    },
    string_interner::{StringInterner, SymbolId},
};

pub struct HeapStringMarker;

pub type StrId = SymbolId<HeapStringMarker>;
pub type HeapStringInterner = StringInterner<HeapStringMarker>;

#[derive(derive_more::Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[debug("GcHandle({_0})")]
pub struct GcHandle(usize);

#[derive(derive_more::Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum GcKind {
    Array,
    Map,
    Struct,
    Function,
}

impl GcKind {
    pub fn type_name(&self) -> &'static str {
        match self {
            GcKind::Array => "Array",
            GcKind::Map => "Map",
            GcKind::Struct => "Struct",
            GcKind::Function => "Function",
        }
    }
}

#[derive(Debug, Clone)]
pub enum GcObject {
    Array(Array),
    Map(Map),
    Struct(Struct),
    Function(Function),
}

impl GcObject {
    pub fn get_kind(&self) -> GcKind {
        match self {
            GcObject::Array(_) => GcKind::Array,
            GcObject::Map(_) => GcKind::Map,
            GcObject::Struct(_) => GcKind::Struct,
            GcObject::Function(_) => GcKind::Function,
        }
    }

    pub fn get_by_subscription(&self, indexee: Value) -> Result<Value, InterpretError> {
        match self {
            GcObject::Array(arr) => {
                let idx = indexee.to_index()?;
                arr.get(idx)
                    .copied()
                    .ok_or(InterpretError::ArrayOutOfBound(arr.len(), idx))
            }
            GcObject::Map(map) => {
                let map_key = MapKey::convert_from_value(indexee)?;
                Ok(map.get(&map_key).copied().unwrap_or(Value::make_nil()))
            }
            _ => Err(InterpretError::GcObjectUnIndexable(self.get_kind())),
        }
    }

    pub fn replace_at_index(
        &mut self,
        indexee: Value,
        new_value: Value,
    ) -> Result<Value, InterpretError> {
        match self {
            GcObject::Array(arr) => {
                let idx = indexee.to_index()?;
                match arr.get_mut(idx) {
                    Some(v) => Ok(std::mem::replace(v, new_value)),
                    None => Err(InterpretError::ArrayOutOfBound(arr.len(), idx)),
                }
            }
            GcObject::Map(map) => {
                let map_key = MapKey::convert_from_value(indexee)?;
                Ok(map.insert(map_key, new_value).unwrap_or(Value::make_nil()))
            }
            _ => Err(InterpretError::GcObjectUnIndexable(self.get_kind())),
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
    string_interner: HeapStringInterner,
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
        self.string_interner.reset_marks();

        // mark strings in root values
        root_values
            .iter()
            .filter_map(|v| v.get_str_id())
            .for_each(|id| self.string_interner.mark_to_keep(id));

        let mut trace_list = root_values
            .iter()
            .filter_map(|v| v.get_handle())
            .collect::<Vec<_>>();

        while let Some(handle) = trace_list.pop() {
            let Some(Some(entry)) = self.slots.get_mut(handle.0) else {
                continue;
            };
            entry.is_marked_for_delete = false;

            // string_interner is a separate field from slots, so borrowing both
            // simultaneously via field projection is allowed by the borrow checker
            match &entry.object {
                GcObject::Function(_) => {}
                GcObject::Array(arr) => {
                    arr.iter()
                        .filter_map(|v| v.get_str_id())
                        .for_each(|id| self.string_interner.mark_to_keep(id));
                    trace_list.extend(arr.iter().filter_map(|v| v.get_handle()));
                }
                GcObject::Struct(struct_) => {
                    trace_list.extend(struct_.fields.values().filter_map(|f| f.value.get_handle()));
                }
                GcObject::Map(map) => {
                    map.keys()
                        .filter_map(|k| k.get_str_id())
                        .for_each(|id| self.string_interner.mark_to_keep(id));
                    map.values()
                        .filter_map(|v| v.get_str_id())
                        .for_each(|id| self.string_interner.mark_to_keep(id));
                    trace_list.extend(map.values().filter_map(|v| v.get_handle()));
                }
            }
        }
    }

    pub fn sweep(&mut self) {
        for (index, entry) in self.slots.iter_mut().enumerate() {
            if let Some(v) = entry
                && v.is_marked_for_delete
            {
                entry.take();
                self.free_list.push(index);
            }
        }
        self.free_list.sort();
        self.free_list.reverse();

        self.string_interner.sweep();
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
                // have to use clone here since we are modifying the backing GC object
                for value in arr.clone() {
                    self.shallow_dispose_value(value);
                }
            }
            GcObject::Struct(_) => {
                unimplemented!()
            }
            GcObject::Map(map) => {
                // have to use clone here since we are modifying the backing GC object
                for (_, value) in map.clone() {
                    self.shallow_dispose_value(value);
                }
            }
        };
    }

    fn copy_by_value(&mut self, value: Value) -> Result<(GcHandle, Value), InterpretError> {
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
    ) -> Result<Value, InterpretError> {
        let (root_handle, root_value) = self.copy_by_value(current_root_value)?;

        let mut current_handle = root_handle;

        for indexee in chain.iter().copied() {
            let current_value = self
                .get_object_or_error(current_handle)?
                .get_by_subscription(indexee)?;

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

    pub fn get_string_or_error(&self, id: StrId) -> Result<&str, InterpretError> {
        self.string_interner
            .get(id)
            .ok_or(InterpretError::StringNotFoundOnHeap(id))
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

    pub fn get_object_or_error(&self, handle: GcHandle) -> Result<&GcObject, InterpretError> {
        self.get_object(handle)
            .ok_or(InterpretError::GcObjectNotFound(handle))
    }

    pub fn get_object_mut_or_error(
        &mut self,
        handle: GcHandle,
    ) -> Result<&mut GcObject, InterpretError> {
        self.get_object_mut(handle)
            .ok_or(InterpretError::GcObjectNotFound(handle))
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

impl DebugString for HeapStats {
    fn debug_string(&self) -> String {
        use std::fmt::Write as FmtWrite;
        let mut s = String::new();

        writeln!(
            s,
            "GC Objects  live: {}  |  dead: {}  |  total: {}",
            self.number_of_used_objects,
            self.number_of_to_be_deleted_objects,
            self.number_of_total_objects,
        )
        .unwrap();
        writeln!(
            s,
            "Slots       allocated: {}  |  free: {}",
            self.slots_size, self.free_list_size,
        )
        .unwrap();

        let kinds = [GcKind::Array, GcKind::Map, GcKind::Function];
        let fmt_kind = |map: &HashMap<GcKind, usize>| -> String {
            kinds
                .iter()
                .map(|k| format!("{}={}", k.type_name(), map.get(k).copied().unwrap_or(0)))
                .collect::<Vec<_>>()
                .join("  ")
        };
        let live_by_kind: HashMap<GcKind, usize> = kinds
            .iter()
            .map(|k| {
                let total = self.total_objects_stats.get(k).copied().unwrap_or(0);
                let dead = self
                    .to_be_deleted_objects_stats
                    .get(k)
                    .copied()
                    .unwrap_or(0);
                (*k, total - dead)
            })
            .collect();

        writeln!(s, "Live by kind:  {}", fmt_kind(&live_by_kind)).unwrap();
        writeln!(
            s,
            "Dead by kind:  {}",
            fmt_kind(&self.to_be_deleted_objects_stats)
        )
        .unwrap();

        s
    }
}

impl DebugString for Heap {
    fn debug_string(&self) -> String {
        use std::fmt::Write as FmtWrite;
        let mut s = String::new();

        writeln!(
            s,
            "Heap (slots={}, free={}):",
            self.slots.len(),
            self.free_list.len()
        )
        .unwrap();

        for (i, slot) in self.slots.iter().enumerate() {
            let Some(entry) = slot else { continue };
            let status = if entry.is_marked_for_delete { "dead" } else { "live" };
            let ref_count = entry.ref_count;
            let type_name = entry.object.get_kind().type_name();
            writeln!(
                s,
                "  [{i:3}] type = {type_name:<8} ref_count = {ref_count:<3} status = {status}"
            )
            .unwrap();
            match &entry.object {
                GcObject::Array(arr) => {
                    for (j, v) in arr.iter().enumerate() {
                        writeln!(s, "    [{j:3}] {v:?}").unwrap();
                    }
                }
                GcObject::Map(map) => {
                    for (k, v) in map.iter() {
                        writeln!(s, "    {k:?} => {v:?}").unwrap();
                    }
                }
                GcObject::Struct(st) => {
                    writeln!(s, "    struct{:?}", st).unwrap();
                }
                GcObject::Function(f) => {
                    writeln!(s, "    params: {:?}", f.params).unwrap();
                }
            }
            writeln!(s).unwrap();
        }

        writeln!(s).unwrap();
        // s += &self.string_interner.debug_string();
        s
    }
}
