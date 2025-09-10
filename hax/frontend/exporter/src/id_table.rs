/// This module provides a notion of table, identifiers and nodes. A
/// `Node<T>` is a `Arc<T>` bundled with a unique identifier such that
/// there exists an entry in a table for that identifier.
///
/// The type `WithTable<T>` bundles a table with a value of type
/// `T`. That value of type `T` may hold an arbitrary number of
/// `Node<_>`s. In the context of a `WithTable<T>`, the type `Node<_>`
/// serializes and deserializes using a table as a state. In this
/// case, serializing a `Node<U>` produces only an identifier, without
/// any data of type `U`. Deserializing a `Node<U>` under a
/// `WithTable<T>` will recover `U` data from the table held by
/// `WithTable`.
///
/// Serde is not designed for stateful (de)serialization. There is no
/// way of deriving `serde::de::DeserializeSeed` systematically. This
/// module thus makes use of global state to achieve serialization and
/// deserialization. This modules provides an API that hides this
/// global state.
use crate::prelude::*;
use std::{
    hash::{Hash, Hasher},
    sync::{atomic::Ordering, Arc, LazyLock, Mutex, MutexGuard},
};

/// Unique IDs in a ID table.
#[derive_group(Serializers)]
#[derive(Default, Clone, Copy, Debug, JsonSchema, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[serde(transparent)]
pub struct Id {
    id: u32,
}

/// A session providing fresh IDs for ID table.
#[derive(Default, Debug)]
pub struct Session {
    next_id: Id,
    table: Table,
}

impl Session {
    pub fn table(&self) -> &Table {
        &self.table
    }
}

/// The different types of values one can store in an ID table.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum Value {
    Ty(Arc<TyKind>),
    DefId(Arc<DefIdContents>),
    ItemRef(Arc<ItemRefContents>),
}

impl SupportedType<Value> for TyKind {
    fn to_types(value: Arc<Self>) -> Value {
        Value::Ty(value)
    }
    fn from_types(t: &Value) -> Option<Arc<Self>> {
        match t {
            Value::Ty(value) => Some(value.clone()),
            _ => None,
        }
    }
}

impl SupportedType<Value> for DefIdContents {
    fn to_types(value: Arc<Self>) -> Value {
        Value::DefId(value)
    }
    fn from_types(t: &Value) -> Option<Arc<Self>> {
        match t {
            Value::DefId(value) => Some(value.clone()),
            _ => None,
        }
    }
}

impl SupportedType<Value> for ItemRefContents {
    fn to_types(value: Arc<Self>) -> Value {
        Value::ItemRef(value)
    }
    fn from_types(t: &Value) -> Option<Arc<Self>> {
        match t {
            Value::ItemRef(value) => Some(value.clone()),
            _ => None,
        }
    }
}

/// A node is a bundle of an ID with a value.
#[derive(Deserialize, Serialize, Debug, JsonSchema, PartialOrd, Ord)]
#[serde(into = "serde_repr::NodeRepr<T>")]
#[serde(try_from = "serde_repr::NodeRepr<T>")]
pub struct Node<T: 'static + SupportedType<Value>> {
    id: Id,
    value: Arc<T>,
}

impl<T: SupportedType<Value>> std::ops::Deref for Node<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.value.as_ref()
    }
}

/// Hax relies on hashes being deterministic for predicates
/// ids. Identifiers are not deterministic: we implement hash for
/// `Node` manually, discarding the field `id`.
impl<T: SupportedType<Value> + Hash> Hash for Node<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.as_ref().hash(state);
    }
}
impl<T: SupportedType<Value> + Eq> Eq for Node<T> {}
impl<T: SupportedType<Value> + PartialEq> PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

/// Manual implementation of `Clone` that doesn't require a `Clone`
/// bound on `T`.
impl<T: SupportedType<Value>> Clone for Node<T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id.clone(),
            value: self.value.clone(),
        }
    }
}

/// A table is a map from IDs to `Value`s. When serialized, we
/// represent a table as a *sorted* vector. Indeed, the values stored
/// in the table might reference each other, without cycle, so the
/// order matters.
#[derive(Default, Debug, Clone, Deserialize, Serialize)]
#[serde(into = "serde_repr::SortedIdValuePairs")]
#[serde(from = "serde_repr::SortedIdValuePairs")]
pub struct Table(HeterogeneousMap<Id, Value>);

mod heterogeneous_map {
    //! This module provides an heterogenous map that can store types
    //! that implement the trait `SupportedType`.

    use std::collections::HashMap;
    use std::hash::Hash;
    use std::sync::Arc;
    #[derive(Clone, Debug)]
    /// An heterogenous map is a map from `Key` to `Value`. It provide
    /// the methods `insert` and `get` for any type `T` that
    /// implements `SupportedType<Value>`.
    pub struct HeterogeneousMap<Key, Value>(HashMap<Key, Value>);

    impl<Id, Value> Default for HeterogeneousMap<Id, Value> {
        fn default() -> Self {
            Self(HashMap::default())
        }
    }

    impl<Key: Hash + Eq + PartialEq, Value> HeterogeneousMap<Key, Value> {
        pub(super) fn insert<T>(&mut self, key: Key, value: Arc<T>)
        where
            T: SupportedType<Value>,
        {
            self.insert_raw_value(key, T::to_types(value));
        }
        pub(super) fn insert_raw_value(&mut self, key: Key, value: Value) {
            self.0.insert(key, value);
        }
        pub(super) fn from_iter(it: impl Iterator<Item = (Key, Value)>) -> Self {
            Self(HashMap::from_iter(it))
        }
        pub(super) fn into_iter(self) -> impl Iterator<Item = (Key, Value)> {
            self.0.into_iter()
        }
        pub(super) fn get<T>(&self, key: &Key) -> Option<Option<Arc<T>>>
        where
            T: SupportedType<Value>,
        {
            self.0.get(key).map(T::from_types)
        }
    }

    /// A type that can be mapped to `Value` and optionally
    /// reconstructed back.
    pub trait SupportedType<Value>: std::fmt::Debug {
        fn to_types(value: Arc<Self>) -> Value;
        fn from_types(t: &Value) -> Option<Arc<Self>>;
    }
}
use heterogeneous_map::*;

impl Session {
    fn fresh_id(&mut self) -> Id {
        let id = self.next_id.id;
        self.next_id.id += 1;
        Id { id }
    }
}

impl<T: Sync + Send + 'static + SupportedType<Value>> Node<T> {
    pub fn new(value: T, session: &mut Session) -> Self {
        let id = session.fresh_id();
        let value = Arc::new(value);
        session.table.0.insert(id.clone(), value.clone());
        Self { id, value }
    }

    pub fn inner(&self) -> &Arc<T> {
        &self.value
    }

    pub fn id(&self) -> Id {
        self.id
    }
}

/// Wrapper for a type `T` that creates a bundle containing both a ID
/// table and a value `T`. That value may contains `Node` values
/// inside it. Serializing `WithTable<T>` will serialize IDs only,
/// skipping values. Deserialization of a `WithTable<T>` will
/// automatically use the table and IDs to reconstruct skipped values.
#[derive(Debug)]
pub struct WithTable<T> {
    table: Table,
    value: T,
}

/// The state used for deserialization: a table.
static DESERIALIZATION_STATE: LazyLock<Mutex<Table>> =
    LazyLock::new(|| Mutex::new(Table::default()));
static DESERIALIZATION_STATE_LOCK: LazyLock<Mutex<()>> = LazyLock::new(|| Mutex::new(()));

/// The mode of serialization: should `Node<T>` ship values of type `T` or not?
static SERIALIZATION_MODE_USE_IDS: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

fn serialize_use_id() -> bool {
    SERIALIZATION_MODE_USE_IDS.load(Ordering::Relaxed)
}

impl<T> WithTable<T> {
    /// Runs `f` with a `WithTable<T>` created out of `map` and
    /// `value`. Any serialization of values of type `Node<_>` will
    /// skip the field `value`.
    pub fn run<R>(map: Table, value: T, f: impl FnOnce(&Self) -> R) -> R {
        if serialize_use_id() {
            panic!("CACHE_MAP_LOCK: only one WithTable serialization can occur at a time (nesting is forbidden)")
        }
        SERIALIZATION_MODE_USE_IDS.store(true, Ordering::Relaxed);
        let result = f(&Self { table: map, value });
        SERIALIZATION_MODE_USE_IDS.store(false, Ordering::Relaxed);
        result
    }
    pub fn destruct(self) -> (T, Table) {
        let Self { value, table: map } = self;
        (value, map)
    }
}

impl<T: Serialize> Serialize for WithTable<T> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut ts = serializer.serialize_tuple_struct("WithTable", 2)?;
        use serde::ser::SerializeTupleStruct;
        ts.serialize_field(&self.table)?;
        ts.serialize_field(&self.value)?;
        ts.end()
    }
}

/// The deserializer of `WithTable<T>` is special. We first decode the
/// table in order: each `(Id, Value)` pair of the table populates the
/// global table state found in `DESERIALIZATION_STATE`. Only then we
/// can decode the value itself, knowing `DESERIALIZATION_STATE` is
/// complete.
impl<'de, T: Deserialize<'de>> serde::Deserialize<'de> for WithTable<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let _lock: MutexGuard<_> = DESERIALIZATION_STATE_LOCK.try_lock().expect("CACHE_MAP_LOCK: only one WithTable deserialization can occur at a time (nesting is forbidden)");
        use serde_repr::WithTableRepr;
        let previous = std::mem::take(&mut *DESERIALIZATION_STATE.lock().unwrap());
        let with_table_repr = WithTableRepr::deserialize(deserializer);
        *DESERIALIZATION_STATE.lock().unwrap() = previous;
        let WithTableRepr(table, value) = with_table_repr?;
        Ok(Self { table, value })
    }
}

/// Defines representations for various types when serializing or/and
/// deserializing via serde
mod serde_repr {
    use super::*;

    #[derive(Serialize, Deserialize, JsonSchema, Debug)]
    pub(super) struct NodeRepr<T> {
        id: Id,
        value: Option<Arc<T>>,
    }

    #[derive(Serialize)]
    pub(super) struct Pair(Id, Value);
    pub(super) type SortedIdValuePairs = Vec<Pair>;

    #[derive(Serialize, Deserialize)]
    pub(super) struct WithTableRepr<T>(pub(super) Table, pub(super) T);

    impl<T: SupportedType<Value>> Into<NodeRepr<T>> for Node<T> {
        fn into(self) -> NodeRepr<T> {
            let value = if serialize_use_id() {
                None
            } else {
                Some(self.value.clone())
            };
            let id = self.id;
            NodeRepr { value, id }
        }
    }

    impl<T: 'static + SupportedType<Value>> TryFrom<NodeRepr<T>> for Node<T> {
        type Error = serde::de::value::Error;

        fn try_from(cached: NodeRepr<T>) -> Result<Self, Self::Error> {
            use serde::de::Error;
            let table = DESERIALIZATION_STATE.lock().unwrap();
            let id = cached.id;
            let kind = if let Some(kind) = cached.value {
                kind
            } else {
                table
                    .0
                    .get(&id)
                    .ok_or_else(|| {
                        Self::Error::custom(&format!(
                            "Stateful deserialization failed for id {:?}: not found in cache",
                            id
                        ))
                    })?
                    .ok_or_else(|| {
                        Self::Error::custom(&format!(
                            "Stateful deserialization failed for id {:?}: wrong type",
                            id
                        ))
                    })?
            };
            Ok(Self { value: kind, id })
        }
    }

    impl<'de> serde::Deserialize<'de> for Pair {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            let (id, v) = <(Id, Value)>::deserialize(deserializer)?;
            DESERIALIZATION_STATE
                .lock()
                .unwrap()
                .0
                .insert_raw_value(id.clone(), v.clone());
            Ok(Pair(id, v))
        }
    }

    impl Into<SortedIdValuePairs> for Table {
        fn into(self) -> SortedIdValuePairs {
            let mut vec: Vec<_> = self.0.into_iter().map(|(x, y)| Pair(x, y)).collect();
            vec.sort_by_key(|o| o.0.clone());
            vec
        }
    }

    impl From<SortedIdValuePairs> for Table {
        fn from(t: SortedIdValuePairs) -> Self {
            Self(HeterogeneousMap::from_iter(
                t.into_iter().map(|Pair(x, y)| (x, y)),
            ))
        }
    }
}
