pub use edb_derive::Table;
pub use hashbrown;
pub use slotmap;

pub type Index<K, Key = KeyData> = hashbrown::HashMap<K, Key>;
pub type IndexSet<K, Key = KeyData> = hashbrown::HashMap<K, hashbrown::HashSet<Key>>;

pub type Storage<K, V> = slotmap::SlotMap<K, V>;

pub type KeyData = slotmap::DefaultKey;
