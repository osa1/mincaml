use fxhash::FxHashMap;
use std::hash::Hash;
use std::num::NonZeroU32;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InternId {
    // NonZeroU32 used so that Option<InternId> is one 8 bytes instead of 12
    value: NonZeroU32,
    // Table id used to avoid indexing a table with InternId obtained from another
    table_id: u32,
}

impl InternId {
    fn from_u32(table_id: u32, i: u32) -> InternId {
        assert!(i < std::u32::MAX);
        InternId {
            table_id,
            value: unsafe { NonZeroU32::new_unchecked(i + 1) },
        }
    }
}

#[derive(Debug)]
pub struct InternTable<K> {
    table_id: u32,
    map: FxHashMap<K, InternId>,
    values: Vec<Rc<K>>,
    free: InternId,
}

static mut TABLE_ID: u32 = 0;

impl<K> Default for InternTable<K>
where
    K: Eq + Hash,
{
    fn default() -> Self {
        // NOTE: Not thread-safe!
        let table_id = unsafe { TABLE_ID };
        unsafe {
            TABLE_ID += 1;
        }
        Self {
            table_id,
            map: Default::default(),
            values: Default::default(),
            free: InternId::from_u32(table_id, 0),
        }
    }
}

impl<K> InternTable<K>
where
    K: Eq + Hash,
{
    fn new_intern_id(&mut self) -> InternId {
        let intern_id = self.free;
        self.free = InternId::from_u32(self.table_id, intern_id.value.get() + 1);
        intern_id
    }

    pub fn intern(&mut self, k: K) -> InternId {
        match self.map.get(&k) {
            Some(intern_id) => *intern_id,
            None => {
                let intern_id = self.new_intern_id();
                self.values.push(Rc::new(k));
                intern_id
            }
        }
    }

    pub fn get(&self, id: InternId) -> Rc<K> {
        assert_eq!(id.table_id, self.table_id);
        self.values[id.value.get() as usize].clone()
    }
}
