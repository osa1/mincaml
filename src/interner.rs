use fxhash::FxHashMap;
use std::hash::Hash;
use std::num::NonZeroU32;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InternId {
    // NonZeroU32 used so that Option<InternId> is one 8 bytes instead of 12
    value: NonZeroU32,
    // Table id used to avoid indexing a table with InternId obtained from another
    // table_id: u32,
}

impl InternId {
    fn from_u32(i: u32) -> InternId {
        assert!(i < std::u32::MAX);
        InternId { value: unsafe { NonZeroU32::new_unchecked(i + 1) } }
    }

    fn to_u32(&self) -> u32 {
        self.value.get() - 1
    }
}

#[derive(Debug)]
pub struct InternTable<K> {
    // table_id: u32,
    map: FxHashMap<K, InternId>,
    values: Vec<Rc<K>>,
}

impl<K> Default for InternTable<K>
where
    K: Eq + Hash,
{
    fn default() -> Self {
        Self { map: Default::default(), values: Default::default() }
    }
}

impl<K> InternTable<K>
where
    K: Eq + Hash,
{
    pub fn intern(&mut self, k: K) -> InternId {
        match self.map.get(&k) {
            Some(intern_id) => *intern_id,
            None => {
                let idx = self.values.len();
                self.values.push(Rc::new(k));
                InternId::from_u32(idx as u32)
            }
        }
    }

    pub fn get(&self, id: InternId) -> Rc<K> {
        // assert_eq!(id.table_id, self.table_id);
        self.values[id.to_u32() as usize].clone()
    }
}
