use fxhash::FxHashMap;
use std::hash::Hash;
use std::mem::replace;

pub struct Locals<K, V>
where
    K: Clone + Hash + Eq,
    V: Clone,
{
    env: FxHashMap<K, V>,
    // `V` below is the `V` we override, not the `V` of the binder in the current scope!
    current_scope: Vec<(K, Option<V>)>,
    scopes: Vec<Vec<(K, Option<V>)>>,
}

impl<K, V> Locals<K, V>
where
    K: Clone + Hash + Eq,
    V: Clone,
{
    pub fn new(globals: FxHashMap<K, V>) -> Locals<K, V> {
        Locals {
            env: globals,
            current_scope: vec![],
            scopes: vec![],
        }
    }

    pub fn new_scope(&mut self) {
        self.scopes.push(replace(&mut self.current_scope, vec![]));
    }

    pub fn pop_scope(&mut self) {
        assert!(!self.scopes.is_empty());
        for (id, old_ty) in self.current_scope.drain(..) {
            self.env.remove(&id);
            if let Some(old_ty) = old_ty {
                self.env.insert(id, old_ty);
            }
        }
        self.current_scope = self.scopes.pop().unwrap();
    }

    pub fn add(&mut self, var: K, ty: V) {
        let old_ty = self.env.get(&var).cloned();
        self.current_scope.push((var.clone(), old_ty));
        self.env.insert(var, ty);
    }

    pub fn get(&self, var: &K) -> Option<&V> {
        self.env.get(var)
    }
}
