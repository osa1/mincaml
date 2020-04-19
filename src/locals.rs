use std::collections::HashMap;
use std::mem::replace;

pub struct Locals<A> {
    env: HashMap<String, A>,
    // `A` below is the `A` we override, not the `A` of the binder in the current scope!
    current_scope: Vec<(String, Option<A>)>,
    scopes: Vec<Vec<(String, Option<A>)>>,
}

impl<A: Clone> Locals<A> {
    pub fn new(globals: HashMap<String, A>) -> Locals<A> {
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

    pub fn add(&mut self, id: String, ty: A) {
        let old_ty = self.env.get(&id).cloned();
        self.current_scope.push((id.clone(), old_ty));
        self.env.insert(id, ty);
    }

    pub fn get(&self, id: &str) -> Option<&A> {
        self.env.get(id)
    }
}
