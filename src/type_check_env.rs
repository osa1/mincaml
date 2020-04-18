use std::collections::HashMap;
use std::mem::replace;

use crate::type_check::Type;

pub(crate) struct Locals {
    env: HashMap<String, Type>,
    // Type below is the Type we override, not the type of the binder in the current scope!
    current_scope: Vec<(String, Option<Type>)>,
    scopes: Vec<Vec<(String, Option<Type>)>>,
}

impl Locals {
    pub(crate) fn new(globals: HashMap<String, Type>) -> Locals {
        Locals {
            env: globals,
            current_scope: vec![],
            scopes: vec![],
        }
    }

    pub(crate) fn new_scope(&mut self) {
        self.scopes.push(replace(&mut self.current_scope, vec![]));
    }

    pub(crate) fn pop_scope(&mut self) {
        assert!(!self.scopes.is_empty());
        for (id, old_ty) in self.current_scope.drain(..) {
            self.env.remove(&id);
            if let Some(old_ty) = old_ty {
                self.env.insert(id, old_ty);
            }
        }
        self.current_scope = self.scopes.pop().unwrap();
    }

    pub(crate) fn add(&mut self, id: String, ty: Type) {
        let old_ty = self.env.get(&id).cloned();
        self.current_scope.push((id.clone(), old_ty));
        self.env.insert(id, ty);
    }

    pub(crate) fn get(&self, id: &str) -> Option<&Type> {
        self.env.get(id)
    }
}
