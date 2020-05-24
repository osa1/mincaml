//! A double-ended iterator over entity references and entities.

use super::EntityRef;
use std::iter::Enumerate;
use std::marker::PhantomData;
use std::slice;

/// Iterate over all keys in order.
pub struct Iter<'a, K: EntityRef, V>
where
    V: 'a,
{
    enumerate: Enumerate<slice::Iter<'a, V>>,
    unused: PhantomData<K>,
}

impl<'a, K: EntityRef, V> Iter<'a, K, V> {
    /// Create an `Iter` iterator that visits the `PrimaryMap` keys and values
    /// of `iter`.
    pub fn new(iter: slice::Iter<'a, V>) -> Self {
        Self {
            enumerate: iter.enumerate(),
            unused: PhantomData,
        }
    }
}

impl<'a, K: EntityRef, V> Iterator for Iter<'a, K, V> {
    type Item = (K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        self.enumerate.next().map(|(i, v)| (K::new(i), v))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.enumerate.size_hint()
    }
}

impl<'a, K: EntityRef, V> DoubleEndedIterator for Iter<'a, K, V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.enumerate.next_back().map(|(i, v)| (K::new(i), v))
    }
}

impl<'a, K: EntityRef, V> ExactSizeIterator for Iter<'a, K, V> {}

/// Iterate over all keys in order.
pub struct IterMut<'a, K: EntityRef, V>
where
    V: 'a,
{
    enumerate: Enumerate<slice::IterMut<'a, V>>,
    unused: PhantomData<K>,
}

impl<'a, K: EntityRef, V> IterMut<'a, K, V> {
    /// Create an `IterMut` iterator that visits the `PrimaryMap` keys and values
    /// of `iter`.
    pub fn new(iter: slice::IterMut<'a, V>) -> Self {
        Self {
            enumerate: iter.enumerate(),
            unused: PhantomData,
        }
    }
}

impl<'a, K: EntityRef, V> Iterator for IterMut<'a, K, V> {
    type Item = (K, &'a mut V);

    fn next(&mut self) -> Option<Self::Item> {
        self.enumerate.next().map(|(i, v)| (K::new(i), v))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.enumerate.size_hint()
    }
}

impl<'a, K: EntityRef, V> DoubleEndedIterator for IterMut<'a, K, V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.enumerate.next_back().map(|(i, v)| (K::new(i), v))
    }
}

impl<'a, K: EntityRef, V> ExactSizeIterator for IterMut<'a, K, V> {}
