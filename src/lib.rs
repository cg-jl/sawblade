#![deny(unsafe_op_in_unsafe_fn)]
#![feature(trusted_len)]
#![feature(new_uninit)]
#![feature(allocator_api)]
#![feature(is_some_and)]
#![feature(binary_heap_into_iter_sorted)]
#![feature(unchecked_math)]
#![feature(slice_as_chunks)]
#![feature(maybe_uninit_uninit_array)]
#![feature(maybe_uninit_array_assume_init)]
#![feature(ptr_internals)]

use std::{
    collections::{HashMap, HashSet},
    ops::Range,
};
pub mod allocators;
pub mod arch;
pub mod ast;
pub mod hlir;
pub mod index;
pub mod optir;

#[derive(Debug)]
pub struct PackedSlice<'elt, 'r, T> {
    pub elements: &'elt [T],
    pub ranges: &'r [Range<usize>],
}

pub trait DependencyTracker<K> {
    fn contains_dependency(&self, dep: &K) -> bool;
    fn remove_dependency(&mut self, dep: K) -> K;
    fn is_self_contained(&self) -> bool {
        true
    }
}

#[macro_export]
macro_rules! impl_self_dependency_eq {
    ($type:ident) => {
        impl $crate::DependencyTracker<$type> for $type {
            fn contains_dependency(&self, dep: &Self) -> bool {
                self == dep
            }
            fn remove_dependency(&mut self, dep: Self) -> Self {
                dep
            }
        }
    };
    ($type:ident, deps: $($type_deps:ident),+) => {
        impl <$($type_deps),+ > DependencyTracker<$type> for $type {
            fn contains_dependency(&self, dep: &Self) -> bool {
                self == dep
            }
        }
    };
    ($type:ident, deps: $($type_deps:ident),+, constraints: $($constraints:tt),+) => {
        impl <$($type_deps),+ > DependencyTracker<$type> for $type
            where
                $type: ::core::cmp::PartialEq,$($constraints),+
        {
            fn contains_dependency(&self, dep: &Self) -> bool {
                self == dep
            }
        }
    };
}

pub struct DependencyOrderIter<K, V> {
    inner_map: HashMap<K, V>,
}

pub struct DependencyOrderIterWithSet<T> {
    inner_map: HashMap<T, HashSet<T>>,
}

impl<T> DependencyOrderIterWithSet<T> {
    pub fn new(map: HashMap<T, HashSet<T>>) -> Self {
        Self { inner_map: map }
    }

    pub fn release(self) -> HashMap<T, HashSet<T>> {
        self.inner_map
    }
}

impl<T: core::hash::Hash + core::cmp::Eq + Copy + Clone> Iterator
    for DependencyOrderIterWithSet<T>
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let next_key = self
            .inner_map
            .keys()
            .copied()
            .find(|k| self.inner_map.values().find(|v| v.contains(k)).is_none())?;

        self.inner_map.remove(&next_key);
        Some(next_key)
    }
}

impl<K: core::hash::Hash + core::cmp::Eq + Copy + Clone, V: DependencyTracker<K>> Iterator
    for DependencyOrderIter<K, V>
{
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        // find the value that is not key to the same map.
        let next_key = self.inner_map.keys().copied().find(|k| {
            !self
                .inner_map
                .values()
                .find(|v| v.contains_dependency(k))
                .is_some()
        })?;

        self.inner_map.remove_entry(&next_key)
    }
}

impl<K, V> DependencyOrderIter<K, V> {
    pub fn new(map: HashMap<K, V>) -> Self {
        Self { inner_map: map }
    }

    // release the inner hashmap. This is useful in situations where the iterator is exhausted
    // but there might be elements inside it as there might be a dependency loop.
    pub fn release(self) -> HashMap<K, V> {
        self.inner_map
    }
}

pub struct BoxIntoIter<T, A: core::alloc::Allocator> {
    ptr: core::ptr::Unique<T>,
    start: usize,
    len: usize,
    alloc: A,
}

impl<T, A: core::alloc::Allocator> BoxIntoIter<T, A> {
    pub fn new(r#box: Box<[T], A>) -> Self {
        let len = r#box.len();
        let (ptr, alloc) = Box::into_raw_with_allocator(r#box);
        Self {
            ptr: unsafe { core::ptr::Unique::new_unchecked(ptr) }.cast(),
            start: 0,
            len,
            alloc,
        }
    }
}
impl<T, A: core::alloc::Allocator> Iterator for BoxIntoIter<T, A> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start == self.len {
            None
        } else {
            let ptr = unsafe { self.ptr.as_ptr().add(self.start) };
            self.start += 1;
            Some(unsafe { ptr.read() })
        }
    }
}

impl<T, A: core::alloc::Allocator> Drop for BoxIntoIter<T, A> {
    fn drop(&mut self) {
        // drop the set of values that weren't given by the iterator.
        // SAFE: [..self.start] is not owned by us anymore, but [self.start..] is.
        unsafe {
            let undropped_values = core::slice::from_raw_parts_mut(
                self.ptr.as_ptr().add(self.start),
                self.len - self.start,
            );
            core::ptr::drop_in_place(undropped_values);
        };

        unsafe {
            self.alloc.deallocate(
                std::ptr::NonNull::new_unchecked(self.ptr.cast().as_ptr()),
                core::alloc::Layout::array::<T>(self.len).unwrap(),
            )
        }
    }
}
