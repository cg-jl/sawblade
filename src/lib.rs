#![deny(unsafe_op_in_unsafe_fn)]
#![feature(trusted_len)]
#![feature(new_uninit)]
#![feature(allocator_api)]
#![feature(step_trait)]
#![feature(is_some_with)]
#![feature(binary_heap_into_iter_sorted)]

use std::ops::Range;
pub mod allocators;
pub mod arch;
pub mod ast;
pub mod hlir;
pub mod index;
pub mod optir;

pub struct PackedSlice<'elt, 'r, T> {
    pub elements: &'elt [T],
    pub ranges: &'r [Range<usize>],
}
