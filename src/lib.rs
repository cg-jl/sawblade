#![deny(unsafe_op_in_unsafe_fn)]
#![feature(trusted_len)]
#![feature(new_uninit)]
#![feature(allocator_api)]
#![feature(step_trait)]
#![feature(generic_const_exprs)]
#![feature(is_some_with)]
pub mod arch;
pub mod ast;
pub mod hlir;
pub mod index;
pub mod optir;
pub mod allocators;
