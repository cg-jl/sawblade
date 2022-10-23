#![allow(unused)]
use crate::arch::{Architecture, RegisterSet};

use crate::hlir::Spec;
use crate::index::{self, Register, RegisterRange};
use crate::optir::{self, FixedArray, Op};
use std::collections::HashSet;
use std::mem::MaybeUninit;
use std::ops::Range;

struct SpilledReturn {
    binding: u16,
    index: u16,
}

struct BindingSet {
    inner_set: HashSet<FullBinding>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct FullBinding {
    block: u16,
    binding: u16,
}

impl core::hash::Hash for FullBinding {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u16(self.block);
        state.write_u16(self.binding);
    }
}

impl FullBinding {}

impl BindingSet {
    fn wrap_binding(block: u16, binding: u16) -> u32 {
        (block as u32) << 16 | binding as u32
    }

    fn get_block<'s>(&'s self, block_index: u16) -> BindingSetBlock<'s> {
        BindingSetBlock {
            block_index,
            r#ref: &self.inner_set,
        }
    }

    fn get_block_mut<'s>(&'s mut self, block_index: u16) -> BindingSetBlockMut<'s> {
        BindingSetBlockMut {
            block_index,
            r#ref: &mut self.inner_set,
        }
    }
}

struct BindingSetBlockMut<'a> {
    block_index: u16,
    r#ref: &'a mut HashSet<FullBinding>,
}

impl<'a> BindingSetBlockMut<'a> {
    fn wrap_binding(&self, binding: u16) -> u32 {
        BindingSet::wrap_binding(self.block_index, binding)
    }

    fn extend(&mut self, it: impl IntoIterator<Item = u16>) {
        let block = self.block_index;
        self.r#ref
            .extend(it.into_iter().map(|binding| FullBinding { block, binding }))
    }

    fn insert(&mut self, binding: u16) -> bool {
        self.r#ref.insert(FullBinding {
            block: self.block_index,
            binding,
        })
    }

    fn contains(&self, binding: u16) -> bool {
        self.r#ref.contains(&FullBinding {
            block: self.block_index,
            binding,
        })
    }
}

struct BindingSetBlock<'a> {
    block_index: u16,
    r#ref: &'a HashSet<FullBinding>,
}

impl<'a> BindingSetBlock<'a> {
    fn wrap_binding(&self, binding: u16) -> u32 {
        BindingSet::wrap_binding(self.block_index, binding)
    }

    fn contains(&self, binding: u16) -> bool {
        self.r#ref.contains(&FullBinding {
            block: self.block_index,
            binding,
        })
    }
}

struct ActiveBindingSet {
    bindings: Vec<u16>,
}

// set of bindings that keeps them sorted by binding index.
impl ActiveBindingSet {
    fn add(&mut self, binding: u16, binding_ends: &[u16]) {
        let binding_end = binding_ends[binding as usize];
        for i in 0..self.bindings.len() {
            if binding_ends[self.bindings[i] as usize] > binding_end {
                self.bindings.insert(i, binding);
                return;
            }
        }
        self.bindings.push(binding);
    }

    fn last(&self) -> Option<u16> {
        self.bindings.last().copied()
    }

    fn contains(&self, binding: u16) -> bool {
        self.bindings.contains(&binding)
    }

    fn remove(&mut self, binding: u16) -> bool {
        let found = self.bindings.iter().position(|other| other == &binding);

        if let Some(i) = found {
            self.bindings.remove(i);
            true
        } else {
            false
        }
    }

    const fn new() -> Self {
        Self {
            bindings: Vec::new(),
        }
    }

    fn len(&self) -> usize {
        self.bindings.len()
    }
}

fn linear_alloc_block<'alloc_bindings, 'spilled_bindings>(
    registers: &mut [MaybeUninit<Register>],
    allocated_bindings: BindingSetBlock<'alloc_bindings>,
    register_set: RegisterSet,
    starts: &[u16],
    ends: &[u16],
    ordered_bindings_by_start: &[u16],
    ordered_bindings_by_end: &[u16],
    mut spills: BindingSetBlockMut<'spilled_bindings>,
) {
    debug_assert!(
        register_set.gp_registers.len() > 0,
        "no registers to allocate from!"
    );
    let mut active = ActiveBindingSet::new();
    let mut pool: HashSet<_> = register_set.gp_registers.collect();
    for binding in ordered_bindings_by_start.iter().copied() {
        let start = starts[binding as usize];
        // expire old intervals
        let end_i = active
            .bindings
            .iter()
            .copied()
            .enumerate()
            .find(|(i, binding)| ends[*binding as usize] > start)
            .map(|(i, _)| i)
            .unwrap_or(active.bindings.len());

        for dropped_binding in active.bindings.drain(..end_i) {
            debug_assert!(
                pool.insert(unsafe { registers[dropped_binding as usize].assume_init() }),
                "register should have been in use!"
            );
        }

        // if it's known, skip the handling of a register.
        let already_known_register = if allocated_bindings.contains(binding) {
            Some(unsafe { registers[binding as usize].assume_init() })
        } else {
            None
        };

        // try to find an available register.
        let unused_register = already_known_register.or_else(|| {
            register_set
                .gp_registers
                .clone()
                .find(|register| pool.contains(register))
        });

        let found_register = unused_register.or_else(|| {
            // SAFE: we know that there must be at least one active binding
            let longest_lived = unsafe { active.last().unwrap_unchecked() };
            if ends[longest_lived as usize] > ends[binding as usize] {
                // spill the longest lived and allocate this one
                active.remove(longest_lived);
                spills.insert(longest_lived);
                let reg = unsafe { registers[longest_lived as usize].assume_init() };
                pool.insert(reg);
                Some(reg)
            } else {
                None
            }
        });

        if let Some(register) = found_register {
            debug_assert!(
                pool.remove(&register),
                "register should not have been in use"
            );
            registers[binding as usize].write(register);
            active.add(binding, ends);
        } else {
            spills.insert(binding);
        }
    }
}

fn resolve_allocs_from_spec<'alloc_bindings, A>(
    spec: &[crate::hlir::Spec<A>],
    block_index: usize,
    block: &optir::Block,
    allocator: &mut GPAllocator,
    registers: &mut [MaybeUninit<Register>],
    mut allocated_bindings: BindingSetBlockMut<'alloc_bindings>,
    spilled_returns: &mut [Option<u16>],
) {
    for (index, register) in (0..block.arg_count).zip(&spec[block_index].arguments) {
        registers[index].write(*register);
    }

    allocated_bindings.extend((0..spec[block_index].arguments.len() as u16));

    match &block.end {
        optir::CFTransfer::Return(bindings) => {
            for (index, (binding, register)) in bindings
                .iter()
                .copied()
                .zip(spec[block_index].returns.iter().copied())
                .enumerate()
            {
                let index = index as u16;
                let binding = unsafe { binding.to_index() };
                // don't try to reallocate. Returns are easy to reallocate since upon return th
                if !allocated_bindings.contains(binding) {
                    registers[binding as usize].write(register);
                } else {
                    spilled_returns[binding as usize] = Some(binding);
                }
            }
        }
        _ => {}
    }
}

#[derive(Debug, Clone, Copy)]
struct Lifetime {
    pub start: u16,
    pub end: optir::bucket::UsageIndex,
}

impl Lifetime {
    #[inline(always)]
    pub fn collides_with(self, other: Lifetime) -> bool {
        (self.start >= other.start && other.end > self.start)
            || (other.start >= self.start && self.end < other.start)
    }
}

#[derive(Clone, Copy)]
struct Tracker(u32);

impl Tracker {
    const fn new(index: u16, value: u16) -> Self {
        Self((index as u32) << 16 | value as u32)
    }
    const fn value(&self) -> u16 {
        (self.0 & 0xFFFF) as u16
    }
    fn index(&self) -> u16 {
        (unsafe { self.0.unchecked_shr(16) }) as u16
    }

    // transforms the array of trackers to an array of indices.
    // Allocates since we would be wasting half of our allocated space.
    fn drop_values(trackers: Box<[Tracker]>) -> Box<[u16]> {
        let mut values = Box::new_uninit_slice(trackers.len());

        for (value, target) in trackers.iter().copied().zip(values.iter_mut()) {
            target.write(value.index());
        }

        unsafe { values.assume_init() }
    }
}

fn compute_lifetimes(
    block: &optir::Block,
    starts: &mut [MaybeUninit<u16>],
    ends: &mut [MaybeUninit<u16>],
) {
    for (usage, target) in block
        .binding_usages
        .iter()
        .map(|usages| {
            usages
                .iter()
                .max_by_key(|usage| usage.index)
                .expect("all bindings must be used")
                .index
        })
        .zip(ends)
    {
        let end = usage.as_index().unwrap_or(block.operations.len() as u16);
        target.write(end);
    }

    for (value, target) in block
        .binding_defs
        .iter()
        .copied()
        .map(optir::bucket::Definition::index)
        .zip(starts)
    {
        target.write(value);
    }
}

// TODO: don't collide selective usages that match selection buckets.
fn compute_lifetime_collisions(block: &optir::Block, collisions: &mut [HashSet<index::Binding>]) {
    let ends = block.binding_usages.iter().map(|usages| {
        usages
            .iter()
            .min_by_key(|usage| usage.index)
            .expect("all bindings must be used")
            .index
    });

    // NOTE: len() - 1 should not panic here: blocks with nothing to do are discarded.
    let lifetimes = block.binding_defs[..block.binding_defs.len()]
        .iter()
        .copied()
        .map(optir::bucket::Definition::index)
        .zip(ends)
        .map(|(start, end)| Lifetime { end, start })
        .collect::<FixedArray<_>>();

    for index in (0..(block.binding_defs.len() - 1) as u16).rev() {
        let binding = unsafe { index::Binding::from_index(index) };
        let self_lifetime = &lifetimes[index as usize];

        collisions[index as usize].extend(
            (0..lifetimes.len() as u16)
                .zip(lifetimes.iter())
                .skip(index as usize + 1)
                .filter(|(_, other_lifetime)| self_lifetime.collides_with(**other_lifetime))
                .map(|(index, _)| unsafe { index::Binding::from_index(index) }),
        );
    }
}

struct GPAllocator {
    buckets: FixedArray<Vec<index::Binding>>,
    gp_registers: RegisterRange,
}

impl GPAllocator {
    pub fn new(gp_registers: RegisterRange) -> Self {
        Self {
            buckets: vec![Vec::new(); gp_registers.len() as usize].into_boxed_slice(),
            gp_registers,
        }
    }

    unsafe fn assign_allocation(&mut self, binding: index::Binding, register: Register) {
        if self.gp_registers.contains(register) {
            self.buckets[(unsafe { register.as_index() } - self.gp_registers.start) as usize]
                .push(binding);
        }
    }

    fn allocate(
        &mut self,
        binding: index::Binding,
        collisions: &HashSet<index::Binding>,
    ) -> Option<Register> {
        for (index, bucket) in (0..self.buckets.len() as u8).zip(self.buckets.iter_mut()) {
            if bucket.iter().all(|binding_in_bucket| {
                collisions
                    .iter()
                    .find(|other_binding| *other_binding == binding_in_bucket)
                    .is_none()
            }) {
                bucket.push(binding);
                return Some(unsafe { self.gp_registers.get_unchecked(index) });
            }
        }
        None
    }
}

fn resolve_general_purpose_allocations(
    collisions: &[HashSet<index::Binding>],
    allocator: &mut GPAllocator,
    target_registers: &mut [MaybeUninit<Register>],
) {
    // NOTE: not using scan() because it uses Some() as a scan_while

    for index in (0..collisions.len() as u16) {
        let register = allocator
            .allocate(
                unsafe { index::Binding::from_index(index) },
                &collisions[index as usize],
            )
            .expect("should get a general purpose register");
        target_registers[index as usize].write(register);
    }
}

struct BlockIndices {
    start: u32,
    len: u16,
}

// returns an array of registres and an array of the register range per block
pub fn allocate_registers<A: Architecture>(
    ir: &optir::IR,
    spec: &[Spec<A>],
) -> (FixedArray<Register>, FixedArray<Range<usize>>) {
    let register_set = A::register_set();

    // TODO: sort the blocks in dependency order so that the most constrained are processed first
    // and hints are given to the leaves:
    //  - converts bindings from "local space" (block) to "world space" (TODO: make OPTIR be
    //  already constructed for this)
    //  - grabs collisions between bindings of each block
    //  - grabs calls to other blocks (args/returns)
    //  - assigns forced ABI allocations for their blocks, emitting the block index and argument index
    //  - resolves minor allocations (noncolliding, collisions without any special bindings)
    //  - assigns Argument/Return allocation hints
    //  - resolves 'call collisions' for each argument/return:
    //    - grabs intersection of available non-repeating registers from each block
    //    - if intersection is null, then grab the next least-used register for the blocks
    //    - assign that register to that argument/return, registering it in the regspecs of each block

    // pre-allocate the collision map
    let (block_ranges, total_binding_size) = {
        let mut block_indices = Vec::with_capacity(ir.blocks.len());

        let total_binding_size =
            ir.blocks
                .iter()
                .map(|block| block.binding_defs.len())
                .fold(0, |total, len| {
                    block_indices.push(total..total + len);
                    total + len
                });

        (block_indices.into_boxed_slice(), total_binding_size)
    };

    let mut registers = Box::new_uninit_slice(total_binding_size);

    let mut allocator = GPAllocator::new(register_set.gp_registers);

    let (mut spilled_returns, spilled_return_ranges) = {
        let mut ranges = Box::new_uninit_slice(ir.blocks.len());

        let total_arg_count = ir
            .blocks
            .iter()
            .map(|block| block.arg_count)
            .enumerate()
            .fold(0, |start, (index, count)| {
                let end = start + count;
                ranges[index].write(start..end);
                end
            });

        (vec![None; total_arg_count].into_boxed_slice(), unsafe {
            ranges.assume_init()
        })
    };

    let mut allocated_bindings = BindingSet {
        inner_set: HashSet::new(),
    };

    for (index, range) in block_ranges.iter().cloned().enumerate() {
        resolve_allocs_from_spec(
            &spec,
            index,
            &ir.blocks[index],
            &mut allocator,
            &mut registers[range.clone()],
            allocated_bindings.get_block_mut(index as u16),
            &mut spilled_returns[spilled_return_ranges[index].clone()],
        );
    }
    // TODO: for the new assembly format, generate more instructions to move
    // spilled returns to their respective registers
    assert!(
        spilled_returns.iter().all(Option::is_none),
        "there is no infrastructure to support re-moving returns to their respective next usages"
    );

    // starts, ends could be a single allocation...
    let lifetime_data = {
        let mut all = Box::new_uninit_slice(total_binding_size * 4);
        {
            let [starts, ends, start_bindings, end_bindings] = chunkify_exact_mut(&mut all);

            for (index, range) in block_ranges.iter().cloned().enumerate() {
                {
                    let end_bindings = &mut end_bindings[range.clone()];
                    let start_bindings = &mut start_bindings[range.clone()];
                    for index in 0..range.len() {
                        end_bindings[index].write(index as u16);
                        start_bindings[index].write(index as u16);
                    }
                }
                compute_lifetimes(
                    &ir.blocks[index],
                    &mut starts[range.clone()],
                    &mut ends[range.clone()],
                );
            }
        }
        let mut lifetime_data = unsafe { all.assume_init() };

        // sort the bindings based on their start/end
        {
            let [starts, ends, start_bindings, end_bindings] =
                chunkify_exact_mut(&mut lifetime_data);
            for (index, range) in block_ranges.iter().cloned().enumerate() {
                let start_bindings = &mut start_bindings[range.clone()];
                let end_bindings = &mut end_bindings[range.clone()];
                let starts = &mut starts[range.clone()];
                let ends = &mut ends[range];

                start_bindings.sort_by_key(|index| starts[*index as usize]);
                end_bindings.sort_by_key(|index| ends[*index as usize]);
            }
        }

        lifetime_data
    };

    let mut spilled_bindings = BindingSet {
        inner_set: HashSet::new(),
    };
    {
        let [starts, ends, ordered_bindings_by_start, ordered_bindings_by_end] =
            chunkify_exact(&lifetime_data);
        for (index, range) in block_ranges.iter().cloned().enumerate() {
            let ordered_bindings_by_start = &ordered_bindings_by_start[range.clone()];
            let ordered_bindings_by_end = &ordered_bindings_by_end[range.clone()];
            let starts = &starts[range.clone()];
            let ends = &ends[range.clone()];
            let registers = &mut registers[range];

            linear_alloc_block(
                registers,
                allocated_bindings.get_block(index as u16),
                register_set,
                starts,
                ends,
                ordered_bindings_by_start,
                ordered_bindings_by_end,
                spilled_bindings.get_block_mut(index as u16),
            )
        }
    }

    assert!(
        spilled_bindings.inner_set.is_empty(),
        "should have no spills right now"
    );

    // FIXME: register allocator does not take into account that the return of one block is the
    // same as the result of the call. So if allocating a register for call is not the same as where
    // the block returns, it's flawed. Currently this move is automatically managed by the
    // assembler. It also doesn't handle the case where the call clobbers registers.
    // Perhaps the returns from calls should not be handled by the linear scan? That would leave a
    // gap that we might want to fill in with another pass... But how?

    // allocate memory for each block for the spilled values
    // ... and communicate this to the assembler:
    // - use the stack register/frame pointer to flag them as a load from memory into a (scratch?)
    // register

    (unsafe { registers.assume_init() }, block_ranges)
}

fn chunkify_exact<const N: usize, T>(slice: &[T]) -> [&[T]; N] {
    assert_eq!(
        slice.len() % N,
        0,
        "slice length should be aligned to the exact chunks!"
    );
    let per_chunk_len = slice.len() / N;

    let mut array = MaybeUninit::uninit_array();

    let mut ptr = slice.as_ptr();
    for target in array.iter_mut() {
        target.write(unsafe { core::slice::from_raw_parts(ptr, per_chunk_len) });
        ptr = unsafe { ptr.add(per_chunk_len) };
    }

    unsafe { MaybeUninit::array_assume_init(array) }
}
fn chunkify_exact_mut<const N: usize, T>(slice: &mut [T]) -> [&mut [T]; N] {
    assert_eq!(
        slice.len() % N,
        0,
        "slice length should be aligned to the exact chunks!"
    );
    let per_chunk_len = slice.len() / N;

    let mut array = MaybeUninit::uninit_array();

    let mut ptr = slice.as_mut_ptr();
    for target in array.iter_mut() {
        target.write(unsafe { core::slice::from_raw_parts_mut(ptr, per_chunk_len) });
        ptr = unsafe { ptr.add(per_chunk_len) };
    }

    unsafe { MaybeUninit::array_assume_init(array) }
}
