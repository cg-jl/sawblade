#![allow(unused)]
use crate::arch::{Architecture, RegisterSet};

use crate::hlir::Spec;
use crate::index::{self, Register, RegisterRange};
use crate::optir::{self, FixedArray, Op};
use std::collections::HashSet;
use std::mem::MaybeUninit;
use std::ops::Range;

type BindingMap<T> = std::collections::HashMap<index::Binding, T>;

fn resolve_allocs_from_spec<A>(
    spec: &crate::hlir::Spec<A>,
    block: &optir::Block,
    registers: &mut [MaybeUninit<Register>],
) {
    for op in block.operations.iter() {
        if let Op::Call { args, .. } = op {
            for (binding, register) in args.iter().copied().zip(spec.arguments.iter().copied()) {
                registers[unsafe { binding.to_index() } as usize].write(register);
            }
        }
    }

    match &block.end {
        optir::CFTransfer::Return(bindings) => {
            for (binding, register) in bindings.iter().copied().zip(spec.returns.iter().copied()) {
                registers[unsafe { binding.to_index() } as usize].write(register);
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
    gp_registers: RegisterRange,
    target_registers: &mut [MaybeUninit<Register>],
) {
    // NOTE: not using scan() because it uses Some() as a scan_while
    let mut allocator = GPAllocator::new(gp_registers);

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
    let (block_ranges, mut collision_map) = {
        let mut block_indices = Vec::with_capacity(ir.blocks.len());

        let total_binding_size =
            ir.blocks
                .iter()
                .map(|block| block.binding_defs.len())
                .fold(0, |total, len| {
                    block_indices.push(total..total + len);
                    total + len
                });
        let mut collision_map = Vec::with_capacity(total_binding_size as usize);

        for (index, block) in ir.blocks.iter().enumerate() {
            let len = block_indices[index].len();
            collision_map.extend((0..len).map(|_| HashSet::with_capacity(len as usize)));
        }

        (
            block_indices.into_boxed_slice(),
            collision_map.into_boxed_slice(),
        )
    };

    let mut registers = Box::new_uninit_slice(collision_map.len());

    // NOTE: currently just using collision-based allocation
    for (index, (range, block)) in block_ranges.iter().zip(ir.blocks.iter()).enumerate() {
        compute_lifetime_collisions(block, &mut collision_map[range.clone()]);
        resolve_allocs_from_spec(&spec[index], block, &mut registers[range.clone()]);
        resolve_general_purpose_allocations(
            &collision_map[range.clone()],
            register_set.gp_registers,
            &mut registers[range.clone()],
        );
    }

    (unsafe { registers.assume_init() }, block_ranges)
}
