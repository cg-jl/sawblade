#![allow(unused)]
use crate::arch::{Architecture, RegisterSet};

use crate::hlir::Spec;
use crate::index::Register;
use crate::optir::{self, FixedArray};

pub fn allocate_registers<A: Architecture>(ir: &optir::IR, spec: &Spec<A>) -> FixedArray<Register> {
    let register_set = A::register_set();

    // TODO: sort the blocks in dependency order so that the most constrained are processed first
    // and hints are given to the leaves:
    //  - converts bindings from "local space" (block) to "world space"
    //  - grabs collisions between bindings of each block
    //  - grabs calls to other blocks (args/returns)
    //  - assigns forced ABI allocations for their blocks, emitting the block index and argument index
    //  - resolves minor allocations (noncolliding, collisions without any special bindings)
    //  - assigns Argument/Return allocation hints
    //  - resolves 'call collisions' for each argument/return:
    //    - grabs intersection of available non-repeating registers from each block
    //    - if intersection is null, then grab the next least-used register for the blocks
    //    - assign that register to that argument/return, registering it in the regspecs of each block

    todo!()
}
