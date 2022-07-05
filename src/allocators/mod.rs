use crate::arch::{Architecture, RegisterSet};

use crate::index::Register;
use crate::optir::{self, FixedArray};

pub fn allocate_registers<A: Architecture>(ir: &optir::IR) -> FixedArray<Register> {
    let register_set = A::register_set();

    

    todo!()
}
