use crate::arch::{Architecture, RegisterCapabilities};

use crate::index::Register;
use crate::optir::{self, FixedArray};

pub fn allocate_registers<A: Architecture>(ir: &optir::IR) -> FixedArray<Register> {
    let capabilities_set = A::capabilities_set();

    

    todo!()
}
