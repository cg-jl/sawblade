//! Low Level IR. Contains instructions in a simple, <instruction> <target> <input(s)> format,
//! so it can be lowered with ease to the particular architecture. Note that there are no constant
//! besidse

use crate::index::Register;
use crate::optir::{CFTransfer, Constant};
use crate::PackedSlice;
pub enum Op {
    /// set register a to b
    CopyRegister { target: u8, source: u8 },
    /// Set a constant to a register.
    SetValue { target: u8, value: Constant },
    /// Add two unsigned numbers
    UAdd { target: u8, lhs: u8, rhs: Input },

    /// Call label
    Call { label: crate::index::Label },

    /// Return.
    Ret,
}

pub struct IR {
    pub ops: Box<[Op]>,
    pub label_offsets: Box<[u16]>,
    pub label_names: Box<[String]>,
}

impl IR {
    #[inline(always)]
    pub fn from_optir(
        ir: crate::optir::IR,
        exported_labels: &[&str],
        registers: PackedSlice<Register>,
    ) -> IR {
        optir_to_llir(ir, exported_labels, registers)
    }
}

pub enum Input {
    Constant(Constant),
    Register(u8),
}

#[repr(u8)]
pub enum ByteSize {
    U8,
    U16,
    U32,
    U64,
}

fn optir_to_llir(ir: crate::optir::IR, label_map: &[&str], registers: PackedSlice<Register>) -> IR {
    let label_count = ir.blocks.len();
    let mut label_offsets = crate::InitVec::new(label_count);
    let op_count = ir.blocks.iter().map(|block| block.operations.len()).sum();
    // we might need more space for return adjustments.
    let mut ops = Vec::with_capacity(op_count);

    for (block_index, block) in ir.blocks.iter().enumerate() {
        label_offsets.push(ops.len() as u16);
        // NOTE: currently using op index to get the associated binding . But when stores
        // come then not all operations will be associated to bindings... Since stores don't have a
        // local result.
        for (op_index, op) in block.operations.iter().enumerate() {
            let binding_index = op_index + block.arg_count;
            match op {
                crate::optir::Op::Constant(c) => ops.push(Op::SetValue {
                    target: unsafe {
                        registers.elements[binding_index + registers.ranges[block_index].start]
                            .as_index()
                    },
                    value: *c,
                }),
                crate::optir::Op::Call {
                    label,
                    args,
                    usage_info_index,
                } => {
                    let target_block_index = unsafe { label.to_index() } as usize;
                    // 1. Line up argument registers
                    {
                        let currents = args.iter().map(|arg| {
                            registers.elements[unsafe { arg.to_index() } as usize
                                + registers.ranges[block_index].start]
                        });

                        let targets = registers.elements[registers.ranges[block_index].start..]
                            [..ir.blocks[target_block_index].arg_count]
                            .iter()
                            .copied();

                        line_up_registers(currents, targets, &mut ops);
                    }
                    // 2. Make the call
                    ops.push(Op::Call { label: *label });

                    let usage_info = &block.call_return_usages[*usage_info_index];

                    // 3. Make sure the returns are in the right place.
                    if !usage_info.result_usage.is_empty() {
                        let currents = usage_info.result_binding_range.clone().map(|binding| {
                            registers.elements[unsafe { binding.to_index() } as usize
                                + registers.ranges[block_index].start]
                        });

                        // SAFE: we checked that result_usage is not empty.
                        let max_ret_index = unsafe {
                            usage_info
                                .result_usage
                                .iter()
                                .copied()
                                .max()
                                .unwrap_unchecked()
                        };

                        // the selected return block must have at LEAST max_ret_index values to
                        // return.
                        let selected_return_block_index = ir.return_blocks[target_block_index]
                            .iter()
                            .copied()
                            .map(|index| unsafe { index.to_index() } as usize)
                            .find(|index| ir.return_counts[*index] > max_ret_index)
                            .expect("this call should return the needed values");

                        let targets = if let CFTransfer::Return =
                            &ir.blocks[selected_return_block_index].end
                        {
                            usage_info.result_usage.iter()
                                .copied()
                                .map(|ret_index| ir.blocks[selected_return_block_index].exported_bindings[ret_index as usize])
                                .map(|binding| unsafe { binding.to_index() } as usize + registers.ranges[selected_return_block_index].start).map(|global_index| registers.elements[global_index])
                        } else {
                            unreachable!("blocks marked as return blocks HAVE to return")
                        };

                        line_up_registers(currents, targets, &mut ops);
                    }
                }
                crate::optir::Op::Add { lhs, rhs } => {
                    let lhs = unsafe {
                        registers.elements
                            [lhs.to_index() as usize + registers.ranges[block_index].start]
                            .as_index()
                    };

                    let rhs = unsafe {
                        registers.elements
                            [rhs.to_index() as usize + registers.ranges[block_index].start]
                            .as_index()
                    };

                    let target = unsafe {
                        registers.elements[binding_index + registers.ranges[block_index].start]
                            .as_index()
                    };

                    ops.push(Op::UAdd {
                        target,
                        lhs,
                        rhs: Input::Register(rhs),
                    });
                }
            }
        }

        match &block.end {
            CFTransfer::Return => ops.push(Op::Ret),
            CFTransfer::DirectBranch { .. } => todo!(),
            CFTransfer::ConditionalBranch { .. } => todo!(),
        }
    }

    let label_names = {
        let mut names = Box::new_uninit_slice(ir.blocks.len());
        names
            .iter_mut()
            .skip(label_map.len())
            .enumerate()
            .for_each(|(index, name)| {
                name.write(format!(".BB{}", index));
            });
        names.iter_mut().zip(label_map).for_each(|(name, label)| {
            name.write(label.to_string());
        });
        unsafe { names.assume_init() }
    };

    IR {
        ops: ops.into_boxed_slice(),
        label_offsets: label_offsets.finish(),
        label_names,
    }
}

impl crate::DependencyTracker<Register> for Register {
    fn contains_dependency(&self, dep: &Register) -> bool {
        self == dep
    }

    fn remove_dependency(&mut self, _: Register) -> Register {
        *self
    }
}

// strong recommendation to inline, but giving the option to hold
// back for some (more complex) situations, so that more analysis can
// pour through without adding too much time to the analysis.
#[inline]
fn line_up_registers(
    current: impl IntoIterator<Item = Register>,

    target: impl IntoIterator<Item = Register>,
    assembly: &mut Vec<Op>,
) {
    // build a map of which registers have to be moved
    let dependency_map = current.into_iter().zip(target).collect();

    let mut dep_order = crate::DependencyOrderIter::new(dependency_map);

    for (current, target) in &mut dep_order {
        if current != target {
            assembly.push(Op::CopyRegister {
                target: unsafe { target.as_index() },
                source: unsafe { current.as_index() },
            })
        }
    }

    // for (current, target) in &mut dep_order {
    //     // assembly.push(AssemblyOp::Mov {
    //     //     dest: target,
    //     //     source: DataSource::Register(current),
    //     // })
    // }
}
