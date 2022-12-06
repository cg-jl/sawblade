//! Low Level IR. Contains instructions in a simple, <instruction> <target> <input(s)> format,
//! so it can be lowered with ease to the particular architecture. Note that there are no constant
//! besidse

use crate::hlir::Condition;
use crate::index::{Label, Register};
use crate::optir::{Block, CFTransfer, Constant};
use crate::PackedSlice;
#[derive(Debug)]
pub enum Op {
    /// set register a to b
    CopyRegister {
        target: u8,
        source: u8,
    },
    /// Set a constant to a register.
    SetValue {
        target: u8,
        value: Constant,
    },
    /// Add two unsigned numbers
    UAdd {
        target: u8,
        lhs: u8,
        rhs: Input,
    },
    USub {
        target: u8,
        lhs: u8,
        rhs: Input,
    },

    /// Call label
    Call {
        label: crate::index::Label,
    },

    /// Return.
    Ret,

    /// Branch from a set of flags
    CBranch {
        condition: Condition,
        target: Label,
    },

    /// Branch unconditionally
    Branch {
        target: Label,
    },
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

#[derive(Debug)]
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
    let mut label_offsets = Vec::with_capacity(label_count);
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

                    align_outgoing_registers(
                        *label,
                        &ir.blocks,
                        block_index,
                        args.iter().map(|arg| unsafe { arg.to_index() }),
                        registers,
                        &mut ops,
                    );
                    // 2. Make the call
                    ops.push(Op::Call { label: *label });

                    let usage_info = &block.call_return_usages[*usage_info_index];

                    // 3. Make sure the returns are in the right place.
                    if !usage_info.result_usage.is_empty() {
                        let currents =
                            usage_info
                                .result_binding_range
                                .clone()
                                .into_iter()
                                .map(|binding| {
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
                crate::optir::Op::Sub { lhs, rhs } => {
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

                    ops.push(Op::USub {
                        target,
                        lhs,
                        rhs: Input::Register(rhs),
                    });
                }
                crate::optir::Op::FetchFlags(_) => (),
            }
        }

        match &block.end {
            CFTransfer::Return => ops.push(Op::Ret),
            CFTransfer::DirectBranch { .. } => todo!(),
            CFTransfer::ConditionalBranch {
                stored_condition,
                flag_definition: _, // XXX: I have no idea what to use this for here. Maybe
                // reordering?
                target_if_true,
                target_if_false,
                true_branch_binding_count,
            } => {
                let true_branch_bindings =
                    &block.exported_bindings[..*true_branch_binding_count as usize];
                let false_branch_bindings =
                    &block.exported_bindings[*true_branch_binding_count as usize..];

                // first, know the instructions that are needed to line up each of the bindings
                // with the required registers.

                let true_branch_adjust_block = {
                    let mut ops = Vec::new();

                    align_outgoing_registers(
                        *target_if_true,
                        &ir.blocks,
                        block_index,
                        true_branch_bindings
                            .iter()
                            .copied()
                            .map(|b| unsafe { b.to_index() }),
                        registers,
                        &mut ops,
                    );
                    ops
                };

                // NOTE: we don't need a separate block for adjusting registers for the true branch
                // since we'll make an unconditional branch anyway.

                // put a dummy op here to register the index we'll push the true branch first and
                // then adjust the other branch
                let op_offset = ops.len();

                ops.push(Op::Ret); // <- dummy, we'll update this when we have the correct label.

                // align our false branch registers
                align_outgoing_registers(
                    *target_if_false,
                    &ir.blocks,
                    block_index,
                    false_branch_bindings
                        .iter()
                        .copied()
                        .map(|b| unsafe { b.to_index() }),
                    registers,
                    &mut ops,
                );

                ops.push(Op::Branch {
                    target: *target_if_false,
                });

                // if there's some space needed for adjusting registers for the 'true' branch,
                // we'll have to make another
                let branch_label = if true_branch_adjust_block.is_empty() {
                    *target_if_true
                } else {
                    let label_index = label_offsets.len() as u16;
                    label_offsets.push(ops.len() as u16);
                    unsafe { Label::from_index(label_index) }
                };

                ops[op_offset] = Op::CBranch {
                    condition: *stored_condition,
                    target: branch_label,
                };
            }
        }
    }

    let label_names = {
        let mut names = Box::new_uninit_slice(label_offsets.len());
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

        names
            .iter_mut()
            .skip(ir.blocks.len())
            .enumerate()
            .for_each(|(index, name)| {
                name.write(format!(".aux{}", index));
            });

        unsafe { names.assume_init() }
    };

    IR {
        ops: ops.into_boxed_slice(),
        label_offsets: label_offsets.into_boxed_slice(),
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

/// This one is just a way to avoid repeating code.
#[inline(always)]
fn align_outgoing_registers(
    target: Label,
    blocks: &[Block],
    block_index: usize,
    args: impl IntoIterator<Item = u16>,
    registers: PackedSlice<Register>,
    ops: &mut Vec<Op>,
) {
    let target_block_index = unsafe { target.to_index() };
    let current_block_register_start = registers.ranges[block_index].start;
    let currents = args
        .into_iter()
        .map(|ix| registers.elements[ix as usize + current_block_register_start]);

    let targets = registers.elements[current_block_register_start..]
        [..blocks[target_block_index as usize].arg_count]
        .iter()
        .copied();

    line_up_registers(currents, targets, ops);
}
