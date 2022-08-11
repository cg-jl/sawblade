#![deny(unsafe_op_in_unsafe_fn)]
#![feature(new_uninit)]
#![feature(is_some_with)]
#![feature(allocator_api)]
#![feature(alloc_layout_extra)]

use sawblade::PackedSlice;

struct BoxIntoIter<T, A: core::alloc::Allocator> {
    ptr: *mut T,
    start: usize,
    len: usize,
    alloc: A,
}

impl<T, A: core::alloc::Allocator> BoxIntoIter<T, A> {
    pub fn new(r#box: Box<[T], A>) -> Self {
        let len = r#box.len();
        let (ptr, alloc) = Box::into_raw_with_allocator(r#box);
        let ptr = ptr as *mut T;
        Self {
            ptr,
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
            let ptr = unsafe { self.ptr.add(self.start) };
            self.start += 1;
            Some(unsafe { ptr.read() })
        }
    }
}

impl<T, A: core::alloc::Allocator> Drop for BoxIntoIter<T, A> {
    fn drop(&mut self) {
        unsafe {
            self.alloc.deallocate(
                std::ptr::NonNull::new_unchecked(self.ptr as *mut u8),
                core::alloc::Layout::array::<T>(self.len).unwrap(),
            )
        }
    }
}

use sawblade::arch::Architecture;
fn main() {
    let source = include_str!("../examples/test.sawblade");

    let ast = sawblade::ast::parse_source(source);
    let hlir = sawblade::hlir::IR::<sawblade::arch::X86_64Nasm>::from_ast(ast);
    let optir = sawblade::optir::dissect_from_hlir(hlir.blocks);
    let (registers, ranges) =
        sawblade::allocators::allocate_registers::<sawblade::arch::X86_64Nasm>(&optir, &hlir.specs);

    let mut output = std::io::stdout();

    let (ops, ops_ranges, definitions, ends) = {
        let mut op_ranges = Box::new_uninit_slice(optir.blocks.len());
        let total_op_count = optir
            .blocks
            .iter()
            .map(|block| block.operations.len())
            .enumerate()
            .fold(0, |start, (index, len)| {
                let end = start + len;
                op_ranges[index].write(start..end);
                end
            });

        let op_ranges = unsafe { op_ranges.assume_init() };
        let mut definitions =
            Box::new_uninit_slice(optir.blocks.iter().map(|b| b.binding_defs.len()).sum());

        {
            let mut current_block_offset = 0usize;
            let mut current_op_offset = 0u32;
            for block in &optir.blocks {
                block
                    .binding_defs
                    .iter()
                    .map(|def| def.index() as u32 + current_op_offset)
                    .zip(definitions.iter_mut().skip(current_block_offset as usize))
                    .for_each(|(src, target)| {
                        target.write(src);
                    });
                current_block_offset += block.binding_defs.len();
                current_op_offset += block.operations.len() as u32;
            }
        }

        let mut all_ops = Box::new_uninit_slice(total_op_count);
        let mut all_ends = Box::new_uninit_slice(optir.blocks.len());

        for (ops, range) in optir
            .blocks
            .into_iter()
            .enumerate()
            .map(|(index, block)| {
                all_ends[index].write(block.end);
                block.operations
            })
            .zip(op_ranges.iter().cloned())
        {
            let slice = &mut all_ops[range];
            for (index, op) in BoxIntoIter::new(ops).enumerate() {
                slice[index].write(op);
            }
        }

        (
            unsafe { all_ops.assume_init() },
            op_ranges,
            unsafe { definitions.assume_init() },
            unsafe { all_ends.assume_init() },
        )
    };

    let label_map = {
        let mut map = Box::new_uninit_slice(hlir.label_map.export_count as usize);
        hlir.label_map
            .labels
            .into_iter()
            .filter(|(_label, index)| *index < hlir.label_map.export_count)
            .for_each(|(label, index)| {
                map[index as usize].write(label);
            });
        unsafe { map.assume_init() }
    };

    sawblade::arch::X86_64Nasm::assemble(
        PackedSlice {
            elements: &ops,
            ranges: &ops_ranges,
        },
        PackedSlice {
            elements: &registers,
            ranges: &ranges,
        },
        &definitions,
        &ends,
        &label_map,
        &mut output,
    )
    .unwrap();
}
