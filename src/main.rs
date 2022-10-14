#![deny(unsafe_op_in_unsafe_fn)]
#![feature(new_uninit)]
#![feature(is_some_and)]
#![feature(allocator_api)]
#![feature(alloc_layout_extra)]
#![feature(trusted_len)]

use sawblade::PackedSlice;

use sawblade::arch::Architecture;
use sawblade::optir::CFTransfer;
fn main() {
    let source = include_str!("../examples/test.sawblade");

    let ast = sawblade::ast::parse_source(source);
    let hlir = sawblade::hlir::IR::<sawblade::arch::X86_64Nasm>::from_ast(ast);
    let optir = sawblade::optir::dissect_from_hlir(hlir.blocks);
    let (registers, register_ranges) =
        sawblade::allocators::allocate_registers::<sawblade::arch::X86_64Nasm>(&optir, &hlir.specs);

    let mut output = std::io::stdout();

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
        optir,
        PackedSlice {
            elements: &registers,
            ranges: &register_ranges,
        },
        &label_map,
        &mut output,
    )
    .unwrap();
}
