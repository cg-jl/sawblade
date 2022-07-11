#![deny(unsafe_op_in_unsafe_fn)]
#![feature(is_some_with)]

fn main() {
    let source =
        include_str!("../fuzz/artifacts/sawblade/timeout-817caa6d93dcd5ee0e9762ebdd0d3a783b6efcb0");

    let ast = sawblade::ast::parse_source(source);
    let hlir = sawblade::hlir::IR::<sawblade::arch::X86_64Nasm>::from_ast(ast);
    dbg!(&hlir);
    let optir = sawblade::optir::dissect_from_hlir(hlir.blocks);
    dbg!(optir);
}
