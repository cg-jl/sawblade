#![deny(unsafe_op_in_unsafe_fn)]
#![feature(is_some_with)]
use std::fs;

fn main() {
    let source = fs::read_to_string("examples/test.abism").unwrap();

    let ast = abism::ast::parse_source(&source);
    dbg!(&ast);
    let ir = abism::ir::IR::<abism::ir::X86_64Nasm>::from_ast(ast);
    dbg!(ir);
}
