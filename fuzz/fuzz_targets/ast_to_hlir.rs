#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: Vec<sawblade::ast::Block>| {
    // fuzzed code goes here
    let _ = sawblade::hlir::IR::<sawblade::arch::X86_64Nasm>::from_ast(data);
});
