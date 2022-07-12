#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: Vec<sawblade::hlir::Block>| {
    if let Ok(data) = sawblade::hlir::check_arbitary_blocks(data) {
        let _ = sawblade::optir::dissect_from_hlir(data);
    }
});
