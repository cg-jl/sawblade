#![no_main]
use sawblade::ast::parse_source;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &str| {
    // fuzzed code goes here
    let _ = parse_source(data);
});
