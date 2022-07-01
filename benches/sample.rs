use criterion::{black_box, criterion_group, criterion_main, Criterion};

const SAMPLE: &str = include_str!("../examples/test.abism");

pub fn parse_sample(c: &mut Criterion) {
    c.bench_function("parse sample", |b| {
        b.iter(|| abism::ast::parse_source(black_box(SAMPLE)))
    });
}

pub fn hlir_sample(c: &mut Criterion) {
    let source = abism::ast::parse_source(SAMPLE);
    c.bench_function("ast to HLIR (x64 registers)", |b| {
        b.iter(|| abism::hlir::IR::<abism::arch::X86_64Nasm>::from_ast(black_box(source.clone())))
    });
}

criterion_group!(benches, parse_sample, hlir_sample);
criterion_main!(benches);
