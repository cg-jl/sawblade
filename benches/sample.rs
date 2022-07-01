use criterion::{black_box, criterion_group, criterion_main, Criterion};

const SAMPLE: &str = include_str!("../examples/test.abism");

pub fn parse_sample(c: &mut Criterion) {
    c.bench_function("test sample", |b| {
        b.iter(|| abism::ast::parse_source(black_box(SAMPLE)))
    });
}

criterion_group!(benches, parse_sample);
criterion_main!(benches);
