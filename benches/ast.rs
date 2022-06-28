use criterion::{black_box, Criterion, criterion_main, criterion_group};

pub fn test_sample(c: &mut Criterion) {
    let sample = include_str!("../examples/test.abism");

    c.bench_function("test sample", |b| {
        b.iter(|| abism::ast::parse_source(black_box(sample)))
    });
}

criterion_group!(benches, test_sample);
criterion_main!(benches);
