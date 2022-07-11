use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};

const SAMPLE: &str = include_str!("../examples/test.sawblade");

pub fn parse_sample(c: &mut Criterion) {
    let mut group = c.benchmark_group("Sample");
    group.sample_size(SAMPLE.len());
    group.throughput(criterion::Throughput::Bytes(SAMPLE.len() as u64));
    group.bench_with_input(BenchmarkId::new("input", SAMPLE.len()), SAMPLE, |b, i| {
        b.iter(|| sawblade::ast::parse_source(i));
    });
    group.finish();
}

pub fn hlir_sample(c: &mut Criterion) {
    let source = sawblade::ast::parse_source(SAMPLE);
    c.bench_function("ast to HLIR (x64 registers)", |b| {
        b.iter(|| sawblade::hlir::IR::<sawblade::arch::X86_64Nasm>::from_ast(black_box(source.clone())))
    });
}

pub fn optir_sample(c: &mut Criterion) {
    let ir = sawblade::hlir::IR::<sawblade::arch::X86_64Nasm>::from_ast(sawblade::ast::parse_source(SAMPLE));
    c.bench_function("HLIR to OPTIR", |b| b.iter(|| {
        let blocks = ir.blocks.clone();
        sawblade::optir::dissect_from_hlir(black_box(blocks))
    }));
}

criterion_group!(benches, parse_sample, hlir_sample, optir_sample);
criterion_main!(benches);
