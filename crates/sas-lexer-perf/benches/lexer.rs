//! Callgrind (instruction count) benchmarks used as the CI performance regression gate.
//!
//! Run via `just perf-bench` / `just perf-check` (Linux + valgrind + gungraun-runner).
//! Input loading/generation happens in `setup` and is not counted.

// gungraun's `main!` exits the process with the benchmark status.
#![allow(clippy::exit)]

use std::hint::black_box;

use gungraun::{
    library_benchmark, library_benchmark_group, main, Callgrind, LibraryBenchmarkConfig,
};
use sas_lexer_perf::{
    api, real_corpus, synthetic_comments, synthetic_datalines, synthetic_macro, synthetic_numerics,
    synthetic_strings,
};

// `lex_program` alone: the core lexer hot path.
#[library_benchmark]
#[bench::real(setup = real_corpus)]
#[bench::macros(setup = synthetic_macro)]
#[bench::strings(setup = synthetic_strings)]
#[bench::comments(setup = synthetic_comments)]
#[bench::datalines(setup = synthetic_datalines)]
#[bench::numerics(setup = synthetic_numerics)]
fn lex(source: String) -> usize {
    black_box(api::lex(black_box(&source)))
}

// `lex_program` plus resolving every token, as the Python binding does.
#[library_benchmark]
#[bench::real(setup = real_corpus)]
fn lex_resolve(source: String) -> usize {
    black_box(api::lex_resolve(black_box(&source)))
}

library_benchmark_group!(name = lexer, benchmarks = [lex, lex_resolve]);

main!(
    // Cache simulation adds the cache metrics and `EstimatedCycles`.
    config = LibraryBenchmarkConfig::default().tool(Callgrind::with_args(["--cache-sim=yes"]));
    library_benchmark_groups = lexer
);
