use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use pprof::criterion::{Output, PProfProfiler};
use sas_lexer::lex_program;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;
use zip::ZipArchive;

fn read_sas_files_from_zip<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let file = File::open(path)?;
    let mut archive = ZipArchive::new(file)?;
    let mut combined_source = String::new();

    for i in 0..archive.len() {
        let mut file = archive.by_index(i)?;
        if file.name().ends_with(".sas") {
            let mut contents = Vec::new();
            file.read_to_end(&mut contents)?;
            if let Ok(contents) = String::from_utf8(contents) {
                combined_source.push_str(&contents)
            }
        }
    }

    Ok(combined_source)
}

fn benchmark_lex(c: &mut Criterion) {
    let source = read_sas_files_from_zip("benches/enlighten-apply-code-only.zip")
        .expect("Failed to read SAS files from zip");

    let source_len = source.len() as u64;

    println!("Source length: {}KB", source_len as f64 / 1024.0);

    c.benchmark_group("lex")
        .throughput(Throughput::Bytes(source_len))
        .bench_function("lex", |b| {
            b.iter(|| {
                lex_program(&source).expect("Lexing failed");
            })
        });
}

criterion_group! {
    name = benches;
    config = Criterion::default()
        .sample_size(60)
        .with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    targets = benchmark_lex
}
criterion_main!(benches);
