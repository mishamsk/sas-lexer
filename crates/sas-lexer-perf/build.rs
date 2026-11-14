//! Sets `cfg(perf_base)` when `just perf-check` builds this harness against the BASE lexer.
//!
//! This is deliberately not a Cargo feature: workspace checks run with `--all-features`
//! (clippy, udeps) and would then compile the base-only adapter code against HEAD.

fn main() {
    println!("cargo::rustc-check-cfg=cfg(perf_base)");
    println!("cargo::rerun-if-env-changed=SAS_LEXER_PERF_BASE");
    if std::env::var_os("SAS_LEXER_PERF_BASE").is_some_and(|v| v == "1") {
        println!("cargo::rustc-cfg=perf_base");
    }
}
