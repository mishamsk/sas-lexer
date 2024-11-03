use std::path::PathBuf;

#[cfg(feature = "polars")]
mod pl;

#[cfg(not(feature = "polars"))]
mod console;

#[cfg(feature = "polars")]
pub(super) fn gen_stats(samples: &PathBuf, output: &Option<PathBuf>, error_context_lines: usize) {
    if let Err(err) = pl::gen_stats_with_polars(output, samples, error_context_lines) {
        eprintln!("Failed to generate stats: {err:?}");
    }
}

#[cfg(not(feature = "polars"))]
pub(super) fn gen_stats(samples: &PathBuf) {
    console::gen_stats_for_console(samples);
}
