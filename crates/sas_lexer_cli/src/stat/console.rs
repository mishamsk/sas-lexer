use core::f64;

use std::fs::metadata;

use std::{fs, path::PathBuf};

use indicatif::{ProgressBar, ProgressStyle};

use walkdir::WalkDir;

use crate::lex::safe_lex;

#[allow(clippy::cast_possible_truncation)]
pub(super) fn gen_stats_for_console(samples: &PathBuf) {
    let all_files = WalkDir::new(samples)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.path().extension().and_then(|ext| ext.to_str()) == Some("sas"))
        .collect::<Vec<_>>();

    let total_files = all_files.len();
    let mut files_with_errors = 0usize;

    let mut file_bytes = Vec::with_capacity(total_files);
    let mut file_readable = Vec::with_capacity(total_files);
    let mut file_lexed = Vec::with_capacity(total_files);
    let mut file_ws_only_or_empty = Vec::with_capacity(total_files);
    let mut file_lex_duration = Vec::with_capacity(total_files);
    let mut file_max_mode_stack_depth = Vec::with_capacity(total_files);
    let mut file_token_count = Vec::with_capacity(total_files);
    let mut file_error_count = Vec::with_capacity(total_files);
    let mut file_code_error_count = Vec::with_capacity(total_files);

    let pb = ProgressBar::new(total_files as u64);
    pb.set_style(
        ProgressStyle::default_bar()
            .template("{msg} [{bar:40.cyan/blue}] {pos}/{len} ({eta})")
            .unwrap()
            .progress_chars("#>-"),
    );

    pb.set_message("Lexing files");

    for entry in all_files {
        pb.inc(1);

        let entry_path = entry.path();
        let file_size_in_bytes = metadata(entry_path).map_or_else(|_| None, |m| Some(m.len()));

        file_bytes.push(file_size_in_bytes);

        let mut readable = true;
        let mut lexed = true;
        let mut ws_only = false;
        let mut duration = None;
        let mut max_mode_stack_depth = None;
        let mut token_count = None;
        let mut error_count = None;
        let mut code_error_count = None;

        if let Ok(contents) = fs::read_to_string(entry_path) {
            // Skip whitespace only files, including empty files
            if contents.trim().is_empty() {
                ws_only = true;
                lexed = false;
            } else {
                match safe_lex(&contents, false, false) {
                    Some((tok_buffer, errors, dur, stack_depth)) => {
                        token_count = Some(tok_buffer.token_count());
                        error_count = Some(errors.len());
                        code_error_count = Some(
                            errors
                                .iter()
                                .filter(|e| e.error_kind().is_code_error())
                                .count(),
                        );

                        if !errors.is_empty() {
                            files_with_errors += 1;
                        }

                        duration = Some(dur);
                        max_mode_stack_depth = stack_depth.map(|d| d as u64);
                    }
                    None => {
                        lexed = false;
                    }
                }
            }
        } else {
            readable = false;
            lexed = false;
        }

        file_readable.push(readable);
        file_lexed.push(lexed);
        file_ws_only_or_empty.push(ws_only);
        file_lex_duration.push(duration);
        file_max_mode_stack_depth.push(max_mode_stack_depth);
        file_token_count.push(token_count);
        file_error_count.push(error_count);
        file_code_error_count.push(code_error_count);
    }

    let total_readable = file_readable.clone().iter().filter(|&&b| b).count();
    let total_readable_pct = total_readable as f64 / total_files as f64 * 100.0;

    let total_non_empty =
        total_readable - file_ws_only_or_empty.clone().iter().filter(|&&b| b).count();
    let total_non_empty_pct = total_non_empty as f64 / total_files as f64 * 100.0;

    let total_lexed = file_lexed.clone().iter().filter(|&&b| b).count();
    let total_lexed_pct = total_lexed as f64 / total_files as f64 * 100.0;

    let max_mode_stack_depth_str = file_max_mode_stack_depth
        .iter()
        .filter_map(|d| *d)
        .max()
        .map_or("Unknown in release build".to_string(), |d| format!("{d}"));

    pb.finish_with_message(format!(
        "Done. Total files: {total_files}.\nReadable: {total_readable_pct:.2}%.\n\
        Non-empty: {total_non_empty_pct:.2}%.\nLexed: {total_lexed_pct:.2}%.\n\
        Max mode stack depth: {max_mode_stack_depth_str}.",
    ));

    // Calculate throughput
    let file_lex_bytes_throughput = file_bytes
        .into_iter()
        .zip(file_lex_duration.clone())
        .filter_map(|(bytes, duration)| match (bytes, duration) {
            (Some(bytes), Some(duration)) => {
                Some(bytes as f64 / duration.as_secs_f64() / 1024.0 / 1024.0)
            }
            _ => None,
        })
        .collect::<Vec<_>>();

    let file_lex_token_throughput = file_token_count
        .clone()
        .into_iter()
        .zip(file_lex_duration)
        .filter_map(|(tokens, duration)| match (tokens, duration) {
            (Some(tokens), Some(duration)) => {
                Some(tokens as f64 / duration.as_secs_f64() / 10_f64.powf(6.0))
            }
            _ => None,
        })
        .collect::<Vec<_>>();

    let tokens_total: u32 = file_token_count.into_iter().flatten().sum();
    let errors_total: usize = file_error_count.into_iter().flatten().sum();
    let code_errors_total: usize = file_code_error_count.into_iter().flatten().sum();

    println!("Total tokens: {tokens_total}");
    println!("Throughput:");
    println!(
        "Bytes (max): {bmax:.2} MB/s\n\
        Bytes (min): {bmin:.2} MB/s\n\
        Bytes (avg): {mavg:.2} MB/s\n\
        Tokens (max): {tmax:.2} Mil tokens/s\n\
        Tokens (min): {tmin:.2} Mil tokens/s\n\
        Tokens (avg): {tavg:.2} Mil tokens/s",
        bmax = file_lex_bytes_throughput
            .clone()
            .into_iter()
            .reduce(f64::max)
            .unwrap(),
        bmin = file_lex_bytes_throughput
            .clone()
            .into_iter()
            .reduce(f64::min)
            .unwrap(),
        mavg = file_lex_bytes_throughput.into_iter().sum::<f64>() / total_lexed as f64,
        tmax = file_lex_token_throughput
            .clone()
            .into_iter()
            .reduce(f64::max)
            .unwrap(),
        tmin = file_lex_token_throughput
            .clone()
            .into_iter()
            .reduce(f64::min)
            .unwrap(),
        tavg = file_lex_token_throughput.into_iter().sum::<f64>() / total_lexed as f64,
    );

    // Error statistics
    let error_rate = files_with_errors as f64 / total_files as f64 * 100.0;
    println!("Error statistics:");
    println!("User/Total errors: {code_errors_total}/{errors_total}");
    println!("Files with errors: {files_with_errors}/{total_files} ({error_rate:.2}%)");
}
