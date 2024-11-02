use sas_lexer::{error::ErrorInfo, lex_program, LexResult, TokenizedBuffer};
use std::panic::catch_unwind;
use std::{
    io::Write,
    time::{Duration, Instant},
};
use strum::EnumMessage;

use crate::print::{print_errors, print_tokens};

pub(crate) fn safe_lex(
    source: &String,
    print_lex_return_errors: bool,
    print_stack_unwind_errors: bool,
) -> Option<(TokenizedBuffer, Vec<ErrorInfo>, Duration, Option<usize>)> {
    let start = Instant::now();

    let result = catch_unwind(|| match lex_program(source) {
        Ok(LexResult {
            buffer: tok_buffer,
            errors,
            max_mode_stack_depth,
        }) => Some((
            tok_buffer,
            errors,
            start.elapsed(),
            Some(max_mode_stack_depth),
        )),
        Err(error) => {
            if print_lex_return_errors {
                eprintln!("Error: {}", error.get_message().unwrap_or("Unknown error"));
            }
            None
        }
    });

    match result {
        Err(err) => {
            if print_stack_unwind_errors {
                if let Some(s) = err.downcast_ref::<&str>() {
                    println!("Panic occurred while lexing: {s}");
                } else {
                    println!("Panic occurred, but can't read the message");
                };
            }

            None
        }
        Ok(v) => v,
    }
}

pub(super) struct LexDurations {
    pub(super) lex_duration: Duration,
    pub(super) gen_tok_vec_duration: Duration,
}

pub(super) struct LexPrintConfig {
    pub(super) print_file_name: bool,
    pub(super) print_tokens: bool,
    pub(super) print_token_totals: bool,
    pub(super) print_errors: bool,
    pub(super) print_error_totals: bool,
    pub(super) print_lex_return_errors: bool,
    pub(super) print_stack_unwind_errors: bool,
    pub(super) context_lines: Option<usize>,
}

pub(super) fn lex_and_print(
    source: &String,
    print_config: LexPrintConfig,
    file_name: Option<String>,
) -> Option<LexDurations> {
    let source_str = if let Some(file_name) = file_name {
        format!("file: {file_name}")
    } else {
        "from stdin".to_string()
    };

    if print_config.print_file_name {
        println!("Lexing {source_str}");
    }

    match safe_lex(
        source,
        print_config.print_lex_return_errors,
        print_config.print_stack_unwind_errors,
    ) {
        Some((tok_buffer, errors, lex_duration, _)) => {
            // If file name printing weren't directly requested but any of the
            // other print options are enabled and conditions meant, print the file name
            if !print_config.print_file_name
                && (print_config.print_tokens
                    || print_config.print_token_totals
                    || ((print_config.print_errors || print_config.print_error_totals)
                        && !errors.is_empty()))
            {
                println!("Lexing {source_str}");
            }

            let total_tokens = tok_buffer.token_count();
            let (c_int, c_user) =
                errors
                    .iter()
                    .fold((0, 0), |(c_int, c_user), e| match e.error_kind() {
                        e if e.is_internal() => (c_int + 1, c_user),
                        _ => (c_int, c_user + 1),
                    });

            let mut gen_tok_vec_duration = Duration::default();

            if print_config.print_tokens || print_config.print_errors {
                let stdout = std::io::stdout();
                let mut lock = stdout.lock();

                let start = Instant::now();
                let tokens = tok_buffer.into_resolved_token_vec();
                gen_tok_vec_duration = start.elapsed();

                let string_literals_buffer = tok_buffer.string_literals_buffer();

                if print_config.print_tokens && !tokens.is_empty() {
                    writeln!(lock, "Tokens:").unwrap();
                    print_tokens(&mut lock, &tokens, string_literals_buffer, source);
                }

                if print_config.print_errors && !errors.is_empty() {
                    writeln!(lock, "Errors:").unwrap();
                    print_errors(
                        &mut lock,
                        &errors,
                        &tokens,
                        string_literals_buffer,
                        source,
                        print_config.context_lines,
                    );
                }
            }

            if print_config.print_token_totals {
                println!("Done! Found {total_tokens} tokens.");
            }

            if print_config.print_error_totals && !errors.is_empty() {
                if c_int > 0 {
                    println!("Internal errors: {c_int}");
                }

                if c_user > 0 {
                    println!("User errors: {c_user}");
                }
            }

            Some(LexDurations {
                lex_duration,
                gen_tok_vec_duration,
            })
        }
        None => {
            if !print_config.print_file_name
                && (print_config.print_lex_return_errors || print_config.print_stack_unwind_errors)
            {
                println!("During lexing {source_str}");
            }

            None
        }
    }
}
