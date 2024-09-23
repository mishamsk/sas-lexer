use std::panic::catch_unwind;
use std::{
    io::Write,
    time::{Duration, Instant},
};

use sas_lexer::{
    error::{ErrorInfo, ErrorType},
    lex, TokenizedBuffer,
};

use crate::print::{print_errors, print_tokens};

pub(crate) fn safe_lex(
    source: &String,
    print_lex_return_errors: bool,
    print_stack_unwind_errors: bool,
) -> Option<(TokenizedBuffer, Vec<ErrorInfo>, Duration)> {
    let start = Instant::now();

    let result = catch_unwind(|| match lex(source) {
        Ok((tok_buffer, errors)) => Some((tok_buffer, errors, start.elapsed())),
        Err(error) => {
            if print_lex_return_errors {
                eprintln!("Error: {error}");
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
    pub(super) print_tokens: bool,
    pub(super) print_token_totals: bool,
    pub(super) print_errors: bool,
    pub(super) print_error_totals: bool,
    pub(super) print_lex_return_errors: bool,
    pub(super) print_stack_unwind_errors: bool,
}

pub(super) fn lex_and_print(source: &String, print_config: LexPrintConfig) -> Option<LexDurations> {
    match safe_lex(
        source,
        print_config.print_lex_return_errors,
        print_config.print_stack_unwind_errors,
    ) {
        Some((tok_buffer, errors, lex_duration)) => {
            let total_tokens = tok_buffer.token_count();
            let (c_int, c_unknown, c_user) =
                errors
                    .iter()
                    .fold((0, 0, 0), |(c_int, c_unknown, c_user), e| {
                        match e.error_type() {
                            e if e.is_internal() => (c_int + 1, c_unknown, c_user),
                            ErrorType::UnexpectedCharacter => (c_int, c_unknown + 1, c_user),
                            _ => (c_int, c_unknown, c_user + 1),
                        }
                    });

            let mut gen_tok_vec_duration = Duration::default();

            if print_config.print_tokens || print_config.print_errors {
                let stdout = std::io::stdout();
                let mut lock = stdout.lock();

                let start = Instant::now();
                let tokens = tok_buffer.into_resolved_token_vec();
                gen_tok_vec_duration = start.elapsed();

                let string_literals_buffer = tok_buffer.string_literals_buffer();

                if print_config.print_tokens && tokens.len() > 0 {
                    writeln!(lock, "Tokens:").unwrap();
                    print_tokens(&mut lock, &tokens, &string_literals_buffer, source);
                }

                if print_config.print_errors && errors.len() > 0 {
                    writeln!(lock, "Errors:").unwrap();
                    print_errors(&mut lock, &errors, &tokens, &string_literals_buffer, source);
                }
            }

            if print_config.print_token_totals {
                println!("Done! Found {total_tokens} tokens.");
            }

            if print_config.print_error_totals {
                if errors.len() > 0 {
                    if c_int > 0 {
                        println!("Internal errors: {c_int}");
                    }

                    if c_unknown > 0 {
                        println!("Unknown characters: {c_unknown}");
                    }

                    if c_user > 0 {
                        println!("User errors: {c_user}");
                    }
                } else {
                    println!("No errors found!");
                }
            }

            Some(LexDurations {
                lex_duration,
                gen_tok_vec_duration,
            })
        }
        None => None,
    }
}
