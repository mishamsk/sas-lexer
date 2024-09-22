use std::io::Write;
use std::panic::catch_unwind;

use sas_lexer::{
    error::{ErrorInfo, ErrorType},
    lex, TokenizedBuffer,
};

use crate::print::{print_errors, print_tokens};

pub(crate) fn safe_lex(source: &String) -> Option<(TokenizedBuffer, Vec<ErrorInfo>)> {
    let result = catch_unwind(|| match lex(source) {
        Ok((tok_buffer, errors)) => Some((tok_buffer, errors)),
        Err(error) => {
            eprintln!("Error: {error}");
            None
        }
    });

    match result {
        Err(err) => {
            if let Some(s) = err.downcast_ref::<&str>() {
                println!("Panic occurred while lexing: {s}");
            } else {
                println!("Panic occurred, but can't read the message");
            };

            None
        }
        Ok(v) => v,
    }
}

pub(super) fn lex_and_print(source: &String, print: bool, err_only: bool) {
    match safe_lex(source) {
        Some((tok_buffer, errors)) => {
            let tokens = tok_buffer.into_resolved_token_vec();
            let string_literals_buffer = tok_buffer.string_literals_buffer();

            let total_tokens = tokens.len();
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

            if print {
                let stdout = std::io::stdout();
                let mut lock = stdout.lock();

                if !err_only {
                    writeln!(lock, "Tokens:").unwrap();
                    print_tokens(&mut lock, &tokens, &string_literals_buffer, source);
                }

                if errors.len() > 0 {
                    writeln!(lock, "Errors:").unwrap();
                    print_errors(&mut lock, &errors, &tokens, &string_literals_buffer, source);
                }
            }

            if !err_only || errors.len() > 0 {
                println!("Done! Found {total_tokens} tokens.");

                if c_int > 0 {
                    println!("Internal errors: {c_int}");
                }

                if c_unknown > 0 {
                    println!("Unknown characters: {c_unknown}");
                }

                if c_user > 0 {
                    println!("User errors: {c_user}");
                }
            }
        }
        None => {}
    };
}
