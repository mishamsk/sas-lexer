#![allow(clippy::print_stderr, clippy::print_stdout)]

mod print;
mod stat;

use clap::Parser;
use clap::Subcommand;
use convert_case::Boundary;
use convert_case::Case;
use convert_case::Converter;
use sas_lexer::error::ErrorType;
use sas_lexer::ResolvedTokenInfo;
use sas_lexer::TokenType;
use std::io::Write;
use strum::EnumCount;
use strum::IntoEnumIterator;

use print::error_to_string;
use print::token_to_string;
use sas_lexer::error::ErrorInfo;
use sas_lexer::lex;
use walkdir::WalkDir;

use std::fs;
use std::io;
use std::panic::catch_unwind;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "SAS Lexer")]
#[command(version, author, about, long_about = None)]
/// Utilities for SAS lexer crate
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Turn debugging information on. Unused currently.
    #[arg(short, long, action = clap::ArgAction::Count)]
    debug: u8,
}

#[derive(Subcommand)]
enum Commands {
    /// Lex SAS code from a file or stdin and optionally print tokens and errors
    Lex {
        /// A file or folder path to read from. If not provided, reads from stdin.
        /// If a folder is provided, reads all files with the `.sas` extension.
        file_or_dir: Option<PathBuf>,

        /// Print the tokens to the console. Ignored if a folder is provided.
        #[arg(short, long)]
        print: bool,

        /// Print only errors and totals only if errors were found.
        #[arg(short, long)]
        err_only: bool,
    },
    /// Generate ANTLR .tokens file
    Gen {
        /// The path to output the .tokens file to. If not provided, writes to stdout.
        grammar: Option<PathBuf>,
    },
}

pub fn print_tokens(
    dst: &mut impl Write,
    tokens: &Vec<ResolvedTokenInfo>,
    string_literals_buffer: &str,
    source: &str,
) {
    for token in tokens {
        writeln!(
            dst,
            "{}",
            token_to_string(token, string_literals_buffer, source)
        )
        .unwrap();
    }
}

pub fn print_errors(
    dst: &mut impl Write,
    errors: &Vec<ErrorInfo>,
    tokens: &Vec<ResolvedTokenInfo>,
    string_literals_buffer: &str,
    source: &str,
) {
    for error in errors {
        writeln!(
            dst,
            "{}",
            error_to_string(&error, tokens, string_literals_buffer, source)
        )
        .unwrap();
    }
}

fn lex_and_print(source: &String, print: bool, err_only: bool) {
    let result = catch_unwind(|| match lex(source) {
        Ok((tok_buffer, errors)) => {
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
        Err(error) => eprintln!("Error: {error}"),
    });

    if let Err(err) = result {
        if let Some(s) = err.downcast_ref::<&str>() {
            println!("Panic occurred while lexing: {s}");
        } else {
            println!("Panic occurred, but can't read the message");
        }
    }
}

fn generate_tokens_file() -> String {
    let mut result = String::with_capacity(TokenType::COUNT * 40);
    let conv = Converter::new()
        .from_case(Case::Pascal)
        .remove_boundaries(&[Boundary::LowerDigit, Boundary::UpperDigit])
        .to_case(Case::UpperSnake);

    for token in TokenType::iter().filter(|t| *t != TokenType::EOF) {
        result.push_str(conv.convert(token.to_string()).as_str());
        result.push('=');
        result.push_str((token as u16).to_string().as_str());
        result.push('\n');
    }
    result
}

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Lex {
            file_or_dir,
            print,
            err_only,
        } => {
            if let Some(source_path) = file_or_dir.as_ref() {
                if source_path.is_dir() {
                    for entry in WalkDir::new(source_path).into_iter().filter_map(Result::ok) {
                        let entry_path = entry.path();
                        if entry_path.extension().and_then(|ext| ext.to_str()) == Some("sas") {
                            if let Ok(contents) = fs::read_to_string(entry_path) {
                                println!("Lexing file: {}", entry_path.display());
                                lex_and_print(&contents, false, *err_only); // Always pass false for cli.print
                            } else {
                                eprintln!("Failed to read file: {}", entry_path.display());
                            }
                        }
                    }
                } else if let Ok(contents) = fs::read_to_string(source_path) {
                    let file_path_str = source_path.to_str().unwrap_or("<invalid path>");

                    println!("Lexing file: {file_path_str}");

                    lex_and_print(&contents, *print, *err_only);
                } else {
                    let file_path_str = source_path.to_str().unwrap_or("<invalid path>");

                    eprintln!("Failed to read file: {file_path_str}");
                }
            } else {
                println!("Enter input (Ctrl-D to submit, Ctrl-C to exit):");

                let mut buffer = String::new();
                loop {
                    match io::stdin().read_line(&mut buffer) {
                        Ok(0) => {
                            println!("Lexing from stdin...");
                            lex_and_print(&buffer, *print, *err_only);

                            buffer.clear();
                        }
                        Ok(_) => {}
                        Err(error) => return Err(error),
                    }
                }
            }
        }
        Commands::Gen { grammar } => {
            let tokens_file = generate_tokens_file();

            if let Some(grammar_path) = grammar {
                fs::write(grammar_path, tokens_file)?;
            } else {
                println!("{tokens_file}");
            }
        }
    }

    Ok(())
}
