#![allow(clippy::print_stderr, clippy::print_stdout)]

use clap::Parser;
use std::io::Write;

use sas_lexer::error::ErrorInfo;
use sas_lexer::lex;
use sas_lexer::print::error_to_string;
use sas_lexer::print::token_to_string;
use sas_lexer::TokenIdx;
use sas_lexer::TokenizedBuffer;
use walkdir::WalkDir;

use std::fs;
use std::io;
use std::panic::catch_unwind;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "SAS Lexer")]
#[command(version, author, about, long_about = None)]
/// Lex SAS code from a file or stdin
struct Cli {
    /// A file or folder path to read from. If not provided, reads from stdin.
    /// If a folder is provided, reads all files with the `.sas` extension.
    file_or_dir: Option<PathBuf>,

    /// Print the tokens to the console. Ignored if a folder is provided.
    #[arg(short, long)]
    print: bool,

    /// Turn debugging information on
    #[arg(short, long, action = clap::ArgAction::Count)]
    debug: u8,
}

pub fn print_tokens<'a, I, S>(dst: &mut impl Write, tokens: I, buffer: &TokenizedBuffer, source: &S)
where
    I: IntoIterator<Item = TokenIdx>,
    S: AsRef<str> + 'a,
{
    for token in tokens {
        writeln!(dst, "{}", token_to_string(token, buffer, source)).unwrap();
    }
}

pub fn print_errors<'a, I, S>(dst: &mut impl Write, errors: I, buffer: &TokenizedBuffer, source: &S)
where
    I: IntoIterator<Item = ErrorInfo>,
    S: AsRef<str> + 'a,
{
    for error in errors {
        writeln!(dst, "{}", error_to_string(&error, buffer, source)).unwrap();
    }
}

fn lex_and_print(source: &String, print: bool) {
    let result = catch_unwind(|| match lex(source) {
        Ok((tok_buffer, errors)) => {
            let tokens: Vec<TokenIdx> = tok_buffer.into_iter().collect();

            let total_tokens = tokens.len();
            let total_errors = errors.len();

            if print {
                let stdout = std::io::stdout();
                let mut lock = stdout.lock();

                writeln!(lock, "Tokens:").unwrap();
                print_tokens(&mut lock, tokens, &tok_buffer, source);

                if errors.len() > 0 {
                    writeln!(lock, "Errors:").unwrap();
                    print_errors(&mut lock, errors, &tok_buffer, source);
                }
            }

            println!("Done! Found {total_tokens} tokens. Had {total_errors} errors!");
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

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    if let Some(source_path) = cli.file_or_dir.as_ref() {
        if source_path.is_dir() {
            for entry in WalkDir::new(source_path).into_iter().filter_map(Result::ok) {
                let entry_path = entry.path();
                if entry_path.extension().and_then(|ext| ext.to_str()) == Some("sas") {
                    if let Ok(contents) = fs::read_to_string(entry_path) {
                        println!("Lexing file: {}", entry_path.display());
                        lex_and_print(&contents, false); // Always pass false for cli.print
                    } else {
                        eprintln!("Failed to read file: {}", entry_path.display());
                    }
                }
            }
        } else if let Ok(contents) = fs::read_to_string(source_path) {
            let file_path_str = source_path.to_str().unwrap_or("<invalid path>");

            println!("Lexing file: {file_path_str}");

            lex_and_print(&contents, cli.print);
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
                    lex_and_print(&buffer, cli.print);

                    buffer.clear();
                }
                Ok(_) => {}
                Err(error) => return Err(error),
            }
        }
    }

    Ok(())
}
