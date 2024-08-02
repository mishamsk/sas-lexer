#![allow(clippy::print_stderr, clippy::print_stdout)]

use clap::Parser;

use sas_lexer::error::ErrorInfo;
use sas_lexer::lex;
use sas_lexer::print::error_to_string;
use sas_lexer::print::token_to_string;
use sas_lexer::DetachedTokenizedBuffer;
use sas_lexer::TokenIdx;

use std::fs;
use std::io;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "SAS Lexer")]
#[command(version, author, about, long_about = None)]
/// Lex SAS code from a file or stdin
struct Cli {
    /// A file path to read from. If not provided, reads from stdin
    file: Option<PathBuf>,

    /// Print the tokens to the console
    #[arg(short, long)]
    print: bool,

    /// Turn debugging information on
    #[arg(short, long, action = clap::ArgAction::Count)]
    debug: u8,
}

pub fn print_tokens<'a, I, S>(tokens: I, buffer: &DetachedTokenizedBuffer, source: &S)
where
    I: IntoIterator<Item = TokenIdx>,
    S: AsRef<str> + 'a,
{
    for token in tokens {
        println!("{}", token_to_string(token, buffer, source));
    }
}

pub fn print_errors<'a, I, S>(errors: I, buffer: &DetachedTokenizedBuffer, source: &S)
where
    I: IntoIterator<Item = ErrorInfo>,
    S: AsRef<str> + 'a,
{
    for error in errors {
        println!("{}", error_to_string(&error, buffer, source));
    }
}

fn lex_and_print(source: &String, print: bool) {
    match lex(source.as_str()) {
        Ok((tok_buffer, errors)) => {
            let tokens: Vec<TokenIdx> = tok_buffer.into_iter().collect();

            let total_tokens = tokens.len();
            let total_errors = errors.len();

            if print {
                println!("Tokens:");
                print_tokens(tokens, &tok_buffer, source);

                println!("Errors:");
                print_errors(errors, &tok_buffer, source);
            }

            println!("Done! Found {total_tokens} tokens. Had {total_errors} errors!");
        }
        Err(error) => eprintln!("Error: {error}"),
    }
}

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    if let Some(file_path) = cli.file.as_ref() {
        let file_path_str = file_path.to_str().unwrap_or("<invalid path>");

        if let Ok(contents) = fs::read_to_string(file_path) {
            println!("Lexing file: {file_path_str}");

            lex_and_print(&contents, cli.print);
        } else {
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
