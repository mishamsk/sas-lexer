#![allow(clippy::print_stderr, clippy::print_stdout)]

use clap::Parser;

use sas_lexer::lex;
use sas_lexer::print::to_pretty_string;
use sas_lexer::TokenIdx;
use sas_lexer::TokenizedBuffer;

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

pub fn print_tokens<I>(tokens: I, buffer: &TokenizedBuffer)
where
    I: IntoIterator<Item = TokenIdx>,
{
    for token in tokens {
        println!("{}", to_pretty_string(token, buffer));
    }
}

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    if let Some(file_path) = cli.file.as_ref() {
        let file_path_str = file_path.to_str().unwrap_or("<invalid path>");

        if let Ok(contents) = fs::read_to_string(file_path) {
            println!("Lexing file: {file_path_str}");

            match lex(contents.as_str()) {
                Ok(tok_buffer) => {
                    let tokens: Vec<TokenIdx> = tok_buffer.into_iter().collect();
                    println!("Done! Found {} tokens", tokens.len());

                    if cli.print {
                        print_tokens(tokens, &tok_buffer);
                    }
                }
                Err(error) => eprintln!("Error: {error}"),
            }
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
                    match lex(buffer.as_str()) {
                        Ok(tok_buffer) => {
                            if cli.print {
                                print_tokens(&tok_buffer, &tok_buffer);
                            } else {
                                let tokens: Vec<TokenIdx> = tok_buffer.into_iter().collect();
                                println!("Done! Found {} tokens", tokens.len());
                            }
                        }
                        Err(error) => eprintln!("Error: {error}"),
                    }

                    buffer.clear();
                }
                Ok(_) => {}
                Err(error) => return Err(error),
            }
        }
    }

    Ok(())
}
