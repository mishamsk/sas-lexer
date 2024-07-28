use clap::Parser;

use sas_lexer::lex;
use sas_lexer::print::print_tokens;
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

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    if let Some(file_path) = cli.file.as_ref() {
        let file_path_str = file_path.to_str().unwrap_or("<invalid path>");

        if let Ok(contents) = fs::read_to_string(file_path) {
            println!("Lexing file: {file_path_str}");
            let tok_buffer = lex(contents.as_str());
            let tokens: Vec<TokenIdx> = tok_buffer.into_iter().collect();
            println!("Done! Found {} tokens", tokens.len());

            if cli.print {
                print_tokens(tokens, &tok_buffer);
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
                    let tok_buffer = lex(buffer.as_str());
                    print_tokens(&tok_buffer, &tok_buffer);
                    println!("Done!");
                    buffer.clear();
                }
                Ok(_) => {}
                Err(error) => return Err(error),
            }
        }
    }

    Ok(())
}
