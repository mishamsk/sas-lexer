use clap::{Arg, Command};
use lexer::print::print_tokens;
use lexer::TokenIdx;
use std::fs;
use std::io;

mod lexer;

fn main() -> io::Result<()> {
    let matches = Command::new("SAS Lexer")
        .version(env!("CARGO_PKG_VERSION"))
        .author("Mike Perlov <mishamsk@gmail.com>")
        .about("Lex SAS code from a file or stdin")
        .arg(
            Arg::new("file")
                .index(1)
                .value_name("FILE")
                .help("A file path to read from"),
        )
        .get_matches();

    if let Some(file_path) = matches.get_one::<String>("file") {
        if let Ok(contents) = fs::read_to_string(file_path) {
            println!("Lexing file: {}", file_path);
            let tok_buffer = lexer::lex(contents.as_str());
            let tokens: Vec<TokenIdx> = (0..tok_buffer.token_count() - 1)
                .map(|idx| TokenIdx(idx))
                .collect();
            print_tokens(tokens, &tok_buffer);
            println!("Done!");
        } else {
            eprintln!("Failed to read file: {}", file_path);
        }
    } else {
        println!("Enter input (Ctrl-D to submit, Ctrl-C to exit):");

        let mut buffer = String::new();
        loop {
            match io::stdin().read_line(&mut buffer) {
                Ok(0) => {
                    println!("Lexing from stdin...");
                    let tok_buffer = lexer::lex(buffer.as_str());
                    let tokens: Vec<TokenIdx> = (0..tok_buffer.token_count() - 1)
                        .map(|idx| TokenIdx(idx))
                        .collect();
                    print_tokens(tokens, &tok_buffer);
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
