#![allow(clippy::print_stderr, clippy::print_stdout)]

mod antlr;
mod lex;
mod print;
mod stat;

use antlr::write_tokens_file;
use clap::Parser;
use clap::Subcommand;
use lex::lex_and_print;
use stat::gen_stats;

use walkdir::WalkDir;

use std::fs;
use std::io;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "SAS Lexer")]
#[command(version, author, about, long_about = None)]
/// Utilities for SAS lexer crate
struct Cli {
    #[command(subcommand)]
    command: Commands,
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
        grammar_file_path: Option<PathBuf>,
    },
    /// Run lexing over samples and generate various statistics used for
    /// debugging and optimization.
    Stats {
        /// Path to put the resulting stat tabls to. If not provided
        /// only the summary report on console is produced
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// A folder with samples. Reads all files with the `.sas` extension.
        #[arg(env = "SAS_LEX_SAMPLES", required = true)]
        samples: PathBuf,
    },
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
        Commands::Gen {
            grammar_file_path: grammar,
        } => {
            write_tokens_file(grammar)?;
        }
        Commands::Stats { output, samples } => gen_stats(output, samples),
    }

    Ok(())
}
