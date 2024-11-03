mod lex;
mod print;
mod stat;

use clap::Parser;
use clap::Subcommand;
use lex::lex_and_print;
use lex::LexPrintConfig;
use stat::gen_stats;

use walkdir::WalkDir;

use std::fs;
use std::io;
use std::path::PathBuf;
use std::time::Duration;
use std::time::Instant;

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

        /// Print file names before lexing.
        #[arg(short)]
        file_name: bool,

        /// Print totals for token and error counts to the console.
        /// Implies `-f` (print file name).
        #[arg(short)]
        totals: bool,

        /// Print durations for lexing and token generation to the console.
        #[arg(short)]
        durations: bool,

        /// Print the tokens to the console.
        /// Implies `-f` (print file name).
        #[arg(long)]
        print_tokens: bool,

        /// Print errors if errors were found.
        /// Implies `-f` (print file name), but if `--print-tokens` is not
        /// set then only files with errors are printed.
        #[arg(long)]
        print_errors: bool,

        /// Number of lines around an error to print as context.
        /// If not provided, nothing is printed.
        #[arg(short)]
        context_lines: Option<usize>,
    },
    /// Run lexing over samples and generate various statistics used for
    /// debugging and optimization.
    Stats {
        /// Path to put the resulting stat tabls to. If not provided
        /// only the summary report on console is produced
        #[cfg(feature = "polars")]
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Number of lines around an error to store as context.
        /// If not provided, defaults to 1.
        #[cfg(feature = "polars")]
        #[arg(short = 'C', long)]
        error_context_lines: Option<usize>,

        /// A folder with samples. Reads all files with the `.sas` extension.
        #[arg(env = "SAS_LEX_SAMPLES", required = true)]
        samples: PathBuf,
    },
}

#[allow(clippy::too_many_lines)]
fn main() -> io::Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Lex {
            file_or_dir,
            file_name: print_file_name,
            totals: print_totals,
            durations: print_durations,
            print_tokens,
            print_errors,
            context_lines,
        } => {
            let start = Instant::now();

            if let Some(source_path) = file_or_dir.as_ref() {
                if source_path.is_dir() {
                    let mut total_lex_duration = Duration::new(0, 0);
                    let mut total_gen_tok_vec_duration = Duration::new(0, 0);

                    for entry in WalkDir::new(source_path).into_iter().filter_map(Result::ok) {
                        let entry_path = entry.path();
                        if entry_path.extension().and_then(|ext| ext.to_str()) == Some("sas") {
                            if let Ok(contents) = fs::read_to_string(entry_path) {
                                // Never print tokens or errors if a folder is provided
                                if let Some(durations) = lex_and_print(
                                    &contents,
                                    LexPrintConfig {
                                        print_file_name: *print_file_name,
                                        print_tokens: *print_tokens,
                                        print_token_totals: *print_totals,
                                        print_errors: *print_errors,
                                        print_error_totals: *print_totals,
                                        print_lex_return_errors: *print_errors,
                                        print_stack_unwind_errors: *print_errors,
                                        context_lines: *context_lines,
                                    },
                                    Some(entry_path.display().to_string()),
                                ) {
                                    total_lex_duration += durations.lex_duration;
                                    total_gen_tok_vec_duration += durations.gen_tok_vec_duration;
                                }
                            } else if *print_file_name {
                                eprintln!("Failed to read file: {}", entry_path.display());
                            }
                        }
                    }

                    let total_time = start.elapsed();

                    if *print_durations {
                        // Print total time, file read time, lex time, and token generation time
                        let file_read_time =
                            total_time - total_lex_duration - total_gen_tok_vec_duration;

                        println!("Total time: {}ms", total_time.as_millis());
                        println!("File read time: {}ms", file_read_time.as_millis());
                        println!("Lexing took: {}ms", total_lex_duration.as_millis());
                        println!(
                            "Token generation took: {}ms",
                            total_gen_tok_vec_duration.as_millis()
                        );
                    }
                } else if let Ok(contents) = fs::read_to_string(source_path) {
                    if let Some(dur) = lex_and_print(
                        &contents,
                        LexPrintConfig {
                            print_file_name: *print_file_name,
                            print_tokens: *print_tokens,
                            print_token_totals: *print_totals,
                            print_errors: *print_errors,
                            print_error_totals: *print_totals,
                            print_lex_return_errors: *print_errors,
                            print_stack_unwind_errors: *print_errors,
                            context_lines: *context_lines,
                        },
                        Some(source_path.display().to_string()),
                    ) {
                        let total_time = start.elapsed();

                        if *print_durations {
                            // Print total time, file read time, lex time, and token generation time
                            let file_read_time =
                                total_time - dur.lex_duration - dur.gen_tok_vec_duration;

                            println!("Total time: {}ms", total_time.as_millis());
                            println!("File read time: {}ms", file_read_time.as_millis());
                            println!("Lexing took: {}ms", dur.lex_duration.as_millis());
                            println!(
                                "Token generation took: {}ms",
                                dur.gen_tok_vec_duration.as_millis()
                            );
                        }
                    }
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
                            lex_and_print(
                                &buffer,
                                LexPrintConfig {
                                    print_file_name: *print_file_name,
                                    print_tokens: *print_tokens,
                                    print_token_totals: *print_totals,
                                    print_errors: *print_errors,
                                    print_error_totals: *print_totals,
                                    print_lex_return_errors: *print_errors,
                                    print_stack_unwind_errors: *print_errors,
                                    context_lines: *context_lines,
                                },
                                None,
                            );

                            buffer.clear();
                        }
                        Ok(_) => {}
                        Err(error) => return Err(error),
                    }
                }
            }
        }
        Commands::Stats {
            samples,
            #[cfg(feature = "polars")]
            output,
            #[cfg(feature = "polars")]
            error_context_lines,
        } => gen_stats(
            samples,
            #[cfg(feature = "polars")]
            output,
            #[cfg(feature = "polars")]
            error_context_lines.unwrap_or(1),
        ),
    }

    Ok(())
}
