use clap::{Arg, Command};
use std::fs;
use std::io::{self, Read};

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
        let contents = fs::read(file_path)?;
        println!("File contents as bytes: {:?}", contents);
    } else {
        let mut buffer = Vec::new();
        io::stdin().lock().read_to_end(&mut buffer)?;
        println!("Stdin contents as bytes: {:?}", buffer);
    }

    Ok(())
}
