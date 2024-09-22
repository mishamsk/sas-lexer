use std::fs;
use std::io;
use std::path::PathBuf;

use convert_case::Boundary;
use convert_case::Case;
use convert_case::Converter;
use sas_lexer::TokenType;
use strum::EnumCount;
use strum::IntoEnumIterator;

fn generate_tokens_file_as_string() -> String {
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

pub(super) fn write_tokens_file(path: &Option<PathBuf>) -> Result<(), io::Error> {
    let tokens_file = generate_tokens_file_as_string();
    Ok(if let Some(grammar_path) = path {
        fs::write(grammar_path, tokens_file)?;
    } else {
        println!("{tokens_file}");
    })
}
