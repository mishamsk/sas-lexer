use std::{env, fs};

use anyhow::{anyhow, Result};
use convert_case::{Boundary, Case, Converter};
use sas_lexer::{TokenChannel, TokenType};
use strum::{EnumCount, IntoEnumIterator};

fn generate_token_type_python_enum() -> Result<()> {
    let mut enum_str = String::with_capacity(TokenType::COUNT * 40);

    enum_str.push_str("class TokenType(IntEnum):\n");

    let conv = Converter::new()
        .from_case(Case::Pascal)
        .remove_boundaries(&[Boundary::LowerDigit, Boundary::UpperDigit])
        .to_case(Case::UpperSnake);

    for token_type in TokenType::iter() {
        enum_str.push_str(
            format!(
                "    {} = {}\n",
                conv.convert(token_type.to_string()),
                token_type as u16
            )
            .as_str(),
        );
    }

    // Now write the enum to a file
    // Find the path to token_type.py
    let token_type_path = fs::canonicalize(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../src/sas_lexer/token_type.py"
    ))?;

    // Read the contents
    let mut file_content = fs::read_to_string(&token_type_path)?;

    // Find the position where the current class code starts
    let pos = file_content
        .find("class TokenType(IntEnum):")
        .ok_or(anyhow!("class TokenType not found"))?;

    // Overwrite the class code with the new string
    file_content.replace_range(pos.., &enum_str);

    // Write the updated content back to token_type.py
    fs::write(token_type_path, file_content)?;

    Ok(())
}

fn generate_token_channel_python_enum() -> Result<()> {
    let mut enum_str = String::with_capacity(TokenChannel::COUNT * 40);

    enum_str.push_str("class TokenChannel(IntEnum):\n");

    for token_channel in TokenChannel::iter() {
        enum_str.push_str(
            format!(
                "    {} = {}\n",
                token_channel.to_string(),
                token_channel as u8
            )
            .as_str(),
        );
    }

    // Now write the enum to a file
    // Find the path to token_channel.py
    let token_channel_path = fs::canonicalize(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../src/sas_lexer/token_channel.py"
    ))?;

    // Read the contents
    let mut file_content = fs::read_to_string(&token_channel_path)?;

    // Find the position where the current class code starts
    let pos = file_content
        .find("class TokenChannel(IntEnum):")
        .ok_or(anyhow!("class TokenChannel not found"))?;

    // Overwrite the class code with the new string
    file_content.replace_range(pos.., &enum_str);

    // Write the updated content back to token_channel.py
    fs::write(token_channel_path, file_content)?;

    Ok(())
}

fn main() -> Result<()> {
    generate_token_type_python_enum()?;
    generate_token_channel_python_enum()?;

    Ok(())
}
