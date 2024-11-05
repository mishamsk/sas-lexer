use std::{env, fs};

use anyhow::{anyhow, Result};
use convert_case::{Boundary, Case, Converter};
use sas_lexer::{
    error::{ErrorKind, CODE_ERROR_RANGE, INTERNAL_ERROR_RANGE, WARNING_RANGE},
    TokenChannel, TokenType,
};
use strum::{EnumCount, EnumMessage, IntoEnumIterator};

#[cfg(target_os = "windows")]
const LINE_FEED: &str = "\r\n";

#[cfg(not(target_os = "windows"))]
const LINE_FEED: &str = "\n";

fn generate_token_type_python_enum() -> Result<()> {
    let mut enum_str = String::with_capacity(TokenType::COUNT * 40);

    enum_str.push_str(&format!("class TokenType(IntEnum):{LINE_FEED}"));

    let conv = Converter::new()
        .from_case(Case::Pascal)
        .remove_boundaries(&[Boundary::LowerDigit, Boundary::UpperDigit])
        .to_case(Case::UpperSnake);

    for token_type in TokenType::iter() {
        enum_str.push_str(
            format!(
                "    {} = {}{LINE_FEED}",
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

    enum_str.push_str(&format!("class TokenChannel(IntEnum):{LINE_FEED}"));

    for token_channel in TokenChannel::iter() {
        enum_str.push_str(
            format!("    {} = {}{LINE_FEED}", token_channel, token_channel as u8).as_str(),
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

#[allow(clippy::too_many_lines)]
fn generate_error_kind_python_module() -> Result<()> {
    // Find the path to token_channel.py
    let error_kind_path = fs::canonicalize(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../src/sas_lexer/error_kind.py"
    ))?;

    // Read the contents
    let mut file_content = fs::read_to_string(&error_kind_path)?;

    // First generate the enum
    let mut enum_str = String::with_capacity(ErrorKind::COUNT * 40);

    enum_str.push_str(&format!("class ErrorKind(IntEnum):{LINE_FEED}"));

    let conv = Converter::new()
        .from_case(Case::Pascal)
        .to_case(Case::UpperSnake);

    let mut error_kinds = ErrorKind::iter().collect::<Vec<_>>();

    // Sort the error kinds by their u16 value
    error_kinds.sort_by_key(|error_kind| *error_kind as u16);

    for error_kind in &error_kinds {
        enum_str.push_str(
            format!(
                "    {} = {}{LINE_FEED}",
                conv.convert(error_kind.to_string()),
                *error_kind as u16
            )
            .as_str(),
        );
    }

    // Find the position where the current class code starts
    let start_pos = file_content
        .find("class ErrorKind(IntEnum):")
        .ok_or(anyhow!("class ErrorKind not found"))?;

    // Find the position where the current class code ends
    let end_pos = start_pos
        + file_content[start_pos..]
            .lines()
            .take_while(|line| !line.is_empty())
            .map(|line| line.len() + LINE_FEED.len())
            .sum::<usize>();

    // Overwrite the class code with the new string
    file_content.replace_range(start_pos..end_pos, &enum_str);

    // Now the ERROR_MESSAGE dictionary
    let mut error_message_str = String::with_capacity(ErrorKind::COUNT * 100);

    error_message_str.push_str(&format!(
        "ERROR_MESSAGE: dict[ErrorKind, str] = {{{LINE_FEED}"
    ));

    for error_kind in error_kinds {
        error_message_str.push_str(
            format!(
                "    ErrorKind.{}: \"{}\",{LINE_FEED}",
                conv.convert(error_kind.to_string()),
                error_kind
                    .get_message()
                    .unwrap_or_default()
                    .replace('\\', "\\\\")
                    .replace('"', "\\\"")
                    .replace('\n', "\\n")
                    .replace('\t', "\\t")
            )
            .as_str(),
        );
    }

    error_message_str.push('}');

    // Find the position where the current dictionary code starts
    let start_pos = file_content
        .find("ERROR_MESSAGE: dict[ErrorKind, str] = {")
        .ok_or(anyhow!("ERROR_MESSAGE dictionary not found"))?;

    // Find the position where the current dictionary code ends
    let end_pos = start_pos
        + file_content[start_pos..]
            .lines()
            .take_while(|line| *line != "}")
            .map(|line| line.len() + LINE_FEED.len())
            .sum::<usize>()
        + 1;

    // Overwrite the dictionary code with the new string
    file_content.replace_range(start_pos..end_pos, &error_message_str);

    // And finally update the ranges in `is_xxx_error` functions.
    // First we find the line number where each of the functions is defined,
    // then the range will be on the following line starting with `range(`.
    let range_pos_common_prefix_len = format!(
        "error_kind: ErrorKind) -> bool:{LINE_FEED}    \
        return error_kind in range("
    )
    .len();

    let is_internal_error_range_start_pos = file_content
        .lines()
        .take_while(|line| !line.contains("def is_internal_error("))
        .map(|line| line.len() + LINE_FEED.len())
        .sum::<usize>()
        + "def is_internal_error(".len()
        + range_pos_common_prefix_len;

    let range_str = format!(
        "{}, {})",
        INTERNAL_ERROR_RANGE.start, INTERNAL_ERROR_RANGE.end
    );

    let is_internal_error_range_end_pos = is_internal_error_range_start_pos + range_str.len();

    // Overwrite the range with the new value
    file_content.replace_range(
        is_internal_error_range_start_pos..is_internal_error_range_end_pos,
        &range_str,
    );

    let is_code_error_range_start_pos = file_content
        .lines()
        .take_while(|line| !line.contains("def is_code_error("))
        .map(|line| line.len() + LINE_FEED.len())
        .sum::<usize>()
        + "def is_code_error(".len()
        + range_pos_common_prefix_len;

    let range_str = format!("{}, {})", CODE_ERROR_RANGE.start, CODE_ERROR_RANGE.end);

    let is_code_error_range_end_pos = is_code_error_range_start_pos + range_str.len();

    // Overwrite the range with the new value
    file_content.replace_range(
        is_code_error_range_start_pos..is_code_error_range_end_pos,
        &range_str,
    );

    let is_warning_range_start_pos = file_content
        .lines()
        .take_while(|line| !line.contains("def is_warning("))
        .map(|line| line.len() + LINE_FEED.len())
        .sum::<usize>()
        + "def is_warning(".len()
        + range_pos_common_prefix_len;

    let range_str = format!("{}, {})", WARNING_RANGE.start, WARNING_RANGE.end);

    let is_warning_range_end_pos = is_warning_range_start_pos + range_str.len();

    // Overwrite the range with the new value
    file_content.replace_range(
        is_warning_range_start_pos..is_warning_range_end_pos,
        &range_str,
    );

    // Write the updated content back to token_channel.py
    fs::write(error_kind_path, file_content)?;

    Ok(())
}

fn main() -> Result<()> {
    generate_token_type_python_enum()?;
    generate_token_channel_python_enum()?;
    generate_error_kind_python_module()?;

    Ok(())
}
