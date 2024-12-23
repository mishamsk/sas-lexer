use sas_lexer::error::ErrorInfo;
/// Functions to print the token
use sas_lexer::{Payload, ResolvedTokenInfo};
use std::cmp::Ordering;
use std::io::Write;

pub(crate) fn get_token_raw_text(token: &ResolvedTokenInfo, source: &str) -> String {
    let token_start = token.start as usize;
    let token_stop = token.stop as usize;

    match token_stop.cmp(&token_start) {
        Ordering::Less => {
            // Should never happen, but just in case
            "<invalid text range>".into()
        }
        Ordering::Equal => {
            // Ephemeral token
            "<no text>".into()
        }
        Ordering::Greater => source
            .chars()
            .skip(token_start)
            .take(token_stop - token_start)
            .collect(),
    }
}

pub(crate) fn get_string_literal(string_literals_buffer: &str, start: u32, stop: u32) -> String {
    string_literals_buffer
        .get(start as usize..stop as usize)
        .unwrap_or("Invalid string literal range")
        .to_string()
}

pub(crate) fn get_error_context(error: &ErrorInfo, source: &str, context_lines: usize) -> String {
    source
        .lines()
        .skip((error.on_line() as usize).saturating_sub(context_lines))
        .take(context_lines * 2)
        .collect::<String>()
}

#[must_use]
pub(crate) fn token_to_string(
    token: &ResolvedTokenInfo,
    string_literals_buffer: &str,
    source: &str,
) -> String {
    let token_index = token.token_index;
    let start_line = token.line;
    let end_line = token.end_line;
    let start_column = token.column;
    let end_column = token.end_column;
    let token_start = token.start;
    let token_stop = token.stop;

    // Get chars from start to end
    let token_raw_text = get_token_raw_text(token, source);

    let token_type = token.token_type.to_string();
    let token_channel = token.channel.to_string()[..1].to_uppercase();

    let payload_str = match token.payload {
        Payload::None => "<None>".to_string(),
        Payload::Integer(val) => val.to_string(),
        Payload::Float(val) => {
            if ((val * 1000.0).round() / 1000.0 - val).abs() < f64::EPSILON {
                format!("{val:.3}")
            } else {
                format!("{val:.3e}")
            }
        }
        Payload::StringLiteral(start, stop) => {
            get_string_literal(string_literals_buffer, start, stop)
        }
    };

    // Constructing the string representation of the token
    format!(
        "[@{token_index},{token_start}:{token_stop}={token_raw_text:?},<{token_type}>,\
        L{start_line}:C{start_column}-L{end_line}:C{end_column},chl={token_channel},\
        pl={payload_str:?}]"
    )
}

pub(crate) fn error_to_string(
    error: &ErrorInfo,
    tokens: &[ResolvedTokenInfo],
    string_literals_buffer: &str,
    source: &str,
    context_lines: Option<usize>,
) -> String {
    let error_kind = error.error_kind();
    let at_byte_offset = error.at_byte_offset();
    let at_char_offset = error.at_char_offset();
    let on_line = error.on_line();
    let at_column = error.at_column();

    let last_token_str = match error.last_token() {
        Some(last_token_idx) => {
            let last_token_idx = last_token_idx.get() as usize;
            tokens
                .get(last_token_idx)
                .map(|token| token_to_string(token, string_literals_buffer, source))
                .unwrap_or("<invalid token idx>".to_string())
        }
        None => "<no last token>".to_string(),
    };

    if let Some(context_lines) = context_lines {
        let context = get_error_context(error, source, context_lines);
        format!(
            "{error_kind:?} at byte offset {at_byte_offset}, char offset {at_char_offset}, \
            L{on_line}:C{at_column}. Last token: {last_token_str}.\nContext:\n{context}\n"
        )
    } else {
        format!(
            "{error_kind:?} at byte offset {at_byte_offset}, char offset {at_char_offset}, \
        L{on_line}:C{at_column}. Last token: {last_token_str}"
        )
    }
}

pub(crate) fn print_tokens(
    dst: &mut impl Write,
    tokens: &Vec<ResolvedTokenInfo>,
    string_literals_buffer: &str,
    source: &str,
) {
    for token in tokens {
        writeln!(
            dst,
            "{}",
            token_to_string(token, string_literals_buffer, source)
        )
        .unwrap();
    }
}

pub(crate) fn print_errors(
    dst: &mut impl Write,
    errors: &Vec<ErrorInfo>,
    tokens: &[ResolvedTokenInfo],
    string_literals_buffer: &str,
    source: &str,
    context_lines: Option<usize>,
) {
    for error in errors {
        writeln!(
            dst,
            "{}",
            error_to_string(error, tokens, string_literals_buffer, source, context_lines)
        )
        .unwrap();
    }
}
