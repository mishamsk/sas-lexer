use crate::error::ErrorInfo;
/// Functions to print the token
use crate::lexer::buffer::{DetachedTokenizedBuffer, Payload, TokenIdx};

#[must_use]
pub fn token_to_string<'a, S: AsRef<str> + 'a>(
    token: TokenIdx,
    buffer: &DetachedTokenizedBuffer,
    source: &'a S,
) -> String {
    let token_idx = token.get();
    let start_line = buffer.get_token_start_line(token);
    let end_line = buffer.get_token_end_line(token);
    let start_column = buffer.get_token_start_column(token);
    let end_column = buffer.get_token_end_column(token);
    let token_start = buffer.get_token_start(token).get();
    let token_end = buffer.get_token_end(token).get();
    let token_text = buffer.get_token_text(token, source).unwrap_or("<no text>");
    let token_type = buffer.get_token_type(token);
    let token_channel = buffer.get_token_channel(token);
    let payload = buffer.get_token_payload(token);

    let payload_str = match payload {
        Payload::None => "<None>".to_string(),
    };

    // Constructing the string representation of the token
    let token_repr = format!(
        "[@{token_idx},{token_start}:{token_end}={token_text:?},<{token_type}>,\
        L{start_line}:C{start_column}-L{end_line}:C{end_column},chl={token_channel},\
        pl={payload_str:?}]"
    );

    token_repr
}

pub fn error_to_string<'a, S: AsRef<str> + 'a>(
    error: &ErrorInfo,
    buffer: &DetachedTokenizedBuffer,
    source: &'a S,
) -> String {
    let error_type = error.error_type();
    let at_byte_offset = error.at_byte_offset();
    let at_char_offset = error.at_char_offset();
    let on_line = error.on_line();
    let at_column = error.at_column();

    let last_token_str = match error.last_token() {
        Some(last_token_idx) => token_to_string(last_token_idx, buffer, source),
        None => "<no last token>".to_string(),
    };

    format!(
        "{error_type:?} at byte offset {at_byte_offset}, char offset {at_char_offset}, \
        L{on_line}:C{at_column}. Last token: {last_token_str}"
    )
}
