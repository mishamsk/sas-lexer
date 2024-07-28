/// Functions to print the token
use crate::lexer::buffer::{Payload, TokenIdx, TokenizedBuffer};

#[must_use]
pub fn to_pretty_string(token: TokenIdx, buffer: &TokenizedBuffer) -> String {
    let start_line = buffer.get_token_start_line(token);
    let end_line = buffer.get_token_end_line(token);
    let start_column = buffer.get_token_start_column(token);
    let end_column = buffer.get_token_end_column(token);
    let token_start = buffer.get_token_start(token);
    let token_end = buffer.get_token_end(token);
    let token_text = buffer.get_token_text(token).unwrap_or("<no text>");
    let token_type = buffer.get_token_type(token);
    let token_channel = buffer.get_token_channel(token);
    let payload = buffer.get_token_payload(token);

    let payload_str = match payload {
        Payload::Error(e) => e.to_string(),
        Payload::None => "<None>".to_string(),
    };

    // Constructing the string representation of the token
    let token_repr = format!(
        "[@{token},{token_start}:{token_end}={token_text:?},<{token_type}>,\
        L{start_line}:C{start_column}-L{end_line}:C{end_column},chl={token_channel},\
        pl={payload_str:?}]"
    );

    token_repr
}
