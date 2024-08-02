#![allow(dead_code)]

use sas_lexer::{
    error::{ErrorInfo, ErrorType},
    DetachedTokenizedBuffer, Payload, TokenChannel, TokenIdx, TokenType,
};

#[macro_export]
macro_rules! set_snapshot_suffix {
    ($($expr:expr),*) => {
        let mut settings = insta::Settings::clone_current();
        settings.set_snapshot_suffix(format!($($expr,)*));
        let _guard = settings.bind_to_scope();
    }
}

pub(crate) fn check_token(
    source: &str,
    token: TokenIdx,
    buffer: &DetachedTokenizedBuffer,
    start_byte_offset: u32,
    end_byte_offset: u32,
    start_char_offset: u32,
    end_char_offset: u32,
    start_line: u32,
    line_count: u32,
    start_column: u32,
    end_column: u32,
    token_type: TokenType,
    token_channel: TokenChannel,
    payload: Payload,
    token_text: Option<&str>,
) {
    // Byte offsets
    assert_eq!(
        buffer.get_token_start_byte_offset(token).get(),
        start_byte_offset,
        "Expected start byte offset {}, got {}",
        start_byte_offset,
        buffer.get_token_start(token).get()
    );

    assert_eq!(
        buffer.get_token_end_byte_offset(token).get(),
        end_byte_offset,
        "Expected end byte offset {}, got {}",
        end_byte_offset,
        buffer.get_token_end(token).get()
    );

    // Char offsets
    assert_eq!(
        buffer.get_token_start(token).get(),
        start_char_offset,
        "Expected start char offset {}, got {}",
        start_char_offset,
        buffer.get_token_start(token).get()
    );
    assert_eq!(
        buffer.get_token_end(token).get(),
        end_char_offset,
        "Expected end char offset {}, got {}",
        end_char_offset,
        buffer.get_token_end(token).get()
    );

    // Line and column
    assert_eq!(
        buffer.get_token_start_line(token),
        start_line,
        "Expected start line {}, got {}",
        start_line,
        buffer.get_token_start_line(token)
    );

    let end_line = start_line + line_count - 1;

    assert_eq!(
        buffer.get_token_end_line(token),
        end_line,
        "Expected end line {}, got {}",
        end_line,
        buffer.get_token_end_line(token)
    );

    assert_eq!(
        buffer.get_token_start_column(token),
        start_column,
        "Expected start column {}, got {}",
        start_column,
        buffer.get_token_start_column(token)
    );

    assert_eq!(
        buffer.get_token_end_column(token),
        end_column,
        "Expected end column {}, got {}",
        end_column,
        buffer.get_token_end_column(token)
    );

    // Token type, channel, payload and text
    assert_eq!(
        buffer.get_token_type(token),
        token_type,
        "Expected token type {:?}, got {:?}",
        token_type,
        buffer.get_token_type(token)
    );

    assert_eq!(
        buffer.get_token_channel(token),
        token_channel,
        "Expected token channel {:?}, got {:?}",
        token_channel,
        buffer.get_token_channel(token)
    );

    assert_eq!(
        buffer.get_token_payload(token),
        payload,
        "Expected token payload {:?}, got {:?}",
        payload,
        buffer.get_token_payload(token)
    );

    assert_eq!(
        buffer.get_token_text(token, &source),
        token_text,
        "Expected text {:?}, got {:?}",
        token_text,
        buffer.get_token_text(token, &source)
    );
}

pub(crate) fn check_error(
    error: &ErrorInfo,
    error_type: ErrorType,
    at_byte_offset: u32,
    at_char_offset: u32,
    on_line: u32,
    at_column: u32,
    last_token_idx: Option<TokenIdx>,
) {
    assert_eq!(
        error.error_type(),
        error_type,
        "Expected error type {:?}, got {:?}",
        error_type,
        error.error_type()
    );

    assert_eq!(
        error.at_byte_offset(),
        at_byte_offset,
        "Expected byte offset {}, got {}",
        at_byte_offset,
        error.at_byte_offset()
    );

    assert_eq!(
        error.at_char_offset(),
        at_char_offset,
        "Expected char offset {}, got {}",
        at_char_offset,
        error.at_char_offset()
    );

    assert_eq!(
        error.on_line(),
        on_line,
        "Expected line {}, got {}",
        on_line,
        error.on_line()
    );

    assert_eq!(
        error.at_column(),
        at_column,
        "Expected column {}, got {}",
        at_column,
        error.at_column()
    );

    let last_token = error.last_token();

    assert_eq!(
        last_token, last_token_idx,
        "Expected last token index {:?}, got {:?}",
        last_token_idx, last_token
    );
}
