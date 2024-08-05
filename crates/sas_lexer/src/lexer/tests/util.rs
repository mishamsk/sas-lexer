use crate::{
    error::{ErrorInfo, ErrorType},
    lex,
    print::{error_to_string, token_to_string},
    Payload, TokenChannel, TokenIdx, TokenType, TokenizedBuffer,
};

pub(crate) trait TokenTestCase {
    fn token_type(&self) -> TokenType;
    fn token_channel(&self) -> TokenChannel;
    fn payload(&self) -> Payload;
    fn text<S: AsRef<str>>(&self, source: S) -> Option<String>;
}

impl TokenTestCase for (&str, TokenType, TokenChannel) {
    fn token_type(&self) -> TokenType {
        self.1
    }

    fn token_channel(&self) -> TokenChannel {
        self.2
    }

    fn payload(&self) -> Payload {
        Payload::None
    }

    fn text<S: AsRef<str>>(&self, _source: S) -> Option<String> {
        if self.0.is_empty() {
            None
        } else {
            Some(self.0.to_owned())
        }
    }
}

impl TokenTestCase for (&str, TokenType) {
    fn token_type(&self) -> TokenType {
        self.1
    }

    fn token_channel(&self) -> TokenChannel {
        match self.1 {
            TokenType::WS => TokenChannel::HIDDEN,
            _ => TokenChannel::default(),
        }
    }

    fn payload(&self) -> Payload {
        Payload::None
    }

    fn text<S: AsRef<str>>(&self, _source: S) -> Option<String> {
        if self.0.is_empty() {
            None
        } else {
            Some(self.0.to_owned())
        }
    }
}

impl TokenTestCase for (&str, TokenType, Payload) {
    fn token_type(&self) -> TokenType {
        self.1
    }

    fn token_channel(&self) -> TokenChannel {
        match self.1 {
            TokenType::WS => TokenChannel::HIDDEN,
            _ => TokenChannel::default(),
        }
    }

    fn payload(&self) -> Payload {
        self.2
    }

    fn text<S: AsRef<str>>(&self, _source: S) -> Option<String> {
        if self.0.is_empty() {
            None
        } else {
            Some(self.0.to_owned())
        }
    }
}

impl TokenTestCase for (TokenType, TokenChannel) {
    fn token_type(&self) -> TokenType {
        self.0
    }

    fn token_channel(&self) -> TokenChannel {
        self.1
    }

    fn payload(&self) -> Payload {
        Payload::None
    }

    fn text<S: AsRef<str>>(&self, source: S) -> Option<String> {
        // If a test case doesn't provide a text, it means the whole source
        // is the token text
        if source.as_ref().is_empty() {
            None
        } else {
            Some(source.as_ref().to_owned())
        }
    }
}

impl TokenTestCase for TokenType {
    fn token_type(&self) -> TokenType {
        *self
    }

    fn token_channel(&self) -> TokenChannel {
        match self {
            TokenType::WS => TokenChannel::HIDDEN,
            _ => TokenChannel::default(),
        }
    }

    fn payload(&self) -> Payload {
        Payload::None
    }

    fn text<S: AsRef<str>>(&self, source: S) -> Option<String> {
        // If a test case doesn't provide a text, it means the whole source
        // is the token text
        match self {
            TokenType::EOF => None,
            TokenType::WS => {
                // Special case for automatic whitespace token text
                // Just find the first non-whitespace character
                let text = source.as_ref();
                let end = text
                    .find(|c: char| !c.is_whitespace())
                    .unwrap_or(source.as_ref().len());

                // This is a bug in the test case
                assert_ne!(end, 0, "Whitespace token text is empty");

                Some(text[..end].to_owned())
            }
            _ => {
                if source.as_ref().is_empty() {
                    None
                } else {
                    Some(source.as_ref().to_owned())
                }
            }
        }
    }
}

impl TokenTestCase for (TokenType, f64) {
    fn token_type(&self) -> TokenType {
        self.0
    }

    fn token_channel(&self) -> TokenChannel {
        match self.0 {
            TokenType::WS => TokenChannel::HIDDEN,
            _ => TokenChannel::default(),
        }
    }

    fn payload(&self) -> Payload {
        Payload::Float(self.1)
    }

    fn text<S: AsRef<str>>(&self, source: S) -> Option<String> {
        // If a test case doesn't provide a text, it means the whole source
        // is the token text
        if source.as_ref().is_empty() {
            None
        } else {
            Some(source.as_ref().to_owned())
        }
    }
}

impl TokenTestCase for (TokenType, i64) {
    fn token_type(&self) -> TokenType {
        self.0
    }

    fn token_channel(&self) -> TokenChannel {
        match self.0 {
            TokenType::WS => TokenChannel::HIDDEN,
            _ => TokenChannel::default(),
        }
    }

    fn payload(&self) -> Payload {
        Payload::Integer(self.1)
    }

    fn text<S: AsRef<str>>(&self, source: S) -> Option<String> {
        // If a test case doesn't provide a text, it means the whole source
        // is the token text
        if source.as_ref().is_empty() {
            None
        } else {
            Some(source.as_ref().to_owned())
        }
    }
}

fn format_tokens_for_trace<'a, I, S>(tokens: I, buffer: &TokenizedBuffer, source: &S) -> String
where
    I: IntoIterator<Item = TokenIdx>,
    S: AsRef<str> + 'a,
{
    tokens
        .into_iter()
        .map(|token| format!("- {}", token_to_string(token, buffer, source)))
        .collect::<Vec<_>>()
        .join("\n")
}

pub(crate) fn check_token<S: AsRef<str>>(
    source: &str,
    token: TokenIdx,
    buffer: &TokenizedBuffer,
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
    token_text: Option<S>,
) {
    assert_ne!(line_count, 0, "Line count must be greater than 0");

    // Byte offsets
    assert_eq!(
        start_byte_offset,
        buffer.get_token_start_byte_offset(token).get(),
        "Expected start byte offset {}, got {}: {}",
        start_byte_offset,
        buffer.get_token_start(token).get(),
        token_to_string(token, buffer, &source)
    );

    assert_eq!(
        end_byte_offset,
        buffer.get_token_end_byte_offset(token).get(),
        "Expected end byte offset {}, got {}: {}",
        end_byte_offset,
        buffer.get_token_end(token).get(),
        token_to_string(token, buffer, &source)
    );

    // Char offsets
    assert_eq!(
        start_char_offset,
        buffer.get_token_start(token).get(),
        "Expected start char offset {}, got {}: {}",
        start_char_offset,
        buffer.get_token_start(token).get(),
        token_to_string(token, buffer, &source)
    );
    assert_eq!(
        end_char_offset,
        buffer.get_token_end(token).get(),
        "Expected end char offset {}, got {}: {}",
        end_char_offset,
        buffer.get_token_end(token).get(),
        token_to_string(token, buffer, &source)
    );

    // Line and column
    assert_eq!(
        start_line,
        buffer.get_token_start_line(token),
        "Expected start line {}, got {}: {}",
        start_line,
        buffer.get_token_start_line(token),
        token_to_string(token, buffer, &source)
    );

    let end_line = start_line + line_count - 1;

    assert_eq!(
        end_line,
        buffer.get_token_end_line(token),
        "Expected end line {}, got {}: {}",
        end_line,
        buffer.get_token_end_line(token),
        token_to_string(token, buffer, &source)
    );

    assert_eq!(
        start_column,
        buffer.get_token_start_column(token),
        "Expected start column {}, got {}: {}",
        start_column,
        buffer.get_token_start_column(token),
        token_to_string(token, buffer, &source)
    );

    assert_eq!(
        end_column,
        buffer.get_token_end_column(token),
        "Expected end column {}, got {}: {}",
        end_column,
        buffer.get_token_end_column(token),
        token_to_string(token, buffer, &source)
    );

    // Token type, channel, payload and text
    assert_eq!(
        token_type,
        buffer.get_token_type(token),
        "Expected token type {:?}, got {:?}: {}",
        token_type,
        buffer.get_token_type(token),
        token_to_string(token, buffer, &source)
    );

    assert_eq!(
        token_channel,
        buffer.get_token_channel(token),
        "Expected token channel {:?}, got {:?}: {}",
        token_channel,
        buffer.get_token_channel(token),
        token_to_string(token, buffer, &source)
    );

    assert_eq!(
        payload,
        buffer.get_token_payload(token),
        "Expected token payload {:?}, got {:?}: {}",
        payload,
        buffer.get_token_payload(token),
        token_to_string(token, buffer, &source)
    );

    let token_text = token_text.as_ref().map(|s| s.as_ref());

    assert_eq!(
        token_text,
        buffer.get_token_text(token, &source),
        "Expected text {:?}, got {:?}",
        token_text,
        buffer.get_token_text(token, &source)
    );
}

pub(crate) trait ErrorTestCase {
    fn error_type(&self) -> ErrorType;

    fn at_byte_offset(&self, last_byte_offset: u32) -> u32 {
        last_byte_offset
    }

    fn at_char_offset(&self, last_char_offset: u32) -> u32 {
        last_char_offset
    }

    fn on_line(&self, last_line: u32) -> u32 {
        last_line
    }

    fn at_column(&self, last_column: u32) -> u32 {
        last_column
    }

    fn last_token_idx(&self, ast_non_eof_token: Option<TokenIdx>) -> Option<TokenIdx> {
        ast_non_eof_token
    }
}

impl ErrorTestCase for ErrorType {
    fn error_type(&self) -> ErrorType {
        *self
    }
}

fn format_errors_for_trace<'a, I, S>(errors: I, buffer: &TokenizedBuffer, source: &S) -> String
where
    I: IntoIterator<Item = ErrorInfo>,
    S: AsRef<str> + 'a,
{
    errors
        .into_iter()
        .map(|error| format!("- {}", error_to_string(&error, buffer, source)))
        .collect::<Vec<_>>()
        .join("\n")
}

pub(crate) fn check_error(
    source: &str,
    buffer: &TokenizedBuffer,
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
        "Expected error type {:?}, got {:?}: {}",
        error_type,
        error.error_type(),
        error_to_string(error, buffer, &source)
    );

    assert_eq!(
        error.at_byte_offset(),
        at_byte_offset,
        "Expected byte offset {}, got {}: {}",
        at_byte_offset,
        error.at_byte_offset(),
        error_to_string(error, buffer, &source)
    );

    assert_eq!(
        error.at_char_offset(),
        at_char_offset,
        "Expected char offset {}, got {}: {}",
        at_char_offset,
        error.at_char_offset(),
        error_to_string(error, buffer, &source)
    );

    assert_eq!(
        error.on_line(),
        on_line,
        "Expected line {}, got {}: {}",
        on_line,
        error.on_line(),
        error_to_string(error, buffer, &source)
    );

    assert_eq!(
        error.at_column(),
        at_column,
        "Expected column {}, got {}: {}",
        at_column,
        error.at_column(),
        error_to_string(error, buffer, &source)
    );

    let last_token = error.last_token();

    assert_eq!(
        last_token,
        last_token_idx,
        "Expected last token index {last_token_idx:?}, got {last_token:?}.\n\
        Token {} instead of {}, for error: {}",
        if let Some(lt_idx) = last_token {
            token_to_string(lt_idx, buffer, &source)
        } else {
            "<no token>".to_string()
        },
        if let Some(lt_idx) = last_token_idx {
            token_to_string(lt_idx, buffer, &source)
        } else {
            "<no token>".to_string()
        },
        error_to_string(error, buffer, &source)
    );
}

pub(crate) fn assert_lexing(
    source: &str,
    expected_tokens: Vec<impl TokenTestCase>,
    expected_errors: Vec<impl ErrorTestCase>,
) {
    let (buffer, errors) = lex(&source).unwrap();

    // Check tokens
    let tokens: Vec<TokenIdx> = buffer.into_iter().collect();

    // Save last_non_eof_token_idx for error checking below
    let last_non_eof_token_idx = tokens
        .iter()
        .filter(|&t| buffer.get_token_type(*t) != TokenType::EOF)
        .last()
        .copied();

    // Check total token count
    assert_eq!(
        expected_tokens.len() + 1,
        tokens.len(),
        "Expected {} tokens including EOF, got {}:\n{}",
        expected_tokens.len() + 1,
        tokens.len(),
        format_tokens_for_trace(tokens, &buffer, &source)
    );

    // reverse the tokens, so we could compare left to right
    let mut tokens = tokens.into_iter().rev().collect::<Vec<_>>();

    let mut cur_start_line = 1u32;
    let mut cur_start_byte_offset = 0u32;
    let mut cur_start_char_offset = 0u32;
    let mut cur_start_column = 0u32;

    for expected_tok in expected_tokens {
        let lexed_token = tokens.pop().unwrap();

        // Calculate the remaining test text
        let rem_source = &source[cur_start_byte_offset as usize..];

        // Get the expected token text
        let expected_tok_text = expected_tok.text(rem_source);

        // Calculate various offsets
        let cur_end_byte_offset = cur_start_byte_offset
            + expected_tok_text
                .as_ref()
                .map_or(0, |text| text.len() as u32);

        let text_tok_chars = expected_tok_text
            .as_ref()
            .map_or(0, |text| text.chars().count() as u32);

        let cur_end_char_offset = cur_start_char_offset + text_tok_chars;

        let line_count = expected_tok_text
            .as_ref()
            .map_or(1, |text| text.lines().count() as u32);

        let ends_with_newline = expected_tok_text
            .as_ref()
            .map_or(false, |text| text.ends_with('\n'));

        let cur_end_line = cur_start_line + line_count - 1;

        let cur_end_column = match expected_tok_text.as_ref() {
            Some(text) => {
                if line_count > 1 {
                    text.lines().last().unwrap().chars().count() as u32
                        + u32::from(ends_with_newline)
                } else {
                    cur_start_column + text_tok_chars
                }
            }
            None => cur_start_column,
        };

        check_token(
            source,
            lexed_token,
            &buffer,
            cur_start_byte_offset,
            cur_end_byte_offset,
            cur_start_char_offset,
            cur_end_char_offset,
            cur_start_line,
            line_count,
            cur_start_column,
            cur_end_column,
            expected_tok.token_type(),
            expected_tok.token_channel(),
            expected_tok.payload(),
            expected_tok_text.as_ref(),
        );

        cur_start_line = cur_end_line + u32::from(ends_with_newline);
        cur_start_byte_offset = cur_end_byte_offset;
        cur_start_char_offset = cur_end_char_offset;
        cur_start_column = if ends_with_newline { 0 } else { cur_end_column };
    }

    // Chec EOF token
    check_token::<String>(
        source,
        tokens.pop().unwrap(),
        &buffer,
        cur_start_byte_offset,
        cur_start_byte_offset,
        cur_start_char_offset,
        cur_start_char_offset,
        cur_start_line,
        1,
        cur_start_column,
        cur_start_column,
        TokenType::EOF,
        TokenChannel::DEFAULT,
        Payload::None,
        None,
    );

    // Check errors
    assert_eq!(
        expected_errors.len(),
        errors.len(),
        "Expected {} errors, got {}:\n{}",
        expected_errors.len(),
        errors.len(),
        format_errors_for_trace(errors, &buffer, &source)
    );

    for (i, expected_err) in expected_errors.iter().enumerate() {
        let lexed_err = &errors[i];

        check_error(
            source,
            &buffer,
            lexed_err,
            expected_err.error_type(),
            expected_err.at_byte_offset(cur_start_byte_offset),
            expected_err.at_char_offset(cur_start_char_offset),
            expected_err.on_line(cur_start_line),
            expected_err.at_column(cur_start_column),
            expected_err.last_token_idx(last_non_eof_token_idx),
        );
    }
}
