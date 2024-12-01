use strum::EnumMessage;

use crate::{
    error::{ErrorInfo, ErrorKind},
    lex_program, LexResult, Payload, TokenChannel, TokenIdx, TokenType, TokenizedBuffer,
};

fn token_to_string_inner<S: AsRef<str>>(
    token: TokenIdx,
    buffer: &TokenizedBuffer,
    source: &S,
) -> Result<String, ErrorKind> {
    let start_line = buffer.get_token_start_line(token)?;
    let end_line = buffer.get_token_end_line(token)?;
    let start_column = buffer.get_token_start_column(token)?;
    let end_column = buffer.get_token_end_column(token)?;
    let token_start = buffer.get_token_start(token)?.get();
    let token_end = buffer.get_token_end(token)?.get();
    let token_text = buffer
        .get_token_raw_text(token, source)?
        .unwrap_or("<no text>");
    let token_type = buffer.get_token_type(token)?;
    let token_channel = buffer.get_token_channel(token)?.to_string()[..1].to_uppercase();
    let payload = buffer.get_token_payload(token)?;

    let payload_str = match payload {
        Payload::None => "<None>".to_string(),
        Payload::Integer(val) => val.to_string(),
        Payload::Float(val) => {
            if ((val * 1000.0).round() / 1000.0 - val).abs() < f64::EPSILON {
                format!("{val:.3}")
            } else {
                format!("{val:.3e}")
            }
        }
        Payload::StringLiteral(start, end) => {
            format!("\"{}\"", buffer.get_string_literal(start, end)?.to_string())
        }
    };

    // Constructing the string representation of the token
    let token_repr = format!(
        "[@{token},{token_start}:{token_end}=\"{token_text}\",<{token_type}>,\
        L{start_line}:C{start_column}-L{end_line}:C{end_column},chl=<{token_channel}>,\
        pl={payload_str}]"
    );

    Ok(token_repr)
}

#[must_use]
pub(super) fn token_to_string<S: AsRef<str>>(
    token: TokenIdx,
    buffer: &TokenizedBuffer,
    source: &S,
) -> String {
    token_to_string_inner(token, buffer, source)
        .unwrap_or_else(|err| err.get_message().unwrap_or("Unknown error").to_string())
}

pub(super) fn error_to_string<S: AsRef<str>>(
    error: &ErrorInfo,
    buffer: &TokenizedBuffer,
    source: &S,
) -> String {
    let error_kind = error.error_kind();
    let at_byte_offset = error.at_byte_offset();
    let at_char_offset = error.at_char_offset();
    let on_line = error.on_line();
    let at_column = error.at_column();

    let last_token_str = match error.last_token() {
        Some(last_token_idx) => token_to_string(last_token_idx, buffer, source),
        None => "<no last token>".to_string(),
    };

    format!(
        "{error_kind:?} at byte offset {at_byte_offset}, char offset {at_char_offset}, \
        L{on_line}:C{at_column}. Last token: {last_token_str}"
    )
}

pub(super) trait TokenTestCase {
    fn token_type(&self) -> TokenType;
    fn token_channel(&self) -> TokenChannel;
    fn payload(&self) -> Payload;
    fn raw_text<S: AsRef<str>>(&self, source: S) -> Option<String>;
    fn text<S: AsRef<str>>(&self, source: S) -> Option<String> {
        self.raw_text(source)
    }
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

    fn raw_text<S: AsRef<str>>(&self, _source: S) -> Option<String> {
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
            TokenType::PredictedCommentStat
            | TokenType::CStyleComment
            | TokenType::MacroComment => TokenChannel::COMMENT,
            _ => TokenChannel::default(),
        }
    }

    fn payload(&self) -> Payload {
        Payload::None
    }

    fn raw_text<S: AsRef<str>>(&self, _source: S) -> Option<String> {
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
            TokenType::PredictedCommentStat
            | TokenType::CStyleComment
            | TokenType::MacroComment => TokenChannel::COMMENT,
            _ => TokenChannel::default(),
        }
    }

    fn payload(&self) -> Payload {
        self.2
    }

    fn raw_text<S: AsRef<str>>(&self, _source: S) -> Option<String> {
        if self.0.is_empty() {
            None
        } else {
            Some(self.0.to_owned())
        }
    }
}

impl TokenTestCase for (&str, TokenType, Payload, &str) {
    fn token_type(&self) -> TokenType {
        self.1
    }

    fn token_channel(&self) -> TokenChannel {
        match self.1 {
            TokenType::WS => TokenChannel::HIDDEN,
            TokenType::PredictedCommentStat
            | TokenType::CStyleComment
            | TokenType::MacroComment => TokenChannel::COMMENT,
            _ => TokenChannel::default(),
        }
    }

    fn payload(&self) -> Payload {
        self.2
    }

    fn raw_text<S: AsRef<str>>(&self, _source: S) -> Option<String> {
        if self.0.is_empty() {
            None
        } else {
            Some(self.0.to_owned())
        }
    }

    fn text<S: AsRef<str>>(&self, _source: S) -> Option<String> {
        if self.3.is_empty() {
            None
        } else {
            self.3.to_owned().into()
        }
    }
}

impl TokenTestCase for (&str, TokenType, TokenChannel, Payload, &str) {
    fn token_type(&self) -> TokenType {
        self.1
    }

    fn token_channel(&self) -> TokenChannel {
        self.2
    }

    fn payload(&self) -> Payload {
        self.3
    }

    fn raw_text<S: AsRef<str>>(&self, _source: S) -> Option<String> {
        if self.0.is_empty() {
            None
        } else {
            Some(self.0.to_owned())
        }
    }

    fn text<S: AsRef<str>>(&self, _source: S) -> Option<String> {
        if self.4.is_empty() {
            None
        } else {
            self.4.to_owned().into()
        }
    }
}

impl TokenTestCase for (TokenType, Payload, &str) {
    fn token_type(&self) -> TokenType {
        self.0
    }

    fn token_channel(&self) -> TokenChannel {
        match self.0 {
            TokenType::WS => TokenChannel::HIDDEN,
            TokenType::PredictedCommentStat
            | TokenType::CStyleComment
            | TokenType::MacroComment => TokenChannel::COMMENT,
            _ => TokenChannel::default(),
        }
    }

    fn payload(&self) -> Payload {
        self.1
    }

    fn raw_text<S: AsRef<str>>(&self, source: S) -> Option<String> {
        // If a test case doesn't provide a text, it means the whole source
        // is the token text
        match self.0 {
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

    fn text<S: AsRef<str>>(&self, _source: S) -> Option<String> {
        if self.2.is_empty() {
            None
        } else {
            self.2.to_owned().into()
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

    fn raw_text<S: AsRef<str>>(&self, source: S) -> Option<String> {
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
            TokenType::PredictedCommentStat
            | TokenType::CStyleComment
            | TokenType::MacroComment => TokenChannel::COMMENT,
            _ => TokenChannel::default(),
        }
    }

    fn payload(&self) -> Payload {
        Payload::None
    }

    fn raw_text<S: AsRef<str>>(&self, source: S) -> Option<String> {
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
            TokenType::PredictedCommentStat
            | TokenType::CStyleComment
            | TokenType::MacroComment => TokenChannel::COMMENT,
            _ => TokenChannel::default(),
        }
    }

    fn payload(&self) -> Payload {
        Payload::Float(self.1)
    }

    fn raw_text<S: AsRef<str>>(&self, source: S) -> Option<String> {
        // If a test case doesn't provide a text, it means the whole source
        // is the token text
        if source.as_ref().is_empty() {
            None
        } else {
            Some(source.as_ref().to_owned())
        }
    }
}

impl TokenTestCase for (TokenType, u64) {
    fn token_type(&self) -> TokenType {
        self.0
    }

    fn token_channel(&self) -> TokenChannel {
        match self.0 {
            TokenType::WS => TokenChannel::HIDDEN,
            TokenType::PredictedCommentStat
            | TokenType::CStyleComment
            | TokenType::MacroComment => TokenChannel::COMMENT,
            _ => TokenChannel::default(),
        }
    }

    fn payload(&self) -> Payload {
        Payload::Integer(self.1)
    }

    fn raw_text<S: AsRef<str>>(&self, source: S) -> Option<String> {
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

pub(super) fn check_token<S: AsRef<str>>(
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
        buffer
            .get_token_start_byte_offset(token)
            .expect("wrong token")
            .get(),
        "Expected start byte offset {}, got {}: {}",
        start_byte_offset,
        buffer.get_token_start(token).expect("wrong token").get(),
        token_to_string(token, buffer, &source)
    );

    assert_eq!(
        end_byte_offset,
        buffer
            .get_token_end_byte_offset(token)
            .expect("wrong token")
            .get(),
        "Expected end byte offset {}, got {}: {}",
        end_byte_offset,
        buffer.get_token_end(token).expect("wrong token").get(),
        token_to_string(token, buffer, &source)
    );

    // Char offsets
    assert_eq!(
        start_char_offset,
        buffer.get_token_start(token).expect("wrong token").get(),
        "Expected start char offset {}, got {}: {}",
        start_char_offset,
        buffer.get_token_start(token).expect("wrong token").get(),
        token_to_string(token, buffer, &source)
    );
    assert_eq!(
        end_char_offset,
        buffer.get_token_end(token).expect("wrong token").get(),
        "Expected end char offset {}, got {}: {}",
        end_char_offset,
        buffer.get_token_end(token).expect("wrong token").get(),
        token_to_string(token, buffer, &source)
    );

    // Line and column
    assert_eq!(
        start_line,
        buffer.get_token_start_line(token).expect("wrong token"),
        "Expected start line {}, got {}: {}",
        start_line,
        buffer.get_token_start_line(token).expect("wrong token"),
        token_to_string(token, buffer, &source)
    );

    let end_line = start_line + line_count - 1;

    assert_eq!(
        end_line,
        buffer.get_token_end_line(token).expect("wrong token"),
        "Expected end line {}, got {}: {}",
        end_line,
        buffer.get_token_end_line(token).expect("wrong token"),
        token_to_string(token, buffer, &source)
    );

    assert_eq!(
        start_column,
        buffer.get_token_start_column(token).expect("wrong token"),
        "Expected start column {}, got {}: {}",
        start_column,
        buffer.get_token_start_column(token).expect("wrong token"),
        token_to_string(token, buffer, &source)
    );

    assert_eq!(
        end_column,
        buffer.get_token_end_column(token).expect("wrong token"),
        "Expected end column {}, got {}: {}",
        end_column,
        buffer.get_token_end_column(token).expect("wrong token"),
        token_to_string(token, buffer, &source)
    );

    // Token type, channel, payload and text
    assert_eq!(
        token_type,
        buffer.get_token_type(token).expect("wrong token"),
        "Expected token type {:?}, got {:?}: {}",
        token_type,
        buffer.get_token_type(token),
        token_to_string(token, buffer, &source)
    );

    assert_eq!(
        token_channel,
        buffer.get_token_channel(token).expect("wrong token"),
        "Expected token channel {:?}, got {:?}: {}",
        token_channel,
        buffer.get_token_channel(token),
        token_to_string(token, buffer, &source)
    );

    assert_eq!(
        payload,
        buffer.get_token_payload(token).expect("wrong token"),
        "Expected token payload {:?}, got {:?}: {}",
        payload,
        buffer.get_token_payload(token),
        token_to_string(token, buffer, &source)
    );

    let expected_token_text = token_text.as_ref().map(std::convert::AsRef::as_ref);

    let actual_token_text = buffer.get_token_resolved_text(token, &source).unwrap();

    assert_eq!(
        expected_token_text, actual_token_text,
        "Expected text {expected_token_text:?}, got {actual_token_text:?}"
    );
}

pub(super) trait ErrorTestCase {
    fn error_kind(&self) -> ErrorKind;
    fn at_char_offset(&self, source: &str) -> u32;
    fn last_token_idx(&self, buffer: &TokenizedBuffer) -> Option<TokenIdx>;
}

impl ErrorTestCase for ErrorKind {
    fn error_kind(&self) -> ErrorKind {
        *self
    }

    fn at_char_offset(&self, source: &str) -> u32 {
        source.chars().count() as u32
    }

    fn last_token_idx(&self, buffer: &TokenizedBuffer) -> Option<TokenIdx> {
        buffer
            .iter_tokens()
            .filter(|t| buffer.get_token_type(*t).expect("wrong token") != TokenType::EOF)
            .last()
    }
}

impl ErrorTestCase for (ErrorKind, usize) {
    fn error_kind(&self) -> ErrorKind {
        self.0
    }

    fn at_char_offset(&self, _: &str) -> u32 {
        self.1 as u32
    }

    fn last_token_idx(&self, buffer: &TokenizedBuffer) -> Option<TokenIdx> {
        buffer
            .iter_tokens()
            .filter(|t| buffer.get_token_type(*t).expect("wrong token") != TokenType::EOF)
            .find(|&t| buffer.get_token_end(t).expect("wrong token").get() == self.1 as u32)
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

pub(super) fn check_error(
    source: &str,
    buffer: &TokenizedBuffer,
    error: &ErrorInfo,
    error_kind: ErrorKind,
    at_byte_offset: u32,
    at_char_offset: u32,
    on_line: u32,
    at_column: u32,
    last_token_idx: Option<TokenIdx>,
) {
    assert_eq!(
        error.error_kind(),
        error_kind,
        "Expected error kind {:?}, got {:?}: {}",
        error_kind,
        error.error_kind(),
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

pub(super) fn assert_lexing<TT: TokenTestCase, ET: ErrorTestCase>(
    source: &str,
    expected_tokens: Vec<TT>,
    expected_errors: Vec<ET>,
) {
    let LexResult { buffer, errors, .. } = lex_program(&source).unwrap();

    // Check tokens
    let tokens: Vec<TokenIdx> = buffer.iter_tokens().collect();

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
        let expected_tok_raw_text = expected_tok.raw_text(rem_source);

        // Calculate various offsets
        let cur_end_byte_offset = cur_start_byte_offset
            + expected_tok_raw_text
                .as_ref()
                .map_or(0, |text| text.len() as u32);

        let text_tok_chars = expected_tok_raw_text
            .as_ref()
            .map_or(0, |text| text.chars().count() as u32);

        let cur_end_char_offset = cur_start_char_offset + text_tok_chars;

        let line_count = expected_tok_raw_text
            .as_ref()
            .map_or(1, |text| text.lines().count() as u32);

        let ends_with_newline = expected_tok_raw_text
            .as_ref()
            .map_or(false, |text| text.ends_with('\n'));

        let cur_end_line = cur_start_line + line_count - 1;

        let cur_end_column = match expected_tok_raw_text.as_ref() {
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
            expected_tok.text(source).as_ref(),
        );

        cur_start_line = cur_end_line + u32::from(ends_with_newline);
        cur_start_byte_offset = cur_end_byte_offset;
        cur_start_char_offset = cur_end_char_offset;
        cur_start_column = if ends_with_newline { 0 } else { cur_end_column };
    }

    // Check EOF token
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

        let at_byte_offset = source
            .chars()
            .take(expected_err.at_char_offset(source) as usize)
            .map(|c| c.len_utf8() as u32)
            .sum::<u32>();
        let expected_line = source[..at_byte_offset as usize].lines().count();
        let expected_column = source[..at_byte_offset as usize]
            .lines()
            .last()
            .unwrap_or("")
            .chars()
            .count();

        check_error(
            source,
            &buffer,
            lexed_err,
            expected_err.error_kind(),
            at_byte_offset,
            expected_err.at_char_offset(source),
            expected_line as u32,
            expected_column as u32,
            expected_err.last_token_idx(&buffer),
        );
    }
}

pub(super) fn mangle_case<S: AsRef<str>>(source: S) -> String {
    source
        .as_ref()
        .chars()
        .enumerate()
        .map(|(i, c)| {
            if !c.is_ascii() {
                c
            } else if i % 2 == 0 {
                c.to_ascii_lowercase()
            } else {
                c.to_ascii_uppercase()
            }
        })
        .collect::<String>()
}
