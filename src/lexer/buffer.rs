use std::fmt;

use serde::Serialize;

use crate::lexer::channel;
use crate::lexer::token_type;

/// A token index, used get actual token data via the tokenized buffer.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize)]
pub struct TokenIdx(u32);

impl From<u32> for TokenIdx {
    fn from(val: u32) -> Self {
        TokenIdx(val)
    }
}

#[derive(Debug, Serialize)]
pub struct MaterializedToken {
    token_idx: TokenIdx,
    token_start: u32,
    token_end: u32,
    token_type: token_type::TokenType,
    start_line: u32,
    start_column: u32,
    end_line: u32,
    end_column: u32,
    token_channel: channel::TokenChannel,
    token_text: String,
    payload: Payload,
}

impl fmt::Display for TokenIdx {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A line index in the tokenized buffer.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) struct LineIdx(u32);

impl From<u32> for LineIdx {
    fn from(val: u32) -> Self {
        LineIdx(val)
    }
}

/// Enum representing varios types of extra data associated with a token.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize)]
pub(crate) enum Payload {
    None,
}

/// A struct to hold information about the lines in the tokenized buffer.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct LineInfo {
    // Zero-based byte offset of the line start in the source string slice.
    // u32 as we only support 4gb files
    start: u32,
}

/// A struct to hold information about the tokens in the tokenized buffer.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct TokenInfo {
    channel: channel::TokenChannel,
    token_type: token_type::TokenType,
    // Zero-based byte offset of the token in the source string slice.
    // u32 as we only support 4gb files
    start: u32,
    // Starting line of the token, zero-based
    // Also an index of the LineInfo in the TokenizedBuffer line_infos vector
    line: LineIdx,

    // Extra data associated with the token
    payload: Payload,
}

const MIN_CAPACITY: usize = 4;
const LINE_INFO_DIVISOR: usize = 120;
const TOKEN_INFO_DIVISOR: usize = 4;

#[derive(Debug)]
pub struct TokenizedBuffer<'a> {
    source: &'a str,
    line_infos: Vec<LineInfo>,
    token_infos: Vec<TokenInfo>,
}

impl TokenizedBuffer<'_> {
    pub(crate) fn new(source: &str, char_len: Option<usize>) -> TokenizedBuffer {
        match char_len {
            Some(0) | None => TokenizedBuffer {
                source,
                line_infos: Vec::new(),
                token_infos: Vec::new(),
            },
            Some(len) => TokenizedBuffer {
                source,
                line_infos: Vec::with_capacity(std::cmp::max(
                    len / LINE_INFO_DIVISOR,
                    MIN_CAPACITY,
                )),
                token_infos: Vec::with_capacity(std::cmp::max(
                    len / TOKEN_INFO_DIVISOR,
                    MIN_CAPACITY,
                )),
            },
        }
    }

    pub(crate) fn add_line(&mut self, start: u32) -> LineIdx {
        debug_assert!(
            (start as usize) <= self.source.len(),
            "Line start out of bounds"
        );
        self.line_infos.push(LineInfo { start });
        LineIdx(self.line_infos.len() as u32 - 1)
    }

    pub(crate) fn add_token(
        &mut self,
        channel: channel::TokenChannel,
        token_type: token_type::TokenType,
        start: u32,
        line: LineIdx,
        payload: Payload,
    ) -> TokenIdx {
        // Check that the token start is within the source string
        debug_assert!(
            start as usize <= self.source.len(),
            "Token start out of bounds"
        );

        // Check that the token start is more or equal to the start of the previous token
        if cfg!(debug_assertions) {
            if let Some(last_token) = self.token_infos.last() {
                debug_assert!(
                    start >= last_token.start,
                    "Token start before previous token start"
                );
            } else {
                debug_assert!(start == 0, "First token start not at 0");
            }
        }

        // Check that the line index is within the line_infos vector
        debug_assert!(
            line.0 as usize <= self.line_infos.len(),
            "Line index out of bounds"
        );

        // Check that the token start is greater or equal than the line start
        debug_assert!(
            start >= self.line_infos[line.0 as usize].start,
            "Token start before line start"
        );

        self.token_infos.push(TokenInfo {
            channel,
            token_type,
            start,
            line,
            payload,
        });
        TokenIdx(self.token_infos.len() as u32 - 1)
    }

    pub(crate) fn token_count(&self) -> u32 {
        self.token_infos.len() as u32
    }

    pub(crate) fn get_token_start(&self, token: TokenIdx) -> u32 {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos[tidx].start
    }

    pub(crate) fn get_token_end(&self, token: TokenIdx) -> u32 {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");

        let end = if tidx + 1 < self.token_infos.len() {
            self.token_infos[tidx + 1].start as usize
        } else {
            self.source.len()
        };

        end as u32
    }

    pub(crate) fn get_token_start_line(&self, token: TokenIdx) -> u32 {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos[tidx].line.0 + 1
    }

    pub(crate) fn get_token_end_line(&self, token: TokenIdx) -> u32 {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");

        // This is more elaborate than the start line, as we need to count
        // the number of newlines in the token text
        let tok_text = self.get_token_text(token);

        let line_count = if let Some(text) = tok_text {
            text.matches('\n').count() as u32
        } else {
            0
        };

        self.token_infos[tidx].line.0 + 1 + line_count
    }

    pub(crate) fn get_token_start_column(&self, token: TokenIdx) -> u32 {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");

        let token_info = self.token_infos[tidx];
        let line_info = self.line_infos[token_info.line.0 as usize];

        token_info.start - line_info.start
    }

    pub(crate) fn get_token_end_column(&self, token: TokenIdx) -> u32 {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");

        let token_end = self.get_token_end(token);
        let token_end_line_info = self.line_infos[(self.get_token_end_line(token) - 1) as usize];

        token_end - token_end_line_info.start
    }

    pub(crate) fn get_token_type(&self, token: TokenIdx) -> token_type::TokenType {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos[tidx].token_type
    }

    pub(crate) fn get_token_channel(&self, token: TokenIdx) -> channel::TokenChannel {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos[tidx].channel
    }

    pub(crate) fn get_token_text(&self, token: TokenIdx) -> Option<&str> {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        let token_info = self.token_infos[tidx];

        let start = token_info.start as usize;

        let end = self.get_token_end(token) as usize;

        debug_assert!(start <= end, "Token start is after end");

        if start == end {
            return None;
        }

        Some(&self.source[start..end])
    }

    pub(crate) fn get_token_payload(&self, token: TokenIdx) -> Payload {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos[tidx].payload
    }

    pub fn materialize_token(&self, token: TokenIdx) -> MaterializedToken {
        let token_start = self.get_token_start(token);
        let token_end = self.get_token_end(token);
        let start_line = self.get_token_start_line(token);
        let end_line = self.get_token_end_line(token);
        let start_column = self.get_token_start_column(token);
        let end_column = self.get_token_end_column(token);
        let token_type = self.get_token_type(token);
        let token_channel = self.get_token_channel(token);
        let token_text = self.get_token_text(token).unwrap_or_default();
        let payload = self.get_token_payload(token);

        MaterializedToken {
            token_idx: token,
            token_start,
            token_end,
            token_type,
            start_line,
            start_column,
            end_line,
            end_column,
            token_channel,
            token_text: token_text.to_string(),
            payload: payload,
        }
    }
}

impl IntoIterator for &TokenizedBuffer<'_> {
    type Item = TokenIdx;
    type IntoIter = std::iter::Map<std::ops::Range<u32>, fn(u32) -> TokenIdx>;

    fn into_iter(self) -> Self::IntoIter {
        (0..self.token_count()).map(TokenIdx::from)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenized_buffer_new() {
        let source = "Hello, world!";
        let buffer = TokenizedBuffer::new(source, Some(source.len()));

        assert_eq!(buffer.source, source);
        assert_eq!(buffer.line_infos.capacity(), 4);
        assert_eq!(buffer.token_infos.capacity(), 4);
    }

    fn get_two_tok_buffer() -> (TokenizedBuffer<'static>, TokenIdx, TokenIdx) {
        let source = "Hello, world!\nThis is a test.";
        let mut buffer = TokenizedBuffer::new(source, Some(source.len()));

        let line1 = buffer.add_line(0);
        let token1 = buffer.add_token(
            channel::TokenChannel::DEFAULT,
            token_type::TokenType::BaseCode,
            0,
            line1,
            Payload::None,
        );
        let line2 = buffer.add_line(14);
        let token2 = buffer.add_token(
            channel::TokenChannel::DEFAULT,
            token_type::TokenType::BaseCode,
            15,
            line2,
            Payload::None,
        );

        (buffer, token1, token2)
    }

    #[test]
    fn tokenized_buffer_get_token_text() {
        let (buffer, token1, token2) = get_two_tok_buffer();

        assert_eq!(buffer.get_token_text(token1).unwrap(), "Hello, world!\nT");
        assert_eq!(buffer.get_token_text(token2).unwrap(), "his is a test.");
    }

    #[test]
    fn tokenized_buffer_get_token_end() {
        let (buffer, token1, token2) = get_two_tok_buffer();

        assert_eq!(buffer.get_token_end(token1), 15);
        assert_eq!(buffer.get_token_end(token2), 29);
    }

    #[test]
    fn tokenized_buffer_get_token_start_line() {
        let (buffer, token1, token2) = get_two_tok_buffer();

        assert_eq!(buffer.get_token_start_line(token1), 1);
        assert_eq!(buffer.get_token_start_line(token2), 2);
    }

    #[test]
    fn tokenized_buffer_get_token_end_line() {
        let (buffer, token1, token2) = get_two_tok_buffer();

        assert_eq!(buffer.get_token_end_line(token1), 2);
        assert_eq!(buffer.get_token_end_line(token2), 2);
    }

    #[test]
    fn tokenized_buffer_get_token_start_column() {
        let (buffer, token1, token2) = get_two_tok_buffer();

        assert_eq!(buffer.get_token_start_column(token1), 0);
        assert_eq!(buffer.get_token_start_column(token2), 1);
    }

    #[test]
    fn tokenized_buffer_get_token_end_column() {
        let (buffer, token1, token2) = get_two_tok_buffer();

        assert_eq!(buffer.get_token_end_column(token1), 1);
        assert_eq!(buffer.get_token_end_column(token2), 15);
    }

    #[test]
    fn tokenized_buffer_get_token_type() {
        let source = "Hello, world!";
        let mut buffer = TokenizedBuffer::new(source, Some(source.len()));

        let line = buffer.add_line(0);
        let token = buffer.add_token(
            channel::TokenChannel::DEFAULT,
            token_type::TokenType::BaseCode,
            0,
            line,
            Payload::None,
        );

        assert_eq!(
            buffer.get_token_type(token),
            token_type::TokenType::BaseCode
        );
    }

    #[test]
    fn tokenized_buffer_get_token_channel() {
        let source = "Hello, world!";
        let mut buffer = TokenizedBuffer::new(source, Some(source.len()));

        let line = buffer.add_line(0);
        let token = buffer.add_token(
            channel::TokenChannel::DEFAULT,
            token_type::TokenType::BaseCode,
            0,
            line,
            Payload::None,
        );

        assert_eq!(
            buffer.get_token_channel(token),
            channel::TokenChannel::DEFAULT
        );
    }
}
