use std::fmt;

use crate::lexer::channel;
use crate::lexer::token_type;

/// A token index, used get actual token data via the tokenized buffer.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct TokenIdx(pub u32);

impl fmt::Display for TokenIdx {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A line index in the tokenized buffer.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) struct LineIdx(u32);

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
}

const MIN_CAPACITY: usize = 4;
const LINE_INFO_DIVISOR: usize = 120;
const TOKEN_INFO_DIVISOR: usize = 4;

#[derive(Debug)]
pub(crate) struct TokenizedBuffer<'a> {
    source: &'a str,
    line_infos: Vec<LineInfo>,
    token_infos: Vec<TokenInfo>,
}

impl TokenizedBuffer<'_> {
    pub(crate) fn new(source: &str, char_len: Option<usize>) -> TokenizedBuffer {
        match char_len {
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
            None => TokenizedBuffer {
                source,
                line_infos: Vec::new(),
                token_infos: Vec::new(),
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
    ) -> TokenIdx {
        debug_assert!(
            start as usize <= self.source.len(),
            "Token start out of bounds"
        );
        debug_assert!(
            line.0 as usize <= self.line_infos.len(),
            "Line index out of bounds"
        );
        self.token_infos.push(TokenInfo {
            channel,
            token_type,
            start,
            line,
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

        let line_count = tok_text.unwrap_or("").matches('\n').count() as u32;

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
        );
        let line2 = buffer.add_line(14);
        let token2 = buffer.add_token(
            channel::TokenChannel::DEFAULT,
            token_type::TokenType::BaseCode,
            15,
            line2,
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
        );

        assert_eq!(
            buffer.get_token_channel(token),
            channel::TokenChannel::DEFAULT
        );
    }
}
