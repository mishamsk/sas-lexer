use std::ops::Range;

use crate::lexer::channel;
use crate::lexer::token_type;

use super::text::ByteOffset;
use super::text::CharOffset;

/// A token index, used to get actual token data via the tokenized buffer.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct TokenIdx(u32);

impl TokenIdx {
    #[must_use]
    pub fn new(val: u32) -> Self {
        TokenIdx(val)
    }

    #[must_use]
    pub fn get(self) -> u32 {
        self.0
    }
}

/// A line index in the tokenized buffer.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) struct LineIdx(u32);

impl LineIdx {
    pub(crate) fn new(val: u32) -> Self {
        LineIdx(val)
    }
}

/// Enum representing varios types of extra data associated with a token.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Payload {
    None,
}

/// A struct to hold information about the lines in the tokenized buffer.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(super) struct LineInfo {
    /// Zero-based byte offset of the line start in the source string slice.
    /// u32 as we only support 4gb files
    byte_offset: ByteOffset,

    /// Zero-based char index of the line start in the source string.
    /// Char here means a Unicode code point, not graphemes. This is
    /// what Python uses to index strings, and IDEs show for cursor position.
    /// u32 as we only support 4gb files
    start: CharOffset,
}

impl LineInfo {
    #[must_use]
    pub(super) fn start(self) -> CharOffset {
        self.start
    }
}

/// A struct to hold information about the tokens in the tokenized buffer.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(super) struct TokenInfo {
    /// Channel of the token.
    channel: channel::TokenChannel,

    /// Type of the token.
    token_type: token_type::TokenType,

    /// Zero-based byte offset of the token in the source string slice.
    /// u32 as we only support 4gb files
    byte_offset: ByteOffset,

    /// Zero-based char index of the token start in the source string.
    /// Char here means a Unicode code point, not graphemes. This is
    /// what Python uses to index strings, and IDEs show for cursor position.
    /// u32 as we only support 4gb files
    start: CharOffset,

    /// Starting line of the token, zero-based.
    /// Also an index of the `LineInfo` in the `TokenizedBuffer.line_infos` vector
    line: LineIdx,

    // Extra data associated with the token
    payload: Payload,
}

impl TokenInfo {
    // #[must_use]
    // pub(super) fn channel(&self) -> channel::TokenChannel {
    //     self.channel
    // }

    #[must_use]
    pub(super) fn token_type(&self) -> token_type::TokenType {
        self.token_type
    }

    #[must_use]
    pub(super) fn byte_offset(&self) -> ByteOffset {
        self.byte_offset
    }

    #[must_use]
    pub(super) fn start(&self) -> CharOffset {
        self.start
    }

    #[must_use]
    pub(super) fn line(&self) -> LineIdx {
        self.line
    }

    // #[must_use]
    // pub(super) fn payload(&self) -> Payload {
    //     self.payload
    // }
}

const MIN_CAPACITY: usize = 4;
const LINE_INFO_DIVISOR: usize = 88;
const TOKEN_INFO_DIVISOR: usize = 4;

/// A special structure used during lexing that stores
/// the full information about lexed tokens and lines.
/// A struct of arrays, used to optimize memory usage and cache locality.
///
/// It is not used as a public API, but is used internally by the lexer.
/// The public API uses `TokenizedBuffer` instead.
#[derive(Debug, Clone)]
pub(crate) struct WorkTokenizedBuffer {
    source_len: ByteOffset,
    line_infos: Vec<LineInfo>,
    token_infos: Vec<TokenInfo>,
}

impl WorkTokenizedBuffer {
    pub(super) fn new(source: &str) -> WorkTokenizedBuffer {
        // SAFETY: This can only be created from lexer, which explicitly checks for the length
        #[allow(clippy::unwrap_used)]
        let source_len = ByteOffset::new(u32::try_from(source.len()).unwrap());

        match source.len() {
            0 => WorkTokenizedBuffer {
                source_len,
                line_infos: Vec::new(),
                token_infos: Vec::new(),
            },
            len => WorkTokenizedBuffer {
                source_len,
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

    /// Converts the `WorkTokenizedBuffer` into a `TokenizedBuffer`.
    /// This is supposed to be called only when the tokenization is finished.
    ///
    /// # Errors
    ///
    /// Returns an error if the EOF token is not the last token in the buffer.
    pub(super) fn into_detached(self) -> Result<TokenizedBuffer, &'static str> {
        if cfg!(debug_assertions)
            && !self
                .token_infos
                .last()
                .map_or(false, |t| t.token_type == token_type::TokenType::EOF)
        {
            return Err("EOF token is not the last token");
        }

        Ok(TokenizedBuffer {
            line_infos: self.line_infos.into_boxed_slice(),
            token_infos: self.token_infos.into_boxed_slice(),
        })
    }

    pub(super) fn add_line(&mut self, byte_offset: ByteOffset, start: CharOffset) -> LineIdx {
        debug_assert!(
            byte_offset <= self.source_len,
            "Line byte offset out of bounds"
        );
        self.line_infos.push(LineInfo { byte_offset, start });
        LineIdx(self.line_count() - 1)
    }

    pub(super) fn add_token(
        &mut self,
        channel: channel::TokenChannel,
        token_type: token_type::TokenType,
        byte_offset: ByteOffset,
        start: CharOffset,
        line: LineIdx,
        payload: Payload,
    ) -> TokenIdx {
        // Check that the token start is within the source string
        debug_assert!(
            byte_offset <= self.source_len,
            "Token byte offset out of bounds"
        );

        // Check that the token start is more or equal to the start of the previous token
        if cfg!(debug_assertions) {
            if let Some(last_token) = self.token_infos.last() {
                debug_assert!(
                    byte_offset >= last_token.byte_offset,
                    "Token byte offset before previous token byte offset"
                );
            } else {
                // It may be poosible for the first token to start at offset > 0
                // e.g. due to BOM
            }
        }

        // Check that the line index is within the line_infos vector
        debug_assert!(
            line.0 as usize <= self.line_infos.len(),
            "Line index out of bounds"
        );

        // Check that the token start is greater or equal than the line start
        debug_assert!(
            byte_offset >= self.line_infos[line.0 as usize].byte_offset,
            "Token byte offset before line byte offset"
        );

        self.token_infos.push(TokenInfo {
            channel,
            token_type,
            byte_offset,
            start,
            line,
            payload,
        });
        TokenIdx::new(self.token_count() - 1)
    }

    pub(super) fn pop_token(&mut self) -> Option<TokenInfo> {
        self.token_infos.pop()
    }

    // pub(super) fn iter_line_infos(&self) -> std::slice::Iter<'_, LineInfo> {
    //     self.line_infos.iter()
    // }

    pub(super) fn last_line_info(&self) -> Option<&LineInfo> {
        self.line_infos.last()
    }

    // pub(super) fn iter_token_infos(&self) -> std::slice::Iter<'_, TokenInfo> {
    //     self.token_infos.iter()
    // }

    pub(super) fn last_token_info(&self) -> Option<&TokenInfo> {
        self.token_infos.last()
    }

    #[inline]
    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub(super) fn line_count(&self) -> u32 {
        self.line_infos.len() as u32
    }

    #[inline]
    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub(super) fn token_count(&self) -> u32 {
        // theoretically number of tokens may be larger than text size,
        // which checked to be no more than u32, but this is not possible in practice
        self.token_infos.len() as u32
    }
}

/// A special structure produced by the lexer that stores the full information
/// about lexed tokens and lines.
/// A struct of arrays, used to optimize memory usage and cache locality.
///
/// It is immutable by design and can only be created by the lexer.
/// It doesn't store reference to the original source string, so it can be used
/// after the source string is dropped. But to get the text of the token,
/// the source string must be provided.
///
/// EOF token is always the last token.
#[derive(Debug, Clone)]
pub struct TokenizedBuffer {
    line_infos: Box<[LineInfo]>,
    token_infos: Box<[TokenInfo]>,
}

impl TokenizedBuffer {
    #[inline]
    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub fn line_count(&self) -> u32 {
        self.line_infos.len() as u32
    }

    #[inline]
    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub fn token_count(&self) -> u32 {
        // theoretically number of tokens may be larger than text size,
        // which checked to be no more than u32, but this is not possible in practice
        self.token_infos.len() as u32
    }

    pub(super) fn iter(&self) -> std::iter::Map<std::ops::Range<u32>, fn(u32) -> TokenIdx> {
        (0..self.token_count()).map(TokenIdx::new)
    }

    /// Returns byte offset of the token in the source string slice.
    #[must_use]
    pub fn get_token_start_byte_offset(&self, token: TokenIdx) -> ByteOffset {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos[tidx].byte_offset
    }

    /// Returns char offset of the token in the source string.
    /// Char here means a Unicode code point, not graphemes. This is
    /// what Python uses to index strings, and IDEs show for cursor position.
    #[must_use]
    pub fn get_token_start(&self, token: TokenIdx) -> CharOffset {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos[tidx].start
    }

    /// Returns byte offset right after the token in the source string slice.
    ///
    /// This is the same as the start of the next token or EOF.
    ///
    /// Note that EOF offset is 1 more than the mximum valid index in the source string.
    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub fn get_token_end_byte_offset(&self, token: TokenIdx) -> ByteOffset {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");

        if tidx + 1 < self.token_infos.len() {
            self.token_infos[tidx + 1].byte_offset
        } else {
            // Must be EOF token => same as start of EOF token
            self.token_infos[tidx].byte_offset
        }
    }

    /// Returns char offset right after the token in the source string.
    ///
    /// This is the same as the start of the next token or EOF.
    ///
    /// Note that EOF offset is 1 more than the mximum valid index in the source string.
    ///
    /// Char here means a Unicode code point, not graphemes. This is
    /// what Python uses to index strings, and IDEs show for cursor position.
    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub fn get_token_end(&self, token: TokenIdx) -> CharOffset {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");

        if tidx + 1 < self.token_infos.len() {
            self.token_infos[tidx + 1].start
        } else {
            // Must be EOF token => same as start of EOF token
            self.token_infos[tidx].start
        }
    }

    /// Returns line number of the token start, one-based.
    #[must_use]
    pub fn get_token_start_line(&self, token: TokenIdx) -> u32 {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos[tidx].line.0 + 1
    }

    /// Returns line number of the token end, one-based.
    #[must_use]
    pub fn get_token_end_line(&self, token: TokenIdx) -> u32 {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");

        // This is more elaborate than the start line, as we need to get the
        // the start line of the next token and compare the offsets
        let next_token_line_idx = if tidx + 1 < self.token_infos.len() {
            self.token_infos[tidx + 1].line
        } else {
            // Must be EOF token => same as start line of EOF token
            return self.token_infos[tidx].line.0 + 1;
        };

        let next_token_line_info = self.line_infos[next_token_line_idx.0 as usize];

        let tok_end_byte_offset = self.get_token_end_byte_offset(token);

        next_token_line_idx.0
            // lines are 1-based, but line indexes are 0-based. 
            // if this token ends beyond the start of the next token line, 
            // we need to add 1 to the line count. Otherwise it must be on the previous line,
            // so we don't need to add anything.
            + u32::from(tok_end_byte_offset.get() > next_token_line_info.byte_offset.get())
    }

    /// Returns column number of the token start, zero-based.
    #[must_use]
    pub fn get_token_start_column(&self, token: TokenIdx) -> u32 {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");

        let token_info = self.token_infos[tidx];
        let line_info = self.line_infos[token_info.line.0 as usize];

        token_info.start.get() - line_info.start.get()
    }

    /// Returns column number of the token end, zero-based.
    #[must_use]
    pub fn get_token_end_column(&self, token: TokenIdx) -> u32 {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");

        let token_end = self.get_token_end(token);
        let token_end_line_info = self.line_infos[(self.get_token_end_line(token) - 1) as usize];

        token_end.get() - token_end_line_info.start.get()
    }

    /// Returns the token type
    #[must_use]
    pub fn get_token_type(&self, token: TokenIdx) -> token_type::TokenType {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos[tidx].token_type
    }

    /// Returns the token channel
    #[must_use]
    pub fn get_token_channel(&self, token: TokenIdx) -> channel::TokenChannel {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos[tidx].channel
    }

    /// Retruns the text of the token.
    #[must_use]
    pub fn get_token_text<'a, S: AsRef<str> + 'a>(
        &'a self,
        token: TokenIdx,
        source: &'a S,
    ) -> Option<&'a str> {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        let token_info = self.token_infos[tidx];

        let text_range = Range {
            start: token_info.byte_offset.into(),
            end: self.get_token_end_byte_offset(token).into(),
        };

        debug_assert!(
            text_range.start <= text_range.end,
            "Token start is after end"
        );

        if text_range.is_empty() {
            return None;
        }

        Some(&source.as_ref()[text_range])
    }

    /// Returns the payload of the token.
    #[must_use]
    pub fn get_token_payload(&self, token: TokenIdx) -> Payload {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos[tidx].payload
    }
}

impl IntoIterator for &TokenizedBuffer {
    type Item = TokenIdx;
    type IntoIter = std::iter::Map<std::ops::Range<u32>, fn(u32) -> TokenIdx>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl TryFrom<WorkTokenizedBuffer> for TokenizedBuffer {
    type Error = &'static str;

    fn try_from(buffer: WorkTokenizedBuffer) -> Result<Self, Self::Error> {
        buffer.into_detached()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenized_buffer_new() {
        let source = "Hello, world!";
        let buffer = WorkTokenizedBuffer::new(source);

        assert_eq!(buffer.line_infos.capacity(), 4);
        assert_eq!(buffer.token_infos.capacity(), 4);
    }

    fn get_two_tok_buffer() -> (TokenizedBuffer, TokenIdx, TokenIdx) {
        let source = "Hello, world!\nThis is a test.";
        let mut buffer = WorkTokenizedBuffer::new(source);

        let line1 = buffer.add_line(ByteOffset::default(), CharOffset::default());
        let token1 = buffer.add_token(
            channel::TokenChannel::DEFAULT,
            token_type::TokenType::BaseCode,
            ByteOffset::default(),
            CharOffset::default(),
            line1,
            Payload::None,
        );
        let line2 = buffer.add_line(ByteOffset::new(14), CharOffset::new(14));
        let token2 = buffer.add_token(
            channel::TokenChannel::DEFAULT,
            token_type::TokenType::BaseCode,
            ByteOffset::new(15),
            CharOffset::new(15),
            line2,
            Payload::None,
        );

        // add EOF
        buffer.add_token(
            channel::TokenChannel::DEFAULT,
            token_type::TokenType::EOF,
            ByteOffset::new(29),
            CharOffset::new(29),
            line2,
            Payload::None,
        );

        (buffer.try_into().unwrap(), token1, token2)
    }

    #[test]
    fn check_buffer_eof_invariant() {
        let source = "Hello, world!";
        let work_buf = WorkTokenizedBuffer::new(source);

        let buf = work_buf.into_detached();
        assert_eq!(buf.unwrap_err(), "EOF token is not the last token");
    }

    #[test]
    fn tokenized_buffer_get_token_text() {
        let (buffer, token1, token2) = get_two_tok_buffer();

        assert_eq!(
            buffer
                .get_token_text(token1, &"Hello, world!\nThis is a test.")
                .unwrap(),
            "Hello, world!\nT"
        );
        assert_eq!(
            buffer
                .get_token_text(token2, &"Hello, world!\nThis is a test.")
                .unwrap(),
            "his is a test."
        );
    }

    #[test]
    fn tokenized_buffer_get_token_end() {
        let (buffer, token1, token2) = get_two_tok_buffer();

        assert_eq!(buffer.get_token_end(token1).get(), 15);
        assert_eq!(buffer.get_token_end(token2).get(), 29);
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
    fn tokenized_buffer_get_token_type_channel() {
        let source = "Hello, world!";
        let mut work_buf = WorkTokenizedBuffer::new(source);

        let line = work_buf.add_line(ByteOffset::default(), CharOffset::default());
        let token = work_buf.add_token(
            channel::TokenChannel::DEFAULT,
            token_type::TokenType::BaseCode,
            ByteOffset::default(),
            CharOffset::default(),
            line,
            Payload::None,
        );

        // add EOF
        work_buf.add_token(
            channel::TokenChannel::DEFAULT,
            token_type::TokenType::EOF,
            ByteOffset::new(13),
            CharOffset::new(13),
            line,
            Payload::None,
        );

        let buf = work_buf.into_detached().unwrap();

        assert_eq!(buf.get_token_type(token), token_type::TokenType::BaseCode);
        assert_eq!(buf.get_token_channel(token), channel::TokenChannel::DEFAULT);
    }
}
