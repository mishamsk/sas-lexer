use std::ops::Range;

use crate::lexer::channel;
use crate::lexer::token_type;

use super::error::LexerError;
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
    Error(LexerError),
}

/// A struct to hold information about the lines in the tokenized buffer.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct LineInfo {
    /// Zero-based byte offset of the line start in the source string slice.
    /// u32 as we only support 4gb files
    byte_offset: ByteOffset,

    /// Zero-based char index of the line start in the source string.
    /// Char here means a Unicode code point, not graphemes. This is
    /// what Python uses to index strings, and IDEs show for cursor position.
    /// u32 as we only support 4gb files
    start: CharOffset,
}

/// A struct to hold information about the tokens in the tokenized buffer.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct TokenInfo {
    channel: channel::TokenChannel,
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

const MIN_CAPACITY: usize = 4;
const LINE_INFO_DIVISOR: usize = 88;
const TOKEN_INFO_DIVISOR: usize = 4;

#[derive(Debug)]
pub struct TokenizedBuffer<'a> {
    source: &'a str,
    source_len: ByteOffset,
    line_infos: Vec<LineInfo>,
    token_infos: Vec<TokenInfo>,
}

impl TokenizedBuffer<'_> {
    pub(super) fn new(source: &str) -> TokenizedBuffer {
        // SAFETY: This can only be created from lexer, which explicitly checks for the length
        #[allow(clippy::unwrap_used)]
        let source_len = ByteOffset::new(u32::try_from(source.len()).unwrap());

        match source.len() {
            0 => TokenizedBuffer {
                source,
                source_len,
                line_infos: Vec::new(),
                token_infos: Vec::new(),
            },
            len => TokenizedBuffer {
                source,
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

    pub(super) fn iter(&self) -> std::iter::Map<std::ops::Range<u32>, fn(u32) -> TokenIdx> {
        (0..self.token_count()).map(TokenIdx::new)
    }

    pub fn into_detached(&self) -> Result<DetachedTokenizedBuffer, &str> {
        if !self
            .token_infos
            .last()
            .map_or(false, |t| t.token_type == token_type::TokenType::EOF)
        {
            return Err("EOF token is not the last token");
        }

        Ok(DetachedTokenizedBuffer {
            line_infos: self.line_infos.clone().into_boxed_slice(),
            token_infos: self.token_infos.clone().into_boxed_slice(),
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
            self.source_len
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
            // This is quite inefficient, but since normally the last token is the EOF token,
            // and it is unlikely that someone cares about the end of the EOF token, this is fine.
            #[allow(clippy::cast_possible_truncation)]
            CharOffset::new(self.source.chars().count() as u32)
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
    #[allow(clippy::cast_possible_truncation)]
    pub fn get_token_end_line(&self, token: TokenIdx) -> u32 {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");

        // This is more elaborate than the start line, as we need to count
        // the number of newlines in the token text
        let tok_text = self.get_token_text(token);

        let line_count = if let Some(text) = tok_text {
            (text.lines().count() - 1) as u32
        } else {
            0
        };

        self.token_infos[tidx].line.0 + 1 + line_count
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
    pub fn get_token_text(&self, token: TokenIdx) -> Option<&str> {
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

        Some(&self.source[text_range])
    }

    /// Returns the payload of the token.
    #[must_use]
    pub fn get_token_payload(&self, token: TokenIdx) -> Payload {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos[tidx].payload
    }
}

impl IntoIterator for &TokenizedBuffer<'_> {
    type Item = TokenIdx;
    type IntoIter = std::iter::Map<std::ops::Range<u32>, fn(u32) -> TokenIdx>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// A `TokenizedBuffer` that has been detached from the source string.
///
/// It is immutable by design and can only be created from a finished `TokenizedBuffer`.
/// Partly becomes it relies (and checks on creation) an invariant that
/// EOF token is always the last token.
#[derive(Debug, Clone)]
pub struct DetachedTokenizedBuffer {
    line_infos: Box<[LineInfo]>,
    token_infos: Box<[TokenInfo]>,
}

impl DetachedTokenizedBuffer {
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
            self.token_infos[tidx].line
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

impl IntoIterator for &DetachedTokenizedBuffer {
    type Item = TokenIdx;
    type IntoIter = std::iter::Map<std::ops::Range<u32>, fn(u32) -> TokenIdx>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenized_buffer_new() {
        let source = "Hello, world!";
        let buffer = TokenizedBuffer::new(source);

        assert_eq!(buffer.source, source);
        assert_eq!(buffer.line_infos.capacity(), 4);
        assert_eq!(buffer.token_infos.capacity(), 4);
    }

    fn get_two_tok_buffer() -> (TokenizedBuffer<'static>, TokenIdx, TokenIdx) {
        let source = "Hello, world!\nThis is a test.";
        let mut buffer = TokenizedBuffer::new(source);

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

        (buffer, token1, token2)
    }

    #[test]
    fn check_detached_buffer_eof_invariant() {
        let source = "Hello, world!";
        let buffer = TokenizedBuffer::new(source);

        let detached = buffer.into_detached();
        assert!(detached.is_err());
        assert_eq!(detached.unwrap_err(), "EOF token is not the last token");
    }

    fn validate_detached_buffer(buffer: &mut TokenizedBuffer) {
        // Add eof token if not already present
        if buffer
            .token_infos
            .last()
            .map_or(true, |t| t.token_type != token_type::TokenType::EOF)
        {
            buffer.add_token(
                channel::TokenChannel::DEFAULT,
                token_type::TokenType::EOF,
                buffer.source_len,
                CharOffset::new(buffer.source.chars().count() as u32),
                LineIdx::new(buffer.line_count() - 1),
                Payload::None,
            );
        }

        let detached = buffer.into_detached().unwrap();

        // Now compare all getters between the original and detached buffer
        assert_eq!(buffer.line_count(), detached.line_count());
        assert_eq!(buffer.token_count(), detached.token_count());
        assert_eq!(
            buffer.iter().collect::<Vec<_>>(),
            detached.iter().collect::<Vec<_>>()
        );

        for token in buffer.iter() {
            assert_eq!(
                buffer.get_token_start_byte_offset(token),
                detached.get_token_start_byte_offset(token)
            );
            assert_eq!(
                buffer.get_token_start(token),
                detached.get_token_start(token)
            );
            assert_eq!(
                buffer.get_token_end_byte_offset(token),
                detached.get_token_end_byte_offset(token)
            );
            assert_eq!(buffer.get_token_end(token), detached.get_token_end(token));
            assert_eq!(
                buffer.get_token_start_line(token),
                detached.get_token_start_line(token)
            );
            assert_eq!(
                buffer.get_token_end_line(token),
                detached.get_token_end_line(token)
            );
            assert_eq!(
                buffer.get_token_start_column(token),
                detached.get_token_start_column(token)
            );
            assert_eq!(
                buffer.get_token_end_column(token),
                detached.get_token_end_column(token)
            );
            assert_eq!(buffer.get_token_type(token), detached.get_token_type(token));
            assert_eq!(
                buffer.get_token_channel(token),
                detached.get_token_channel(token)
            );
            assert_eq!(
                buffer.get_token_text(token),
                detached.get_token_text(token, &buffer.source)
            );
            assert_eq!(
                buffer.get_token_payload(token),
                detached.get_token_payload(token)
            );
        }
    }

    #[test]
    fn test_detached_two_tokens() {
        let (mut buffer, _, _) = get_two_tok_buffer();

        validate_detached_buffer(&mut buffer);
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
    fn tokenized_buffer_get_token_type() {
        let source = "Hello, world!";
        let mut buffer = TokenizedBuffer::new(source);

        let line = buffer.add_line(ByteOffset::default(), CharOffset::default());
        let token = buffer.add_token(
            channel::TokenChannel::DEFAULT,
            token_type::TokenType::BaseCode,
            ByteOffset::default(),
            CharOffset::default(),
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
        let mut buffer = TokenizedBuffer::new(source);

        let line = buffer.add_line(ByteOffset::default(), CharOffset::default());
        let token = buffer.add_token(
            channel::TokenChannel::DEFAULT,
            token_type::TokenType::BaseCode,
            ByteOffset::default(),
            CharOffset::default(),
            line,
            Payload::None,
        );

        assert_eq!(
            buffer.get_token_channel(token),
            channel::TokenChannel::DEFAULT
        );
    }
}
