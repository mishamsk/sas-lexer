use std::fmt;
use std::ops::Range;

use super::channel::TokenChannel;
use super::token_type::TokenType;

use super::text::ByteOffset;
use super::text::CharOffset;

/// A token index, used to get actual token data via the tokenized buffer.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct TokenIdx(u32);

impl TokenIdx {
    #[must_use]
    fn new(val: u32) -> Self {
        TokenIdx(val)
    }
}

impl fmt::Display for TokenIdx {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A line index in the tokenized buffer.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) struct LineIdx(u32);

impl LineIdx {
    fn new(val: u32) -> Self {
        LineIdx(val)
    }
}

/// Enum representing varios types of extra data associated with a token.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Payload {
    None,
    /// Stores parsed integer value
    Integer(i64),
    /// Stores parsed float value
    Float(f64),
    /// Stores the range in buffer.string_literals with the
    /// properly unescaped value of a lexed string with escaped
    /// (quoted in SAS parlance) characters.
    StringLiteral(u32, u32),
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
#[derive(Debug, PartialEq, Clone, Copy)]
pub(super) struct TokenInfo {
    /// Channel of the token.
    channel: TokenChannel,

    /// Type of the token.
    token_type: TokenType,

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
    // pub(super) fn channel(&self) -> TokenChannel {
    //     self.channel
    // }

    #[must_use]
    pub(super) fn token_type(&self) -> TokenType {
        self.token_type
    }

    // #[must_use]
    // pub(super) fn byte_offset(&self) -> ByteOffset {
    //     self.byte_offset
    // }

    // #[must_use]
    // pub(super) fn start(&self) -> CharOffset {
    //     self.start
    // }

    // #[must_use]
    // pub(super) fn line(&self) -> LineIdx {
    //     self.line
    // }

    // #[must_use]
    // pub(super) fn payload(&self) -> Payload {
    //     self.payload
    // }
}

/// Specifies minimal initial capacity of token_infos & line_infos vectors
const MIN_CAPACITY: usize = 4;

/// Heursitic for determining an optimal initial capactiy for line_infos vector
const LINE_INFO_CAPACITY_DIVISOR: usize = 88;

/// Heursitic for determining an optimal initial capactiy for token_infos vector
const TOKEN_INFO_CAPACITY_DIVISOR: usize = 4;

/// Heursitic for determining an optimal initial capactiy for unescaped string literals vector
/// I didn't do a scientific test of the frequency of quote usage, but between
/// %nrstr, %str, 'string with '' quote', "string with "" quote" and the fact
/// that one occurence of smth. like %% inside %nrstr will put the whole contents
/// into our buffer - thought we may afford overallocating. Let it be 5%
const STR_LIT_CAPACITY_DIVISOR: usize = 20;

const TOKEN_IDX_OUT_OF_BOUNDS: &str = "Token index out of bounds";

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
    /// Stores unescaped string literals as a single continous string
    /// Payloads of tokens that repsent strings with escaped characters
    /// store the range of the literal within this string.
    string_literals_buffer: String,
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
                string_literals_buffer: String::new(),
            },
            len => WorkTokenizedBuffer {
                source_len,
                line_infos: Vec::with_capacity(std::cmp::max(
                    len / LINE_INFO_CAPACITY_DIVISOR,
                    MIN_CAPACITY,
                )),
                token_infos: Vec::with_capacity(std::cmp::max(
                    len / TOKEN_INFO_CAPACITY_DIVISOR,
                    MIN_CAPACITY,
                )),
                string_literals_buffer: String::with_capacity(std::cmp::max(
                    len / STR_LIT_CAPACITY_DIVISOR,
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
        if !self
            .token_infos
            .last()
            .map_or(false, |t| t.token_type == TokenType::EOF)
        {
            return Err("EOF token is not the last token");
        }

        Ok(TokenizedBuffer {
            line_infos: self.line_infos.into_boxed_slice(),
            token_infos: self.token_infos.into_boxed_slice(),
            string_literals_buffer: self.string_literals_buffer,
        })
    }

    pub(super) fn add_line(&mut self, byte_offset: ByteOffset, start: CharOffset) -> LineIdx {
        debug_assert!(
            byte_offset <= self.source_len,
            "Line byte offset out of bounds"
        );
        self.line_infos.push(LineInfo { byte_offset, start });
        LineIdx::new(self.line_count() - 1)
    }

    pub(super) fn add_token(
        &mut self,
        channel: TokenChannel,
        token_type: TokenType,
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

        #[allow(clippy::indexing_slicing)]
        if cfg!(debug_assertions) {
            // Check that the token start is more or equal to the start of the previous token
            if let Some(last_token) = self.token_infos.last() {
                debug_assert!(
                    byte_offset >= last_token.byte_offset,
                    "Token byte offset before previous token byte offset"
                );
            } else {
                // It may be poosible for the first token to start at offset > 0
                // e.g. due to BOM
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
        }

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

    /// Adds an unescaped string to the buffer.
    ///
    /// # Returns
    ///
    /// Returns a tuple of the start and end index of the string in the buffer.string_literals.
    ///
    /// Use it to generate a `Payload::StringLiteral` for a token.
    #[allow(clippy::cast_possible_truncation)]
    pub(super) fn add_string_literal<S: AsRef<str>>(&mut self, literal: S) -> (u32, u32) {
        // SAFETY: This can only be created from lexer, which restricts the length of the source
        let start = self.string_literals_buffer.len() as u32;
        self.string_literals_buffer.push_str(literal.as_ref());

        (start, self.string_literals_buffer.len() as u32)
    }

    #[inline]
    pub(super) fn next_string_literal_start(&self) -> u32 {
        self.string_literals_buffer.len() as u32
    }

    pub(super) fn update_last_token(
        &mut self,
        channel: TokenChannel,
        token_type: TokenType,
        payload: Payload,
    ) -> bool {
        self.token_infos.last_mut().map_or(false, |t| {
            t.channel = channel;
            t.token_type = token_type;
            t.payload = payload;
            true
        })
    }

    pub(super) fn rollback(&mut self, last_token: Option<TokenIdx>) -> Result<(), &'static str> {
        if let Some(last_token_idx) = last_token {
            // Get info of the last token to be retained
            let last_token_info = self
                .token_infos
                .get(last_token_idx.0 as usize)
                .ok_or(TOKEN_IDX_OUT_OF_BOUNDS)?;

            // Get the index of the token end line
            let last_token_end_line =
                self.get_token_end_line_idx(last_token_idx.0 as usize, last_token_info)?;

            // Remove all tokens after the last token
            self.token_infos.truncate(last_token_idx.0 as usize + 1);

            // Remove all lines after the last token end line
            self.line_infos.truncate(last_token_end_line.0 as usize + 1);

            // We do not bother to remove potentially unused string literals
            Ok(())
        } else {
            Ok(())
        }
    }

    // pub(super) fn iter_line_infos(&self) -> std::slice::Iter<'_, LineInfo> {
    //     self.line_infos.iter()
    // }

    pub(super) fn last_line(&self) -> Option<LineIdx> {
        match self.line_count() {
            0 => None,
            n => Some(LineIdx::new(n - 1)),
        }
    }

    pub(super) fn last_line_info(&self) -> Option<&LineInfo> {
        self.line_infos.last()
    }

    // pub(super) fn iter_token_infos(&self) -> std::slice::Iter<'_, TokenInfo> {
    //     self.token_infos.iter()
    // }

    pub(super) fn last_token(&self) -> Option<TokenIdx> {
        match self.token_count() {
            0 => None,
            n => Some(TokenIdx::new(n - 1)),
        }
    }

    pub(super) fn last_token_info(&self) -> Option<&TokenInfo> {
        self.token_infos.last()
    }

    pub(super) fn last_token_info_on_channel_info(
        &self,
        channel: TokenChannel,
    ) -> Option<&TokenInfo> {
        self.token_infos.iter().rev().find(|t| t.channel == channel)
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

    /// Returns index of line info of the token end.
    ///
    /// # Errors
    ///
    /// Returns an error if the token index is out of bounds.
    fn get_token_end_line_idx(
        &self,
        tidx: usize,
        token_info: &TokenInfo,
    ) -> Result<LineIdx, &'static str> {
        // This is more elaborate than the start line, as we need to get the
        // the start line of the next token and compare the offsets
        if tidx == self.token_infos.len() - 1 {
            // Must be EOF token => same as start line of EOF token
            return Ok(token_info.line);
        }

        let next_tok_inf = self
            .token_infos
            .get(tidx + 1)
            .ok_or(TOKEN_IDX_OUT_OF_BOUNDS)?;

        let (next_token_line_idx, next_token_line_info) = self
            .line_infos
            .get(next_tok_inf.line.0 as usize)
            .map(|li| (next_tok_inf.line, li))
            .ok_or(TOKEN_IDX_OUT_OF_BOUNDS)?;

        Ok(LineIdx::new(
            next_token_line_idx.0
            // If the next token starts at the start of the next line,
            // means this token ends on the previous line, =>
            // we must subtract 1 from the next token start line index
            // Otherwise this tokens ends on the same line as the next token starts
            // SAFETY: it is logicaly imporssible to overflow here
            - u32::from(next_tok_inf.byte_offset == next_token_line_info.byte_offset),
        ))
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
    string_literals_buffer: String,
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

    fn iter(&self) -> std::iter::Map<std::ops::Range<u32>, fn(u32) -> TokenIdx> {
        (0..self.token_count()).map(TokenIdx::new)
    }

    /// Returns byte offset of the token in the source string slice.
    ///
    /// # Errors
    ///
    /// Returns an error if the token index is out of bounds.
    pub fn get_token_start_byte_offset(&self, token: TokenIdx) -> Result<ByteOffset, &'static str> {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos
            .get(tidx)
            .map_or(Err(TOKEN_IDX_OUT_OF_BOUNDS), |t| Ok(t.byte_offset))
    }

    /// Returns char offset of the token in the source string.
    /// # Errors
    ///
    /// Returns an error if the token index is out of bounds.
    ///
    /// Char here means a Unicode code point, not graphemes. This is
    /// what Python uses to index strings, and IDEs show for cursor position.
    pub fn get_token_start(&self, token: TokenIdx) -> Result<CharOffset, &'static str> {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos
            .get(tidx)
            .map_or(Err(TOKEN_IDX_OUT_OF_BOUNDS), |t| Ok(t.start))
    }

    /// Returns byte offset right after the token in the source string slice.
    /// # Errors
    ///
    /// Returns an error if the token index is out of bounds.
    ///
    /// This is the same as the start of the next token or EOF.
    ///
    /// Note that EOF offset is 1 more than the mximum valid index in the source string.    
    pub fn get_token_end_byte_offset(&self, token: TokenIdx) -> Result<ByteOffset, &'static str> {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");

        if tidx + 1 < self.token_infos.len() {
            self.token_infos
                .get(tidx + 1)
                .map_or(Err(TOKEN_IDX_OUT_OF_BOUNDS), |t| Ok(t.byte_offset))
        } else {
            // Must be EOF token => same as start of EOF token
            self.token_infos
                .get(tidx)
                .map_or(Err(TOKEN_IDX_OUT_OF_BOUNDS), |t| Ok(t.byte_offset))
        }
    }

    /// Returns char offset right after the token in the source string.
    /// # Errors
    ///
    /// Returns an error if the token index is out of bounds.
    ///
    /// This is the same as the start of the next token or EOF.
    ///
    /// Note that EOF offset is 1 more than the mximum valid index in the source string.
    ///
    /// Char here means a Unicode code point, not graphemes. This is
    /// what Python uses to index strings, and IDEs show for cursor position.    
    pub fn get_token_end(&self, token: TokenIdx) -> Result<CharOffset, &'static str> {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");

        if tidx + 1 < self.token_infos.len() {
            self.token_infos
                .get(tidx + 1)
                .map_or(Err(TOKEN_IDX_OUT_OF_BOUNDS), |t| Ok(t.start))
        } else {
            // Must be EOF token => same as start of EOF token
            self.token_infos
                .get(tidx)
                .map_or(Err(TOKEN_IDX_OUT_OF_BOUNDS), |t| Ok(t.start))
        }
    }

    /// Returns line number of the token start, one-based.
    ///
    /// # Errors
    ///
    /// Returns an error if the token index is out of bounds.
    pub fn get_token_start_line(&self, token: TokenIdx) -> Result<u32, &'static str> {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos
            .get(tidx)
            .map_or(Err(TOKEN_IDX_OUT_OF_BOUNDS), |t| Ok(t.line.0 + 1))
    }

    /// Returns line number of the token end, one-based.
    ///
    /// # Errors
    ///
    /// Returns an error if the token index is out of bounds.
    pub fn get_token_end_line(&self, token: TokenIdx) -> Result<u32, &'static str> {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");

        // This is more elaborate than the start line, as we need to get the
        // the start line of the next token and compare the offsets
        if tidx == self.token_infos.len() - 1 {
            // Must be EOF token => same as start line of EOF token
            return self.get_token_start_line(token);
        }

        let next_tok_inf = self
            .token_infos
            .get(tidx + 1)
            .ok_or(TOKEN_IDX_OUT_OF_BOUNDS)?;

        let (next_token_line_idx, next_token_line_info) = self
            .line_infos
            .get(next_tok_inf.line.0 as usize)
            .map(|li| (next_tok_inf.line, li))
            .ok_or(TOKEN_IDX_OUT_OF_BOUNDS)?;

        Ok(next_token_line_idx.0
            // lines are 1-based, but line indexes are 0-based. 
            // If the next token starts not at the start of the next line,
            // means this token ends on the next token line, =>
            // we need to add 1 to the line count. Otherwise it must be on the previous line,
            // so we don't need to add anything.
            + u32::from(next_tok_inf.byte_offset > next_token_line_info.byte_offset))
    }

    /// Returns column number of the token start, zero-based.
    ///
    /// # Errors
    ///
    /// Returns an error if the token index is out of bounds.
    pub fn get_token_start_column(&self, token: TokenIdx) -> Result<u32, &'static str> {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");

        let token_info = self.token_infos.get(tidx).ok_or(TOKEN_IDX_OUT_OF_BOUNDS)?;
        let line_info = self
            .line_infos
            .get(token_info.line.0 as usize)
            .ok_or(TOKEN_IDX_OUT_OF_BOUNDS)?;

        Ok(token_info.start.get() - line_info.start.get())
    }

    /// Returns column number of the token end, zero-based.
    ///
    /// # Errors
    ///
    /// Returns an error if the token index is out of bounds.
    pub fn get_token_end_column(&self, token: TokenIdx) -> Result<u32, &'static str> {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");

        let token_end = self.get_token_end(token)?;
        let token_end_line_info =
            self.get_token_end_line(token)
                .and_then(|end_line_one_based| {
                    self.line_infos
                        .get((end_line_one_based - 1) as usize)
                        .ok_or(TOKEN_IDX_OUT_OF_BOUNDS)
                })?;

        Ok(token_end.get() - token_end_line_info.start.get())
    }

    /// Returns the token type
    ///
    /// # Errors
    ///
    /// Returns an error if the token index is out of bounds.
    pub fn get_token_type(&self, token: TokenIdx) -> Result<TokenType, &'static str> {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos
            .get(tidx)
            .map_or(Err(TOKEN_IDX_OUT_OF_BOUNDS), |t| Ok(t.token_type))
    }

    /// Returns the token channel
    ///
    /// # Errors
    ///
    /// Returns an error if the token index is out of bounds.
    pub fn get_token_channel(&self, token: TokenIdx) -> Result<TokenChannel, &'static str> {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos
            .get(tidx)
            .map_or(Err(TOKEN_IDX_OUT_OF_BOUNDS), |t| Ok(t.channel))
    }

    /// Retruns the text slice from the source using the token range.
    /// If the range is empty, returns `None`, not an empty string!
    ///
    /// # Errors
    ///
    /// Returns an error if the token index is out of bounds.
    pub fn get_token_raw_text<'a, S: AsRef<str> + 'a>(
        &'a self,
        token: TokenIdx,
        source: &'a S,
    ) -> Result<Option<&'a str>, &'static str> {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");

        let Some(token_info) = self.token_infos.get(tidx) else {
            return Err(TOKEN_IDX_OUT_OF_BOUNDS);
        };

        let text_range = if let Ok(end_offset) = self.get_token_end_byte_offset(token) {
            Range {
                start: token_info.byte_offset.into(),
                end: end_offset.into(),
            }
        } else {
            return Err(TOKEN_IDX_OUT_OF_BOUNDS);
        };

        debug_assert!(
            text_range.start <= text_range.end,
            "Token start is after end"
        );

        if text_range.is_empty() {
            return Ok(None);
        }

        Ok(source.as_ref().get(text_range))
    }

    /// Returns the payload of the token.
    ///
    /// # Errors
    ///
    /// Returns an error if the token index is out of bounds.
    pub fn get_token_payload(&self, token: TokenIdx) -> Result<Payload, &'static str> {
        let tidx = token.0 as usize;

        debug_assert!(tidx < self.token_infos.len(), "Token index out of bounds");
        self.token_infos
            .get(tidx)
            .map_or(Err(TOKEN_IDX_OUT_OF_BOUNDS), |t| Ok(t.payload))
    }

    /// Returns the string literal from range defined by start and stop.
    ///
    /// The range is supposed to come from the `Payload::StringLiteral` of a token.
    ///
    /// # Errors
    ///
    /// Returns an error if the range is out of bounds.
    pub fn get_string_literal(&self, start: u32, stop: u32) -> Result<&str, &'static str> {
        let start = start as usize;
        let stop = stop as usize;

        self.string_literals_buffer
            .get(start..stop)
            .map_or(Err("String literal range out of bounds"), |s| Ok(s))
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
            TokenChannel::DEFAULT,
            TokenType::UNKNOWN,
            ByteOffset::default(),
            CharOffset::default(),
            line1,
            Payload::None,
        );
        let line2 = buffer.add_line(ByteOffset::new(14), CharOffset::new(14));
        let token2 = buffer.add_token(
            TokenChannel::DEFAULT,
            TokenType::UNKNOWN,
            ByteOffset::new(15),
            CharOffset::new(15),
            line2,
            Payload::None,
        );

        // add EOF
        buffer.add_token(
            TokenChannel::DEFAULT,
            TokenType::EOF,
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
                .get_token_raw_text(token1, &"Hello, world!\nThis is a test.")
                .unwrap()
                .unwrap(),
            "Hello, world!\nT"
        );
        assert_eq!(
            buffer
                .get_token_raw_text(token2, &"Hello, world!\nThis is a test.")
                .unwrap()
                .unwrap(),
            "his is a test."
        );
    }

    #[test]
    fn tokenized_buffer_get_token_end() {
        let (buffer, token1, token2) = get_two_tok_buffer();

        assert_eq!(buffer.get_token_end(token1).expect("wrong token").get(), 15);
        assert_eq!(buffer.get_token_end(token2).expect("wrong token").get(), 29);
    }

    #[test]
    fn tokenized_buffer_get_token_start_line() {
        let (buffer, token1, token2) = get_two_tok_buffer();

        assert_eq!(buffer.get_token_start_line(token1).expect("wrong token"), 1);
        assert_eq!(buffer.get_token_start_line(token2).expect("wrong token"), 2);
    }

    #[test]
    fn tokenized_buffer_get_token_end_line() {
        let (buffer, token1, token2) = get_two_tok_buffer();

        assert_eq!(buffer.get_token_end_line(token1).expect("wrong token"), 2);
        assert_eq!(buffer.get_token_end_line(token2).expect("wrong token"), 2);
    }

    #[test]
    fn tokenized_buffer_get_token_start_column() {
        let (buffer, token1, token2) = get_two_tok_buffer();

        assert_eq!(
            buffer.get_token_start_column(token1).expect("wrong token"),
            0
        );
        assert_eq!(
            buffer.get_token_start_column(token2).expect("wrong token"),
            1
        );
    }

    #[test]
    fn tokenized_buffer_get_token_end_column() {
        let (buffer, token1, token2) = get_two_tok_buffer();

        assert_eq!(buffer.get_token_end_column(token1).expect("wrong token"), 1);
        assert_eq!(
            buffer.get_token_end_column(token2).expect("wrong token"),
            15
        );
    }

    #[test]
    fn tokenized_buffer_get_token_type_channel() {
        let source = "Hello, world!";
        let mut work_buf = WorkTokenizedBuffer::new(source);

        let line = work_buf.add_line(ByteOffset::default(), CharOffset::default());
        let token = work_buf.add_token(
            TokenChannel::DEFAULT,
            TokenType::UNKNOWN,
            ByteOffset::default(),
            CharOffset::default(),
            line,
            Payload::None,
        );

        // add EOF
        work_buf.add_token(
            TokenChannel::DEFAULT,
            TokenType::EOF,
            ByteOffset::new(13),
            CharOffset::new(13),
            line,
            Payload::None,
        );

        let buf = work_buf.into_detached().unwrap();

        assert_eq!(
            buf.get_token_type(token).expect("wrong token"),
            TokenType::UNKNOWN
        );
        assert_eq!(
            buf.get_token_channel(token).expect("wrong token"),
            TokenChannel::DEFAULT
        );
    }
}
