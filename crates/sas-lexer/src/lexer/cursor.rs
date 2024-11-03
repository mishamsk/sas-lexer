use std::str::Chars;

/// Peekable iterator over a char sequence.
/// Based on [`rustc`'s `Cursor`](https://github.com/rust-lang/rust/blob/d1b7355d3d7b4ead564dbecb1d240fcc74fff21b/compiler/rustc_lexer/src/cursor.rs)
#[derive(Debug, Clone)]
pub(crate) struct Cursor<'a> {
    /// Iterator over chars. Slightly faster than a &str.
    chars: Chars<'a>,

    /// Stores the char offset in the input stream.
    char_offset: u32,

    /// Stores the previous char for debug assertions
    #[cfg(debug_assertions)]
    prev_char: char,
}

pub(super) const EOF_CHAR: char = '\0';

impl<'a> Cursor<'a> {
    pub(super) fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            chars: input.chars(),
            char_offset: 0,
            #[cfg(debug_assertions)]
            prev_char: EOF_CHAR,
        }
    }

    pub(super) fn as_str(&self) -> &'a str {
        self.chars.as_str()
    }

    /// Returns a clone of the underlying char iterator.
    pub(super) fn chars(&self) -> Chars<'a> {
        self.chars.clone()
    }

    /// Peeks the next symbol from the input stream without consuming it.
    /// If requested position doesn't exist, None is returned.
    pub(super) fn peek(&self) -> Option<char> {
        // `.next()` optimizes better than `.nth(0)`
        self.chars.clone().next()
    }

    /// Peeks the second symbol from the input stream without consuming it.
    /// If requested position doesn't exist, `EOF_CHAR` is returned.
    /// However, getting `EOF_CHAR` doesn't always mean actual end of file,
    /// it should be checked with `is_eof` method.
    pub(super) fn peek_next(&self) -> char {
        // `.next()` optimizes better than `.nth(1)`
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().unwrap_or(EOF_CHAR)
    }

    /// Moves to the next character.
    pub(super) fn advance(&mut self) -> Option<char> {
        let c = self.chars.next()?;

        #[cfg(debug_assertions)]
        {
            self.prev_char = c;
        }

        self.char_offset += 1;
        Some(c)
    }

    /// Moves N characters forward.
    ///
    /// Returns the last character advanced to or EOF if not enough characters left.
    ///
    /// SAFETY: N should be greater than 0.
    pub(super) fn advance_by(&mut self, n: u32) {
        debug_assert!(n > 0);

        for i in 0..n {
            if cfg!(debug_assertions) {
                #[cfg(debug_assertions)]
                if let Some(c) = self.chars.next() {
                    self.prev_char = c;
                } else {
                    self.char_offset += i;
                    return;
                }
            } else if self.chars.next().is_none() {
                self.char_offset += i;
                return;
            }
        }

        self.char_offset += n;
    }

    pub(super) fn eat_char(&mut self, c: char) -> bool {
        if self.peek() == Some(c) {
            self.advance();
            true
        } else {
            false
        }
    }

    #[inline]
    pub(super) fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while let Some(c) = self.peek() {
            if !predicate(c) {
                return;
            }

            self.advance();
        }
    }

    /// Returns the previous character. Debug only
    #[cfg(debug_assertions)]
    pub(super) const fn prev_char(&self) -> char {
        self.prev_char
    }

    /// Returns the length of the remaining text in bytes.
    /// This is used to calculate the offset of the current token.
    /// SAFETY: Cursor is only used from the lexer, which guarantees that the
    /// input length is not more than u32.
    #[allow(clippy::cast_possible_truncation)]
    pub(super) fn remaining_len(&self) -> u32 {
        self.chars.as_str().len() as u32
    }

    /// Returns the current char offset in the input stream.
    pub(super) fn char_offset(&self) -> u32 {
        self.char_offset
    }
}
