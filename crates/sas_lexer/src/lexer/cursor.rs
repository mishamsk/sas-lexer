use std::str::Chars;

/// Peekable iterator over a char sequence.
/// Based on [`rustc`'s `Cursor`](https://github.com/rust-lang/rust/blob/d1b7355d3d7b4ead564dbecb1d240fcc74fff21b/compiler/rustc_lexer/src/cursor.rs)
#[derive(Debug)]
pub(crate) struct Cursor<'a> {
    /// Iterator over chars. Slightly faster than a &str.
    chars: Chars<'a>,

    /// Stores the previous char for debug assertions
    #[cfg(debug_assertions)]
    prev_char: char,
}

pub(super) const EOF_CHAR: char = '\0';

impl<'a> Cursor<'a> {
    pub(super) fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            chars: input.chars(),
            #[cfg(debug_assertions)]
            prev_char: EOF_CHAR,
        }
    }

    // pub(super) fn as_str(&self) -> &'a str {
    //     self.chars.as_str()
    // }

    /// Peeks the next symbol from the input stream without consuming it.
    /// If requested position doesn't exist, `EOF_CHAR` is returned.
    /// However, getting `EOF_CHAR` doesn't always mean actual end of file,
    /// it should be checked with `is_eof` method.
    pub(super) fn peek(&self) -> char {
        // `.next()` optimizes better than `.nth(0)`
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    /// Peeks the second symbol from the input stream without consuming it.
    pub(super) fn peek_next(&self) -> char {
        // `.next()` optimizes better than `.nth(1)`
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().unwrap_or(EOF_CHAR)
    }

    /// Returns the iterator over the remaining characters.
    /// Maybe used for moe efficient arbitrary lookahead, by avoiding
    /// extra clones.
    pub(super) fn chars(&self) -> Chars<'a> {
        self.chars.clone()
    }

    /// Checks if there is nothing more to consume.
    pub(super) fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    /// Moves to the next character.
    pub(super) fn advance(&mut self) -> Option<char> {
        let c = self.chars.next()?;

        #[cfg(debug_assertions)]
        {
            self.prev_char = c;
        }

        Some(c)
    }

    /// Moves N characters forward.
    /// Returns the last character moved to.
    // pub(super) fn advance_by(&mut self, n: usize) -> Option<char> {
    //     for _ in 0..n - 1 {
    //         self.chars.next()?;
    //     }
    //     self.chars.next()
    // }

    #[inline]
    pub(super) fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        // It was tried making optimized version of this for eg. line comments, but
        // LLVM can inline all of this and compile it down to fast iteration over bytes.
        while predicate(self.peek()) && !self.is_eof() {
            self.advance();
        }
    }

    /// Skips the next `count` bytes.
    ///
    /// ## Panics
    ///  - If `count` is larger than the remaining bytes in the input stream.
    ///  - If `count` indexes into a multi-byte character.
    pub(super) fn skip_bytes(&mut self, count: usize) {
        #[cfg(debug_assertions)]
        {
            self.prev_char = self.chars.as_str()[..count]
                .chars()
                .next_back()
                .unwrap_or('\0');
        }

        self.chars = self.chars.as_str()[count..].chars();
    }

    /// Returns the previous character. Debug only
    // #[cfg(debug_assertions)]
    // pub(super) const fn prev_char(&self) -> char {
    //     self.prev_char
    // }

    /// Returns the length of the remaining text in bytes.
    /// This is used to calculate the offset of the current token.
    pub(super) fn text_len(&self) -> u32 {
        self.chars.as_str().len() as u32
    }
}
