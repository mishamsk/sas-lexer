use std::ops::{Add, Sub};

/// Various text-related types

/// A byte offset in the input text. We limit the input text to 4GB => 32 bits
///
/// This wrapper is used to clearly distinguish between byte offsets and char offsets.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
pub struct ByteOffset(u32);

impl ByteOffset {
    pub fn new(val: u32) -> Self {
        ByteOffset(val)
    }

    pub fn get(self) -> u32 {
        self.0
    }
}

impl From<ByteOffset> for u32 {
    fn from(offset: ByteOffset) -> Self {
        offset.0
    }
}

impl From<ByteOffset> for usize {
    fn from(offset: ByteOffset) -> Self {
        offset.0 as usize
    }
}

impl Add<u32> for ByteOffset {
    type Output = ByteOffset;

    fn add(self, rhs: u32) -> Self::Output {
        ByteOffset(self.0 + rhs)
    }
}

impl Sub<u32> for ByteOffset {
    type Output = ByteOffset;

    fn sub(self, rhs: u32) -> Self::Output {
        ByteOffset(self.0 - rhs)
    }
}

/// A char offset in the input text. We limit the input text to 4GB => 32 bits
///
/// Chars are Unicode Scalar Values (code points), not grapheme clusters.
///
/// This wrapper is used to clearly distinguish between byte offsets and char offsets.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
pub struct CharOffset(u32);

impl CharOffset {
    pub fn new(val: u32) -> Self {
        CharOffset(val)
    }

    pub fn get(self) -> u32 {
        self.0
    }
}

impl From<CharOffset> for u32 {
    fn from(offset: CharOffset) -> Self {
        offset.0
    }
}

impl From<CharOffset> for usize {
    fn from(offset: CharOffset) -> Self {
        offset.0 as usize
    }
}
