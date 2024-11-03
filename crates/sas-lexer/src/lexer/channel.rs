use strum::{Display, EnumCount, EnumIter};

#[cfg(feature = "serde")]
use serde_repr::Serialize_repr;

/// Token channel.
///
/// Most tokens are on the `DEFAULT` channel. However, there are a number of
/// exceptions:
///
/// - Whitespace and other insignificant (syntax-wsie) tokens are on the `HIDDEN` channel.
/// - Comments are on the `COMMENT` channel.
#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumCount, EnumIter, Display, Default)]
#[cfg_attr(feature = "serde", derive(Serialize_repr))]
#[repr(u8)]
pub enum TokenChannel {
    #[default]
    DEFAULT,
    HIDDEN,
    COMMENT,
}
