use strum::{Display, EnumCount, EnumIter};

#[cfg(feature = "serde")]
use serde_repr::Serialize_repr;

#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumCount, EnumIter, Display, Default)]
#[cfg_attr(feature = "serde", derive(Serialize_repr))]
#[repr(u8)]
pub enum TokenChannel {
    #[default]
    DEFAULT,
    COMMENT,
    HIDDEN, // whitespace and other things to skip
}
