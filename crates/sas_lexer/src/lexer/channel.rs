use strum::Display;

#[cfg(feature = "serde")]
use serde_repr::Serialize_repr;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Display, Default)]
#[cfg_attr(feature = "serde", derive(Serialize_repr))]
#[repr(u8)]
pub enum TokenChannel {
    #[strum(serialize = "<D>")]
    #[default]
    DEFAULT,
    #[strum(serialize = "<C>")]
    COMMENT,
    #[strum(serialize = "<H>")]
    HIDDEN, // whitespace and other things to skip
}
