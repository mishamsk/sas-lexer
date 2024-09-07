use strum::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Display, Default)]
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
