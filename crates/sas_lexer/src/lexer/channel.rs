use strum::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Display, Default)]
pub enum TokenChannel {
    #[strum(serialize = "<D>")]
    #[default]
    DEFAULT,
    #[strum(serialize = "<C>")]
    COMMENT,
    #[strum(serialize = "<H>")]
    HIDDEN, // whitespace and other things to skip
}

impl From<TokenChannel> for u8 {
    fn from(variant: TokenChannel) -> Self {
        match variant {
            TokenChannel::DEFAULT => 0,
            TokenChannel::COMMENT => 1,
            TokenChannel::HIDDEN => 2,
        }
    }
}
