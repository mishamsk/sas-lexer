use strum::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Display)]
pub(crate) enum TokenChannel {
    #[strum(serialize = "<D>")]
    DEFAULT,
    #[strum(serialize = "<C>")]
    COMMENT,
    #[strum(serialize = "<H>")]
    HIDDEN, // whitespace
}

impl Default for TokenChannel {
    fn default() -> Self {
        TokenChannel::DEFAULT
    }
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
