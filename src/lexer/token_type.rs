use serde::Serialize;
use strum::{Display, EnumIter};

#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumIter, Display, Serialize)]
pub(crate) enum TokenType {
    ERROR,
    EOF,
    WS,
    TermQuote, // *'; and *" ";
    AMP,
    PERCENT,
    BaseCode,
}

impl From<TokenType> for i16 {
    fn from(variant: TokenType) -> Self {
        match variant {
            TokenType::ERROR => -1,
            TokenType::EOF => 0,
            TokenType::WS => 1,
            TokenType::TermQuote => 2,
            TokenType::AMP => 10,
            TokenType::PERCENT => 11,
            TokenType::BaseCode => 12,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use strum::IntoEnumIterator;

    #[test]
    fn token_type_values_are_unique() {
        let variants = TokenType::iter();
        let mut values = variants.map(i16::from).collect::<Vec<_>>();
        values.sort_unstable();
        values.dedup();

        assert_eq!(values.len(), TokenType::iter().count());
    }
}
