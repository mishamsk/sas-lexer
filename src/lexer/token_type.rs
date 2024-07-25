use strum::{Display, EnumIter};

#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumIter, Display)]
pub(crate) enum TokenType {
    EOF,
    AMP,
    PERCENT,
    BaseCode,
}

impl From<TokenType> for u8 {
    fn from(variant: TokenType) -> Self {
        match variant {
            TokenType::EOF => 0,
            TokenType::AMP => 1,
            TokenType::PERCENT => 2,
            TokenType::BaseCode => 3,
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
        let mut values = variants.map(u8::from).collect::<Vec<_>>();
        values.sort_unstable();
        values.dedup();

        assert_eq!(values.len(), TokenType::iter().count());
    }
}
