use strum::{Display, EnumIter};

#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumIter, Display)]
pub enum TokenType {
    ERROR,
    EOF,
    WS,
    TermQuote,                     // *'; and *" ";
    SingleQuotedString,            // 'string'
    SingleQuotedBitTestingLiteral, // 'stuff'b
    SingleQuotedDateLiteral,       // 'stuff'd
    SingleQuotedDateTimeLiteral,   // 'stuff'dt
    SingleQuotedNameLiteral,       // 'stuff'n
    SingleQuotedTimeLiteral,       // 'stuff't
    SingleQuotedHexStringLiteral,  // 'stuff'x
    AMP,
    PERCENT,
    BaseCode,
    CStyleComment, // /* ... */
}

impl From<TokenType> for i16 {
    fn from(variant: TokenType) -> Self {
        match variant {
            TokenType::ERROR => -1,
            TokenType::EOF => 0,
            TokenType::WS => 1,
            TokenType::TermQuote => 2,
            TokenType::SingleQuotedString => 3,
            TokenType::SingleQuotedBitTestingLiteral => 4,
            TokenType::SingleQuotedDateLiteral => 5,
            TokenType::SingleQuotedDateTimeLiteral => 6,
            TokenType::SingleQuotedNameLiteral => 7,
            TokenType::SingleQuotedTimeLiteral => 8,
            TokenType::SingleQuotedHexStringLiteral => 9,
            TokenType::AMP => 10,
            TokenType::PERCENT => 11,
            TokenType::BaseCode => 12,
            TokenType::CStyleComment => 13,
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
