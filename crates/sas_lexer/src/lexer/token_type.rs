use sas_lexer_macro::{FromU16, ToU16};
use strum::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy, ToU16, FromU16, Display)]
pub enum TokenType {
    EOF,
    WS,
    SEMI,
    TermQuote,                // *'; and *" ";
    StringLiteral,            // 'string'
    BitTestingLiteral,        // 'stuff'b
    DateLiteral,              // 'stuff'd
    DateTimeLiteral,          // 'stuff'dt
    NameLiteral,              // 'stuff'n
    TimeLiteral,              // 'stuff't
    HexStringLiteral,         // 'stuff'x
    StringExprStart,          // "
    StringExprText,           // "&mv.-->string<--"
    StringExprEnd,            // "&mv.string" <-- this is the end
    BitTestingLiteralExprEnd, // "&mv.stuff"b
    DateLiteralExprEnd,       // "&mv.stuff"d
    DateTimeLiteralExprEnd,   // "&mv.stuff"dt
    NameLiteralExprEnd,       // "&mv.stuff"n
    TimeLiteralExprEnd,       // "&mv.stuff"t
    HexStringLiteralExprEnd,  // "&mv.stuff"x
    AMP,
    PERCENT,
    CStyleComment,  // /* ... */
    MacroVarExpr,   // &&mvar&another. etc.
    DatalinesStart, // datalines/cards[4];
    DatalinesData,  // datalines data
    DatalinesEnd,   // the closing ;[;;;]
    // Put pure second pass tokens after this line only
    BaseIdentifier,
    BaseCode,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eof_u16_conversion() {
        assert_eq!(TokenType::EOF as u16, 0);
        assert_eq!(Some(TokenType::EOF), TokenType::from_u16(0));
    }

    #[test]
    fn test_all_tokens_round_trip() {
        const TOKEN_COUNT: u16 = 29;

        for i in 0..TOKEN_COUNT {
            let token = TokenType::from_u16(i).unwrap();
            assert_eq!(i, token as u16);
        }

        assert_eq!(TokenType::from_u16(TOKEN_COUNT), None);
    }
}
