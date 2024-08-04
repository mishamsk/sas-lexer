use phf::phf_map;
use sas_lexer_macro::{FromU16, KeywordMap, ToU16};
use strum::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy, ToU16, FromU16, Display, KeywordMap)]
pub enum TokenType {
    EOF,
    UNKNOWN,
    WS,
    SEMI,           // ';'
    AMP,            // '&'+
    PERCENT,        // '%'
    LPAREN,         // '('
    RPAREN,         // ')'
    LCURLY,         // '{'
    RCURLY,         // '}'
    LBRACK,         // '['
    RBRACK,         // ']'
    STAR,           // '*'
    EXCL,           // '!'
    EXCL2,          // '!!'
    BPIPE,          // '¦'
    BPIPE2,         // '¦¦'
    PIPE2,          // '||'
    STAR2,          // '**'
    NOT,            // '¬' | '^' | '~' | '∘'
    FSLASH,         // '/'
    PLUS,           // '+'
    MINUS,          // '-'
    GTLT,           // '><'
    LTGT,           // '<>'
    LT,             // '<'
    LE,             // '<='
    NE,             // '¬=' | '^=' | '~=' | '∘='
    GT,             // '>'
    GE,             // '>='
    SoundsLike,     // '=*'
    PIPE,           // '|'
    DOT,            // '.'
    COMMA,          // ','
    COLON,          // ':'
    ASSIGN,         // '='
    DOLLAR,         // '$'
    AT,             // '@'
    HASH,           // '#'
    QUESTION,       // '?'
    TermQuote,      // *'; and *" ";
    IntegerLiteral, // 42
    // 42., 42.000 - this is seprate due to ambiguity with numeric formats
    // but not 042. or [-/+]42. because width in the format can't be 0 and can't be
    // preceded by a sign
    IntegerDotLiteral,
    FloatLiteral,             // 42.1, .42
    FloatExponentLiteral,     // 42.1e[+,-,]1 - separate due to ambiguity with numeric formats
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
    CStyleComment,            // /* ... */
    MacroVarExpr,             // &&mvar&another. etc.
    DatalinesStart,           // datalines/cards[4];
    DatalinesData,            // datalines data
    // the closing ;[;;;] after dataines is lexed as SEMI
    // Put pure second pass tokens after this line only
    BaseIdentifier,
    KwLT,
    KwLE,
    KwEQ,
    KwIN,
    KwNE,
    KwGT,
    KwGE,
    KwEQT,
    KwGTT,
    KwLTT,
    KwGET,
    KwLET,
    KwNET,
    KwAND,
    KwOR,
    // Global SAS statement keywords & shared keywords
    KwComment,
    KwLibname,
    KwFilename,
    KwClear,
    KwList,
    KwCancel,
    #[keyword("_ALL_")]
    KwAllVar,

    // Data step & proc shared keywords
    KwArray,
    KwAttrib,
    KwCall,
    KwData,
    KwDefault,
    KwDescending,
    KwFormat,
    KwGroupformat,
    KwId,
    KwIf,
    KwInfile,
    KwInformat,
    KwKeep,
    KwLabel,
    KwLength,
    KwMerge,
    #[keyword("_NULL_")]
    KwNullDataset,
    KwOutput,
    KwPgm,
    KwRename,
    KwRun,
    KwSet,
    KwStop,
    KwVar,
    KwView,
    KwWith,

    // SAS Procs
    KwDelete,
    KwNotsorted,
    KwProc,
    KwQuit,
    KwRanks,

    // SQL Keywords
    KwAll,
    KwAny,
    KwAs,
    KwAsc,
    KwBetween,
    KwBoth,
    KwBtrim,
    KwBy,
    KwCalculated,
    KwCase,
    KwConnect,
    KwConnection,
    KwContains,
    #[keyword("CORR", "CORRESPONDING")]
    KwCorr,
    KwCreate,
    KwCross,
    KwDesc,
    KwDisconnect,
    KwDistinct,
    KwDo,
    KwDrop,
    KwElse,
    KwEnd,
    KwEscape,
    KwExcept,
    #[keyword("EXEC", "EXECUTE")]
    KwExecute,
    KwExists,
    KwFor,
    KwFrom,
    KwFull,
    KwGroup,
    KwHaving,
    KwIndex,
    KwInner,
    KwInsert,
    KwIntersect,
    KwInto,
    KwIs,
    KwJoin,
    KwKey,
    KwLeading,
    KwLeft,
    KwLike,
    KwMissing,
    KwNatural,
    KwNotrim,
    KwNull,
    KwOn,
    KwOrder,
    KwOuter,
    KwPrimary,
    KwRight,
    KwSelect,
    KwSeparated,
    KwSubstring,
    KwTable,
    KwThen,
    KwTo,
    KwTrailing,
    KwTrimmed,
    KwUnion,
    KwUnique,
    KwUpdate,
    KwUsing,
    KwValues,
    KwWhen,
    KwWhere,

    // Hash object keywords
    KwDeclare,
    KwHash,
    KwHiter,

    // SAS Functions expecting formats as arguments
    KwInput,
    KwPut,
}

pub(super) fn parse_keyword<S: AsRef<str>>(ident: S) -> Option<TokenType> {
    KEYWORDS.get(ident.as_ref()).copied()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_map() {
        assert_eq!(parse_keyword("EQ"), Some(TokenType::KwEQ));
        assert_eq!(parse_keyword("IN"), Some(TokenType::KwIN));
        assert_eq!(parse_keyword("_ALL_"), Some(TokenType::KwAllVar));
        assert_eq!(parse_keyword("CORR"), Some(TokenType::KwCorr));
        assert_eq!(parse_keyword("CORRESPONDING"), Some(TokenType::KwCorr));
        assert_eq!(parse_keyword("ALL_"), None);
    }

    #[test]
    fn test_eof_u16_conversion() {
        assert_eq!(TokenType::EOF as u16, 0);
        assert_eq!(Some(TokenType::EOF), TokenType::from_u16(0));
    }

    #[test]
    fn test_all_tokens_round_trip() {
        const TOKEN_COUNT: u16 = 191;

        for i in 0..TOKEN_COUNT {
            let token = TokenType::from_u16(i).unwrap();
            assert_eq!(i, token as u16);
        }

        assert_eq!(TokenType::from_u16(TOKEN_COUNT), None);
    }
}
