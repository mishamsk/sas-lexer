use phf::phf_map;
use sas_lexer_macro::{FromU16, KeywordMap, MacroKeywordMap, ToU16};
use strum::{Display, EnumCount, EnumIter};

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    EnumCount,
    EnumIter,
    ToU16,
    FromU16,
    Display,
    KeywordMap,
    MacroKeywordMap,
)]
#[kw_map_name = "KEYWORDS"]
#[kwm_map_name = "MKEYWORDS"]
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
    DatalinesStart,           // datalines/cards[4];
    DatalinesData,            // datalines data
    // the closing ;[;;;] after dataines is lexed as SEMI
    CharFormat, // $charformat.
    // ----------------MACRO TOKENS----------------
    MacroComment,    // %* ...;
    MacroVarExpr,    // &&mvar&another. etc.
    MacroNeverExpr,  // the weird %= in eval context
    MacroIdentifier, // %macro_name
    MacroString,     // %let var = macro_string;
    // Macro built in function keywords
    KwmEval,          // EVAL
    KwmIndex,         // INDEX
    KwmLength,        // LENGTH
    KwmLowcase,       // LOWCASE
    KwmQLowcase,      // QLOWCASE
    KwmQScan,         // QSCAN
    KwmQSubstr,       // QSUBSTR
    KwmQsysfunc,      // QSYSFUNC
    KwmQUpcase,       // QUPCASE
    KwmScan,          // SCAN
    KwmSubstr,        // SUBSTR
    KwmSymExist,      // SYMEXIST
    KwmSymGlobl,      // SYMGLOBL
    KwmSymLocal,      // SYMLOCAL
    KwmSysevalf,      // SYSEVALF
    KwmSysfunc,       // SYSFUNC
    KwmSysget,        // SYSGET
    KwmSysmacexec,    // SYSMACEXEC
    KwmSysmacexist,   // SYSMACEXIST
    KwmSysmexecdepth, // SYSMEXECDEPTH
    KwmSysmexecname,  // SYSMEXECNAME
    KwmSysprod,       // SYSPROD
    KwmUnquote,       // UNQUOTE
    KwmUpcase,        // UPCASE
    // Runtime Quoting functions
    KwmBquote,   // BQUOTE
    KwmNrBquote, // NRBQUOTE
    KwmNrQuote,  // NRQUOTE
    KwmQuote,    // QUOTE
    KwmSuperq,   // SUPERQ
    // Sudo functions (compile time quoting)
    KwmStr,   // STR
    KwmNrStr, // NRSTR
    // Statements
    KwmAbort,          // ABORT
    KwmCopy,           // COPY
    KwmDisplay,        // DISPLAY
    KwmDo,             // DO
    KwmTo,             // TO
    KwmBy,             // BY
    KwmUntil,          // UNTIL
    KwmWhile,          // WHILE
    KwmEnd,            // END
    KwmGlobal,         // GLOBAL
    KwmGoto,           // GOTO
    KwmIf,             // IF
    KwmThen,           // THEN
    KwmElse,           // ELSE
    KwmInput,          // INPUT
    KwmLet,            // LET
    KwmLocal,          // LOCAL
    KwmMacro,          // MACRO
    KwmMend,           // MEND
    KwmPut,            // PUT
    KwmReturn,         // RETURN
    KwmSymdel,         // SYMDEL
    KwmSyscall,        // SYSCALL
    KwmSysexec,        // SYSEXEC
    KwmSyslput,        // SYSLPUT
    KwmSysmacdelete,   // SYSMACDELETE
    KwmSysmstoreclear, // SYSMSTORECLEAR
    KwmSysrput,        // SYSRPUT
    KwmWindow,         // WINDOW
    // Special statements, that are half macro half base
    #[keyword("INCLUDE", "INC")]
    KwmInclude, // INCLUDE or INC
    KwmList, // LIST
    // ----------------MACRO TOKENS----------------
    // Put pure second pass tokens after this line only
    Identifier,
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
    #[keyword("_AUTOMATIC_")]
    KwAutomaticVar,
    #[keyword("_GLOBAL_")]
    KwGlobalVar,
    #[keyword("_LOCAL_")]
    KwLocalVar,
    #[keyword("_READONLY_")]
    KwReadonlyVar,
    #[keyword("_USER_")]
    KwUserVar,
    #[keyword("_WRITABLE_")]
    KwWritableVar,

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

pub(super) fn parse_macro_keyword<S: AsRef<str>>(ident: S) -> Option<TokenType> {
    MKEYWORDS.get(ident.as_ref()).copied()
}

#[inline]
pub(crate) fn get_macro_quote_call_token_type_range() -> std::ops::RangeInclusive<u16> {
    TokenType::KwmBquote as u16..=TokenType::KwmNrStr as u16
}

#[inline]
pub(crate) fn get_macro_stat_token_type_range() -> std::ops::RangeInclusive<u16> {
    TokenType::KwmAbort as u16..=TokenType::KwmWindow as u16
}

#[cfg(test)]
mod tests {
    use strum::IntoEnumIterator;

    use super::*;

    #[test]
    fn test_keyword_map() {
        assert_eq!(parse_keyword("EQ"), Some(TokenType::KwEQ));
        assert_eq!(parse_keyword("IN"), Some(TokenType::KwIN));
        assert_eq!(parse_keyword("_ALL_"), Some(TokenType::KwAllVar));
        assert_eq!(parse_keyword("CORR"), Some(TokenType::KwCorr));
        assert_eq!(parse_keyword("CORRESPONDING"), Some(TokenType::KwCorr));
        assert_eq!(parse_keyword("ALL_"), None);
        // Check a couple of keywords in macro map are NOT in the main map
        assert_eq!(parse_keyword("BQUOTE"), None);
        assert_eq!(parse_keyword("NRQUOTE"), None);
    }

    #[test]
    fn test_macro_keyword_map() {
        assert_eq!(parse_macro_keyword("BQUOTE"), Some(TokenType::KwmBquote));
        assert_eq!(parse_macro_keyword("NRQUOTE"), Some(TokenType::KwmNrQuote));
        assert_eq!(parse_macro_keyword("RANDOM"), None);
        // Check a couple of keywords in main map are NOT in the macro map
        assert_eq!(parse_macro_keyword("EQ"), None);
        assert_eq!(parse_macro_keyword("_NULL_"), None);
    }

    #[test]
    fn test_get_macro_stat_token_type_range() {
        let range = get_macro_stat_token_type_range();

        for variant in TokenType::iter() {
            let variant_val = variant as u16;

            assert_eq!(
                range.contains(&variant_val),
                matches!(
                    variant,
                    TokenType::KwmAbort
                        | TokenType::KwmCopy
                        | TokenType::KwmDisplay
                        | TokenType::KwmDo
                        | TokenType::KwmTo
                        | TokenType::KwmBy
                        | TokenType::KwmUntil
                        | TokenType::KwmWhile
                        | TokenType::KwmEnd
                        | TokenType::KwmGlobal
                        | TokenType::KwmGoto
                        | TokenType::KwmIf
                        | TokenType::KwmThen
                        | TokenType::KwmElse
                        | TokenType::KwmInput
                        | TokenType::KwmLet
                        | TokenType::KwmLocal
                        | TokenType::KwmMacro
                        | TokenType::KwmMend
                        | TokenType::KwmPut
                        | TokenType::KwmReturn
                        | TokenType::KwmSymdel
                        | TokenType::KwmSyscall
                        | TokenType::KwmSysexec
                        | TokenType::KwmSyslput
                        | TokenType::KwmSysmacdelete
                        | TokenType::KwmSysmstoreclear
                        | TokenType::KwmSysrput
                        | TokenType::KwmWindow
                )
            );
        }
    }

    #[test]
    fn test_get_macro_quote_call_token_type_range() {
        let range = get_macro_quote_call_token_type_range();

        for variant in TokenType::iter() {
            let variant_val = variant as u16;

            assert_eq!(
                range.contains(&variant_val),
                matches!(
                    variant,
                    TokenType::KwmBquote
                        | TokenType::KwmNrBquote
                        | TokenType::KwmNrQuote
                        | TokenType::KwmQuote
                        | TokenType::KwmSuperq
                        | TokenType::KwmStr
                        | TokenType::KwmNrStr
                )
            );
        }
    }

    #[test]
    fn test_eof_u16_conversion() {
        assert_eq!(TokenType::EOF as u16, 0);
        assert_eq!(Some(TokenType::EOF), TokenType::from_u16(0));
    }

    #[test]
    fn test_all_tokens_round_trip() {
        for i in 0..=u16::MAX {
            match TokenType::from_u16(i) {
                Some(token) => {
                    assert_eq!(i, token as u16);
                }
                None => {
                    assert_eq!(
                        i as usize,
                        TokenType::COUNT,
                        "Unexpected number of tokens: {i}"
                    );
                    break;
                }
            };
        }
    }
}
