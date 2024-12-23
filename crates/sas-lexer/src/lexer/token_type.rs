use phf::phf_map;
use sas_lexer_macro::{KeywordMap, MacroKeywordMap, TokenTypeSubset};
use strum::{Display, EnumCount, EnumIter};

#[cfg(feature = "serde")]
use serde_repr::Serialize_repr;

/// What you expect - the token types.
///
/// Order of variants are VERY important, as multiple
/// predicates rely on integer values of the variants that
/// are generated automatically from the order.
///
/// Naming is also important. Namely all non-macro keywords
/// must be preceded with `Kw` and macro keywords are all start
/// with `Kwm`. These are used to autogenerate static hash maps
/// for keywords and other downstream codegen outside of this crate
#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    PartialOrd,
    Ord,
    EnumCount,
    EnumIter,
    Display,
    KeywordMap,
    MacroKeywordMap,
    TokenTypeSubset,
)]
#[cfg_attr(feature = "serde", derive(Serialize_repr))]
#[kw_map_name = "KEYWORDS"]
#[kwm_map_name = "MKEYWORDS"]
#[subset(name = TokenTypeMacroCallOrStat, start = MacroIdentifier, end = KwmRun)]
#[repr(u16)]
pub enum TokenType {
    EOF,
    /// A special virtual token that is emitted between open code and macro statements
    /// when there is no "natural" separator, or when semicolon is missing between two
    /// macro statements (a coding error).
    ///
    /// This may be used by a downstream parser as a reliable terminating token for dynamic
    /// open code and thus avoid doing lookaheads. Dynamic, means that the statement has a
    /// macro statements in it, like `data %if cond %then %do; t1 %end; %else %do; t2 %end;;`
    ///
    /// It is emitted when `macro_sep` feature is enabled.
    MacroSep,
    CatchAll,
    WS,
    SEMI,       // ';'
    AMP,        // '&'+ (except if it is a macro var resolve operator)
    PERCENT,    // '%'
    LPAREN,     // '('
    RPAREN,     // ')'
    LCURLY,     // '{'
    RCURLY,     // '}'
    LBRACK,     // '['
    RBRACK,     // ']'
    STAR,       // '*'
    EXCL,       // '!'
    EXCL2,      // '!!'
    BPIPE,      // '¦'
    BPIPE2,     // '¦¦'
    PIPE2,      // '||'
    STAR2,      // '**'
    NOT,        // '¬' | '^' | '~' | '∘'
    FSLASH,     // '/'
    PLUS,       // '+'
    MINUS,      // '-'
    GTLT,       // '><'
    LTGT,       // '<>'
    LT,         // '<'
    LE,         // '<='
    NE,         // '¬=' | '^=' | '~=' | '∘='
    GT,         // '>'
    GE,         // '>='
    SoundsLike, // '=*'
    PIPE,       // '|'
    DOT,        // '.'
    COMMA,      // ','
    COLON,      // ':'
    ASSIGN,     // '='
    DOLLAR,     // '$'
    AT,         // '@'
    HASH,       // '#'
    QUESTION,   // '?'
    // Mnemonics for logical expressions
    KwLT,           // LT
    KwLE,           // LE
    KwEQ,           // EQ
    KwIN,           // IN
    KwNE,           // NE
    KwGT,           // GT
    KwGE,           // GE
    KwAND,          // AND
    KwOR,           // OR
    KwNOT,          // NOT
    IntegerLiteral, // 42
    FloatLiteral,   // 42.1, .42
    // separate due to format ambiguity with numeric formats
    FloatExponentLiteral,     // 42.1e[+,-,]1
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
    PredictedCommentStat,     // * ...;
    DatalinesStart,           // datalines/cards[4];
    DatalinesData,            // datalines data
    // the closing ;[;;;] after dataines is lexed as SEMI
    CharFormat, // $charformat.
    // ----------------MACRO TOKENS----------------
    MacroComment,     // %* ...;
    MacroVarResolve,  // &+ (when it is a macro var resolve operator)
    MacroVarTerm,     // The "." in "&mv.". Unlike DOT, we emit this one separately to ease parsing
    MacroString,      // %let var = macro_string;
    MacroStringEmpty, // implicit empty macro string in logical expr `%eval(= rhs)`
    MacroLabel,       // %macro_label: without :. Colon is lexed as colon on hidden channel
    // From here and on to KwmRun are the token type subset `TokenTypeMacroCallOrStat`
    // DO NOT ADD ANYTHING IN BETWEEN
    MacroIdentifier, // %macro_name
    // Macro built in function keywords
    // Non masking versions
    KwmCmpres,        // CMPRES
    KwmCompstor,      // COMPSTOR
    KwmDatatyp,       // DATATYP
    KwmEval,          // EVAL
    KwmIndex,         // INDEX
    KwmLeft,          // LEFT
    KwmLength,        // LENGTH
    KwmLowcase,       // LOWCASE
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
    KwmTrim,          // TRIM
    KwmUnquote,       // UNQUOTE
    KwmUpcase,        // UPCASE
    KwmVerify,        // VERIFY
    // NLS functions (non-masking)
    KwmKCmpres,  // KCMPRES
    KwmKIndex,   // KINDEX
    KwmKLeft,    // KLEFT
    KwmKLength,  // KLENGTH
    KwmKLowcase, // KLLOWCASE
    KwmKScan,    // KSCAN
    KwmKSubstr,  // KSUBSTR
    KwmKTrim,    // KTRIM
    KwmKUpcase,  // KUPCASE
    KwmKVerify,  // KVERIFY
    KwmValidchs, // VALIDCHS
    // Macro Masking versions. They mask the resolved value
    // at runtime, much like quoting functions below, so do not
    // change the lexing itself (but obviously influence downstream)
    KwmQCmpres,  // QCMPRES
    KwmQLeft,    // QLEFT
    KwmQLowcase, // QLOWCASE
    KwmQScan,    // QSCAN
    KwmQSubstr,  // QSUBSTR
    KwmQTrim,    // QTRIM
    KwmQSysfunc, // QSYSFUNC
    KwmQUpcase,  // QUPCASE
    // NLS functions (masking)
    KwmQKCmpres,  // QKCMPRES
    KwmQKLeft,    // QKLEFT
    KwmQKLowcase, // QKLOWCASE
    KwmQKScan,    // QKSCAN
    KwmQKSubstr,  // QKSUBSTR
    KwmQKTrim,    // QKTRIM
    KwmQKUpcase,  // QKUPCASE
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
    KwmRun,  // RUN
    // ----------------MACRO TOKENS (end) ----------------
    // Put pure non macro tokens after this line only
    Identifier,
    KwEQT,
    KwGTT,
    KwLTT,
    KwGET,
    KwLET,
    KwNET,
    // Global SAS statement keywords & shared keywords
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

#[derive(PartialEq)]
pub(super) enum MacroKwType {
    None,
    MacroCall,
    MacroStat,
}

pub(super) fn parse_keyword(ident: &str) -> Option<TokenType> {
    KEYWORDS.get(ident.as_ref()).copied()
}

pub(super) fn parse_macro_keyword(ident: &str) -> Option<TokenType> {
    MKEYWORDS.get(ident.as_ref()).copied()
}

pub(super) const MACRO_QUOTE_CALL_TOKEN_TYPE_RANGE: (u16, u16) =
    (TokenType::KwmBquote as u16, TokenType::KwmNrStr as u16);

pub(super) const MACRO_STAT_TOKEN_TYPE_RANGE: (u16, u16) =
    (TokenType::KwmAbort as u16, TokenType::KwmRun as u16);

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

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
        assert_eq!(
            KEYWORDS
                .into_iter()
                .fold(0usize, |acc, new| acc.max(new.0.len())),
            MAX_KEYWORDS_LEN
        );
    }

    #[test]
    fn test_macro_keyword_map() {
        assert_eq!(parse_macro_keyword("BQUOTE"), Some(TokenType::KwmBquote));
        assert_eq!(parse_macro_keyword("NRQUOTE"), Some(TokenType::KwmNrQuote));
        assert_eq!(parse_macro_keyword("RANDOM"), None);
        // Check a couple of keywords in main map are NOT in the macro map
        assert_eq!(parse_macro_keyword("EQ"), None);
        assert_eq!(parse_macro_keyword("_NULL_"), None);
        assert_eq!(
            MKEYWORDS
                .into_iter()
                .fold(0usize, |acc, new| acc.max(new.0.len())),
            MAX_MKEYWORDS_LEN
        );
    }

    #[test]
    fn test_macro_stat_or_call_token_subset() {
        for stat_tok_type in TokenTypeMacroCallOrStat::iter() {
            assert_eq!(stat_tok_type as u16, TokenType::from(stat_tok_type) as u16);
        }

        let mut all_stat_tokens = TokenTypeMacroCallOrStat::iter()
            .filter(|t| !matches!(*t, TokenTypeMacroCallOrStat::MacroIdentifier))
            .map(TokenType::from)
            .collect::<Vec<_>>();

        all_stat_tokens.sort();

        let mut all_mkws = MKEYWORDS
            .values()
            .copied()
            // >1 keyword may map to the same token type
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();

        all_mkws.sort();

        assert_eq!(all_stat_tokens, all_mkws);
    }

    #[test]
    fn test_get_macro_stat_token_type_range() {
        for variant in TokenType::iter() {
            let variant_val = variant as u16;

            assert_eq!(
                MACRO_STAT_TOKEN_TYPE_RANGE.0 <= variant_val
                    && MACRO_STAT_TOKEN_TYPE_RANGE.1 >= variant_val,
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
                        | TokenType::KwmInclude
                        | TokenType::KwmList
                        | TokenType::KwmRun
                )
            );
        }
    }

    #[test]
    fn test_get_macro_quote_call_token_type_range() {
        for variant in TokenType::iter() {
            let variant_val = variant as u16;

            assert_eq!(
                MACRO_QUOTE_CALL_TOKEN_TYPE_RANGE.0 <= variant_val
                    && MACRO_QUOTE_CALL_TOKEN_TYPE_RANGE.1 >= variant_val,
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
    }

    #[test]
    fn test_all_tokens_round_trip() {
        assert_eq!(
            (0..TokenType::COUNT as u16).collect::<Vec<_>>(),
            TokenType::iter().map(|t| t as u16).collect::<Vec<_>>()
        );
    }
}
