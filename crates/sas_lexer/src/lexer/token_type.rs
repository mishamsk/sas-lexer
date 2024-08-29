use phf::phf_map;
use sas_lexer_macro::{FromU16, KeywordMap, MacroKeywordMap, ToU16};
use strum::{Display, EnumCount, EnumIter};

/// What you expect - the token types.
///
/// Order of variants are VERY important, as multiple
/// predicates rely on integer values of the variants that
/// are generated automatically from the order.
///
/// Naming also is used to autogenerate static hash maps
/// for regular keywords and macro keywords.
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
    SEMI,       // ';'
    AMP,        // '&'+
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
    TermQuote,  // *'; and *" ";
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
    MacroComment,     // %* ...;
    MacroVarExpr,     // &&mvar&another. etc.
    MacroIdentifier,  // %macro_name
    MacroString,      // %let var = macro_string;
    MacroStringEmpty, // implicit empty macro string in logical expr `%eval(= rhs)`
    // Macro built in function keywords
    // Non masking versions
    KwmEval,          // EVAL
    KwmIndex,         // INDEX
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
    KwmUnquote,       // UNQUOTE
    KwmUpcase,        // UPCASE
    // NLS functions (non-masking)
    KwmKCmpres,  // KCMPRES
    KwmKIndex,   // KINDEX
    KwmKLeft,    // KLEFT
    KwmKLength,  // KLENGTH
    KwmKScan,    // KSCAN
    KwmKSubstr,  // KSUBSTR
    KwmKUpcase,  // KUPCASE
    KwmValidchs, // VALIDCHS
    // Macro Masking versions. They mask the resolved value
    // at runtime, much like quoting functions below, so do not
    // change the lexing itself (but obviously influence downstream)
    KwmQLowcase, // QLOWCASE
    KwmQScan,    // QSCAN
    KwmQSubstr,  // QSUBSTR
    KwmQsysfunc, // QSYSFUNC
    KwmQUpcase,  // QUPCASE
    // NLS functions (masking)
    KwmQKCmpres, // QKCMPRES
    KwmQKLeft,   // QKLEFT
    KwmQKScan,   // QKSCAN
    KwmQKSubstr, // QKSUBSTR
    KwmQKUpcase, // QKUPCASE
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

#[derive(Default)]
pub(super) struct TokenFlags {
    bits: [u64; (TokenType::COUNT + 63) / 64],
}

impl TokenFlags {
    pub(super) fn set(&mut self, tok_type: TokenType) {
        let index = tok_type as u16;
        let (word, bit) = ((index / 64) as usize, index % 64);

        assert!(word < self.bits.len());

        self.bits[word] |= 1 << bit;
    }

    pub(super) fn contains(&self, tok_type: TokenType) -> bool {
        let index = tok_type as u16;
        let (word, bit) = ((index / 64) as usize, index % 64);

        assert!(word < self.bits.len());

        self.bits[word] & (1 << bit) != 0
    }

    // Add other useful methods if needed
}

pub(super) fn parse_keyword<S: AsRef<str>>(ident: S) -> Option<TokenType> {
    KEYWORDS.get(ident.as_ref()).copied()
}

pub(super) fn parse_macro_keyword<S: AsRef<str>>(ident: S) -> Option<TokenType> {
    MKEYWORDS.get(ident.as_ref()).copied()
}

pub(super) const MACRO_QUOTE_CALL_TOKEN_TYPE_RANGE: (u16, u16) =
    (TokenType::KwmBquote as u16, TokenType::KwmNrStr as u16);

pub(super) const MACRO_STAT_TOKEN_TYPE_RANGE: (u16, u16) =
    (TokenType::KwmAbort as u16, TokenType::KwmList as u16);

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

    #[test]
    fn test_token_flags() {
        // Test setting and checking individual flags
        for token_type in TokenType::iter() {
            let mut flags = TokenFlags::default();
            flags.set(token_type);

            assert!(flags.contains(token_type));

            for other_token_type in TokenType::iter() {
                if other_token_type != token_type {
                    assert!(!flags.contains(other_token_type));
                }
            }
        }

        // Do a spot test for a few tokens
        let mut flags = TokenFlags::default();
        flags.set(TokenType::KwEQ);
        flags.set(TokenType::KwIN);
        flags.set(TokenType::KwAllVar);

        assert!(flags.contains(TokenType::KwEQ));
        assert!(flags.contains(TokenType::KwIN));
        assert!(flags.contains(TokenType::KwAllVar));
        assert!(!flags.contains(TokenType::KwCorr));
        assert!(!flags.contains(TokenType::KwmAbort));
    }
}
