use super::buffer::TokenIdx;
use std::ops::Range;
#[cfg(test)]
use strum::IntoStaticStr;
use strum::{Display, EnumCount, EnumIter, EnumMessage};

#[cfg(feature = "serde")]
use serde::Serialize;
#[cfg(feature = "serde")]
use serde_repr::Serialize_repr;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Display, EnumIter, EnumMessage, EnumCount)]
#[cfg_attr(test, derive(IntoStaticStr))]
#[cfg_attr(feature = "serde", derive(Serialize_repr))]
#[repr(u16)]
pub enum ErrorKind {
    // Source code errors. Codes 1001-1999. Make sure to preserve
    // the existing codes & the range. The latter is used in classification impl
    #[strum(message = "Unterminated string literal")]
    UnterminatedStringLiteral = 1001,
    #[strum(message = "Unterminated comment")]
    UnterminatedComment = 1002,
    #[strum(message = "Unterminated datalines")]
    UnterminatedDatalines = 1003,
    #[strum(message = "Invalid numeric literal")]
    InvalidNumericLiteral = 1004,
    #[strum(message = "Missing `x` at the end of a hex numeric literal")]
    UnterminatedHexNumericLiteral = 1005,
    #[strum(message = "Missing expected character: ')'")]
    MissingExpectedRParen = 1007,
    #[strum(message = "Missing expected character: '='")]
    MissingExpectedAssign = 1008,
    #[strum(message = "Missing expected character: '('")]
    MissingExpectedLParen = 1009,
    #[strum(message = "Missing expected character: ','")]
    MissingExpectedComma = 1010,
    #[strum(message = "Missing expected character: '/'")]
    MissingExpectedFSlash = 1011,
    #[strum(message = "Missing expected character: ';' or end of file")]
    MissingExpectedSemiOrEOF = 1012,
    #[strum(message = "ERROR 180-322: Statement is not valid or it is used out of proper order.")]
    InvalidOrOutOfOrderStatement = 1013,
    #[strum(message = "ERROR: Expecting a variable name after %LET.")]
    InvalidMacroLetVarName = 1014,
    #[strum(message = "ERROR: The macro variable name is either all blank or missing.")]
    InvalidMacroLocalGlobalReadonlyVarName = 1015,
    #[strum(message = "ERROR: Unrecognized keyword on %LOCAL statement.")]
    MissingMacroLocalReadonlyKw = 1016,
    #[strum(message = "ERROR: Unrecognized keyword on %GLOBAL statement.")]
    MissingMacroGlobalReadonlyKw = 1017,
    #[strum(message = "ERROR: Invalid macro name.  \
                        It should be a valid SAS identifier no longer than 32 characters.\
                        \nERROR: A dummy macro will be compiled.")]
    InvalidMacroDefName = 1018,
    #[strum(message = "ERROR: Invalid macro parameter name. \
                        It should be a valid SAS identifier no longer than 32 characters.\
                        \nERROR: A dummy macro will be compiled.")]
    InvalidMacroDefArgName = 1019,
    #[strum(
        message = "ERROR: An unexpected semicolon occurred in the %DO statement.\n\
                        ERROR: A dummy macro will be compiled."
    )]
    UnexpectedSemiInDoLoop = 1020,
    #[strum(message = "ERROR: Open code statement recursion detected.")]
    OpenCodeRecursionError = 1021,
    #[strum(message = "Invalid hex string constant.")]
    InvalidHexStringConstant = 1022,
    #[strum(
        message = "ERROR: Function name missing in %SYSFUNC or %QSYSFUNC macro function reference."
    )]
    MissingSysfuncFuncName = 1023,
    #[strum(message = "ERROR: CALL routine name missing in %SYSCALL macro statement.")]
    MissingSyscallRoutineName = 1024,
    // Token buffer API call user initiated errors.
    // Codes 2001-2999. Make sure to preserve
    // the existing codes & the range. The latter is used in classification impl
    #[strum(message = "Requested token index out of bounds")]
    TokenIdxOutOfBounds = 2001,
    #[strum(message = "String literal range out of bounds")]
    StringLiteralOutOfBounds = 2002,
    // Lexer API call user initiated errors.
    // Codes 3001-3999. Make sure to preserve
    // the existing codes & the range. The latter is used in classification impl
    #[strum(message = "Lexing of files larger than 4GB is not supported")]
    FileTooLarge = 3001,
    // Lexer uncertainty warnings. All cases where our heuristics has significant
    // chances of false-positives
    // Codes 4001-4999. Make sure to preserve
    // the existing codes & the range. The latter is used in classification impl
    // CURENTLY UNUSED
    // Internal errors. Codes 9001-9999. Make sure to preserve
    // the existing codes & the range. The latter is used in classification impl
    #[strum(message = "No checkpoint to rollback")]
    InternalErrorMissingCheckpoint = 9001,
    #[strum(message = "No token text")]
    InternalErrorNoTokenText = 9002,
    #[strum(message = "Internal out of bounds request")]
    InternalErrorOutOfBounds = 9003,
    #[strum(message = "Empty mode stack")]
    InternalErrorEmptyModeStack = 9004,
    #[strum(message = "No token to replace")]
    InternalErrorNoTokenToReplace = 9005,
    #[strum(message = "Unexpected token type")]
    InternalErrorUnexpectedTokenType = 9006,
    #[strum(message = "Unexpected mode stack")]
    InternalErrorUnexpectedModeStack = 9007,
    #[strum(message = "Infinite loop detected")]
    InternalErrorInfiniteLoop = 9008,
    #[strum(message = "Empty pending stat stack")]
    InternalErrorEmptyPendingStatStack = 9009,
}

pub const CODE_ERROR_RANGE: Range<u16> = 1000..2000;
pub const WARNING_RANGE: Range<u16> = 4000..5000;
pub const INTERNAL_ERROR_RANGE: Range<u16> = 9000..10000;

impl ErrorKind {
    #[must_use]
    pub fn is_internal(&self) -> bool {
        INTERNAL_ERROR_RANGE.contains(&(*self as u16))
    }

    #[must_use]
    pub fn is_warning(&self) -> bool {
        WARNING_RANGE.contains(&(*self as u16))
    }

    #[must_use]
    pub fn is_code_error(&self) -> bool {
        CODE_ERROR_RANGE.contains(&(*self as u16))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct ErrorInfo {
    error_kind: ErrorKind,
    at_byte_offset: u32,
    at_char_offset: u32,
    on_line: u32,
    at_column: u32,
    last_token: Option<TokenIdx>,
}

impl ErrorInfo {
    #[must_use]
    pub fn new(
        error_kind: ErrorKind,
        at_byte_offset: u32,
        at_char_offset: u32,
        on_line: u32,
        at_column: u32,
        last_token: Option<TokenIdx>,
    ) -> Self {
        Self {
            error_kind,
            at_byte_offset,
            at_char_offset,
            on_line,
            at_column,
            last_token,
        }
    }

    #[must_use]
    pub fn error_kind(&self) -> ErrorKind {
        self.error_kind
    }

    #[must_use]
    pub fn at_byte_offset(&self) -> u32 {
        self.at_byte_offset
    }

    #[must_use]
    pub fn at_char_offset(&self) -> u32 {
        self.at_char_offset
    }

    #[must_use]
    pub fn on_line(&self) -> u32 {
        self.on_line
    }

    #[must_use]
    pub fn at_column(&self) -> u32 {
        self.at_column
    }

    #[must_use]
    pub fn last_token(&self) -> Option<TokenIdx> {
        self.last_token
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use strum::IntoEnumIterator;

    #[test]
    fn test_all_error_kinds_has_messages() {
        for error in ErrorKind::iter() {
            assert!(
                error.get_message().is_some(),
                "ErrorKind {error:?} has no message"
            );
        }
    }

    #[test]
    fn test_error_kind_is_internal() {
        for error in ErrorKind::iter() {
            let variant_as_str: &'static str = error.into();
            if error.is_internal() {
                assert!(
                    variant_as_str.starts_with("InternalError"),
                    "ErrorKind {error:?} marked as internal but has no InternalError prefix"
                );
            } else {
                assert!(
                    !variant_as_str.starts_with("InternalError"),
                    "ErrorKind {error:?} marked as non internal but has InternalError prefix"
                );
            }
        }
    }
}
