use super::buffer::TokenIdx;
use std::fmt::Display;
#[cfg(test)]
use strum::IntoStaticStr;
use strum::{EnumIter, EnumMessage};

#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumIter, EnumMessage)]
#[cfg_attr(test, derive(IntoStaticStr))]
#[repr(u32)]
pub enum ErrorType {
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
    #[strum(message = "Unexpected character")]
    UnexpectedCharacter = 1006,
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
    #[strum(
        message = "ERROR: A character operand was found in the %EVAL function or %IF \
                            condition where a numeric operand is required."
    )]
    CharExpressionInEvalContext = 1013,
    #[strum(message = "ERROR 180-322: Statement is not valid or it is used out of proper order.")]
    InvalidOrOutOfOrderStatement = 1014,
    #[strum(
        message = "Possible ERROR 180-322: Statement is not valid or it is used out of proper order."
    )]
    MaybeInvalidOrOutOfOrderStatement = 1015,
    #[strum(message = "ERROR: Expecting a variable name after %LET.")]
    InvalidMacroLetVarName = 1016,
    #[strum(message = "ERROR: The macro variable name is either all blank or missing.")]
    InvalidMacroLocalGlobalReadonlyVarName = 1017,
    #[strum(message = "ERROR: Unrecognized keyword on %LOCAL statement.")]
    MissingMacroLocalReadonlyKw = 1018,
    #[strum(message = "ERROR: Unrecognized keyword on %GLOBAL statement.")]
    MissingMacroGlobalReadonlyKw = 1019,
    #[strum(message = "ERROR: Invalid macro name.  \
                        It should be a valid SAS identifier no longer than 32 characters.\
                        \nERROR: A dummy macro will be compiled.")]
    InvalidMacroDefName = 1020,
    #[strum(message = "ERROR: Invalid macro parameter name. \
                        It should be a valid SAS identifier no longer than 32 characters.\
                        \nERROR: A dummy macro will be compiled.")]
    InvalidMacroDefArgName = 1021,
    #[strum(
        message = "ERROR: An unexpected semicolon occurred in the %DO statement.\n\
                        ERROR: A dummy macro will be compiled."
    )]
    UnexpectedSemiInDoLoop = 1022,
    #[strum(message = "ERROR: Open code statement recursion detected.")]
    OpenCodeRecursionError = 1023,
    // Token buffer errors. Codes 2001-2999. Make sure to preserve
    // the existing codes & the range. The latter is used in classification impl
    #[strum(message = "Requested token index out of bounds")]
    TokenIdxOutOfBounds = 2001,
    #[strum(message = "String literal range out of bounds")]
    StringLiteralOutOfBounds = 2002,
    // Internal errors. Codes 3001-3999. Make sure to preserve
    // the existing codes & the range. The latter is used in classification impl
    #[strum(message = "No checkpoint to rollback")]
    InternalErrorMissingCheckpoint = 3001,
    #[strum(message = "No token text")]
    InternalErrorNoTokenText = 3002,
    #[strum(message = "Internal out of bounds request")]
    InternalErrorOutOfBounds = 3003,
    #[strum(message = "Empty mode stack")]
    InternalErrorEmptyModeStack = 3004,
    #[strum(message = "No token to replace")]
    InternalErrorNoTokenToReplace = 3005,
    #[strum(message = "Unexpected token type")]
    InternalErrorUnexpectedTokenType = 3006,
    #[strum(message = "Unexpected mode stack")]
    InternalErrorUnexpectedModeStack = 3007,
    #[strum(message = "Missing EOF token in buffer")]
    InternalErrorMissingEOFToken = 3008,
}

impl ErrorType {
    #[must_use]
    pub fn is_internal(&self) -> bool {
        (*self as u32) > 3000u32
    }

    #[must_use]
    pub fn is_code_error(&self) -> bool {
        (*self as u32) < 2000u32
    }
}

impl Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_message().unwrap_or("Unknown error"))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ErrorInfo {
    error_type: ErrorType,
    at_byte_offset: u32,
    at_char_offset: u32,
    on_line: u32,
    at_column: u32,
    last_token: Option<TokenIdx>,
}

impl ErrorInfo {
    #[must_use]
    pub fn new(
        error_type: ErrorType,
        at_byte_offset: u32,
        at_char_offset: u32,
        on_line: u32,
        at_column: u32,
        last_token: Option<TokenIdx>,
    ) -> Self {
        Self {
            error_type,
            at_byte_offset,
            at_char_offset,
            on_line,
            at_column,
            last_token,
        }
    }

    #[must_use]
    pub fn error_type(&self) -> ErrorType {
        self.error_type
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
    fn test_all_error_types_has_messages() {
        for error in ErrorType::iter() {
            assert!(
                error.get_message().is_some(),
                "ErrorType {:?} has no message",
                error
            );
        }
    }

    #[test]
    fn test_error_type_is_internal() {
        for error in ErrorType::iter() {
            let variant_as_str: &'static str = error.into();
            if error.is_internal() {
                assert!(
                    variant_as_str.starts_with("InternalError"),
                    "ErrorType {:?} marked as internal but has no InternalError prefix",
                    error
                );
            } else {
                assert!(
                    !variant_as_str.starts_with("InternalError"),
                    "ErrorType {:?} marked as non internal but has InternalError prefix",
                    error
                );
            }
        }
    }
}
