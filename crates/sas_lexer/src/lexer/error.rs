use super::buffer::TokenIdx;
use std::fmt::Display;
#[cfg(test)]
use strum::IntoStaticStr;
use strum::{EnumIter, EnumMessage};

#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumIter, EnumMessage)]
#[cfg_attr(test, derive(IntoStaticStr))]
#[repr(u8)]
pub enum ErrorType {
    // Source code errors
    #[strum(message = "Unterminated string literal")]
    UnterminatedStringLiteral = 1,
    #[strum(message = "Unterminated comment")]
    UnterminatedComment,
    #[strum(message = "Unterminated datalines")]
    UnterminatedDatalines,
    #[strum(message = "Invalid numeric literal")]
    InvalidNumericLiteral,
    #[strum(message = "Missing `x` at the end of a hex numeric literal")]
    UnterminatedHexNumericLiteral,
    #[strum(message = "Unexpected character")]
    UnexpectedCharacter,
    #[strum(message = "Missing expected character: ')'")]
    MissingExpectedRParen,
    #[strum(message = "Missing expected character: '='")]
    MissingExpectedAssign,
    #[strum(message = "Missing expected character: '('")]
    MissingExpectedLParen,
    #[strum(message = "Missing expected character: ','")]
    MissingExpectedComma,
    #[strum(message = "Missing expected character: '/'")]
    MissingExpectedFSlash,
    #[strum(message = "Missing expected character: ';' or end of file")]
    MissingExpectedSemiOrEOF,
    #[strum(
        message = "ERROR: A character operand was found in the %EVAL function or %IF \
                            condition where a numeric operand is required."
    )]
    CharExpressionInEvalContext,
    #[strum(message = "ERROR 180-322: Statement is not valid or it is used out of proper order.")]
    InvalidOrOutOfOrderStatement,
    #[strum(
        message = "Possible ERROR 180-322: Statement is not valid or it is used out of proper order."
    )]
    MaybeInvalidOrOutOfOrderStatement,
    #[strum(message = "ERROR: Expecting a variable name after %LET.")]
    InvalidMacroLetVarName,
    #[strum(message = "ERROR: The macro variable name is either all blank or missing.")]
    InvalidMacroLocalGlobalReadonlyVarName,
    #[strum(message = "ERROR: Unrecognized keyword on %LOCAL statement.")]
    MissingMacroLocalReadonlyKw,
    #[strum(message = "ERROR: Unrecognized keyword on %GLOBAL statement.")]
    MissingMacroGlobalReadonlyKw,
    #[strum(message = "ERROR: Invalid macro name.  \
                        It should be a valid SAS identifier no longer than 32 characters.\
                        \nERROR: A dummy macro will be compiled.")]
    InvalidMacroDefName,
    #[strum(message = "ERROR: Invalid macro parameter name. \
                        It should be a valid SAS identifier no longer than 32 characters.\
                        \nERROR: A dummy macro will be compiled.")]
    InvalidMacroDefArgName,
    #[strum(
        message = "ERROR: An unexpected semicolon occurred in the %DO statement.\n\
                        ERROR: A dummy macro will be compiled."
    )]
    UnexpectedSemiInDoLoop,
    #[strum(message = "ERROR: Open code statement recursion detected.")]
    OpenCodeRecursionError,
    // Token buffer errors. Add all new ones below the first existing one!
    // The first one is used as the ending point for the code error range.
    #[strum(message = "Requested token index out of bounds")]
    TokenIdxOutOfBounds,
    #[strum(message = "String literal range out of bounds")]
    StringLiteralOutOfBounds,
    // Internal errors. Add all new ones below the first existing one!
    // The first one is used as the starting point for the internal error range.
    #[strum(message = "No checkpoint to rollback")]
    InternalErrorMissingCheckpoint,
    #[strum(message = "No token text")]
    InternalErrorNoTokenText,
    #[strum(message = "Internal out of bounds request")]
    InternalErrorOutOfBounds,
    #[strum(message = "Empty mode stack")]
    InternalErrorEmptyModeStack,
    #[strum(message = "No token to replace")]
    InternalErrorNoTokenToReplace,
    #[strum(message = "Unexpected token type")]
    InternalErrorUnexpectedTokenType,
    #[strum(message = "Unexpected mode stack")]
    InternalErrorUnexpectedModeStack,
    #[strum(message = "Missing EOF token in buffer")]
    InternalErrorMissingEOFToken,
}

impl ErrorType {
    #[must_use]
    pub fn is_internal(&self) -> bool {
        (*self as u8) >= ErrorType::InternalErrorMissingCheckpoint as u8
    }

    #[must_use]
    pub fn is_code_error(&self) -> bool {
        (*self as u8) < ErrorType::TokenIdxOutOfBounds as u8
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
