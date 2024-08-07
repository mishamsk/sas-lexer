use std::fmt::Display;

use crate::TokenIdx;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ErrorType {
    UnterminatedStringLiteral,
    UnterminatedComment,
    UnterminatedDatalines,
    InvalidNumericLiteral,
    UnterminatedHexNumericLiteral,
    UnknownCharacter(char),
    MissingExpected(&'static str),
    InternalError(&'static str),
}

impl Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorType::UnterminatedStringLiteral => {
                write!(f, "Unterminated string literal")
            }
            ErrorType::UnterminatedComment => {
                write!(f, "Unterminated comment")
            }
            ErrorType::UnterminatedDatalines => {
                write!(f, "Unterminated datalines")
            }
            ErrorType::UnknownCharacter(c) => {
                write!(f, "Unknown character: '{c}'")
            }
            ErrorType::MissingExpected(msg) => {
                write!(f, "Missing expected: {msg}")
            }
            ErrorType::InternalError(msg) => {
                write!(f, "Internal error: {msg}")
            }
            ErrorType::InvalidNumericLiteral => {
                write!(f, "Invalid numeric literal")
            }
            ErrorType::UnterminatedHexNumericLiteral => {
                write!(f, "Missing `x` at the end of a hex numeric literal")
            }
        }
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
