use std::fmt::Display;

use crate::TokenType;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ErrorType {
    UnterminatedStringLiteral,
    UnterminatedComment,
    EmptyModeStack,
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
            ErrorType::EmptyModeStack => {
                write!(f, "Unexpected empty mode stack")
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ErrorInfo {
    pub error_type: ErrorType,
    pub at_byte_offset: u32,
    pub at_char_offset: u32,
    pub on_line: u32,
    pub at_column: u32,
    pub last_token: Option<TokenType>,
}
