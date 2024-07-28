use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LexerError {
    UnterminatedStringLiteral,
    UnterminatedComment,
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::UnterminatedStringLiteral => {
                write!(f, "Unterminated string literal")
            }
            LexerError::UnterminatedComment => {
                write!(f, "Unterminated comment")
            }
        }
    }
}
