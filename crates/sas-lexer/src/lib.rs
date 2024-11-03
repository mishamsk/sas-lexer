#![cfg_attr(rustc_nightly, feature(vec_push_within_capacity))]

mod lexer;

pub use lexer::buffer::{Payload, ResolvedTokenInfo, TokenIdx, TokenizedBuffer};
pub use lexer::channel::TokenChannel;
pub use lexer::error;
pub use lexer::token_type::TokenType;
pub use lexer::{lex_program, LexResult};
