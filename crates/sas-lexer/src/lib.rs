//! # SAS Lexer
//! A lexer for the SAS programming language.
//!
//! ## Usage
//! ```rust
//! use sas_lexer::{lex_program, LexResult, TokenIdx};
//!
//! let source = "data mydata; set mydataset; run;";
//!
//! let LexResult { buffer, .. } = lex_program(&source).unwrap();
//!
//! let tokens: Vec<TokenIdx> = buffer.into_iter().collect();
//!
//! for token in tokens {
//!     println!("{:?}", buffer.get_token_raw_text(token, &source));
//! }
//! ```
//!
//! ## Features
//!
//! * `macro_sep` (enabled by default): Enables a special virtual `MacroSep` token that is emitted between open code and macro statements when there is no "natural" separator, or when semicolon is missing between two macro statements (a coding error). This may be used by a downstream parser as a reliable terminating token for dynamic open code and thus avoid doing lookaheads. Dynamic, means that the statement has a macro statements in it, like `data %if cond %then %do; t1 %end; %else %do; t2 %end;;`
//! * `serde`: Enables serialization and deserialization of the `ResolvedTokenInfo` struct using the `serde` library. For an example of usage, see the Python bindings crate `sas-lexer-py`.
//! * `opti_stats`: Enables some additional statistics during lexing, used for performance tuning. Not intended for general use.
//!
//! ## License
//!
//! Licensed under the Affero GPL v3 license.

#![cfg_attr(rustc_nightly, feature(vec_push_within_capacity))]

mod lexer;

pub use lexer::buffer::{Payload, ResolvedTokenInfo, TokenIdx, TokenizedBuffer};
pub use lexer::channel::TokenChannel;
pub use lexer::error;
pub use lexer::token_type::TokenType;
pub use lexer::{lex_program, LexResult};
