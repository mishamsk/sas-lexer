pub(crate) mod buffer;
pub(crate) mod channel;
mod cursor;
pub mod error;
mod predicate;
pub mod print;
mod sas_lang;
mod text;
pub(crate) mod token_type;

use buffer::{LineIdx, Payload, TokenizedBuffer, WorkTokenizedBuffer};
use channel::TokenChannel;
use cursor::EOF_CHAR;
use error::{ErrorInfo, ErrorType};
use predicate::{is_macro_amp, is_macro_percent};
use sas_lang::is_valid_sas_name_start;
use text::{ByteOffset, CharOffset};
use token_type::TokenType;

use crate::TokenIdx;

const BOM: char = '\u{feff}';

/// The lexer mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum LexerMode {
    /// The default mode
    #[default]
    Default,
    /// Thestring expression, aka double quoted string mode
    StringExpr,
}

#[derive(Debug)]
struct Lexer<'src> {
    // source: &'src str,
    source_len: u32,
    buffer: WorkTokenizedBuffer,
    cursor: cursor::Cursor<'src>,
    /// Start byte offset of the token being lexed
    cur_token_byte_offset: ByteOffset,
    /// Start char offset of the token being lexed
    cur_token_start: CharOffset,
    /// Start line index of the token being lexed
    cur_token_line: LineIdx,
    /// Lexer mode stack
    mode_stack: Vec<LexerMode>,
    /// Errors encountered during lexing
    errors: Vec<ErrorInfo>,
}

impl<'src> Lexer<'src> {
    fn new(source: &str, init_mode: Option<LexerMode>) -> Result<Lexer, &'static str> {
        let Ok(source_len) = u32::try_from(source.len()) else {
            return Err("Lexing of files larger than 4GB is not supported");
        };

        let mut cursor = cursor::Cursor::new(source);
        let mut buffer = WorkTokenizedBuffer::new(source);

        // Skip BOM if present
        let cur_token_start = CharOffset::new(u32::from(cursor.eat_char(BOM)));
        let cur_token_byte_offset = ByteOffset::new(source_len - cursor.remaining_len());

        // Add the first line
        let cur_token_line = buffer.add_line(cur_token_byte_offset, cur_token_start);

        Ok(Lexer {
            // source,
            source_len,
            buffer,
            cursor,
            cur_token_byte_offset,
            cur_token_start,
            cur_token_line,
            mode_stack: vec![init_mode.unwrap_or_default()],
            errors: Vec::new(),
        })
    }

    #[inline]
    fn cur_byte_offset(&self) -> ByteOffset {
        ByteOffset::new(self.source_len - self.cursor.remaining_len())
    }

    #[inline]
    fn cur_char_offset(&self) -> CharOffset {
        CharOffset::new(self.cursor.char_offset())
    }

    #[inline]
    fn push_mode(&mut self, mode: LexerMode) -> LexerMode {
        self.mode_stack.push(mode);
        mode
    }

    fn pop_mode(&mut self) -> LexerMode {
        self.mode_stack.pop().unwrap_or_else(|| {
            self.emit_error(ErrorType::EmptyModeStack);
            self.push_mode(LexerMode::default())
        })
    }

    fn mode(&mut self) -> LexerMode {
        match self.mode_stack.last() {
            Some(&mode) => mode,
            None => {
                self.emit_error(ErrorType::EmptyModeStack);
                self.push_mode(LexerMode::default())
            }
        }
    }

    #[inline]
    fn add_line(&mut self) {
        self.buffer
            .add_line(self.cur_byte_offset(), self.cur_char_offset());
    }

    fn start_token(&mut self) {
        self.cur_token_byte_offset = self.cur_byte_offset();
        self.cur_token_start = self.cur_char_offset();
        self.cur_token_line = LineIdx::new(self.buffer.line_count() - 1);
    }

    fn add_token(&mut self, channel: TokenChannel, token_type: TokenType, payload: Payload) {
        self.buffer.add_token(
            channel,
            token_type,
            self.cur_token_byte_offset,
            self.cur_token_start,
            self.cur_token_line,
            payload,
        );
    }

    fn replace_token(&mut self, channel: TokenChannel, token_type: TokenType, payload: Payload) {
        if let Some(last_token) = self.buffer.pop_token() {
            self.buffer.add_token(
                channel,
                token_type,
                last_token.byte_offset(),
                last_token.start(),
                last_token.line(),
                payload,
            );
        } else {
            // This is an internal error, we should always have a token to replace
            self.emit_error(ErrorType::InternalError("No token to replace"));

            self.buffer.add_token(
                channel,
                token_type,
                self.cur_token_byte_offset,
                self.cur_token_start,
                self.cur_token_line,
                payload,
            );
        }
    }

    fn emit_error(&mut self, error: ErrorType) {
        let last_line_char_offset = if let Some(line_info) = self.buffer.last_line_info() {
            line_info.start().get()
        } else {
            0
        };

        let last_char_offset = self.cur_char_offset().get();

        self.errors.push(ErrorInfo::new(
            error,
            self.cur_byte_offset().get(),
            last_char_offset,
            self.buffer.line_count(),
            last_char_offset - last_line_char_offset,
            match self.buffer.token_count() {
                0 => None,
                count => Some(TokenIdx::new(count - 1)),
            },
        ));
    }

    fn lex(mut self) -> (WorkTokenizedBuffer, Box<[ErrorInfo]>) {
        while !self.cursor.is_eof() {
            self.lex_token();
        }

        self.buffer.add_token(
            TokenChannel::DEFAULT,
            TokenType::EOF,
            self.cur_byte_offset(),
            self.cur_char_offset(),
            LineIdx::new(self.buffer.line_count() - 1), // use the last added line
            Payload::None,
        );

        (self.buffer, self.errors.into_boxed_slice())
    }

    fn lex_token(&mut self) {
        match self.mode() {
            LexerMode::Default => self.lex_mode_default(),
            LexerMode::StringExpr => self.lex_mode_str_expr(),
        }
    }

    fn lex_mode_default(&mut self) {
        debug_assert_eq!(self.mode(), LexerMode::Default);

        self.start_token();

        let c = self.cursor.peek();

        if c.is_whitespace() {
            self.lex_ws();
            return;
        }

        if c.is_ascii() {
            match c {
                '\'' => self.lex_single_quoted_str(),
                '"' => {
                    self.cursor.advance();
                    self.add_token(
                        TokenChannel::DEFAULT,
                        TokenType::StringExprStart,
                        Payload::None,
                    );
                    self.push_mode(LexerMode::StringExpr);
                }
                '/' => {
                    if self.cursor.peek_next() == '*' {
                        self.lex_cstyle_comment();
                    } else {
                        // TODO: this is not done
                        self.cursor.advance();
                        self.add_token(TokenChannel::DEFAULT, TokenType::BaseCode, Payload::None);
                    }
                }
                '&' => {
                    if cfg!(debug_assertions) {
                        // In debug mode, we check if the lex_amp function added a token
                        // in addition to just executing the logic
                        debug_assert!(self.lex_amp());
                    } else {
                        self.lex_amp();
                    }
                }
                '*' => {
                    // TODO: this is not done
                    self.cursor.advance();
                    match (self.cursor.peek(), self.cursor.peek_next()) {
                        ('\'' | '"', ';') => {
                            self.cursor.advance();
                            self.cursor.advance();
                            self.add_token(
                                TokenChannel::HIDDEN,
                                TokenType::TermQuote,
                                Payload::None,
                            );
                        }
                        _ => {
                            self.add_token(
                                TokenChannel::DEFAULT,
                                TokenType::BaseCode,
                                Payload::None,
                            );
                        }
                    }
                }
                _ => {
                    self.cursor.advance();
                    self.add_token(TokenChannel::DEFAULT, TokenType::BaseCode, Payload::None);
                }
            }
        } else {
            self.cursor.advance();
            self.add_token(TokenChannel::DEFAULT, TokenType::BaseCode, Payload::None);
        }
    }

    fn lex_ws(&mut self) {
        debug_assert!(self.cursor.peek().is_whitespace());

        loop {
            if let Some('\n') = self.cursor.advance() {
                self.add_line();
            }

            if !self.cursor.peek().is_whitespace() {
                break;
            }
        }
        self.add_token(TokenChannel::HIDDEN, TokenType::WS, Payload::None);
    }

    fn lex_cstyle_comment(&mut self) {
        debug_assert_eq!(self.cursor.peek(), '/');
        debug_assert_eq!(self.cursor.peek_next(), '*');

        // Eat the opening comment
        self.cursor.advance();
        self.cursor.advance();

        loop {
            if let Some(c) = self.cursor.advance() {
                if c == '*' && self.cursor.peek() == '/' {
                    self.cursor.advance();
                    break;
                }

                if c == '\n' {
                    self.add_line();
                }
            } else {
                // EOF reached without a closing comment
                // Emit an error token and return
                self.add_token(
                    TokenChannel::COMMENT,
                    TokenType::CStyleComment,
                    Payload::None,
                );
                self.emit_error(ErrorType::UnterminatedComment);
                return;
            }
        }

        self.add_token(
            TokenChannel::COMMENT,
            TokenType::CStyleComment,
            Payload::None,
        );
    }

    fn lex_single_quoted_str(&mut self) {
        debug_assert_eq!(self.cursor.peek(), '\'');

        // Eat the opening single quote
        self.cursor.advance();

        loop {
            if let Some(c) = self.cursor.advance() {
                match c {
                    '\'' => {
                        if self.cursor.peek() == '\'' {
                            // escaped single quote
                            self.cursor.advance();
                            continue;
                        }

                        break;
                    }
                    '\n' => {
                        self.add_line();
                    }
                    _ => {}
                }
            } else {
                // EOF reached without a closing single quote
                // Emit an error token and return
                self.add_token(
                    TokenChannel::DEFAULT,
                    TokenType::StringLiteral,
                    Payload::None,
                );
                self.emit_error(ErrorType::UnterminatedStringLiteral);
                return;
            }
        }

        // Now check if this is a single quoted string or one of the other literals
        let tok_type = self.lex_literal_ending();

        self.add_token(TokenChannel::DEFAULT, tok_type, Payload::None);
    }

    /// Lexes the ending of a literal token, returning the type
    /// but does not emit the token
    fn lex_literal_ending(&mut self) -> TokenType {
        #[cfg(debug_assertions)]
        debug_assert!(['"', '\''].contains(&self.cursor.prev_char()));

        let tok_type = match self.cursor.peek() {
            'b' | 'B' => TokenType::BitTestingLiteral,
            'd' | 'D' => {
                if ['t', 'T'].contains(&self.cursor.peek_next()) {
                    self.cursor.advance();

                    TokenType::DateTimeLiteral
                } else {
                    TokenType::DateLiteral
                }
            }
            'n' | 'N' => TokenType::NameLiteral,
            't' | 'T' => TokenType::TimeLiteral,
            'x' | 'X' => TokenType::HexStringLiteral,
            _ => TokenType::StringLiteral,
        };

        // If we found a literal, advance the cursor
        if tok_type != TokenType::StringLiteral {
            self.cursor.advance();
        }

        tok_type
    }

    fn lex_mode_str_expr(&mut self) {
        debug_assert_eq!(self.mode(), LexerMode::StringExpr);

        self.start_token();

        match self.cursor.peek() {
            '"' => {
                if self.cursor.peek_next() == '"' {
                    // escaped double quote => start of a expression text
                    self.lex_str_expr_text();
                    return;
                }

                // So, we have a closing double quote. Two possibilities:
                // 1. This is a real string expression, like "&mv.string"
                // 2. This is just a string literal, like "just a string"
                //
                // In case of (2) this is only possible for an empty string
                // as non-empty must have been handled inside `lex_str_expr_text`
                let last_tok_is_start = if let Some(last_tok_info) = self.buffer.last_token_info() {
                    last_tok_info.token_type() == TokenType::StringExprStart
                } else {
                    false
                };

                if last_tok_is_start {
                    self.lex_double_quoted_literal();
                    return;
                }

                // Consuming the closing double quote
                self.cursor.advance();

                // Now check if this is a regular double quoted string expr
                // or one of the literals-expressions
                let tok_type = match self.cursor.peek() {
                    'b' | 'B' => TokenType::BitTestingLiteralExprEnd,
                    'd' | 'D' => {
                        if ['t', 'T'].contains(&self.cursor.peek_next()) {
                            self.cursor.advance();

                            TokenType::DateTimeLiteralExprEnd
                        } else {
                            TokenType::DateLiteralExprEnd
                        }
                    }
                    'n' | 'N' => TokenType::NameLiteralExprEnd,
                    't' | 'T' => TokenType::TimeLiteralExprEnd,
                    'x' | 'X' => TokenType::HexStringLiteralExprEnd,
                    _ => TokenType::StringExprEnd,
                };

                // If we found a literal, advance the cursor
                if tok_type != TokenType::StringExprEnd {
                    self.cursor.advance();
                }

                self.add_token(TokenChannel::DEFAULT, tok_type, Payload::None);
                self.pop_mode();
            }
            '&' => {
                if self.lex_amp() {
                    // we lexed a macro var expr
                    return;
                }

                // actually a part of string text. and we've already consumed the sequence of &
                // continue to lex the text
                self.lex_str_expr_text();
            }
            '%' => {
                if is_macro_percent(self.cursor.chars()) {
                    !todo!();

                    return;
                }

                // actually a part of string text. and we've already consumed the sequence of &
                // continue to lex the text
                self.lex_str_expr_text();
            }
            EOF_CHAR => {
                // EOF reached without a closing double quote
                self.handle_unterminated_str_expr();
            }
            _ => {
                // Not a macro var, not a macro call and not an ending => lex the middle
                self.lex_str_expr_text();
            }
        }
    }

    fn lex_str_expr_text(&mut self) {
        loop {
            match self.cursor.peek() {
                '&' => {
                    let (is_macro_amp, amp_count) = is_macro_amp(self.cursor.chars());

                    if is_macro_amp {
                        // Hit a macro var expr in the string expression => emit the text token
                        self.add_token(
                            TokenChannel::DEFAULT,
                            TokenType::StringExprText,
                            Payload::None,
                        );

                        return;
                    }

                    // Just amps in the text, consume and continue
                    self.cursor.advance_by(amp_count);
                }
                '%' => {
                    if is_macro_percent(self.cursor.chars()) {
                        // Hit a macro var expr in the string expression => emit the text token
                        self.add_token(
                            TokenChannel::DEFAULT,
                            TokenType::StringExprText,
                            Payload::None,
                        );

                        return;
                    }

                    // Just percent in the text, consume and continue
                    self.cursor.advance();
                }
                '\n' => {
                    self.cursor.advance();
                    self.add_line();
                }
                EOF_CHAR => {
                    // EOF reached without a closing double quote
                    self.handle_unterminated_str_expr();

                    return;
                }
                '"' => {
                    if self.cursor.peek_next() == '"' {
                        // escaped double quote, eat both and continue
                        self.cursor.advance();
                        self.cursor.advance();
                        continue;
                    }

                    // So, we have a closing double quote. Two possibilities:
                    // 1. This is a real string expression, like "&mv.string"
                    // 2. This is just a string literal, like "just a string"
                    let last_tok_is_start =
                        if let Some(last_tok_info) = self.buffer.last_token_info() {
                            last_tok_info.token_type() == TokenType::StringExprStart
                        } else {
                            false
                        };

                    if last_tok_is_start {
                        self.lex_double_quoted_literal();
                        return;
                    }

                    // We are in a genuine string expression, and hit the end - emit the text token
                    // The ending quote will be handled by the caller
                    self.add_token(
                        TokenChannel::DEFAULT,
                        TokenType::StringExprText,
                        Payload::None,
                    );
                    return;
                }
                _ => {
                    self.cursor.advance();
                }
            }
        }
    }

    fn handle_unterminated_str_expr(&mut self) {
        debug_assert_eq!(self.cursor.peek(), EOF_CHAR);
        debug_assert_eq!(self.mode(), LexerMode::StringExpr);

        // This will handle the unterminated string expression
        // Both the case of a real string expression and a string literal
        // emitting the correct "missing" token and an error

        let last_tok_is_start = if let Some(last_tok_info) = self.buffer.last_token_info() {
            last_tok_info.token_type() == TokenType::StringExprStart
        } else {
            false
        };

        if last_tok_is_start {
            self.replace_token(
                TokenChannel::DEFAULT,
                TokenType::StringLiteral,
                Payload::None,
            )
        } else {
            self.add_token(
                TokenChannel::DEFAULT,
                TokenType::StringExprEnd,
                Payload::None,
            );
        }
        self.emit_error(ErrorType::UnterminatedStringLiteral);
        self.pop_mode();
    }

    fn lex_double_quoted_literal(&mut self) {
        debug_assert_eq!(self.cursor.peek(), '"');

        // This is a regular literal. We need to consume the char, figure
        // out which type of literal is this, similar to single quoted
        // string, replace the last token and exit the string expression mode
        self.cursor.advance();

        let tok_type = self.lex_literal_ending();

        self.replace_token(TokenChannel::DEFAULT, tok_type, Payload::None);
        self.pop_mode();
    }

    /// Lexes an ampersand sequence of  macro variable expression
    ///
    /// Modes supported:
    /// - `Default`: generates a `MacroVarExpr` or `Amp` token
    /// - `StringExpr`: generates a `MacroVarExpr` or consumes the sequence of `&` characters
    ///          without generating a token if the sequence is not a macro var expr
    ///
    /// Returns `true` if a token was added, `false` otherwise
    fn lex_amp(&mut self) -> bool {
        debug_assert_eq!(self.cursor.peek(), '&');

        // Consuming leading ampersands
        self.cursor.eat_while(|c| c == '&');

        // Check the next character
        let c = self.cursor.peek();

        if !is_valid_sas_name_start(c) {
            // Not a macro var expr, just a 1+ sequence of &
            // Fork based on the mode, do not consume the next character
            match self.mode() {
                LexerMode::Default => {
                    self.add_token(TokenChannel::DEFAULT, TokenType::AMP, Payload::None);

                    // Report we lexed a token
                    return true;
                }
                LexerMode::StringExpr => {
                    // Just a sequence of & in a string expression, return without adding a token
                    return false;
                }
            }
        }

        // Ok, this is a macro expr for sure
        // and we got the first character of the name
        self.cursor.advance();

        loop {
            match self.cursor.peek() {
                '.' => {
                    // a dot, terminates a macro var expr, consume it
                    self.cursor.advance();

                    // Add the token
                    self.add_token(
                        TokenChannel::DEFAULT,
                        TokenType::MacroVarExpr,
                        Payload::None,
                    );

                    // Report we lexed a token
                    return true;
                }
                '&' => {
                    // Ok, we got at least some portion of the macro var expr. But the next amp
                    // can either be the continuation (like in `&&var&c``)
                    //                                               ^^
                    // which we treat as part of the same macro var expr token
                    // or a trailing amp (like in `&&var&& other stuff``)
                    //                                  ^^
                    // Unfortunately, now an arbitrary number of lookahead is needed to
                    // figure out which case it is as arbitrary number of & characters
                    // are treated as a single one.
                    // Thus we do a lookahead predicate on a cloned iterator to avoid consuming
                    // the original
                    let (is_macro_amp, amp_count) = is_macro_amp(self.cursor.chars());

                    if !is_macro_amp {
                        // The following & characters are not part of the macro var expr

                        // Add the token without consuming the following amp
                        self.add_token(
                            TokenChannel::DEFAULT,
                            TokenType::MacroVarExpr,
                            Payload::None,
                        );

                        // Report we lexed a token
                        return true;
                    }

                    // Ok we know that the amp is part of the macro var expr
                    // we can skip the `amp_count` characters
                    self.cursor.advance_by(amp_count);
                }
                c if c.is_ascii_alphanumeric() || c == '_' => {
                    // Still a name
                    self.cursor.advance();
                }
                _ => {
                    // Reached the end of the macro var expr

                    // Add the token without consuming the following character
                    self.add_token(
                        TokenChannel::DEFAULT,
                        TokenType::MacroVarExpr,
                        Payload::None,
                    );

                    // Report we lexed a token
                    return true;
                }
            }
        }
    }
}

/// Lex the source code and return the tokenized buffer
///
/// # Arguments
/// * `source: &str` - The source code to lex
///
/// # Returns
/// * `Result<TokenizedBuffer, &str>` - The tokenized buffer if lexing was successful
///   or an error message if lexing failed
///
/// # Errors
/// If the source code is larger than 4GB, an error message is returned
///
/// # Examples
/// ```
/// use sas_lexer::lex;
/// let source = "let x = 42;";
/// let result = lex(&source);
/// assert!(result.is_ok());
/// ```
pub fn lex<S: AsRef<str>>(source: &S) -> Result<(TokenizedBuffer, Box<[ErrorInfo]>), &'static str> {
    let lexer = Lexer::new(source.as_ref(), None)?;
    let (buffer, errors) = lexer.lex();
    Ok((buffer.into_detached()?, errors))
}
