pub(crate) mod buffer;
pub(crate) mod channel;
mod cursor;
pub mod error;
mod predicate;
pub mod print;
mod sas_lang;
#[cfg(test)]
mod tests;
mod text;
pub(crate) mod token_type;

use buffer::{LineIdx, Payload, TokenizedBuffer, WorkTokenizedBuffer};
use channel::TokenChannel;
use cursor::EOF_CHAR;
use error::{ErrorInfo, ErrorType};
use predicate::{is_macro_amp, is_macro_percent};
use sas_lang::is_valid_sas_name_start;
use std::str::FromStr;
use text::{ByteOffset, CharOffset};
use token_type::{parse_keyword, TokenType};
use unicode_ident::{is_xid_continue, is_xid_start};

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
    source: &'src str,
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
            source,
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
    fn pending_token_text(&self) -> &str {
        &self.source
            [self.cur_token_byte_offset.get() as usize..self.cur_byte_offset().get() as usize]
    }

    #[inline]
    fn push_mode(&mut self, mode: LexerMode) -> LexerMode {
        self.mode_stack.push(mode);
        mode
    }

    fn pop_mode(&mut self) -> LexerMode {
        self.mode_stack.pop().unwrap_or_else(|| {
            self.emit_error(ErrorType::InternalError("Empty mode stack"));
            self.push_mode(LexerMode::default())
        })
    }

    fn mode(&mut self) -> LexerMode {
        match self.mode_stack.last() {
            Some(&mode) => mode,
            None => {
                self.emit_error(ErrorType::InternalError("Empty mode stack"));
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

        // Skip whitespace if any
        if c.is_whitespace() {
            self.lex_ws();
            return;
        }

        // Dispatch the "big" categories
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
            ';' => {
                self.cursor.advance();
                self.add_token(TokenChannel::DEFAULT, TokenType::SEMI, Payload::None);
            }
            '/' => {
                if self.cursor.peek_next() == '*' {
                    self.lex_cstyle_comment();
                } else {
                    // TODO: this is not done
                    self.cursor.advance();
                    self.add_token(TokenChannel::DEFAULT, TokenType::FSLASH, Payload::None);
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
            '%' => {
                if is_macro_percent(self.cursor.chars()) {
                    !todo!();

                    return;
                }

                self.cursor.advance();
                self.add_token(TokenChannel::DEFAULT, TokenType::PERCENT, Payload::None);
            }
            '0'..='9' => {
                // Numeric literal
                self.lex_numeric_literal();
            }
            c if is_xid_start(c) || c == '_' => {
                self.lex_identifier();
            }
            _ => {
                // Something else must be a symbol or some unknown character
                self.lex_symbols(c);
            }
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
            );
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

    fn lex_identifier(&mut self) {
        debug_assert!(self.cursor.peek() == '_' || is_xid_start(self.cursor.peek()));

        // Eat the first character and start tracking whether the identifier is ASCII
        // It is necessary, as we neew to lower case the identifier if it is ASCII
        // for dispatching, and if it is not ASCII, we know it is not a keyword and can
        // skip the dispatching
        let mut is_ascii = if let Some(c) = self.cursor.advance() {
            c.is_ascii()
        } else {
            // This should not be possible really, but to make the production code
            // more robust, we'll handle this case
            true
        };

        // Eat the rest of the identifier
        self.cursor.eat_while(|c| {
            if c.is_ascii() {
                matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
            } else {
                is_ascii = false;
                is_xid_continue(c)
            }
        });

        // Now the fun part - dispatch all kinds of identifiers
        if !is_ascii {
            // Easy case - not ASCII, just emit the identifier token
            self.add_token(
                TokenChannel::DEFAULT,
                TokenType::BaseIdentifier,
                Payload::None,
            );
            return;
        }

        // My guess is this should be quicker than capturing the value as we consume it
        // avoids the allocation and copying
        let ident = self.pending_token_text().to_ascii_uppercase();

        if let Some(kw_tok_type) = parse_keyword(&ident) {
            self.add_token(TokenChannel::DEFAULT, kw_tok_type, Payload::None);
            return;
        }

        match ident.as_str() {
            "DATALINES" | "CARDS" | "LINES" => {
                if !self.lex_datalines(false) {
                    self.add_token(
                        TokenChannel::DEFAULT,
                        TokenType::BaseIdentifier,
                        Payload::None,
                    );
                };
            }
            "DATALINES4" | "CARDS4" | "LINES4" => {
                if !self.lex_datalines(true) {
                    self.add_token(
                        TokenChannel::DEFAULT,
                        TokenType::BaseIdentifier,
                        Payload::None,
                    );
                };
            }
            _ => {
                // todo: add more keywords
                self.add_token(
                    TokenChannel::DEFAULT,
                    TokenType::BaseIdentifier,
                    Payload::None,
                );
            }
        }
    }

    /// Checks whether the currently lexed token is indeed a datalines start token.
    /// If so, then consumes not only the start, but also the body and the end of the datalines
    /// and returns `true`. Otherwise, returns `false`.
    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    fn lex_datalines(&mut self, is_datalines4: bool) -> bool {
        #[cfg(debug_assertions)]
        if is_datalines4 {
            debug_assert!(matches!(
                self.pending_token_text().to_ascii_uppercase().as_str(),
                "DATALINES4" | "CARDS4" | "LINES4"
            ));
        } else {
            debug_assert!(matches!(
                self.pending_token_text().to_ascii_uppercase().as_str(),
                "DATALINES" | "CARDS" | "LINES"
            ));
        }

        // So, datalines are pretty insane beast. First, we use heuristic to determine
        // if it may be the start of the datalines (must preceeded by `;` on default channel),
        // then we need to peek forward to find a `;`. Only of we find it, we can be sure
        // that this is indeed a datalines start token.
        if let Some(tok_info) = self
            .buffer
            .last_token_on_channel_info(TokenChannel::DEFAULT)
        {
            if tok_info.token_type() != TokenType::SEMI {
                // the previous character is not a semicolon
                return false;
            };
        }

        // Now the forward check
        let mut la_view = self.cursor.chars();

        loop {
            match la_view.next() {
                Some(';') => break,
                Some(c) if c.is_whitespace() => continue,
                // Non whitespace, non semicolon character - not a datalines
                _ => return false,
            }
        }

        // Few! Now we now that this is indeed a datalines! TBH technically we do not
        // as in SAS, it is context sensitive and will only trigger inside a data step
        // but otherwise it is theoretically possible to have smth. like `datalines;` in
        // a macro...but I refuse to support this madness. Hopefully no such code exists

        // A rare case, where we emit multiple tokens and avoid state/modes
        // Seemed too complicated to allow looping in the lexer for the sake of
        // of this very special language construct

        // Now advance to the semi-colon for real before emitting the token the datalines start token
        loop {
            // Have to do the loop to track line changes
            match self.cursor.advance() {
                Some('\n') => self.add_line(),
                Some(c) if c.is_whitespace() => {}
                // in reality we know it will be a semicolon
                _ => break,
            }
        }

        self.add_token(
            TokenChannel::DEFAULT,
            TokenType::DatalinesStart,
            Payload::None,
        );

        // Start the new token
        self.start_token();

        // What are we comparing the ending against
        let ending = if is_datalines4 { ";;;;" } else { ";" };
        let ending_len = ending.len();

        loop {
            match self.cursor.peek() {
                '\n' => {
                    self.cursor.advance();
                    self.add_line();
                }
                ';' => {
                    let rem_text = self.cursor.as_str();

                    if rem_text.len() < ending_len {
                        // Not enough characters left to match the ending
                        // Emit error, but assume that we found the ending
                        self.emit_error(ErrorType::UnterminatedDatalines);
                        break;
                    }

                    if self.cursor.as_str()[..ending_len] == *ending {
                        // Found the ending. Do not consume as it will be the separate token
                        break;
                    }

                    self.cursor.advance();
                }
                _ => {
                    self.cursor.advance();
                }
            }
        }

        // Add the datalines data token
        self.add_token(
            TokenChannel::DEFAULT,
            TokenType::DatalinesData,
            Payload::None,
        );

        // Start the new token
        self.start_token();

        // Consume the ending
        self.cursor.advance_by(ending_len as u32);

        // Add the datalines end token
        self.add_token(
            TokenChannel::DEFAULT,
            TokenType::DatalinesEnd,
            Payload::None,
        );

        true
    }

    fn lex_numeric_literal(&mut self) {
        debug_assert!(matches!(self.cursor.peek(), '0'..='9'));
        // First, SAS supports 3 notations for numeric literals:
        // 1. Standard decimal notation (base 10)
        // 2. Hexadecimal notation (base 16)
        // 3. Scientific notation

        // For HEX notation, the number must start with a number
        // and can have up-to 16 total HEX digits (including the starting one)
        // due to 8 bytes of storage for a number in SAS. It must be
        // followed by an `x` or `X` character

        // consume the first digit
        self.cursor.advance();

        // Now we need to disambiguate between the notations
        let mut expect_hex = true;
        let mut expect_dot = true;
        let mut expect_exp = true;

        loop {
            match self.cursor.peek() {
                '0'..='9' => {
                    // can be any notation
                    self.cursor.advance();
                }
                'a'..='d' | 'A'..='D' | 'f' | 'F' if expect_hex => {
                    // must be HEX notation
                    self.lex_numeric_hex_literal();
                    return;
                }
                'x' | 'X' if expect_hex => {
                    // complete literal in HEX notation
                    // do not advance, such that the `x` is consumed by the HEX parser
                    self.lex_numeric_hex_literal();
                    return;
                }
                'e' | 'E' => {
                    if !expect_hex {
                        // If we already seen the dot and now got E => Scientific notation
                        self.lex_numeric_exp_literal(true);
                        return;
                    }

                    if !expect_exp {
                        // If we already seen the E => HEX notation
                        self.lex_numeric_hex_literal();
                        return;
                    }

                    self.cursor.advance();
                    // now we know it can't be standard notation and we can't have second
                    // E for exponent. This flag helps with both
                    expect_exp = false;
                    // and we can't have a dot after E in neither HEX nor scientific notation
                    expect_dot = false;
                }
                '-' => {
                    // Must be Scientific notation
                    // Do not advance, such that the `-` is consumed by the exponent parser
                    self.lex_numeric_exp_literal(!expect_exp);
                    return;
                }
                '.' => {
                    if expect_dot {
                        // Can be standard decimal or scientific notation
                        // but not HEX now
                        self.cursor.advance();
                        expect_hex = false;
                        continue;
                    }

                    // Second dot, or dot after E - means we looking past the literal
                    if expect_exp {
                        self.lex_decimal_literal();
                    } else {
                        // This can happen only in the case of `NNNe.` which can be considered
                        // either a HEX missing X, or scientific notation with missing exponent.
                        // For simplicity sake we call the scientific parser function which will
                        // report error and recover. This way, we do not need to track the last
                        // character
                        self.lex_numeric_exp_literal(true);
                    }

                    return;
                }
                _ => {
                    if expect_exp {
                        // Must be a standard decimal notation, since we
                        // haven't seen the E yet and definitely any other HEX digits
                        // as they would immediately trigger the HEX notation
                        self.lex_decimal_literal();
                    } else {
                        // This is an error, incomplete scientific notation, like `NNNe `
                        self.lex_numeric_exp_literal(true);
                    }

                    return;
                }
            }
        }
    }

    fn lex_decimal_literal(&mut self) {
        // The way calling logic is done, for regular decimals
        // we must have consumed the entire literal
        debug_assert!(!matches!(self.cursor.peek(), '0'..='9'));

        // Parse as f64. SAS uses 8 bytes and all numbers are stored as f64
        // so it is impossible to have a valid literal that would overflow
        // the f64 range.
        let number = self.pending_token_text();

        let fvalue = match f64::from_str(number) {
            Ok(value) => value,
            Err(_) => {
                self.add_token(
                    TokenChannel::DEFAULT,
                    TokenType::IntegerLiteral,
                    Payload::Integer(0),
                );

                self.emit_error(ErrorType::InvalidNumericLiteral);
                return;
            }
        };

        // See if it is an integer
        if fvalue.fract() == 0.0 {
            // For integers we need to emit different tokens, depending on
            // the presence of the dot as we use different token types.
            // The later is unfortunatelly necesasry due to SAS numeric formats
            // context sensitivity and no way of disambiguating between a number `1.`
            // and the same numeric format `1.`

            // But leading sign or 0 - we can emit the integer token
            match number.as_bytes()[0] {
                b'-' | b'+' | b'0' => {
                    self.add_token(
                        TokenChannel::DEFAULT,
                        TokenType::IntegerLiteral,
                        Payload::Integer(fvalue as i64),
                    );
                }
                _ => {
                    if number.contains('.') {
                        self.add_token(
                            TokenChannel::DEFAULT,
                            TokenType::IntegerDotLiteral,
                            Payload::Integer(fvalue as i64),
                        );
                    } else {
                        self.add_token(
                            TokenChannel::DEFAULT,
                            TokenType::IntegerLiteral,
                            Payload::Integer(fvalue as i64),
                        );
                    }
                }
            }
        } else {
            // And for floats, similarly it is important whether it
            // it was created from scientific notation or not, but
            // this function only handles the decimal literals, so
            // no need to check for that
            self.add_token(
                TokenChannel::DEFAULT,
                TokenType::FloatLiteral,
                Payload::Float(fvalue),
            );
        }
    }

    /// Consumes the remaining tail of a HEX literal and emits the token
    fn lex_numeric_hex_literal(&mut self) {
        #[cfg(debug_assertions)]
        debug_assert!(matches!(self.cursor.prev_char(), '0'..='9' | 'a'..='f' | 'A'..='F'));

        // Eat until the end of the literal (x or X) or identify a missing x/X
        loop {
            match self.cursor.peek() {
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    self.cursor.advance();
                }
                'x' | 'X' => {
                    // First get the text, and only then advance - the radix parser
                    // won't like the trailing x/X
                    let number = self.pending_token_text().to_owned();
                    self.cursor.advance();

                    self.lex_numeric_hex_str(number.as_str());
                    return;
                }
                _ => {
                    // This is an error, incomplete HEX literal
                    // First parse the number, then emit the error
                    let number = self.pending_token_text().to_owned();
                    self.lex_numeric_hex_str(number.as_str());

                    self.emit_error(ErrorType::UnterminatedHexNumericLiteral);
                    return;
                }
            }
        }
    }

    /// Parses a valid HEX number (sans the trailing `x` or `X`) and emits the token
    fn lex_numeric_hex_str(&mut self, number: &str) {
        // Try parse as i64. SAS only allows up-to 8 bytes (16 HEX digits)
        // for HEX literals, with the first one capped at 9F, so it is
        // slightly less then +/-(2^63 - 1), but can theoretically overflow
        match i64::from_str_radix(number, 16) {
            Ok(value) => self.add_token(
                TokenChannel::DEFAULT,
                TokenType::IntegerLiteral,
                Payload::Integer(value),
            ),
            Err(e) => {
                match e.kind() {
                    std::num::IntErrorKind::NegOverflow | std::num::IntErrorKind::PosOverflow => {
                        // Wow, very big number;) Ok

                        // First remove leading sign if any, remember the sign
                        let (truncated, negative) = if number.starts_with('-') {
                            (&number[1..], true)
                        } else if number.starts_with('+') {
                            (&number[1..], false)
                        } else {
                            (number, false)
                        };

                        // Check length, SAS itself wil not allow more than 16 HEX digits
                        let (emit_error, truncated) = if truncated.len() > 16 {
                            // Emit an error, but truncate the number to parse something

                            (true, &truncated[..16])
                        } else {
                            (false, truncated)
                        };

                        // This must work! We truncated the number to 16 HEX digits
                        let int_vale = u64::from_str_radix(truncated, 16)
                            .expect("The truncated number should be valid HEX number");

                        // Convert to f64 and apply the sign
                        let fvalue = if negative {
                            -(int_vale as f64)
                        } else {
                            int_vale as f64
                        };

                        self.add_token(
                            TokenChannel::DEFAULT,
                            TokenType::FloatLiteral,
                            Payload::Float(fvalue),
                        );

                        if emit_error {
                            self.emit_error(ErrorType::InvalidNumericLiteral);
                        }
                    }
                    _ => {
                        // This is an internal error, should not happen
                        self.add_token(
                            TokenChannel::DEFAULT,
                            TokenType::IntegerLiteral,
                            Payload::Integer(0),
                        );

                        self.emit_error(ErrorType::InternalError("Failed to parse HEX literal"));
                    }
                }
            }
        };
    }

    fn lex_numeric_exp_literal(&mut self, seen_exp: bool) {
        #[cfg(debug_assertions)]
        debug_assert!(matches!(self.cursor.prev_char(), 'a'..='f' | 'A'..='F'));

        // Eat until the end of the literal (x or X) or identify a missing x/X
        loop {
            match self.cursor.peek() {
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    self.cursor.advance();
                }
                'x' | 'X' => {
                    // First get the text, and only then advance - the radix parser
                    // won't like the trailing x/X
                    let number = self.pending_token_text().to_owned();
                    self.cursor.advance();

                    self.lex_numeric_hex_str(number.as_str());
                    return;
                }
                _ => {
                    // This is an error, incomplete HEX literal
                    // First parse the number, then emit the error
                    let number = self.pending_token_text().to_owned();
                    self.lex_numeric_hex_str(number.as_str());

                    self.emit_error(ErrorType::UnterminatedHexNumericLiteral);
                    return;
                }
            }
        }
    }

    fn lex_symbols(&mut self, c: char) {
        match c {
            '*' => {
                // TODO: this is not done
                self.cursor.advance();
                match (self.cursor.peek(), self.cursor.peek_next()) {
                    ('\'' | '"', ';') => {
                        self.cursor.advance();
                        self.cursor.advance();
                        self.add_token(TokenChannel::HIDDEN, TokenType::TermQuote, Payload::None);
                    }
                    ('*', _) => {
                        self.cursor.advance();
                        self.add_token(TokenChannel::DEFAULT, TokenType::STAR2, Payload::None);
                    }
                    _ => {
                        self.add_token(TokenChannel::DEFAULT, TokenType::STAR, Payload::None);
                    }
                }
            }
            '(' => {
                self.cursor.advance();
                self.add_token(TokenChannel::DEFAULT, TokenType::LPAREN, Payload::None);
            }
            ')' => {
                self.cursor.advance();
                self.add_token(TokenChannel::DEFAULT, TokenType::RPAREN, Payload::None);
            }
            '!' => {
                self.cursor.advance();

                match self.cursor.peek() {
                    '!' => {
                        self.cursor.advance();
                        self.add_token(TokenChannel::DEFAULT, TokenType::EXCL2, Payload::None);
                    }
                    _ => {
                        self.add_token(TokenChannel::DEFAULT, TokenType::EXCL, Payload::None);
                    }
                }
            }
            '¦' => {
                self.cursor.advance();

                match self.cursor.peek() {
                    '¦' => {
                        self.cursor.advance();
                        self.add_token(TokenChannel::DEFAULT, TokenType::BPIPE2, Payload::None);
                    }
                    _ => {
                        self.add_token(TokenChannel::DEFAULT, TokenType::BPIPE, Payload::None);
                    }
                }
            }
            '|' => {
                self.cursor.advance();

                match self.cursor.peek() {
                    '|' => {
                        self.cursor.advance();
                        self.add_token(TokenChannel::DEFAULT, TokenType::PIPE2, Payload::None);
                    }
                    _ => {
                        self.add_token(TokenChannel::DEFAULT, TokenType::PIPE, Payload::None);
                    }
                }
            }
            '¬' | '^' | '~' | '∘' => {
                self.cursor.advance();

                match self.cursor.peek() {
                    '=' => {
                        self.cursor.advance();
                        self.add_token(TokenChannel::DEFAULT, TokenType::NE, Payload::None);
                    }
                    _ => {
                        self.add_token(TokenChannel::DEFAULT, TokenType::NOT, Payload::None);
                    }
                }
            }
            '+' => {
                self.cursor.advance();

                match self.cursor.peek() {
                    '0'..='9' => {
                        // `+N`
                        self.lex_numeric_literal();
                    }
                    '.' if self.cursor.peek_next().is_ascii_digit() => {
                        // `+.N` is a valid number, but `+. ` is not
                        self.lex_numeric_literal();
                    }
                    _ => {
                        self.add_token(TokenChannel::DEFAULT, TokenType::PLUS, Payload::None);
                    }
                }
            }
            '-' => {
                self.cursor.advance();

                match self.cursor.peek() {
                    '0'..='9' => {
                        // `+N`
                        self.lex_numeric_literal();
                    }
                    '.' if self.cursor.peek_next().is_ascii_digit() => {
                        // `+.N` is a valid number, but `+. ` is not
                        self.lex_numeric_literal();
                    }
                    _ => {
                        self.add_token(TokenChannel::DEFAULT, TokenType::MINUS, Payload::None);
                    }
                }
            }
            '<' => {
                self.cursor.advance();

                match self.cursor.peek() {
                    '=' => {
                        self.cursor.advance();
                        self.add_token(TokenChannel::DEFAULT, TokenType::LE, Payload::None);
                    }
                    '>' => {
                        self.cursor.advance();
                        self.add_token(TokenChannel::DEFAULT, TokenType::LTGT, Payload::None);
                    }
                    _ => {
                        self.add_token(TokenChannel::DEFAULT, TokenType::LT, Payload::None);
                    }
                }
            }
            '>' => {
                self.cursor.advance();

                match self.cursor.peek() {
                    '=' => {
                        self.cursor.advance();
                        self.add_token(TokenChannel::DEFAULT, TokenType::GE, Payload::None);
                    }
                    '<' => {
                        self.cursor.advance();
                        self.add_token(TokenChannel::DEFAULT, TokenType::GTLT, Payload::None);
                    }
                    _ => {
                        self.add_token(TokenChannel::DEFAULT, TokenType::GT, Payload::None);
                    }
                }
            }
            '.' => {
                self.cursor.advance();
                self.add_token(TokenChannel::DEFAULT, TokenType::DOT, Payload::None);
            }
            ',' => {
                self.cursor.advance();
                self.add_token(TokenChannel::DEFAULT, TokenType::COMMA, Payload::None);
            }
            ':' => {
                self.cursor.advance();
                self.add_token(TokenChannel::DEFAULT, TokenType::COLON, Payload::None);
            }
            '=' => {
                self.cursor.advance();

                match self.cursor.peek() {
                    '*' => {
                        self.cursor.advance();
                        self.add_token(TokenChannel::DEFAULT, TokenType::SoundsLike, Payload::None);
                    }
                    _ => {
                        self.add_token(TokenChannel::DEFAULT, TokenType::ASSIGN, Payload::None);
                    }
                }
            }
            '$' => {
                self.cursor.advance();
                self.add_token(TokenChannel::DEFAULT, TokenType::DOLLAR, Payload::None);
            }
            '@' => {
                self.cursor.advance();
                self.add_token(TokenChannel::DEFAULT, TokenType::AT, Payload::None);
            }
            '#' => {
                self.cursor.advance();
                self.add_token(TokenChannel::DEFAULT, TokenType::HASH, Payload::None);
            }
            '?' => {
                self.cursor.advance();
                self.add_token(TokenChannel::DEFAULT, TokenType::QUESTION, Payload::None);
            }
            _ => {
                // Unknown something, consume the character, emit an unknown token, push error
                self.cursor.advance();
                self.add_token(TokenChannel::DEFAULT, TokenType::UNKNOWN, Payload::None);
                self.emit_error(ErrorType::UnknownCharacter(c));
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
