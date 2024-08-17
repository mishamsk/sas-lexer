pub(crate) mod buffer;
pub(crate) mod channel;
mod cursor;
pub mod error;
mod hex;
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
use hex::parse_numeric_hex_str;
use predicate::{is_macro_amp, is_macro_call, is_macro_percent};
use sas_lang::is_valid_sas_name_start;
use smol_str::SmolStr;
use std::{cmp::min, str::FromStr};
use text::{ByteOffset, CharOffset};
use token_type::{parse_keyword, parse_macro_keyword, TokenType};
use unicode_ident::{is_xid_continue, is_xid_start};

use crate::TokenIdx;

const BOM: char = '\u{feff}';

/// The lexer mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum LexerMode {
    /// Default mode aka open code (non macro)
    #[default]
    Default,
    /// String expression, aka double quoted string mode
    StringExpr,
    /// Insignificant WS/comment space. E.g. between macro name and parens in a call
    /// this is the mode where we want to lex all consecutive whitespace and comments
    /// and then return to the previous mode
    WsOrCStyleCommentOnly,
    /// A special mode where only a specific sequence of characters is expected.
    /// In this mode we also auto-recover if the expected character is not found
    /// emitting an error but also creating the expected token
    ///
    /// SAFETY: The string must not contain newlines
    ExpectToken(&'static str, TokenType, TokenChannel),
    /// A special mode that goes after non-statement macro identifiers
    /// that checks if the first NON-ws or cstyle follower is (.
    /// If found, adds necessary mode stack to parse the macro call args.
    /// If not, perorms roll back, so that ws/cstyle comments can be
    /// relexed in different mode.
    ///
    /// Note - it should alwys be preceded by the WsOrCStyleCommentOnly mode
    /// and a checkpoint created!
    MaybeMacroCallArgs,
    /// The state for inner macro expressions, as in `%str(-->1+1<--)`
    MacroStrQuotedExpr,
    /// Macro arithmetic/logical expression, as in `%eval(-->1+1<--)`
    MacroEval,
    /// Mode for lexing right after %let/%local/%global, where
    /// we expect a variable name expression. Boolean flag indicates if we
    /// have found at least one token of the variable name
    MacroLetVarName(bool),
    MacroLetInitializer,
    // The u32 value is the current parenthesis nesting level.
    // Macro arguments allow balanced parenthesis nesting and
    // inside these parenthesis, `,` and `=` are not treated as
    // terminators.
    MacroCallArgOrValue(u32),
    MacroCallValue(u32),
}

#[derive(Debug)]
struct LexerCheckpoint<'src> {
    cursor: cursor::Cursor<'src>,
    cur_token_byte_offset: ByteOffset,
    cur_token_start: CharOffset,
    cur_token_line: LineIdx,
    mode_stack_len: usize,
    last_token_idx: Option<TokenIdx>,
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
    /// Checkpoint for the lexer to rollback to
    checkpoint: Option<LexerCheckpoint<'src>>,

    /// Stores the last state for debug assertions to check
    /// against infinite loops. It is the remaining input length
    /// and the mode stack.
    #[cfg(debug_assertions)]
    last_state: (u32, Vec<LexerMode>),
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
            checkpoint: None,
            #[cfg(debug_assertions)]
            last_state: (source_len, vec![init_mode.unwrap_or_default()]),
        })
    }

    /// Create a checkpoint for the lexer
    ///
    /// Make sure to always clear the checkpoint, even if not rolling back
    fn checkpoint(&mut self) {
        // We should always make sure to clear any checkpoints
        debug_assert!(self.checkpoint.is_none());

        self.checkpoint = Some(LexerCheckpoint {
            cursor: self.cursor.clone(),
            cur_token_byte_offset: self.cur_token_byte_offset,
            cur_token_start: self.cur_token_start,
            cur_token_line: self.cur_token_line,
            mode_stack_len: self.mode_stack.len(),
            last_token_idx: self.buffer.last_token(),
        });
    }
    fn clear_checkpoint(&mut self) {
        self.checkpoint = None;
    }

    fn rollback(&mut self) {
        if let Some(checkpoint) = self.checkpoint.take() {
            self.cursor = checkpoint.cursor;
            self.cur_token_byte_offset = checkpoint.cur_token_byte_offset;
            self.cur_token_start = checkpoint.cur_token_start;
            self.cur_token_line = checkpoint.cur_token_line;
            self.mode_stack.truncate(checkpoint.mode_stack_len);

            if let Err(e) = self.buffer.rollback(checkpoint.last_token_idx) {
                // This is an internal error, we should always be able to correctly rollback
                self.emit_error(ErrorType::InternalError(e));
            }
        } else {
            // Emit an error, we should not be here
            self.emit_error(ErrorType::InternalError("No checkpoint to rollback"));
        }
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
    fn pending_token_text(&mut self) -> &str {
        self.source
            .get(self.cur_token_byte_offset.into()..self.cur_byte_offset().into())
            .unwrap_or_else(|| {
                // This is an internal error, we should always have a token text
                self.emit_error(ErrorType::InternalError("No token text"));
                ""
            })
    }

    fn add_string_literal(
        &mut self,
        start_byte_offset: ByteOffset,
        end_byte_offset: Option<ByteOffset>,
    ) -> (u32, u32) {
        let end_byte_offset = end_byte_offset.unwrap_or_else(|| self.cur_byte_offset());

        debug_assert!(start_byte_offset <= end_byte_offset);

        let text = self
            .source
            .get(start_byte_offset.into()..end_byte_offset.into())
            .unwrap_or_else(|| {
                // This is an internal error, this should at least be an empty string
                self.emit_error(ErrorType::InternalError("Requested out of bounds text"));
                ""
            });

        self.buffer.add_string_literal(text)
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
    fn add_line(&mut self) -> LineIdx {
        self.buffer
            .add_line(self.cur_byte_offset(), self.cur_char_offset())
    }

    fn start_token(&mut self) {
        self.cur_token_byte_offset = self.cur_byte_offset();
        self.cur_token_start = self.cur_char_offset();
        self.cur_token_line = self.buffer.last_line().unwrap_or_else(||
            // Should not be possible, since we add the first line when creating
            // the lexer, but whatever
            self.add_line())
    }

    fn emit_token(&mut self, channel: TokenChannel, token_type: TokenType, payload: Payload) {
        self.buffer.add_token(
            channel,
            token_type,
            self.cur_token_byte_offset,
            self.cur_token_start,
            self.cur_token_line,
            payload,
        );
    }

    fn update_last_token(
        &mut self,
        channel: TokenChannel,
        token_type: TokenType,
        payload: Payload,
    ) {
        if !self.buffer.update_last_token(channel, token_type, payload) {
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

        let last_char_offset = self.cur_char_offset().into();

        self.errors.push(ErrorInfo::new(
            error,
            self.cur_byte_offset().into(),
            last_char_offset,
            self.buffer.line_count(),
            last_char_offset - last_line_char_offset,
            self.buffer.last_token(),
        ));
    }

    fn lex(mut self) -> (WorkTokenizedBuffer, Box<[ErrorInfo]>) {
        while !self.cursor.is_eof() {
            self.lex_token();

            #[cfg(debug_assertions)]
            if cfg!(debug_assertions) {
                let new_state = (self.cursor.remaining_len(), self.mode_stack.clone());
                debug_assert!(self.last_state != new_state, "Infinite loop detected");
                self.last_state = new_state;
            }
        }

        let last_line = self.buffer.last_line().unwrap_or_else(||
            // Should not be possible, since we add the first line when creating
            // the lexer, but whatever
            self.add_line());

        self.buffer.add_token(
            TokenChannel::DEFAULT,
            TokenType::EOF,
            self.cur_byte_offset(),
            self.cur_char_offset(),
            // use the last added line
            last_line,
            Payload::None,
        );

        (self.buffer, self.errors.into_boxed_slice())
    }

    fn lex_token(&mut self) {
        match self.mode() {
            LexerMode::WsOrCStyleCommentOnly => match self.cursor.peek() {
                '/' if self.cursor.peek_next() == '*' => {
                    self.start_token();
                    self.lex_cstyle_comment();
                }
                c if c.is_whitespace() => {
                    self.start_token();
                    self.lex_ws();
                }
                _ => {
                    self.pop_mode();
                }
            },
            LexerMode::MaybeMacroCallArgs => {
                if self.cursor.peek() == '(' {
                    // Add the LPAREN token
                    self.start_token();
                    self.cursor.advance();

                    self.emit_token(TokenChannel::DEFAULT, TokenType::LPAREN, Payload::None);

                    // Clear the checkpoint
                    self.clear_checkpoint();

                    // Pop the MaybeMacroCallArgs mode
                    self.pop_mode();

                    // Populate the remaining expected states for the macro call
                    self.push_mode(LexerMode::ExpectToken(
                        ")",
                        TokenType::RPAREN,
                        TokenChannel::DEFAULT,
                    ));
                    // The handler fo arguments will push the mode for the comma, etc.
                    self.push_mode(LexerMode::MacroCallArgOrValue(0));
                    // Leading insiginificant WS before the first argument
                    self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                } else {
                    // Not a macro call with arguments, rollback (which will implicitly pop the mode)
                    debug_assert!(self.checkpoint.is_some());

                    // Rollback to the checkpoint, which should be before any WS and comments
                    // and right after macro identifier
                    self.rollback();
                }
            }
            LexerMode::ExpectToken(content, tok_type, tok_channel) => {
                self.lex_expected_token(content, tok_type, tok_channel)
            }
            LexerMode::Default => self.lex_mode_default(),
            LexerMode::StringExpr => self.lex_mode_str_expr(),
            LexerMode::MacroEval => !todo!("Macro eval mode"),
            LexerMode::MacroStrQuotedExpr => self.lex_macro_str_quoted_expr(),
            LexerMode::MacroCallArgOrValue(pnl) => self.lex_macro_call_arg_or_value(pnl, true),
            LexerMode::MacroCallValue(pnl) => self.lex_macro_call_arg_or_value(pnl, false),
            LexerMode::MacroLetVarName(found_name) => self.lex_macro_ident_expr(!found_name),
            LexerMode::MacroLetInitializer => self.lex_macro_text_expr(),
        }
    }

    fn lex_expected_token(
        &mut self,
        content: &'static str,
        tok_type: TokenType,
        tok_channel: TokenChannel,
    ) {
        debug_assert_eq!(
            self.mode(),
            LexerMode::ExpectToken(content, tok_type, tok_channel)
        );
        debug_assert!(!content.is_empty() && !content.contains('\n'));

        self.start_token();
        if (content.len() > self.cursor.remaining_len() as usize)
            | (self.cursor.as_str().get(..content.len()) != Some(content))
        {
            // Expected token not found. Emit an error which will point at previous token
            // The token itself is emitted below
            self.emit_error(ErrorType::MissingExpected(content));
        } else {
            // Consume the expected content
            // SAFETY: content is not more than the remaining length
            // and the length can't be more than u32::MAX
            #[allow(clippy::cast_possible_truncation)]
            self.cursor.advance_by(content.chars().count() as u32);
        }

        self.emit_token(tok_channel, tok_type, Payload::None);
        self.pop_mode();
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
                self.emit_token(
                    TokenChannel::DEFAULT,
                    TokenType::StringExprStart,
                    Payload::None,
                );
                self.push_mode(LexerMode::StringExpr);
            }
            ';' => {
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::SEMI, Payload::None);
            }
            '/' => {
                if self.cursor.peek_next() == '*' {
                    self.lex_cstyle_comment();
                } else {
                    self.cursor.advance();
                    self.emit_token(TokenChannel::DEFAULT, TokenType::FSLASH, Payload::None);
                }
            }
            '&' => {
                if !self.lex_macro_var_expr() {
                    // Regular AMP sequence, consume as token
                    self.cursor.eat_while(|c| c == '&');
                    self.emit_token(TokenChannel::DEFAULT, TokenType::AMP, Payload::None);
                }
            }
            '%' => {
                if !self.lex_macro() {
                    // Not a macro, just a percent
                    self.cursor.advance();
                    self.emit_token(TokenChannel::DEFAULT, TokenType::PERCENT, Payload::None);
                }
            }
            '0'..='9' => {
                // Numeric literal
                self.lex_numeric_literal();
            }
            c if is_valid_sas_name_start(c) => {
                self.lex_identifier();
            }
            _ => {
                // Something else must be a symbol or some unknown character
                self.lex_symbols(c);
            }
        }
    }

    fn lex_macro_ident_expr(&mut self, first: bool) {
        debug_assert!(matches!(self.mode(), LexerMode::MacroLetVarName(_)));

        self.start_token();

        let pop_mode_and_check = |lexer: &mut Lexer| {
            if first {
                // This is straight from what SAS emits
                lexer.emit_error(ErrorType::MissingExpected(
                    "ERROR: Expecting a variable name after %LET.",
                ));
            }

            lexer.pop_mode();
        };

        // Helper to update the mode indicating that we have found at least one token
        // First we need to store the index of the mode when we started lexing this,
        // because nested calls can add more modes to the stack, but what we
        // want to update is the mode at the start of this call
        let start_mode_index = self.mode_stack.len() - 1;

        let update_mode = |lexer: &mut Lexer| {
            if let Some(LexerMode::MacroLetVarName(found_name)) =
                lexer.mode_stack.get_mut(start_mode_index)
            {
                *found_name = true;
            };
        };

        // Dispatch the "big" categories
        match self.cursor.peek() {
            '/' if self.cursor.peek_next() == '*' => {
                self.lex_cstyle_comment();
            }
            '&' => {
                if !self.lex_macro_var_expr() {
                    // Not a macro var. pop mode without consuming the character
                    pop_mode_and_check(self);
                    return;
                }

                update_mode(self);
            }
            '%' => {
                if !self.lex_macro_call(false) {
                    // Not a macro, just a percent. Pop mode without consuming the character
                    pop_mode_and_check(self);
                    return;
                }

                update_mode(self);
            }
            c if is_valid_sas_name_start(c) => {
                // A macro string in place of macro identifier
                // Consume as identifier, no reserved words here,
                // so we do not need the full lex_identifier logic
                self.cursor.eat_while(is_xid_continue);

                // Add token, but do not pop the mode, as we may have a full macro text expression
                // that generates an identifier
                self.emit_token(TokenChannel::DEFAULT, TokenType::Identifier, Payload::None);

                update_mode(self);
            }
            _ => {
                // Something else. pop mode without consuming the character
                pop_mode_and_check(self);
            }
        }
    }

    fn lex_macro_text_expr(&mut self) {
        debug_assert!(matches!(self.mode(), LexerMode::MacroLetInitializer));

        self.start_token();

        // Dispatch the "big" categories
        match self.cursor.peek() {
            '\'' => self.lex_single_quoted_str(),
            '"' => {
                self.cursor.advance();
                self.emit_token(
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
                    // not a comment, a slash in a macro string
                    // consume the character and lex the string.
                    // We could have not consumed it and let the
                    // string lexing handle it, but this way we
                    // we avoid one extra check
                    self.cursor.advance();
                    self.lex_macro_string_unrestricted();
                }
            }
            '&' => {
                if !self.lex_macro_var_expr() {
                    // Not a macro var, just a sequence of ampersands
                    // consume the sequence and continue lexing the string
                    self.cursor.eat_while(|c| c == '&');
                    self.lex_macro_string_unrestricted();
                }
            }
            '%' => {
                if !self.lex_macro_call(true) {
                    // Not a macro, but possibly a macro statement
                    if self.cursor.peek_next().is_ascii_alphabetic() {
                        // Hit a following macro statement => pop mode and exit
                        // without consuming the character
                        self.pop_mode();
                        return;
                    }

                    // Just a percent, consume and continue lexing the string
                    // We could have not consumed it and let the
                    // string lexing handle it, but this way we
                    // we avoid one extra check
                    self.cursor.advance();
                    self.lex_macro_string_unrestricted();
                }
            }
            '\n' => {
                // Special case to catch newline
                // We could have not consumed it and let the
                // string lexing handle it, but this way we
                // we avoid one extra check
                self.cursor.advance();
                self.add_line();
                self.lex_macro_string_unrestricted();
            }
            ';' => {
                // Found the terminator, pop the mode and return
                self.pop_mode();
            }
            _ => {
                // Not a terminator, just a regular character in the string
                // consume and continue lexing the string
                self.cursor.advance();
                self.lex_macro_string_unrestricted();
            }
        }
    }

    fn lex_macro_string_unrestricted(&mut self) {
        debug_assert!(matches!(self.mode(), LexerMode::MacroLetInitializer));

        loop {
            match self.cursor.peek() {
                '\'' | '"' | EOF_CHAR => {
                    // Reached the end of the section of a macro string
                    // Emit the text token and return
                    self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);
                    return;
                }
                '/' if self.cursor.peek_next() == '*' => {
                    // Start of a comment in a macro string
                    // Emit the text token and return
                    self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);
                    return;
                }
                '&' => {
                    let (is_macro_amp, amp_count) = is_macro_amp(self.cursor.chars());

                    if is_macro_amp {
                        // Hit a macro var expr in the string expression => emit the text token
                        self.emit_token(
                            TokenChannel::DEFAULT,
                            TokenType::MacroString,
                            Payload::None,
                        );

                        return;
                    }

                    // Just amps in the text, consume and continue
                    self.cursor.advance_by(amp_count);
                }
                '%' => {
                    if is_macro_percent(self.cursor.peek_next(), false) {
                        // Hit a macro call or statment in/after the string expression => emit the text token
                        self.emit_token(
                            TokenChannel::DEFAULT,
                            TokenType::MacroString,
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
                ';' => {
                    // Found the terminator, emit the token, pop the mode and return
                    self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);
                    self.pop_mode();
                    return;
                }
                _ => {
                    // Not a terminator, just a regular character in the string
                    // consume and continue lexing the string
                    self.cursor.advance();
                }
            }
        }
    }

    fn lex_macro_call_arg_or_value(
        &mut self,
        parens_nesting_level: u32,
        terminate_on_assign: bool,
    ) {
        debug_assert!(matches!(
            self.mode(),
            LexerMode::MacroCallArgOrValue(l)
                | LexerMode::MacroCallValue(l) if l == parens_nesting_level
        ));

        self.start_token();

        // Dispatch the "big" categories
        match self.cursor.peek() {
            '\'' => self.lex_single_quoted_str(),
            '"' => {
                self.cursor.advance();
                self.emit_token(
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
                    // not a comment, a slash in a macro string
                    // consume the character and lex the string.
                    // We could have not consumed it and let the
                    // string lexing handle it, but this way we
                    // we avoid one extra check
                    self.cursor.advance();
                    self.lex_macro_string_in_macro_call(parens_nesting_level, terminate_on_assign);
                }
            }
            '&' => {
                if !self.lex_macro_var_expr() {
                    // Not a macro var, just a sequence of ampersands
                    // consume the sequence and continue lexing the string
                    self.cursor.eat_while(|c| c == '&');
                    self.lex_macro_string_in_macro_call(parens_nesting_level, terminate_on_assign);
                }
            }
            '%' => {
                if !self.lex_macro_call(true) {
                    // Not a macro, but possibly a macro statement
                    if self.cursor.peek_next().is_ascii_alphabetic() {
                        // Hit a following macro statement => pop mode and exit
                        // without consuming the character
                        self.pop_mode();
                        return;
                    }

                    // Just a percent, consume and continue lexing the string
                    // We could have not consumed it and let the
                    // string lexing handle it, but this way we
                    // we avoid one extra check
                    self.cursor.advance();
                    self.lex_macro_string_in_macro_call(parens_nesting_level, terminate_on_assign);
                }
            }
            '\n' => {
                // Special case to catch newline
                // We could have not consumed it and let the
                // string lexing handle it, but this way we
                // we avoid one extra check
                self.cursor.advance();
                self.add_line();
                self.lex_macro_string_in_macro_call(parens_nesting_level, terminate_on_assign);
            }
            ',' if parens_nesting_level == 0 => {
                // Found the terminator, pop the mode and push new modes
                // to expect stuff then return
                self.pop_mode();
                self.push_mode(LexerMode::MacroCallArgOrValue(0));
                // Leading insiginificant WS before the argument
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                self.push_mode(LexerMode::ExpectToken(
                    ",",
                    TokenType::COMMA,
                    TokenChannel::DEFAULT,
                ));
            }
            ')' if parens_nesting_level == 0 => {
                // Found the terminator of the entire macro call arguments,
                // just pop the mode and return
                self.pop_mode();
            }
            '=' if terminate_on_assign && parens_nesting_level == 0 => {
                // Found the terminator between argument name and value,
                // pop the mode and push new modes to expect stuff then return
                self.pop_mode();
                self.push_mode(LexerMode::MacroCallValue(0));
                // Leading insiginificant WS before the argument
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                self.push_mode(LexerMode::ExpectToken(
                    "=",
                    TokenType::ASSIGN,
                    TokenChannel::DEFAULT,
                ));
            }
            _ => {
                // Not a terminator, just a regular character in the string
                // Do NOT consume - macro string tracks parens, and this
                // maybe a paren. Continue lexing the string
                self.lex_macro_string_in_macro_call(parens_nesting_level, terminate_on_assign);
            }
        }
    }

    fn lex_macro_string_in_macro_call(
        &mut self,
        parens_nesting_level: u32,
        terminate_on_assign: bool,
    ) {
        debug_assert!(matches!(
            self.mode(),
            LexerMode::MacroCallArgOrValue(_) | LexerMode::MacroCallValue(_)
        ));

        // Helper function to emit the token and update the mode if needed
        let emit_token_update_nesting = |lexer: &mut Lexer, local_parens_nesting: u32| {
            lexer.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);

            // If the local parens nesting has been affected, update the mode
            if local_parens_nesting != 0 {
                // If our logic is correct, it should be impossible for a current
                // string section to push the nesting level below 0
                // as at the moment of reaching 0, we should have popped the mode
                // and exited the lexing of the string
                debug_assert!(parens_nesting_level + local_parens_nesting > 0);

                if let Some(m) = lexer.mode_stack.last_mut() {
                    match m {
                        LexerMode::MacroCallArgOrValue(pnl) | LexerMode::MacroCallValue(pnl) => {
                            *pnl += local_parens_nesting;
                        }
                        _ => unreachable!(),
                    }
                };
            }
        };

        // We track the current section of macro string for parens
        // and eventually combine with the nesting that has been passed
        // via mode. This would trigger a possible mode update if
        // nesting level has been affected.
        let mut local_parens_nesting = 0;

        loop {
            match self.cursor.peek() {
                '\'' | '"' | EOF_CHAR => {
                    // Reached the end of the section of a macro string
                    // Emit the text token and return
                    emit_token_update_nesting(self, local_parens_nesting);
                    return;
                }
                '/' if self.cursor.peek_next() == '*' => {
                    // Start of a comment in a macro string
                    // Emit the text token and return
                    emit_token_update_nesting(self, local_parens_nesting);
                    return;
                }
                '&' => {
                    let (is_macro_amp, amp_count) = is_macro_amp(self.cursor.chars());

                    if is_macro_amp {
                        // Hit a macro var expr in the string expression => emit the text token
                        emit_token_update_nesting(self, local_parens_nesting);

                        return;
                    }

                    // Just amps in the text, consume and continue
                    self.cursor.advance_by(amp_count);
                }
                '%' => {
                    if is_macro_percent(self.cursor.peek_next(), false) {
                        // Hit a macro call or statment in/after the string expression => emit the text token
                        emit_token_update_nesting(self, local_parens_nesting);

                        return;
                    }

                    // Just percent in the text, consume and continue
                    self.cursor.advance();
                }
                '\n' => {
                    self.cursor.advance();
                    self.add_line();
                }
                '(' => {
                    // Increase the local parens nesting level
                    local_parens_nesting += 1;
                    self.cursor.advance();
                }
                ')' if parens_nesting_level + local_parens_nesting != 0 => {
                    // Decrease the local parens nesting level
                    local_parens_nesting -= 1;
                    self.cursor.advance();
                }
                ')' if parens_nesting_level + local_parens_nesting == 0 => {
                    // Found the terminator of the entire macro call arguments,
                    // emit the token, pop the mode and return
                    self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);
                    self.pop_mode();
                    return;
                }
                ',' if parens_nesting_level + local_parens_nesting == 0 => {
                    // Found the terminator, pop the mode and push new modes
                    // to expect stuff, emit token then return
                    self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);
                    self.pop_mode();
                    self.push_mode(LexerMode::MacroCallArgOrValue(0));
                    // Leading insiginificant WS before the argument
                    self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                    self.push_mode(LexerMode::ExpectToken(
                        ",",
                        TokenType::COMMA,
                        TokenChannel::DEFAULT,
                    ));
                    return;
                }
                '=' if terminate_on_assign
                    && (parens_nesting_level + local_parens_nesting == 0) =>
                {
                    // Found the terminator between argument name and value,
                    // pop the mode and push new modes to expect stuff then return
                    self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);
                    // Pop the arg/value mode and push the value mode
                    self.pop_mode();
                    self.push_mode(LexerMode::MacroCallValue(0));
                    // Leading insiginificant WS before the argument
                    self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                    self.push_mode(LexerMode::ExpectToken(
                        "=",
                        TokenType::ASSIGN,
                        TokenChannel::DEFAULT,
                    ));
                    return;
                }
                _ => {
                    // Not a terminator, just a regular character in the string
                    // consume and continue lexing the string
                    self.cursor.advance();
                }
            }
        }
    }

    fn lex_macro_str_quoted_expr(&mut self) {
        debug_assert!(matches!(self.mode(), LexerMode::MacroStrQuotedExpr));

        self.start_token();

        // Dispatch the "big" categories
        match self.cursor.peek() {
            '\'' => self.lex_single_quoted_str(),
            '"' => {
                self.cursor.advance();
                self.emit_token(
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
                    // not a comment, a slash in a macro string
                    // consume the character and lex the string.
                    // We could have not consumed it and let the
                    // string lexing handle it, but this way we
                    // we avoid one extra check
                    self.cursor.advance();
                    self.lex_macro_string_in_str_call();
                }
            }
            '&' => {
                if !self.lex_macro_var_expr() {
                    // Not a macro var, just a sequence of ampersands
                    // consume the sequence and continue lexing the string
                    self.cursor.eat_while(|c| c == '&');
                    self.lex_macro_string_in_str_call();
                }
            }
            '%' => {
                // Check if this is a quote char
                if matches!(self.cursor.peek_next(), '"' | '\'' | '%' | '(' | ')') {
                    self.lex_macro_string_in_str_call();
                    return;
                }

                if !self.lex_macro_call(true) {
                    // Not a macro, but possibly a macro statement
                    if self.cursor.peek_next().is_ascii_alphabetic() {
                        // Hit a following macro statement => pop mode and exit
                        // without consuming the character

                        // This would lead to breaking SAS session with:
                        // ERROR: Open code statement recursion detected.
                        // so we emit an error here in addition to missing )
                        // that will emit during mode stack pop
                        self.emit_error(ErrorType::SASSessionUnrecoverableError(
                            "ERROR: Open code statement recursion detected.",
                        ));
                        self.pop_mode();
                        return;
                    }

                    // Just a percent, consume and continue lexing the string
                    // We could have not consumed it and let the
                    // string lexing handle it, but this way we
                    // we avoid one extra check
                    self.cursor.advance();
                    self.lex_macro_string_in_str_call();
                }
            }
            '\n' => {
                // Special case to catch newline
                // We could have not consumed it and let the
                // string lexing handle it, but this way we
                // we avoid one extra check
                self.cursor.advance();
                self.add_line();
                self.lex_macro_string_in_str_call();
            }
            ')' => {
                // Found the terminator, pop the mode and return
                self.pop_mode();
            }
            _ => {
                // Not a terminator, just a regular character in the string.
                // Do not consume in case it is an opening parens,
                // just continue lexing the string
                self.lex_macro_string_in_str_call();
            }
        }
    }

    fn lex_macro_string_in_str_call(&mut self) {
        debug_assert!(matches!(self.mode(), LexerMode::MacroStrQuotedExpr));

        // Keep track of parens nesting
        let mut parens = 0u32;

        loop {
            match self.cursor.peek() {
                '\'' | '"' | EOF_CHAR => {
                    // Reached the end of the section of a macro string
                    // Emit the text token and return
                    self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);
                    return;
                }
                '/' if self.cursor.peek_next() == '*' => {
                    // Start of a comment in a macro string
                    // Emit the text token and return
                    self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);
                    return;
                }
                '&' => {
                    let (is_macro_amp, amp_count) = is_macro_amp(self.cursor.chars());

                    if is_macro_amp {
                        // Hit a macro var expr in the string expression => emit the text token
                        self.emit_token(
                            TokenChannel::DEFAULT,
                            TokenType::MacroString,
                            Payload::None,
                        );

                        return;
                    }

                    // Just amps in the text, consume and continue
                    self.cursor.advance_by(amp_count);
                }
                '%' => {
                    // Check if this is a quote char
                    if matches!(self.cursor.peek_next(), '"' | '\'' | '%' | '(' | ')') {
                        // Quoted char, consume both % and the quoted one and continue
                        self.cursor.advance();
                        self.cursor.advance();
                        continue;
                    }

                    if is_macro_percent(self.cursor.peek_next(), false) {
                        // Hit a macro call or statment in/after the string expression => emit the text token
                        self.emit_token(
                            TokenChannel::DEFAULT,
                            TokenType::MacroString,
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
                ')' if parens == 0 => {
                    // Found the terminator, emit the token, pop the mode and return
                    self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);
                    self.pop_mode();
                    return;
                }
                '(' => {
                    self.cursor.advance();
                    parens += 1
                }
                ')' => {
                    self.cursor.advance();
                    parens -= 1
                }
                _ => {
                    // Not a terminator, just a regular character in the string
                    // consume and continue lexing the string
                    self.cursor.advance();
                }
            }
        }
    }

    fn eat_ws(&mut self) {
        loop {
            match self.cursor.peek() {
                '\n' => {
                    self.cursor.advance();
                    self.add_line();
                }
                c if c.is_whitespace() => {
                    self.cursor.advance();
                }
                _ => {
                    return;
                }
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
        self.emit_token(TokenChannel::HIDDEN, TokenType::WS, Payload::None);
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
                self.emit_token(
                    TokenChannel::COMMENT,
                    TokenType::CStyleComment,
                    Payload::None,
                );
                self.emit_error(ErrorType::UnterminatedComment);
                return;
            }
        }

        self.emit_token(
            TokenChannel::COMMENT,
            TokenType::CStyleComment,
            Payload::None,
        );
    }

    fn lex_single_quoted_str(&mut self) {
        debug_assert_eq!(self.cursor.peek(), '\'');

        // Eat the opening single quote
        self.cursor.advance();

        // When lexing the string, if we encounter a double quote,
        // i.e. an escaped quote, we'll need to store the
        // an unescaped string literal in the buufer, so we need
        // a number of vars to track the start, the end of the literal
        // in the buffer as well as the byte offset in the cursor as we go
        // The common case is that we'll not need to store the literal

        let mut lit_start_idx = self.buffer.next_string_literal_start();
        let mut lit_end_idx = lit_start_idx;
        let mut last_lit_end_byte_offset = self.cur_byte_offset();

        // Now lex the string
        loop {
            if let Some(c) = self.cursor.advance() {
                match c {
                    '\'' => {
                        if self.cursor.peek() == '\'' {
                            // escaped single quote

                            // First, store the literal section before the escaped quote
                            let (new_start, new_end) =
                                self.add_string_literal(last_lit_end_byte_offset, None);
                            lit_start_idx = min(lit_start_idx, new_start);
                            lit_end_idx = new_end;

                            // And only then advance the cursor
                            self.cursor.advance();

                            // And update the last byte offset
                            last_lit_end_byte_offset = self.cur_byte_offset();

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
                // Emit a token, and error and return
                let payload = self.resolve_string_literal_payload(
                    lit_start_idx,
                    lit_end_idx,
                    last_lit_end_byte_offset,
                    None, // will use the current byte offset
                );

                self.emit_token(TokenChannel::DEFAULT, TokenType::StringLiteral, payload);

                self.emit_error(ErrorType::UnterminatedStringLiteral);
                return;
            }
        }

        // Calculate the byte offset of the end of the string text, which is -1 of the current
        // cursor position, as we've already advanced the cursor to the closing single quote
        let str_text_end_byte_offset = Some(self.cur_byte_offset() - 1);

        // Now check if this is a single quoted string or one of the other literals
        let tok_type = self.lex_literal_ending();

        let payload = self.resolve_string_literal_payload(
            lit_start_idx,
            lit_end_idx,
            last_lit_end_byte_offset,
            str_text_end_byte_offset,
        );

        self.emit_token(TokenChannel::DEFAULT, tok_type, payload);
    }

    /// A helper called from single quoted strings and double quoted string
    /// expressions to create the correct payload for possibly escaped text.
    ///
    /// This function will check if a payload is needed at all. If yes,
    /// it will also add the trailing literal section.
    ///
    /// # Arguments
    ///
    /// * `lit_start_idx` - The index of the start of the literal in the buffer
    /// * `cur_lit_end_idx` - The index of the end of the literal in the buffer
    ///     It is used to determine if a payload is needed
    /// * `last_lit_end_byte_offset` - The byte offset of the end of the last literal
    ///     section already added to the buffer
    /// * `str_text_end_byte_offset` - The byte offset of the end of the string text
    ///    that is being lexed. Not including closing quote or anything after it.
    ///   If `None`, the current byte offset is used.
    fn resolve_string_literal_payload(
        &mut self,
        lit_start_idx: u32,
        cur_lit_end_idx: u32,
        last_lit_end_byte_offset: ByteOffset,
        str_text_end_byte_offset: Option<ByteOffset>,
    ) -> Payload {
        if lit_start_idx == cur_lit_end_idx {
            Payload::None
        } else {
            // Make sure we've added the trailing literal section
            let (_, final_end) =
                self.add_string_literal(last_lit_end_byte_offset, str_text_end_byte_offset);

            Payload::StringLiteral(lit_start_idx, final_end)
        }
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
                    // As this is only possible for an empty string, we know Payload::None
                    self.lex_double_quoted_literal(Payload::None);
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

                self.emit_token(TokenChannel::DEFAULT, tok_type, Payload::None);
                self.pop_mode();
            }
            '&' => {
                if !self.lex_macro_var_expr() {
                    // Not a macro var. actually a part of string text.
                    // and we've already consumed the sequence of &
                    // continue to lex the text
                    self.lex_str_expr_text();
                }
            }
            '%' => {
                if !self.lex_macro() {
                    // just a percent. consume and continue
                    self.cursor.advance();
                    self.lex_str_expr_text();
                }
            }
            EOF_CHAR => {
                // EOF reached without a closing double quote
                self.handle_unterminated_str_expr(Payload::None);
            }
            _ => {
                // Not a macro var, not a macro call and not an ending => lex the middle
                self.lex_str_expr_text();
            }
        }
    }

    fn lex_str_expr_text(&mut self) {
        // When lexing the string, if we encounter a double quote,
        // i.e. an escaped quote, we'll need to store the
        // an unescaped string literal in the buufer, so we need
        // a number of vars to track the start, the end of the literal
        // in the buffer as well as the byte offset in the cursor as we go
        // The common case is that we'll not need to store the literal

        let mut lit_start_idx = self.buffer.next_string_literal_start();
        let mut lit_end_idx = lit_start_idx;
        let mut last_lit_end_byte_offset = self.cur_byte_offset();

        // Now lex the string

        loop {
            match self.cursor.peek() {
                '&' => {
                    let (is_macro_amp, amp_count) = is_macro_amp(self.cursor.chars());

                    if is_macro_amp {
                        // Hit a macro var expr in the string expression => emit the text token

                        // Also calculate the payload (will differ whether we had escaped quotes or not).
                        let payload = self.resolve_string_literal_payload(
                            lit_start_idx,
                            lit_end_idx,
                            last_lit_end_byte_offset,
                            None, // will use the current byte offset
                        );

                        self.emit_token(TokenChannel::DEFAULT, TokenType::StringExprText, payload);

                        return;
                    }

                    // Just amps in the text, consume and continue
                    self.cursor.advance_by(amp_count);
                }
                '%' => {
                    if is_macro_percent(self.cursor.peek_next(), false) {
                        // Hit a macro var expr in the string expression => emit the text token

                        // Also calculate the payload (will differ whether we had escaped quotes or not).
                        let payload = self.resolve_string_literal_payload(
                            lit_start_idx,
                            lit_end_idx,
                            last_lit_end_byte_offset,
                            None, // will use the current byte offset
                        );

                        self.emit_token(TokenChannel::DEFAULT, TokenType::StringExprText, payload);

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

                    // Also calculate the payload (will differ whether we had escaped quotes or not).
                    let payload = self.resolve_string_literal_payload(
                        lit_start_idx,
                        lit_end_idx,
                        last_lit_end_byte_offset,
                        None, // will use the current byte offset
                    );

                    self.handle_unterminated_str_expr(payload);

                    return;
                }
                '"' => {
                    if self.cursor.peek_next() == '"' {
                        // escaped double quote, eat the first, add literal, then second and continue
                        self.cursor.advance();

                        // First, store the literal section before the escaped quote
                        let (new_start, new_end) =
                            self.add_string_literal(last_lit_end_byte_offset, None);
                        lit_start_idx = min(lit_start_idx, new_start);
                        lit_end_idx = new_end;

                        // And only then advance the cursor
                        self.cursor.advance();

                        // And update the last byte offset
                        last_lit_end_byte_offset = self.cur_byte_offset();
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

                    // Also calculate the payload (will differ whether we had escaped quotes or not).
                    // Unlike in single quoted strings, we do not need to calculate the end of the string
                    // as we haven't advanced past the closing quote yet
                    let payload = self.resolve_string_literal_payload(
                        lit_start_idx,
                        lit_end_idx,
                        last_lit_end_byte_offset,
                        None, // will use the current byte offset
                    );

                    if last_tok_is_start {
                        self.lex_double_quoted_literal(payload);
                        return;
                    }

                    // We are in a genuine string expression, and hit the end - emit the text token
                    // The ending quote will be handled by the caller
                    self.emit_token(TokenChannel::DEFAULT, TokenType::StringExprText, payload);
                    return;
                }
                _ => {
                    self.cursor.advance();
                }
            }
        }
    }

    fn handle_unterminated_str_expr(&mut self, payload: Payload) {
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
            self.update_last_token(TokenChannel::DEFAULT, TokenType::StringLiteral, payload);
        } else {
            self.emit_token(TokenChannel::DEFAULT, TokenType::StringExprEnd, payload);
        }
        self.emit_error(ErrorType::UnterminatedStringLiteral);
        self.pop_mode();
    }

    fn lex_double_quoted_literal(&mut self, payload: Payload) {
        debug_assert_eq!(self.cursor.peek(), '"');

        // This is a regular literal. We need to consume the char, figure
        // out which type of literal is this, similar to single quoted
        // string, replace the last token and exit the string expression mode
        self.cursor.advance();

        let tok_type = self.lex_literal_ending();

        self.update_last_token(TokenChannel::DEFAULT, tok_type, payload);
        self.pop_mode();
    }

    /// Tries lexing a macro variable expression
    ///
    /// Consumes input and generates a `MacroVarExpr` token only ig
    /// the sequence is a valid macro var expr.
    ///
    /// Otherwise, retains the input and does not generate a token.
    ///
    /// Returns `true` if a token was added, `false` otherwise
    fn lex_macro_var_expr(&mut self) -> bool {
        debug_assert_eq!(self.cursor.peek(), '&');

        // Consuming leading ampersands
        let (is_macro, amp_count) = is_macro_amp(self.cursor.chars());

        // Not a macro var expr, just a 1+ sequence of &
        if !is_macro {
            return false;
        }

        // Ok, this is a macro expr for sure
        // and we got the first character of the name
        // Consume the ampersands and the first name character
        self.cursor.advance_by(amp_count + 1);

        loop {
            match self.cursor.peek() {
                '.' => {
                    // a dot, terminates a macro var expr, consume it
                    self.cursor.advance();

                    // Add the token
                    self.emit_token(
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
                    let (is_macro, amp_count) = is_macro_amp(self.cursor.chars());

                    if !is_macro {
                        // The following & characters are not part of the macro var expr

                        // Add the token without consuming the following amp
                        self.emit_token(
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
                c if is_xid_continue(c) => {
                    // Still a name
                    self.cursor.advance();
                }
                _ => {
                    // Reached the end of the macro var expr

                    // Add the token without consuming the following character
                    self.emit_token(
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

        // Start tracking whether the identifier is ASCII
        // It is necessary, as we need to upper case the identifier if it is ASCII
        // for dispatching, and if it is not ASCII, we know it is not a keyword and can
        // skip the dispatching
        let mut is_ascii = true;

        // Eat the identifier. We can safely use `is_xid_continue` becase the caller
        // already checked that the first character is a valid start of an identifier
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
            self.emit_token(TokenChannel::DEFAULT, TokenType::Identifier, Payload::None);
            return;
        }

        // My guess is this should be quicker than capturing the value as we consume it
        // avoids the allocation and copying
        // Using SmolStr is faster as it will be stack allocated
        let ident = SmolStr::from_iter(
            self.pending_token_text()
                .chars()
                .map(|c| c.to_ascii_uppercase()),
        );

        if let Some(kw_tok_type) = parse_keyword(&ident) {
            self.emit_token(TokenChannel::DEFAULT, kw_tok_type, Payload::None);
            return;
        }

        match ident.as_str() {
            "DATALINES" | "CARDS" | "LINES" => {
                if !self.lex_datalines(false) {
                    self.emit_token(TokenChannel::DEFAULT, TokenType::Identifier, Payload::None);
                };
            }
            "DATALINES4" | "CARDS4" | "LINES4" => {
                if !self.lex_datalines(true) {
                    self.emit_token(TokenChannel::DEFAULT, TokenType::Identifier, Payload::None);
                };
            }
            _ => {
                // genuine user defined identifier
                self.emit_token(TokenChannel::DEFAULT, TokenType::Identifier, Payload::None);
            }
        }
    }

    /// Checks whether the currently lexed token is indeed a datalines start token.
    /// If so, then consumes not only the start, but also the body and the end of the datalines
    /// and returns `true`. Otherwise, returns `false`.
    #[must_use]
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
            .last_token_info_on_channel_info(TokenChannel::DEFAULT)
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
                Some('\n') => {
                    self.add_line();
                }
                Some(c) if c.is_whitespace() => {}
                // in reality we know it will be a semicolon
                _ => break,
            }
        }

        self.emit_token(
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

                    if self.cursor.as_str().get(..ending_len).unwrap_or("") == ending {
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
        self.emit_token(
            TokenChannel::DEFAULT,
            TokenType::DatalinesData,
            Payload::None,
        );

        // Start the new token
        self.start_token();

        // Consume the ending
        #[allow(clippy::cast_possible_truncation)]
        self.cursor.advance_by(ending_len as u32);

        // Add the datalines end token
        self.emit_token(TokenChannel::DEFAULT, TokenType::SEMI, Payload::None);

        true
    }

    fn lex_numeric_literal(&mut self) {
        debug_assert!(matches!(self.cursor.peek(), '0'..='9' | '.'));
        // First, SAS supports 3 notations for numeric literals:
        // 1. Standard decimal notation (base 10)
        // 2. Hexadecimal notation (base 16)
        // 3. Scientific notation

        // For HEX notation, the number must start with a number
        // and can have up-to 16 total HEX digits (including the starting one)
        // due to 8 bytes of storage for a number in SAS. It must be
        // followed by an `x` or `X` character

        // consume the first digit or the sign
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
                        // consume as lex_numeric_exp_literal() assumes it is working past the E
                        self.cursor.advance();
                        self.lex_numeric_exp_literal();
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
                        // either a HEX missing X (user intended `NNNex`), or scientific notation
                        // with missing exponent. For simplicity sake we call the scientific parser
                        // function which will report error and recover.
                        self.lex_numeric_exp_literal();
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
                        // e.g. `NNNeNNN `
                        self.lex_numeric_exp_literal();
                    }

                    return;
                }
            }
        }
    }

    #[allow(clippy::cast_precision_loss)]
    #[allow(clippy::cast_possible_truncation)]
    fn lex_decimal_literal(&mut self) {
        // The way calling logic is done, for regular decimals
        // we must have consumed the entire literal
        debug_assert!(!self.cursor.peek().is_ascii_digit());

        // Parse as f64. SAS uses 8 bytes and all numbers are stored as f64
        // so it is impossible to have a valid literal that would overflow
        // the f64 range.
        let number = self.pending_token_text();

        let Ok(fvalue) = f64::from_str(number) else {
            self.emit_token(
                TokenChannel::DEFAULT,
                TokenType::IntegerLiteral,
                Payload::Integer(0),
            );

            self.emit_error(ErrorType::InvalidNumericLiteral);
            return;
        };

        // See if it is an integer
        if fvalue.fract() == 0.0 && fvalue.abs() < u64::MAX as f64 {
            // For integers we need to emit different tokens, depending on
            // the presence of the dot as we use different token types.
            // The later is unfortunatelly necesasry due to SAS numeric formats
            // context sensitivity and no way of disambiguating between a number `1.`
            // and the same numeric format `1.`

            // Leading 0 - we can emit the integer token, can't be a format
            let payload = Payload::Integer(fvalue as u64);

            // Unwrap here is safe, as we know the length is > 0
            // but we still provide default value to avoid panics
            match *number.as_bytes().first().unwrap_or(&b'_') {
                b'0' => {
                    self.emit_token(TokenChannel::DEFAULT, TokenType::IntegerLiteral, payload);
                }
                _ => {
                    if number.contains('.') {
                        self.emit_token(
                            TokenChannel::DEFAULT,
                            TokenType::IntegerDotLiteral,
                            payload,
                        );
                    } else {
                        self.emit_token(TokenChannel::DEFAULT, TokenType::IntegerLiteral, payload);
                    }
                }
            }
        } else {
            // And for floats, similarly it is important whether it
            // it was created from scientific notation or not, but
            // this function only handles the decimal literals, so
            // no need to check for that
            self.emit_token(
                TokenChannel::DEFAULT,
                TokenType::FloatLiteral,
                Payload::Float(fvalue),
            );
        }
    }

    /// Consumes the remaining tail of a HEX literal and emits the token
    fn lex_numeric_hex_literal(&mut self) {
        #[cfg(debug_assertions)]
        debug_assert!(self.cursor.prev_char().is_ascii_hexdigit());

        // Eat until the end of the literal (x or X) or identify a missing x/X
        loop {
            match self.cursor.peek() {
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    self.cursor.advance();
                }
                'x' | 'X' => {
                    // First parse the text, and only then advance - the radix parser
                    // won't like the trailing x/X
                    let ((tok_type, payload), error) =
                        parse_numeric_hex_str(self.pending_token_text());

                    self.cursor.advance();

                    self.emit_token(TokenChannel::DEFAULT, tok_type, payload);

                    if let Some(error) = error {
                        self.emit_error(error);
                    }

                    return;
                }
                _ => {
                    // This is an error, incomplete HEX literal
                    // First parse the number, then emit error(s)
                    let ((tok_type, payload), error) =
                        parse_numeric_hex_str(self.pending_token_text());

                    self.emit_token(TokenChannel::DEFAULT, tok_type, payload);

                    if let Some(error) = error {
                        self.emit_error(error);
                    }

                    self.emit_error(ErrorType::UnterminatedHexNumericLiteral);
                    return;
                }
            }
        }
    }

    fn lex_numeric_exp_literal(&mut self) {
        #[cfg(debug_assertions)]
        debug_assert!(matches!(self.cursor.prev_char(), '0'..='9' | 'e' | 'E'));

        let mut seen_sign = false;

        loop {
            match self.cursor.peek() {
                '0'..='9' => {
                    self.cursor.advance();
                }
                '-' | '+' if !seen_sign => {
                    self.cursor.advance();
                    seen_sign = true;
                }
                _ => {
                    break;
                }
            }
        }

        // Now try to parse the number
        let number = self.pending_token_text();

        match f64::from_str(number) {
            Ok(value) => {
                self.emit_token(
                    TokenChannel::DEFAULT,
                    TokenType::FloatLiteral,
                    Payload::Float(value),
                );
            }
            Err(_) => {
                self.emit_token(
                    TokenChannel::DEFAULT,
                    TokenType::FloatLiteral,
                    Payload::Float(0.0),
                );

                self.emit_error(ErrorType::InvalidNumericLiteral);
            }
        };
    }

    #[allow(clippy::too_many_lines)]
    fn lex_symbols(&mut self, c: char) {
        match c {
            '*' => {
                self.cursor.advance();
                match (self.cursor.peek(), self.cursor.peek_next()) {
                    ('\'' | '"', ';') => {
                        self.cursor.advance();
                        self.cursor.advance();
                        self.emit_token(TokenChannel::HIDDEN, TokenType::TermQuote, Payload::None);
                    }
                    ('*', _) => {
                        self.cursor.advance();
                        self.emit_token(TokenChannel::DEFAULT, TokenType::STAR2, Payload::None);
                    }
                    _ => {
                        self.emit_token(TokenChannel::DEFAULT, TokenType::STAR, Payload::None);
                    }
                }
            }
            '(' => {
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::LPAREN, Payload::None);
            }
            ')' => {
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::RPAREN, Payload::None);
            }
            '{' => {
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::LCURLY, Payload::None);
            }
            '}' => {
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::RCURLY, Payload::None);
            }
            '[' => {
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::LBRACK, Payload::None);
            }
            ']' => {
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::RBRACK, Payload::None);
            }
            '!' => {
                self.cursor.advance();

                match self.cursor.peek() {
                    '!' => {
                        self.cursor.advance();
                        self.emit_token(TokenChannel::DEFAULT, TokenType::EXCL2, Payload::None);
                    }
                    _ => {
                        self.emit_token(TokenChannel::DEFAULT, TokenType::EXCL, Payload::None);
                    }
                }
            }
            '¦' => {
                self.cursor.advance();

                match self.cursor.peek() {
                    '¦' => {
                        self.cursor.advance();
                        self.emit_token(TokenChannel::DEFAULT, TokenType::BPIPE2, Payload::None);
                    }
                    _ => {
                        self.emit_token(TokenChannel::DEFAULT, TokenType::BPIPE, Payload::None);
                    }
                }
            }
            '|' => {
                self.cursor.advance();

                match self.cursor.peek() {
                    '|' => {
                        self.cursor.advance();
                        self.emit_token(TokenChannel::DEFAULT, TokenType::PIPE2, Payload::None);
                    }
                    _ => {
                        self.emit_token(TokenChannel::DEFAULT, TokenType::PIPE, Payload::None);
                    }
                }
            }
            '¬' | '^' | '~' | '∘' => {
                self.cursor.advance();

                match self.cursor.peek() {
                    '=' => {
                        self.cursor.advance();
                        self.emit_token(TokenChannel::DEFAULT, TokenType::NE, Payload::None);
                    }
                    _ => {
                        self.emit_token(TokenChannel::DEFAULT, TokenType::NOT, Payload::None);
                    }
                }
            }
            '+' => {
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::PLUS, Payload::None);
            }
            '-' => {
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::MINUS, Payload::None);
            }
            '<' => {
                self.cursor.advance();

                match self.cursor.peek() {
                    '=' => {
                        self.cursor.advance();
                        self.emit_token(TokenChannel::DEFAULT, TokenType::LE, Payload::None);
                    }
                    '>' => {
                        self.cursor.advance();
                        self.emit_token(TokenChannel::DEFAULT, TokenType::LTGT, Payload::None);
                    }
                    _ => {
                        self.emit_token(TokenChannel::DEFAULT, TokenType::LT, Payload::None);
                    }
                }
            }
            '>' => {
                self.cursor.advance();

                match self.cursor.peek() {
                    '=' => {
                        self.cursor.advance();
                        self.emit_token(TokenChannel::DEFAULT, TokenType::GE, Payload::None);
                    }
                    '<' => {
                        self.cursor.advance();
                        self.emit_token(TokenChannel::DEFAULT, TokenType::GTLT, Payload::None);
                    }
                    _ => {
                        self.emit_token(TokenChannel::DEFAULT, TokenType::GT, Payload::None);
                    }
                }
            }
            '.' => {
                self.cursor.advance();
                match self.cursor.peek() {
                    '0'..='9' => {
                        // `.N`
                        self.lex_numeric_literal();
                    }
                    _ => {
                        self.emit_token(TokenChannel::DEFAULT, TokenType::DOT, Payload::None);
                    }
                }
            }
            ',' => {
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::COMMA, Payload::None);
            }
            ':' => {
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::COLON, Payload::None);
            }
            '=' => {
                self.cursor.advance();

                match self.cursor.peek() {
                    '*' => {
                        self.cursor.advance();
                        self.emit_token(
                            TokenChannel::DEFAULT,
                            TokenType::SoundsLike,
                            Payload::None,
                        );
                    }
                    _ => {
                        self.emit_token(TokenChannel::DEFAULT, TokenType::ASSIGN, Payload::None);
                    }
                }
            }
            '$' => {
                self.cursor.advance();

                if !self.lex_char_format() {
                    self.emit_token(TokenChannel::DEFAULT, TokenType::DOLLAR, Payload::None);
                }
            }
            '@' => {
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::AT, Payload::None);
            }
            '#' => {
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::HASH, Payload::None);
            }
            '?' => {
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::QUESTION, Payload::None);
            }
            _ => {
                // Unknown something, consume the character, emit an unknown token, push error
                self.cursor.advance();
                self.emit_token(TokenChannel::HIDDEN, TokenType::UNKNOWN, Payload::None);
                self.emit_error(ErrorType::UnknownCharacter(c));
            }
        }
    }

    fn lex_char_format(&mut self) -> bool {
        #[cfg(debug_assertions)]
        debug_assert_eq!(self.cursor.prev_char(), '$');

        // We'll need to lookahead a lot, clone the cursor
        let mut la_cursor = self.cursor.clone();

        // Start by trying to eat a possible start char of a SAS name
        // Unicode IS allowed in custom formats...
        if is_valid_sas_name_start(la_cursor.peek()) {
            la_cursor.advance();
            la_cursor.eat_while(is_xid_continue);
        };

        // Name can be followed by digits (width)
        la_cursor.eat_while(|c| c.is_ascii_digit());

        // And then it must be followed by a dot, so this is a
        // decision point
        if !la_cursor.eat_char('.') {
            // Not a char format!
            return false;
        }

        // Ok, we have a char format, consume the optional precision
        la_cursor.eat_while(|c| c.is_ascii_digit());

        // Now we need to advance the original cursor to the end of the format
        // and emit the token
        let advance_by = la_cursor.char_offset() - self.cursor.char_offset();

        self.cursor.advance_by(advance_by);

        self.emit_token(TokenChannel::DEFAULT, TokenType::CharFormat, Payload::None);
        true
    }

    fn lex_macro(&mut self) -> bool {
        debug_assert_eq!(self.cursor.peek(), '%');

        match self.cursor.peek_next() {
            '*' => {
                self.lex_macro_comment();
                true
            }
            '~' | '^' if self.mode() == LexerMode::MacroEval => {
                // Consume both
                self.cursor.advance();
                self.cursor.advance();

                self.emit_token(TokenChannel::DEFAULT, TokenType::NE, Payload::None);
                true
            }
            '=' if self.mode() == LexerMode::MacroEval => {
                // Consume both
                self.cursor.advance();
                self.cursor.advance();

                self.emit_token(
                    TokenChannel::DEFAULT,
                    TokenType::MacroNeverExpr,
                    Payload::None,
                );
                true
            }
            EOF_CHAR => false,
            c if is_valid_sas_name_start(c) => {
                self.lex_macro_identifier();
                true
            }
            _ => {
                // todo: implement
                false
            }
        }
    }

    /// Try lexing %xxx, but only allow macro call starts,
    /// i.e any identifier after percent except for reserved statements
    fn lex_macro_call(&mut self, allow_quote_call: bool) -> bool {
        debug_assert_eq!(self.cursor.peek(), '%');

        let (tok_type, advance_by) =
            is_macro_call(&self.cursor, allow_quote_call).unwrap_or_else(|err| {
                self.emit_error(ErrorType::InternalError(err));
                (None, 0)
            });

        match tok_type {
            Some(TokenType::NrStrLiteral) => {
                self.cursor.advance_by(advance_by);

                // This one has special handling
                self.lex_macro_nrstr_quoted_literal();

                true
            }
            Some(tok_type) => {
                self.cursor.advance_by(advance_by);

                self.emit_token(TokenChannel::DEFAULT, tok_type, Payload::None);

                self.expect_macro_call_args();

                true
            }
            None => {
                // Not a macro call
                false
            }
        }
    }

    /// A special helper that allows us to do a complex "parsing" look-ahead
    /// to distinguish between an argument-less macro call and the one
    /// with arguments.
    ///
    /// E.g. in `"&m /*comment*/ ()suffix"` `&m /*comment*/ ()` is a macro call
    /// with arguments. Notice that there is WS & comment between the macro identifier
    /// and the opening parenthesis. It is insignificant and should be lexed as such.
    /// Whie in `"&m /*comment*/ suffix"` `&m` is a macro call without arguments,
    /// and ` /*comment*/ suffix` is a single token of remaing text!
    ///
    /// In reality, in SAS, this is even more complex and impossible to statically
    /// determine, as SAS looks for () only if the macro was defined with parameters!
    /// So in theory, in `"&m /*comment*/ ()suffix"`, the entire ` /*comment*/ ()suffix`
    /// may be text!
    ///
    /// We obviously can't do that, so we will assume that the macro call is with arguments.
    fn expect_macro_call_args(&mut self) {
        // Checkpoint the current state
        self.checkpoint();

        // Push the mode to check if this is a call with parameters.
        // This is as usual in reverse order, first any ws/comments,
        // and then our special mode that will check for the opening parenthesis
        // and possibly rollback to the checkpoint
        self.push_mode(LexerMode::MaybeMacroCallArgs);
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
    }

    fn lex_macro_comment(&mut self) {
        debug_assert_eq!(self.cursor.peek(), '%');
        debug_assert_eq!(self.cursor.peek_next(), '*');

        // Consume the opener
        self.cursor.advance();
        self.cursor.advance();

        // And now simply eat until first semi and semi too
        self.cursor.advance();
        self.cursor.advance();

        loop {
            if let Some(c) = self.cursor.advance() {
                if c == ';' {
                    break;
                }

                if c == '\n' {
                    self.add_line();
                }
            } else {
                // EOF reached without a closing comment
                // Emit an error token and return
                self.emit_token(
                    TokenChannel::COMMENT,
                    TokenType::MacroComment,
                    Payload::None,
                );
                self.emit_error(ErrorType::UnterminatedComment);
                return;
            }
        }

        self.emit_token(
            TokenChannel::COMMENT,
            TokenType::MacroComment,
            Payload::None,
        );
    }

    fn lex_macro_identifier(&mut self) {
        debug_assert_eq!(self.cursor.peek(), '%');
        debug_assert!(is_valid_sas_name_start(self.cursor.peek_next()));

        // Eat the %, at this point this 100% is a macro call, statement or label
        self.cursor.advance();

        // Start tracking whether the identifier is ASCII
        // It is necessary, as we neew to upper case the identifier if it is ASCII
        // for dispatching, and if it is not ASCII, we know it is not a keyword and can
        // skip the dispatching
        let mut is_ascii = true;

        // Eat the identifier. We can safely use `is_xid_continue` becase the caller
        // already checked that the first character is a valid start of an identifier
        self.cursor.eat_while(|c| {
            if c.is_ascii() {
                matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
            } else {
                is_ascii = false;
                is_xid_continue(c)
            }
        });

        // Now the fun part - dispatch all kinds of identifiers
        if is_ascii {
            // My guess is this should be quicker than capturing the value as we consume it
            // avoids the allocation and copying
            let mut has_error = false;

            // Using SmolStr is faster as it will be stack allocated
            let ident = SmolStr::from_iter(
                self.pending_token_text()
                    .get(1..)
                    .unwrap_or_else(|| {
                        has_error = true;
                        ""
                    })
                    .chars()
                    .map(|c| c.to_ascii_uppercase()),
            );

            if has_error {
                self.emit_error(ErrorType::InternalError("Failed to get macro identifier"));
            }

            if let Some(kw_tok_type) = parse_macro_keyword(&ident) {
                match kw_tok_type {
                    TokenType::KwmStr => {
                        // Add the token
                        self.emit_token(TokenChannel::DEFAULT, kw_tok_type, Payload::None);

                        // Populate the expected states for the %str() call
                        // in reverse order, as the lexer will unwind the stack
                        // as it lexes the tokens
                        self.push_mode(LexerMode::ExpectToken(
                            ")",
                            TokenType::RPAREN,
                            TokenChannel::DEFAULT,
                        ));
                        // The handler fo arguments will push the mode for the comma, etc.
                        self.push_mode(LexerMode::MacroStrQuotedExpr);
                        self.push_mode(LexerMode::ExpectToken(
                            "(",
                            TokenType::LPAREN,
                            TokenChannel::DEFAULT,
                        ));
                        // Leading insiginificant WS before opening parenthesis
                        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                    }
                    TokenType::KwmLet => {
                        // Add the token
                        self.emit_token(TokenChannel::DEFAULT, kw_tok_type, Payload::None);

                        // following let we must have name, equal sign and expression.
                        // All maybe surrounded by insignificant whitespace! + the closing semi
                        // Also, SAS happily recovers after missing equal sign, with just a note
                        // Hence we pre-feed all the expected states to the mode stack in reverse order,
                        // and it will unwind as we lex tokens
                        // We do not handle the trailing WS for the initialized, instead defer it to the
                        // parser, to avoid excessive lookahead
                        self.push_mode(LexerMode::ExpectToken(
                            ";",
                            TokenType::SEMI,
                            TokenChannel::DEFAULT,
                        ));
                        self.push_mode(LexerMode::MacroLetInitializer);
                        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                        self.push_mode(LexerMode::ExpectToken(
                            "=",
                            TokenType::ASSIGN,
                            TokenChannel::DEFAULT,
                        ));
                        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                        self.push_mode(LexerMode::MacroLetVarName(false));
                        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                    }
                    _ => {
                        // TODO!!!!!! PLACEHOLDER
                        self.emit_token(TokenChannel::DEFAULT, kw_tok_type, Payload::None);
                    }
                }
                return;
            }

            match ident.as_str() {
                // Nrstr is not a keyword, but a special case of quoted literal
                "NRSTR" => {
                    self.lex_macro_nrstr_quoted_literal();
                    return;
                }
                _ => {
                    // Do nothing, fall through to the custom macro call
                    // or label handling below
                }
            }
        }

        // custom macro call or label
        self.emit_token(
            TokenChannel::DEFAULT,
            TokenType::MacroIdentifier,
            Payload::None,
        );

        self.expect_macro_call_args();
    }

    fn lex_macro_nrstr_quoted_literal(&mut self) {
        debug_assert!(self.pending_token_text().to_ascii_uppercase().as_str() == "%NRSTR");

        // Consume any whitespace before the opening parenthesis
        self.eat_ws();

        // Check left parenthesis
        if !self.cursor.eat_char('(') {
            // Emit an error but continue lexing
            self.emit_error(ErrorType::MissingExpected("("));
        }

        // Keep track of parens nesting
        let mut parens = 1;

        // eat until the closing parenthesis
        while let Some(c) = self.cursor.advance() {
            match c {
                '(' => parens += 1,
                ')' => {
                    parens -= 1;
                    if parens == 0 {
                        self.emit_token(
                            TokenChannel::DEFAULT,
                            TokenType::NrStrLiteral,
                            Payload::None,
                        );
                        return;
                    }
                }
                '%' => {
                    // Check if this is a quote char
                    if matches!(self.cursor.peek(), '%' | '(' | ')') {
                        // Consume the quoted following char
                        self.cursor.advance();
                    }
                }
                '\n' => {
                    self.add_line();
                }
                _ => {}
            }
        }

        // If we reached here, the literal is unterminated
        self.emit_token(
            TokenChannel::DEFAULT,
            TokenType::NrStrLiteral,
            Payload::None,
        );
        self.emit_error(ErrorType::MissingExpected(")"));
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
