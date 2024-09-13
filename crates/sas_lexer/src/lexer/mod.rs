pub(crate) mod buffer;
pub(crate) mod channel;
mod cursor;
pub mod error;
mod lexer_mode;
mod r#macro;
mod numeric;
pub mod print;
mod sas_lang;
#[cfg(test)]
mod tests;
mod text;
pub(crate) mod token_type;

use buffer::{LineIdx, Payload, TokenIdx, TokenInfo, TokenizedBuffer, WorkTokenizedBuffer};
use channel::TokenChannel;
use error::{ErrorInfo, ErrorType, OPEN_CODE_RECURSION_ERR};
use lexer_mode::{LexerMode, MacroEvalExprFlags, MacroEvalNextArgumentMode, MacroEvalNumericMode};

use numeric::{parse_numeric, parse_numeric_hex_str};
use r#macro::{
    is_macro_amp, is_macro_eval_logical_op, is_macro_eval_mnemonic, is_macro_eval_quotable_op,
    is_macro_percent, is_macro_quote_call_tok_type, is_macro_stat, is_macro_stat_tok_type,
    lex_macro_call_stat_or_label,
};
use sas_lang::is_valid_sas_name_start;
use smol_str::SmolStr;
use std::{cmp::min, str::FromStr};
use text::{ByteOffset, CharOffset};
use token_type::{parse_keyword, MacroKwType, TokenType, TokenTypeMacroCallOrStat};
use unicode_ident::{is_xid_continue, is_xid_start};

const MAX_EXPECTED_STACK_DEPTH: usize = 20;

const BOM: char = '\u{feff}';

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

        // Allocate stack vector with initial mode
        let mut mode_stack = Vec::with_capacity(MAX_EXPECTED_STACK_DEPTH);
        mode_stack.push(init_mode.unwrap_or_default());

        Ok(Lexer {
            source,
            source_len,
            buffer,
            cursor,
            cur_token_byte_offset,
            cur_token_start,
            cur_token_line,
            #[cfg(debug_assertions)]
            last_state: (source_len, mode_stack.clone()),
            mode_stack,
            errors: Vec::new(),
            checkpoint: None,
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
    fn push_mode(&mut self, mode: LexerMode) {
        self.mode_stack.push(mode);
    }

    fn pop_mode(&mut self) {
        if !self.mode_stack.pop().is_some() {
            self.emit_error(ErrorType::InternalError("Empty mode stack"));
            self.push_mode(LexerMode::default());
        };
    }

    fn mode(&mut self) -> LexerMode {
        match self.mode_stack.last() {
            Some(mode) => mode.clone(),
            None => {
                self.emit_error(ErrorType::InternalError("Empty mode stack"));
                self.push_mode(LexerMode::default());
                LexerMode::default()
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
            self.add_line());
    }

    /// Special helper to save necessary token start values, without
    /// changing the main state. This can be used to emit a token at
    /// a mark that is different the current cursor position. Use with
    /// `emit_token_at_mark`
    fn mark_token_start(&mut self) -> (ByteOffset, CharOffset, LineIdx) {
        (
            self.cur_byte_offset(),
            self.cur_char_offset(),
            self.buffer.last_line().unwrap_or_else(||
            // Should not be possible, since we add the first line when creating
            // the lexer, but whatever
            self.add_line()),
        )
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

    /// Emits token at a previously locally saved mark. In contrast the
    /// normal `emit_token` uses the Lexer state as the starting point
    /// of a token
    fn emit_token_at_mark(
        &mut self,
        channel: TokenChannel,
        token_type: TokenType,
        payload: Payload,
        mark: (ByteOffset, CharOffset, LineIdx),
    ) {
        self.buffer
            .add_token(channel, token_type, mark.0, mark.1, mark.2, payload);
    }

    /// There are many cases where we need to emit an empty macro string token.
    /// Hence we have a helper for that.
    #[inline]
    fn emit_empty_macro_string_token(&mut self) {
        self.emit_token(
            TokenChannel::DEFAULT,
            TokenType::MacroStringEmpty,
            Payload::None,
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

    fn prep_error_info_at_cur_offset(&self, error: ErrorType) -> ErrorInfo {
        let last_line_char_offset = if let Some(line_info) = self.buffer.last_line_info() {
            line_info.start().get()
        } else {
            0
        };

        let last_char_offset = self.cur_char_offset().into();

        ErrorInfo::new(
            error,
            self.cur_byte_offset().into(),
            last_char_offset,
            self.buffer.line_count(),
            last_char_offset - last_line_char_offset,
            self.buffer.last_token(),
        )
    }

    #[inline]
    fn emit_error(&mut self, error: ErrorType) {
        self.errors.push(self.prep_error_info_at_cur_offset(error));
    }

    #[inline]
    fn emit_error_info(&mut self, error_info: ErrorInfo) {
        self.errors.push(error_info);
    }

    /// Main lexing loop, responsible for driving the lexing forwards
    /// as well as finalizing it with a mandatroy EOF roken.
    fn lex(mut self) -> (WorkTokenizedBuffer, Box<[ErrorInfo]>) {
        while let Some(next_char) = self.cursor.peek() {
            self.lex_token(next_char);

            #[cfg(debug_assertions)]
            {
                let new_state = (self.cursor.remaining_len(), self.mode_stack.clone());
                debug_assert!(self.last_state != new_state, "Infinite loop detected");
                self.last_state = new_state;
            }
        }

        self.finalize_lexing();

        (self.buffer, self.errors.into_boxed_slice())
    }

    /// This function gracefully unwinds the stack, emitting any ephemeral
    /// tokens and errors if necessary, and adds the mandatory EOF token.
    fn finalize_lexing(&mut self) {
        // Iterate over the mode stack in reverse and unwind it
        while let Some(mode) = self.mode_stack.pop() {
            // Release the shared reference to the mode by cloning it
            // let mode = mode.clone();

            self.start_token();

            match mode {
                LexerMode::ExpectSymbol(expected_char, tok_type, tok_channel) => {
                    // If we were expecting a token - call lexing that will effectively
                    // emit an error and the token
                    self.lex_expected_token(None, expected_char, tok_type, tok_channel);
                }
                LexerMode::ExpectSemiOrEOF | LexerMode::MacroDo => {
                    // If we were expecting a semicolon or EOF - emit a virtual semicolon for parser convenience.
                    // The trailing %DO behaves the same way
                    self.start_token();
                    self.emit_token(TokenChannel::DEFAULT, TokenType::SEMI, Payload::None);
                }
                LexerMode::Default
                | LexerMode::WsOrCStyleCommentOnly
                | LexerMode::MaybeMacroCallArgs
                | LexerMode::MacroSemiTerminatedTextExpr => {
                    // These are optional modes, meaning there can be no actual token lexed in it
                    // so we can safely pop them
                }
                LexerMode::MacroStrQuotedExpr { pnl, .. }
                | LexerMode::MacroCallArgOrValue { pnl }
                | LexerMode::MacroCallValue { pnl, .. }
                | LexerMode::MacroEval { pnl, .. } => {
                    // If the parens nesting level is > 0, we should emit the missing number of
                    // closing parens to balance it out

                    if pnl > 0 {
                        self.emit_error(ErrorType::MissingExpected(
                            "Missing closing parenthesis to balance the expression",
                        ));

                        for _ in 0..pnl {
                            self.emit_token(
                                TokenChannel::DEFAULT,
                                TokenType::RPAREN,
                                Payload::None,
                            );
                        }
                    }
                }
                LexerMode::StringExpr { .. } => {
                    // This may happen if we have unbalanced `"` or `'` as the last character
                    self.handle_unterminated_str_expr(Payload::None);
                }
                LexerMode::MacroVarNameExpr(_, err) => {
                    // This may happen if we have %let without a variable name in the end or similar
                    if let Some(err) = err {
                        self.emit_error(err);
                    }
                }
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
    }

    /// Main dispatcher of lexing mode to lexer function
    fn lex_token(&mut self, next_char: char) {
        match self.mode() {
            LexerMode::WsOrCStyleCommentOnly => match next_char {
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
                if next_char == '(' {
                    // Add the LPAREN token
                    self.start_token();
                    self.cursor.advance();

                    self.emit_token(TokenChannel::DEFAULT, TokenType::LPAREN, Payload::None);

                    // Clear the checkpoint
                    self.clear_checkpoint();

                    // Pop the MaybeMacroCallArgs mode
                    self.pop_mode();

                    // Populate the remaining expected states for the macro call
                    self.push_mode(LexerMode::ExpectSymbol(
                        ')',
                        TokenType::RPAREN,
                        TokenChannel::DEFAULT,
                    ));
                    // The handler fo arguments will push the mode for the comma, etc.
                    self.push_mode(LexerMode::MacroCallArgOrValue { pnl: 0 });
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
            LexerMode::ExpectSymbol(expected_char, tok_type, tok_channel) => {
                self.start_token();
                self.lex_expected_token(Some(next_char), expected_char, tok_type, tok_channel);
            }
            LexerMode::ExpectSemiOrEOF => {
                self.start_token();
                // In reality we can only have ; here. On EOF lexer will not call this
                // function, but will finalize the lexing and this mode is handled there
                if next_char == ';' {
                    // Consume the expected content
                    self.cursor.advance();
                } else {
                    // Not a EOF and not a ';' => expected token not found.
                    // Emit an error which will point at previous token.
                    // The token itself is emitted below
                    self.emit_error(ErrorType::MissingExpected("';' or end of file"));
                }

                self.emit_token(TokenChannel::DEFAULT, TokenType::SEMI, Payload::None);
                self.pop_mode();
            }
            LexerMode::Default => self.dispatch_mode_default(next_char),
            LexerMode::StringExpr { allow_stat } => {
                self.dispatch_mode_str_expr(next_char, allow_stat);
            }
            LexerMode::MacroEval {
                macro_eval_flags,
                pnl,
            } => {
                self.dispatch_mode_macro_eval(next_char, macro_eval_flags, pnl);
            }
            LexerMode::MacroStrQuotedExpr { mask_macro, pnl } => {
                self.dispatch_macro_str_quoted_expr(next_char, mask_macro, pnl);
            }
            LexerMode::MacroCallArgOrValue { pnl } => {
                self.dispatch_macro_call_arg_or_value(next_char, pnl, true, true);
            }
            LexerMode::MacroCallValue {
                populate_next_arg_stack,
                pnl,
            } => {
                self.dispatch_macro_call_arg_or_value(
                    next_char,
                    pnl,
                    false,
                    populate_next_arg_stack,
                );
            }
            LexerMode::MacroDo => {
                self.dispatch_macro_do(next_char);
            }
            LexerMode::MacroVarNameExpr(found_name, err) => {
                self.dispatch_macro_name_expr(next_char, !found_name, err);
            }
            LexerMode::MacroSemiTerminatedTextExpr => {
                self.dispatch_macro_semi_term_text_expr(next_char);
            }
        }
    }

    fn lex_expected_token(
        &mut self,
        next_char: Option<char>,
        expected_char: char,
        tok_type: TokenType,
        tok_channel: TokenChannel,
    ) {
        debug_assert!(
            self.mode() == LexerMode::ExpectSymbol(expected_char, tok_type, tok_channel)
                || self.cursor.peek().is_none() // EOF
        );
        debug_assert!(!expected_char.is_ascii_alphabetic());

        if next_char.map_or(true, |c| c != expected_char) {
            // Expected token not found. Emit an error which will point at previous token
            // The token itself is emitted below
            self.emit_error(ErrorType::MissingExpectedChar(expected_char));
        } else {
            // Consume the expected content
            self.cursor.advance();
        }

        self.emit_token(tok_channel, tok_type, Payload::None);
        self.pop_mode();
    }

    fn dispatch_mode_default(&mut self, next_char: char) {
        debug_assert_eq!(self.mode(), LexerMode::Default);

        self.start_token();

        // Dispatch the "big" categories
        match next_char {
            c if c.is_whitespace() => {
                // Lex whitespace
                self.lex_ws();
            }
            '\'' => self.lex_single_quoted_str(),
            '"' => self.lex_string_expression_start(true),
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
                match self.cursor.peek_next() {
                    '*' => {
                        self.lex_macro_comment();
                    }
                    c if is_valid_sas_name_start(c) => {
                        self.lex_macro_identifier();
                    }
                    _ => {
                        // Not a macro, just a percent
                        self.cursor.advance();
                        self.emit_token(TokenChannel::DEFAULT, TokenType::PERCENT, Payload::None);
                    }
                }
            }
            '0'..='9' => {
                // Numeric literal
                self.lex_numeric_literal(false);
            }
            c if is_valid_sas_name_start(c) => {
                self.lex_identifier();
            }
            _ => {
                // Something else must be a symbol or some unknown character
                self.lex_symbols(next_char);
            }
        }
    }

    fn dispatch_mode_macro_eval(
        &mut self,
        next_char: char,
        macro_eval_flags: MacroEvalExprFlags,
        parens_nesting_level: u32,
    ) {
        debug_assert!(matches!(
           self.mode(),
           LexerMode::MacroEval { macro_eval_flags: f, pnl }
                if f == macro_eval_flags && pnl == parens_nesting_level
        ));

        self.start_token();

        // Dispatch the "big" categories
        match next_char {
            '\'' => {
                self.lex_single_quoted_str();
            }
            '"' => {
                self.lex_string_expression_start(false);
            }
            '/' => {
                if self.cursor.peek_next() == '*' {
                    self.lex_cstyle_comment();
                } else {
                    self.cursor.advance();
                    self.emit_token(TokenChannel::DEFAULT, TokenType::FSLASH, Payload::None);
                    // WS after operator or delimiter is insignificant
                    self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                }
            }
            '&' => {
                if !self.lex_macro_var_expr() {
                    // Regular AMP sequence, consume as token
                    self.cursor.eat_while(|c| c == '&');
                    self.emit_token(TokenChannel::DEFAULT, TokenType::AMP, Payload::None);
                    // WS after operator or delimiter is insignificant
                    self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                }
            }
            '%' => {
                match self.lex_macro_call(true, macro_eval_flags.terminate_on_stat()) {
                    MacroKwType::MacroStat => {
                        // Hit a following macro statement => pop mode and exit.
                        // Error has already been emitted by the `lex_macro_call`
                        // if macro stat is not allowed
                        self.maybe_emit_empty_macro_string_in_eval(None);
                        self.pop_mode();

                        // We need to handle one special case - an expression after
                        // %to and before %by - we'll have expect semi on the stack
                        // and we need to pop it
                        if macro_eval_flags.terminate_on_stat()
                            && macro_eval_flags.terminate_on_semi()
                            && self.mode().is_expect_semi_or_eof()
                        {
                            self.pop_mode();
                        }
                    }
                    MacroKwType::None => {
                        // Either a string % or the quoted op.
                        // Whatever is the case, we can advance
                        self.cursor.advance();

                        let second_next = self.cursor.peek().unwrap_or(' ');

                        if is_macro_eval_quotable_op(second_next) {
                            self.lex_macro_eval_operator(second_next);
                        } else {
                            // Just a percent, continue lexing the string
                            // We could have not consumed it and let the
                            // string lexing handle it, but this way we
                            // we avoid one extra check
                            self.lex_macro_string_in_macro_eval_context(macro_eval_flags);
                        }
                    }
                    MacroKwType::MacroCallOrLabel => {}
                }
            }
            ')' if parens_nesting_level == 0 => {
                // Found the end of the expression, pop the mode and return
                self.maybe_emit_empty_macro_string_in_eval(None);
                self.pop_mode();
            }
            ',' if macro_eval_flags.terminate_on_comma() => {
                // Found the end of the expression, pop the mode and return
                self.maybe_emit_empty_macro_string_in_eval(None);
                self.pop_mode();

                // Now push modes for the next argument
                if macro_eval_flags.followed_by_expr() {
                    self.push_mode(LexerMode::MacroEval {
                        macro_eval_flags: MacroEvalExprFlags::new(
                            MacroEvalNumericMode::Integer,
                            MacroEvalNextArgumentMode::None,
                            false,
                            false,
                        ),
                        pnl: 0,
                    });
                } else {
                    self.push_mode(LexerMode::MacroCallValue {
                        populate_next_arg_stack: true,
                        pnl: 0,
                    });
                }
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::COMMA, Payload::None);
                // Leading insiginificant WS before the next argument
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            ';' if macro_eval_flags.terminate_on_semi() => {
                // Found the end of the expression, pop the mode and return
                self.maybe_emit_empty_macro_string_in_eval(None);
                self.pop_mode();
            }
            c => {
                if !self.lex_macro_eval_operator(c) {
                    // Not an operator => must be a macro string
                    self.lex_macro_string_in_macro_eval_context(macro_eval_flags);
                }
            }
        }
        // empty macro string detection
    }

    fn lex_macro_eval_operator(&mut self, next_char: char) -> bool {
        debug_assert!(matches!(self.mode(), LexerMode::MacroEval { .. }));

        // Helper function to emit the token and update the mode if needed
        let update_parens_nesting = |lexer: &mut Lexer, increment: bool| {
            if let Some(LexerMode::MacroEval {
                pnl: parens_nesting_level,
                ..
            }) = lexer.mode_stack.last_mut()
            {
                if increment {
                    *parens_nesting_level = parens_nesting_level.wrapping_add_signed(1);
                } else {
                    // If our logic is correct, it should be impossible for the symbol
                    // lex function to decrement the parens nesting level below 0
                    debug_assert!(*parens_nesting_level > 0);
                    *parens_nesting_level = parens_nesting_level.wrapping_add_signed(-1);
                }
            };
        };

        let (tok_type, extra_advance_by) = match next_char {
            // '/' is not here, becase it is handled in the caller with c-style comment
            // same for `&`
            '*' => {
                if self.cursor.peek_next() == '*' {
                    (TokenType::STAR2, 1)
                } else {
                    (TokenType::STAR, 0)
                }
            }
            '(' => {
                update_parens_nesting(self, true);
                (TokenType::LPAREN, 0)
            }
            ')' => {
                update_parens_nesting(self, false);
                (TokenType::RPAREN, 0)
            }
            '|' => (TokenType::PIPE, 0),
            '¬' | '^' | '~' => {
                if self.cursor.peek_next() == '=' {
                    (TokenType::NE, 1)
                } else {
                    (TokenType::NOT, 0)
                }
            }
            '+' => (TokenType::PLUS, 0),
            '-' => (TokenType::MINUS, 0),
            '<' => {
                if self.cursor.peek_next() == '=' {
                    (TokenType::LE, 1)
                } else {
                    (TokenType::LT, 0)
                }
            }
            '>' => {
                if self.cursor.peek_next() == '=' {
                    (TokenType::GE, 1)
                } else {
                    (TokenType::GT, 0)
                }
            }
            '=' => (TokenType::ASSIGN, 0),
            '#' => (TokenType::HASH, 0),
            'e' | 'n' | 'l' | 'g' | 'a' | 'o' | 'i' | 'E' | 'N' | 'L' | 'G' | 'A' | 'O' | 'I' => {
                if let (Some(tok_type), extra_advance_by) =
                    is_macro_eval_mnemonic(self.cursor.chars())
                {
                    (tok_type, extra_advance_by)
                } else {
                    // not a mnemonic, return
                    return false;
                }
            }
            _ => return false,
        };

        self.maybe_emit_empty_macro_string_in_eval(Some(tok_type));

        self.cursor.advance_by(1 + extra_advance_by);
        self.emit_token(TokenChannel::DEFAULT, tok_type, Payload::None);
        // WS after operator or delimiter is insignificant
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        true
    }

    #[allow(clippy::too_many_lines)]
    fn lex_macro_string_in_macro_eval_context(&mut self, macro_eval_flags: MacroEvalExprFlags) {
        debug_assert!(matches!(self.mode(), LexerMode::MacroEval { .. }));

        // In eval context, the leading/trailing whitespace may or may not be
        // part of a macro string depending on the preceeding/following characters.
        // Before and after operators and delimiter (comma, semicolon) it is not
        // part of the macro string. But in-between of the operands it is.
        //
        // E.g. in a case like this: `%eval(1   + 2)`.
        //                      insignificant^^^
        // While it is significant here: `%eval(pre   &mv=&other)`.
        //                              significant^^^
        // `pre   ` should be a single macro string token on default channel.
        //
        // The WS after operator or delimiter is handled as hidden by pushing the WS mode.
        // However, if we had, say, a '' or "" string literal, after lexing it
        // we'll be back here and can start with whitespace. Or we can be lexing
        // the `pre` in the example above and encounter whitespace before the `&`.
        //
        // Thus when we encounter WS, we "mark" it as a potential start of
        // a WS token and depending on how we end the current token, either
        // emit a hidden WS token or make it part of the macro string.

        // The other aspect is that SAS will recognize numerics (integers for
        // all contexts except `%sysevals` where it will also recognize floats).
        // But it is very clever. `%eval(1+1)` here both `1` and `1` are recognized
        // as integers. But in `%eval(1.0+1)` the `1.0` is lexed as string because
        // it is not a valid integer. Or in `%eval(1 2=12)` the lhs will be a macro
        // string `1 2`, not integers `1` and `2`.
        //
        // It is super hard to implement this correctly, e.g. in `%eval(1/*comment*/2)`
        // thus we do our best, but may not be 100% correct for every edge case.

        let mut ws_mark: Option<(ByteOffset, CharOffset, LineIdx)> = None;
        let mut try_lexing_numeric = true;
        let mut may_preceed_mnemonic = true;

        while let Some(c) = self.cursor.peek() {
            // We read until we've found something that terminates
            // the macro string portion. The type of the terminator is
            // important as it will influence how we handle trailing whitespace
            // and if we'll try lexing integer/floar literals
            match c {
                '*' | '(' | ')' | '|' | '¬' | '^' | '~' | '+' | '-' | '<' | '>' | '=' | '#' => {
                    // Symbol operators and parens which are always terminators
                    break;
                }
                '\'' | '"' => {
                    // If string literal follows, we should not try lexing
                    // whatever we got so far as a numeric literal. But ws
                    // before the string literal should be part of the macro string
                    try_lexing_numeric = false;
                    ws_mark = None;
                    break;
                }
                '/' => {
                    // Both division op and start of a comment signify the end
                    // of at least a macro string portion. Without doing
                    // insane unlimited lookahead, we can't know whether it is
                    // followed by an operator (and hence the preceding WS is
                    // insignificant) or a continuation of a string (and
                    // the WS is significant). We just assume the latter and
                    // accept this is as a known limitation. Worst case
                    // the user of the parsed code will have to call int() on
                    // the macro string to get the numeric value.
                    if self.cursor.peek_next() == '*' {
                        try_lexing_numeric = false;
                        ws_mark = None;
                    }
                    break;
                }
                ';' if macro_eval_flags.terminate_on_semi() => {
                    // Found the end of the expression, break
                    break;
                }
                ',' if macro_eval_flags.terminate_on_comma() => {
                    // Found the end of the expression, break
                    break;
                }
                '&' => {
                    // In macro eval amp always means an end to a macro string.
                    // It is either a following operator or a macro var expression.
                    // But we we need to know if it is operator or macro var expr
                    // to properly handle the trailing WS.
                    if is_macro_amp(self.cursor.chars()).0 {
                        // Not an operator, but a macro var expression
                        // Hence preceeding WS is significant and we should not
                        // try lexing the preceeding as a numeric literal
                        try_lexing_numeric = false;
                        ws_mark = None;
                    }
                    // Stop consuming char and break for standard logic.
                    break;
                }
                '%' => {
                    if is_macro_percent(self.cursor.peek_next(), true) {
                        // Hit a macro call or statment in/after the string expression
                        // Stop consuming char and break for standard logic.

                        // NOTE: this is super expensive look-ahead. If we push down
                        // trailing WS trimming to the parser, it can be avoided.
                        if !is_macro_stat(self.cursor.chars()) {
                            // Not a delimiting statment, but a macro call
                            // Hence preceeding WS is significant and we should not
                            // try lexing the preceeding as a numeric literal
                            try_lexing_numeric = false;
                            ws_mark = None;
                        }
                        break;
                    }

                    // Just percent in the text, consume and continue
                    self.cursor.advance();

                    // Also reset the ws mark (not in ws anymore) and
                    // it can't be part of a numeric literal now
                    try_lexing_numeric = false;
                    ws_mark = None;

                    // And allow an immediate mnemonic
                    may_preceed_mnemonic = true;
                }
                '\n' => {
                    ws_mark = ws_mark.or_else(|| Some(self.mark_token_start()));
                    self.cursor.advance();
                    self.add_line();
                }
                c if c.is_whitespace() => {
                    ws_mark = ws_mark.or_else(|| Some(self.mark_token_start()));
                    self.cursor.advance();
                }
                // Mnemonics should only be recognized if they have WS before them or
                // some character that is allowed before a mnemonic
                'e' | 'n' | 'l' | 'g' | 'a' | 'o' | 'i' | 'E' | 'N' | 'L' | 'G' | 'A' | 'O'
                | 'I'
                    if ws_mark.is_some() || may_preceed_mnemonic =>
                {
                    if let (Some(_), _) = is_macro_eval_mnemonic(self.cursor.chars()) {
                        // Found a mnemonic, break
                        break;
                    }
                    // String continues. Reset the ws mark & integer literal, advance
                    try_lexing_numeric = false;
                    ws_mark = None;
                    self.cursor.advance();
                }
                _ => {
                    // Not a terminator, just a regular character in the string
                    // consume and continue lexing the string
                    self.cursor.advance();

                    // If we had a ws_mark, means string contains WS => can't be a number
                    if ws_mark.is_some() {
                        try_lexing_numeric = false;
                        ws_mark = None;
                    }

                    // Calculate if we can have a mnemonic after this character
                    may_preceed_mnemonic = !is_xid_continue(c);
                }
            }
        }

        // Few. We got here. We can have a few cases:
        // 1. No mark => just a seuqeunce characters, all macro string. We may try lexing it as a number
        //      if flag is set
        // 2. Mark, and nothing else, just WS! => emit hidden WS token
        // 3. A sequence of characters, then a mark which starts at the beginning of WS =>
        //      do both - try lexing the sequence as a number and emit the WS token

        let macro_string = self
            .source
            .get(
                self.cur_token_byte_offset.into()
                    ..ws_mark
                        .map_or_else(|| self.cur_byte_offset(), |m| m.0)
                        .into(),
            )
            .unwrap_or_else(|| {
                // This is an internal error, we should always have a token text
                self.emit_error(ErrorType::InternalError("No token text"));
                ""
            });

        if !macro_string.is_empty() {
            if try_lexing_numeric {
                // Try parsing.
                // Safety: we've checked above that the string is not empty
                #[allow(clippy::indexing_slicing)]
                if [b'x', b'X'].contains(&macro_string.as_bytes()[macro_string.len() - 1])
                    && macro_string.as_bytes()[0].is_ascii_digit()
                {
                    // Try hex
                    let ((tok_type, payload), error) = parse_numeric_hex_str(
                        macro_string.get(..macro_string.len() - 1).unwrap_or(""),
                    );

                    if error.is_none() {
                        self.emit_token(TokenChannel::DEFAULT, tok_type, payload);
                    } else {
                        // Emit as macro string
                        self.emit_token(
                            TokenChannel::DEFAULT,
                            TokenType::MacroString,
                            Payload::None,
                        );
                    };
                } else {
                    // Try integer/float depending on the flag
                    let ((tok_type, payload), error) = parse_numeric(macro_string);

                    if error.is_none() {
                        match tok_type {
                            TokenType::IntegerLiteral | TokenType::IntegerDotLiteral => {
                                // We do not care for the DotIntegerLiteral version here
                                // as format may not be in this context
                                self.emit_token(
                                    TokenChannel::DEFAULT,
                                    TokenType::IntegerLiteral,
                                    payload,
                                );
                            }
                            TokenType::FloatLiteral if macro_eval_flags.float_mode() => {
                                self.emit_token(TokenChannel::DEFAULT, tok_type, payload);
                            }
                            TokenType::FloatLiteral => {
                                // Emit as macro string
                                self.emit_token(
                                    TokenChannel::DEFAULT,
                                    TokenType::MacroString,
                                    Payload::None,
                                );
                            }
                            _ => {
                                // Not a number, emit as macro string
                                self.emit_token(
                                    TokenChannel::DEFAULT,
                                    TokenType::MacroString,
                                    Payload::None,
                                );
                            }
                        }
                    } else {
                        // Emit as macro string
                        self.emit_token(
                            TokenChannel::DEFAULT,
                            TokenType::MacroString,
                            Payload::None,
                        );
                    };
                }
            } else {
                // Not trying to lex as a number, emit as macro string
                self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);
            }
        }

        // Now handle the trailing WS
        if let Some(ws_mark) = ws_mark {
            self.emit_token_at_mark(TokenChannel::HIDDEN, TokenType::WS, Payload::None, ws_mark);
        }
    }

    /// In macro logical expressions all ops except IN/# allow empty lhs/rhs.
    /// We emit special token to make parser's life easier.
    ///
    /// This will handle all lhs and rhs cases, or consecutive ops like in the following `lhs = =`
    /// this is an empty macro string in both rhs position for left associative rules ---------^
    ///
    /// For IN docs say:
    /// > ** When you use the IN operator, both operands must contain a value.
    /// > If the operand contains a null value, an error is generated.
    ///
    /// So we also emit an error mimicking SAS
    ///
    /// We should check either before a logical operator (e.g. ` ne some`)
    ///                                                    here ^
    /// before closing parens that are not the end of exprt  (e.g. `(some ne) or other`)
    ///                                                               here ^
    /// or at any genuine end of expression, which may be comma, semi, statement keyword etc.
    /// In the latter case, `next_expr_tok_type` should be None
    fn maybe_emit_empty_macro_string_in_eval(&mut self, next_expr_tok_type: Option<TokenType>) {
        let expr_end = next_expr_tok_type.map_or(true, |tok_type| {
            matches!(
                tok_type,
                // right paren ends subexpression. AND and OR are lower precedence
                // so they also "end" the current logical subexpression
                TokenType::RPAREN | TokenType::KwAND | TokenType::KwOR
            )
        });

        let op_follows = next_expr_tok_type.is_some_and(is_macro_eval_logical_op);

        if expr_end || op_follows {
            if let Some(&TokenInfo {
                token_type: prev_tok_type,
                ..
            }) = self.buffer.last_token_info_on_default_channel()
            {
                // is we have a preceedning logical operator, we should emit empty string
                // no matter the next token type
                if is_macro_eval_logical_op(prev_tok_type) {
                    // TODO: emit SAS error on missing operand for IN/#
                    self.emit_empty_macro_string_token();
                } else if matches!(
                    prev_tok_type,
                    // These are all possible "starts", tokens preceeding
                    // the start of an evaluated logical macro subexpression.
                    // See: https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/mcrolref/n1alyfc9f4qrten10sd5qz5e1w5q.htm#p17exjo2c9f5e3n19jqgng0ho42u
                    TokenType::LPAREN
                        | TokenType::ASSIGN
                        | TokenType::KwmIf
                        | TokenType::KwmTo
                        | TokenType::KwmBy
                        | TokenType::COMMA
                        | TokenType::KwAND
                        | TokenType::KwOR
                ) {
                    // if we are at the start of expression, or start of parenthesized subexpression
                    // the logic is more involved since if no operand is given in reality SAS will emit
                    // an error about character operand found in eval context.
                    // It doesn't allow bare empty condition.

                    if expr_end {
                        self.emit_error(ErrorType::SASSessionUnrecoverableError(
                            "ERROR: A character operand was found in the %EVAL function or %IF \
                            condition where a numeric operand is required.",
                        ));
                    }

                    self.emit_empty_macro_string_token();
                }
            }
        }
    }

    fn dispatch_macro_name_expr(
        &mut self,
        next_char: char,
        first_token: bool,
        err: Option<ErrorType>,
    ) {
        debug_assert!(
            matches!(self.mode(), LexerMode::MacroVarNameExpr(f, e) if f != first_token && e == err)
        );

        self.start_token();

        let pop_mode_and_check = |lexer: &mut Lexer| {
            if first_token {
                // This is straight from what SAS emits
                if let Some(err) = err {
                    lexer.emit_error(err);
                }
            }

            lexer.pop_mode();
        };

        // Helper to update the mode indicating that we have found at least one non-hidden token
        // First we need to store the index of the mode when we started lexing this,
        // because nested calls can add more modes to the stack, but what we
        // want to update is the mode at the start of this call
        let start_mode_index = self.mode_stack.len() - 1;

        let update_mode = |lexer: &mut Lexer| {
            if let Some(LexerMode::MacroVarNameExpr(found_name, _)) =
                lexer.mode_stack.get_mut(start_mode_index)
            {
                *found_name = true;
            } else {
                lexer.emit_error(ErrorType::InternalError("Unexpected mode stack"));
            };
        };

        // Dispatch the "big" categories

        match next_char {
            '/' if self.cursor.peek_next() == '*' => {
                self.lex_cstyle_comment();
            }
            '&' => {
                if !self.lex_macro_var_expr() {
                    // Not a macro var. pop mode without consuming the character
                    pop_mode_and_check(self);
                    return;
                }

                if first_token {
                    update_mode(self);
                };
            }
            '%' => {
                match self.lex_macro_call(false, false) {
                    MacroKwType::MacroStat | MacroKwType::None => {
                        // Hit a following macro statement or just a percent => pop mode and exit.
                        // Error for statement case has already been emitted by `lex_macro_call`
                        pop_mode_and_check(self);
                    }
                    MacroKwType::MacroCallOrLabel => {
                        if first_token {
                            update_mode(self);
                        };
                    }
                }
            }
            c if is_valid_sas_name_start(c) || (!first_token && is_xid_continue(c)) => {
                // A macro string in place of macro identifier
                // Consume as identifier, no reserved words here,
                // so we do not need the full lex_identifier logic
                self.cursor.eat_while(is_xid_continue);

                // Add token, but do not pop the mode, as we may have a full macro text expression
                // that generates an identifier
                self.emit_token(
                    TokenChannel::DEFAULT,
                    // True identifier is only possible if this is the first (and only) token.
                    if first_token {
                        TokenType::Identifier
                    } else {
                        TokenType::MacroString
                    },
                    Payload::None,
                );

                if first_token {
                    update_mode(self);
                };
            }
            _ => {
                // Something else. pop mode without consuming the character
                pop_mode_and_check(self);
            }
        }
    }

    fn dispatch_macro_semi_term_text_expr(&mut self, next_char: char) {
        debug_assert!(matches!(
            self.mode(),
            LexerMode::MacroSemiTerminatedTextExpr
        ));

        self.start_token();

        // Dispatch the "big" categories
        match next_char {
            '\'' => self.lex_single_quoted_str(),
            '"' => self.lex_string_expression_start(false),
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
                match self.lex_macro_call(true, false) {
                    MacroKwType::MacroStat => {
                        // Hit a following macro statement => pop mode and exit.
                        // Error has already been emitted by the `lex_macro_call`
                        self.pop_mode();
                    }
                    MacroKwType::None => {
                        // Just a percent, consume and continue lexing the string
                        // We could have not consumed it and let the
                        // string lexing handle it, but this way we
                        // we avoid one extra check
                        self.cursor.advance();
                        self.lex_macro_string_unrestricted();
                    }
                    MacroKwType::MacroCallOrLabel => {}
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
        debug_assert!(matches!(
            self.mode(),
            LexerMode::MacroSemiTerminatedTextExpr
        ));

        while let Some(c) = self.cursor.peek() {
            match c {
                '\'' | '"' => {
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

        // EOF
        // Emit the text token and return
        self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);
    }

    fn dispatch_macro_call_arg_or_value(
        &mut self,
        next_char: char,
        parens_nesting_level: u32,
        terminate_on_assign: bool,
        populate_next_arg_stack: bool,
    ) {
        debug_assert!(matches!(
            self.mode(),
            LexerMode::MacroCallArgOrValue { pnl: l }
                | LexerMode::MacroCallValue { pnl: l,.. } if l == parens_nesting_level
        ));

        self.start_token();

        // Dispatch the "big" categories
        match next_char {
            '\'' => self.lex_single_quoted_str(),
            '"' => self.lex_string_expression_start(false),
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
                    self.lex_macro_string_in_macro_call(
                        parens_nesting_level,
                        terminate_on_assign,
                        populate_next_arg_stack,
                    );
                }
            }
            '&' => {
                if !self.lex_macro_var_expr() {
                    // Not a macro var, just a sequence of ampersands
                    // consume the sequence and continue lexing the string
                    self.cursor.eat_while(|c| c == '&');
                    self.lex_macro_string_in_macro_call(
                        parens_nesting_level,
                        terminate_on_assign,
                        populate_next_arg_stack,
                    );
                }
            }
            '%' => {
                match self.lex_macro_call(true, false) {
                    MacroKwType::MacroStat => {
                        // Hit a following macro statement => pop mode and exit.
                        // Error has already been emitted by the `lex_macro_call`
                        self.pop_mode();
                    }
                    MacroKwType::None => {
                        // Just a percent, consume and continue lexing the string
                        // We could have not consumed it and let the
                        // string lexing handle it, but this way we
                        // we avoid one extra check
                        self.cursor.advance();
                        self.lex_macro_string_in_macro_call(
                            parens_nesting_level,
                            terminate_on_assign,
                            populate_next_arg_stack,
                        );
                    }
                    MacroKwType::MacroCallOrLabel => {}
                }
            }
            '\n' => {
                // Special case to catch newline
                // We could have not consumed it and let the
                // string lexing handle it, but this way we
                // we avoid one extra check
                self.cursor.advance();
                self.add_line();
                self.lex_macro_string_in_macro_call(
                    parens_nesting_level,
                    terminate_on_assign,
                    populate_next_arg_stack,
                );
            }
            ',' if parens_nesting_level == 0 => {
                // Found the terminator, pop the mode and push new modes
                // to expect stuff then return
                self.pop_mode();

                if populate_next_arg_stack {
                    self.push_mode(LexerMode::MacroCallArgOrValue { pnl: 0 });
                    // Leading insiginificant WS before the argument
                    self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                    self.push_mode(LexerMode::ExpectSymbol(
                        ',',
                        TokenType::COMMA,
                        TokenChannel::DEFAULT,
                    ));
                }
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
                self.push_mode(LexerMode::MacroCallValue {
                    populate_next_arg_stack: true,
                    pnl: 0,
                });
                // Leading insiginificant WS before the argument
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                self.push_mode(LexerMode::ExpectSymbol(
                    '=',
                    TokenType::ASSIGN,
                    TokenChannel::DEFAULT,
                ));
            }
            _ => {
                // Not a terminator, just a regular character in the string
                // Do NOT consume - macro string tracks parens, and this
                // maybe a paren. Continue lexing the string
                self.lex_macro_string_in_macro_call(
                    parens_nesting_level,
                    terminate_on_assign,
                    populate_next_arg_stack,
                );
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn lex_macro_string_in_macro_call(
        &mut self,
        parens_nesting_level: u32,
        terminate_on_assign: bool,
        populate_next_arg_stack: bool,
    ) {
        debug_assert!(
            matches!(
                self.mode(),
                LexerMode::MacroCallValue{ populate_next_arg_stack: pnas, .. }
                    if pnas == populate_next_arg_stack
            ) && !terminate_on_assign
                || matches!(self.mode(), LexerMode::MacroCallArgOrValue { .. })
                    && terminate_on_assign
        );

        // Helper function to emit the token and update the mode if needed
        let emit_token_update_nesting = |lexer: &mut Lexer, local_parens_nesting: i32| {
            lexer.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);

            // If the local parens nesting has been affected, update the mode
            if local_parens_nesting != 0 {
                // If our logic is correct, it should be impossible for a current
                // string section to push the nesting level below 0
                // as at the moment of reaching 0, we should have popped the mode
                // and exited the lexing of the string
                debug_assert!(
                    i64::from(parens_nesting_level) + i64::from(local_parens_nesting) >= 0
                );

                if let Some(m) = lexer.mode_stack.last_mut() {
                    match m {
                        LexerMode::MacroCallArgOrValue { pnl }
                        | LexerMode::MacroCallValue { pnl, .. } => {
                            *pnl = pnl.wrapping_add_signed(local_parens_nesting);
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
        let mut local_parens_nesting = 0i32;

        while let Some(c) = self.cursor.peek() {
            match c {
                '\'' | '"' => {
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
                ')' if parens_nesting_level.wrapping_add_signed(local_parens_nesting) != 0 => {
                    // Decrease the local parens nesting level
                    local_parens_nesting -= 1;
                    self.cursor.advance();
                }
                ')' if parens_nesting_level.wrapping_add_signed(local_parens_nesting) == 0 => {
                    // Found the terminator of the entire macro call arguments,
                    // emit the token, pop the mode and return
                    self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);
                    self.pop_mode();
                    return;
                }
                ',' if parens_nesting_level.wrapping_add_signed(local_parens_nesting) == 0 => {
                    // Found the terminator, pop the mode and push new modes
                    // to expect stuff, emit token then return
                    self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);
                    self.pop_mode();

                    if populate_next_arg_stack {
                        self.push_mode(LexerMode::MacroCallArgOrValue { pnl: 0 });
                        // Leading insiginificant WS before the argument
                        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                        self.push_mode(LexerMode::ExpectSymbol(
                            ',',
                            TokenType::COMMA,
                            TokenChannel::DEFAULT,
                        ));
                    }
                    return;
                }
                '=' if terminate_on_assign
                    && (parens_nesting_level.wrapping_add_signed(local_parens_nesting) == 0) =>
                {
                    // Found the terminator between argument name and value,
                    // pop the mode and push new modes to expect stuff then return
                    self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);
                    // Pop the arg/value mode and push the value mode
                    self.pop_mode();
                    self.push_mode(LexerMode::MacroCallValue {
                        populate_next_arg_stack: true,
                        pnl: 0,
                    });
                    // Leading insiginificant WS before the argument
                    self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                    self.push_mode(LexerMode::ExpectSymbol(
                        '=',
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
        // Reached EOF
        // Emit the text token and return
        emit_token_update_nesting(self, local_parens_nesting);
    }

    fn dispatch_macro_str_quoted_expr(
        &mut self,
        next_char: char,
        mask_macro: bool,
        parens_nesting_level: u32,
    ) {
        debug_assert!(matches!(
            self.mode(),
            LexerMode::MacroStrQuotedExpr { mask_macro: m, pnl: l }
                if m == mask_macro && l == parens_nesting_level
        ));

        self.start_token();

        // Dispatch the "big" categories
        match next_char {
            '\'' => self.lex_single_quoted_str(),
            '"' => self.lex_string_expression_start(false),
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
                    self.lex_macro_string_in_str_call(mask_macro, parens_nesting_level);
                }
            }
            '&' if !mask_macro => {
                if !self.lex_macro_var_expr() {
                    // Not a macro var, just a sequence of ampersands
                    // consume the sequence and continue lexing the string
                    self.cursor.eat_while(|c| c == '&');
                    self.lex_macro_string_in_str_call(mask_macro, parens_nesting_level);
                }
            }
            '%' if !mask_macro => {
                // Check if this is a quote char
                if matches!(self.cursor.peek_next(), '"' | '\'' | '%' | '(' | ')') {
                    self.lex_macro_string_in_str_call(mask_macro, parens_nesting_level);
                    return;
                }

                match self.lex_macro_call(true, false) {
                    MacroKwType::MacroStat => {
                        // Hit a following macro statement => pop mode and exit.
                        // Error has already been emitted by the `lex_macro_call`
                        self.pop_mode();
                    }
                    MacroKwType::None => {
                        // Just a percent, consume and continue lexing the string
                        // We could have not consumed it and let the
                        // string lexing handle it, but this way we
                        // we avoid one extra check
                        self.cursor.advance();
                        self.lex_macro_string_in_str_call(mask_macro, parens_nesting_level);
                    }
                    MacroKwType::MacroCallOrLabel => {}
                }
            }
            '\n' => {
                // Special case to catch newline
                // We could have not consumed it and let the
                // string lexing handle it, but this way we
                // we avoid one extra check
                self.cursor.advance();
                self.add_line();
                self.lex_macro_string_in_str_call(mask_macro, parens_nesting_level);
            }
            ')' if parens_nesting_level == 0 => {
                // Found the terminator, pop the mode and return
                self.pop_mode();
            }
            _ => {
                // Not a terminator, just a regular character in the string.
                // Do not consume in case it is an opening parens,
                // just continue lexing the string
                self.lex_macro_string_in_str_call(mask_macro, parens_nesting_level);
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn lex_macro_string_in_str_call(&mut self, mask_macro: bool, parens_nesting_level: u32) {
        debug_assert!(matches!(
            self.mode(),
            LexerMode::MacroStrQuotedExpr { mask_macro: m, pnl: l }
                if m == mask_macro && l == parens_nesting_level
        ));

        // Helper function to emit the token and update the mode if needed
        let emit_token_update_nesting =
            |lexer: &mut Lexer, local_parens_nesting: i32, payload: Payload| {
                lexer.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, payload);

                // If the local parens nesting has been affected, update the mode
                if local_parens_nesting != 0 {
                    // If our logic is correct, it should be impossible for a current
                    // string section to push the nesting level below 0
                    // as at the moment of reaching 0, we should have popped the mode
                    // and exited the lexing of the string
                    debug_assert!(
                        i64::from(parens_nesting_level) + i64::from(local_parens_nesting) >= 0
                    );

                    if let Some(m) = lexer.mode_stack.last_mut() {
                        match m {
                            LexerMode::MacroStrQuotedExpr {
                                pnl: parens_nesting_level,
                                ..
                            } => {
                                *parens_nesting_level =
                                    parens_nesting_level.wrapping_add_signed(local_parens_nesting);
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
        let mut local_parens_nesting = 0i32;

        // See `lex_single_quoted_str` for in-depth comments on the logic
        // of lexing possibly escaped text in a string expression
        let mut lit_start_idx = self.buffer.next_string_literal_start();
        let mut lit_end_idx = lit_start_idx;
        let mut last_lit_end_byte_offset = self.cur_byte_offset();

        while let Some(c) = self.cursor.peek() {
            match c {
                '\'' | '"' => {
                    // Reached the end of the section of a macro string
                    // Emit the text token and return

                    let payload = self.resolve_string_literal_payload(
                        lit_start_idx,
                        lit_end_idx,
                        last_lit_end_byte_offset,
                        None, // will use the current byte offset
                    );

                    emit_token_update_nesting(self, local_parens_nesting, payload);
                    return;
                }
                '/' if self.cursor.peek_next() == '*' => {
                    // Start of a comment in a macro string
                    // Emit the text token and return

                    let payload = self.resolve_string_literal_payload(
                        lit_start_idx,
                        lit_end_idx,
                        last_lit_end_byte_offset,
                        None, // will use the current byte offset
                    );

                    emit_token_update_nesting(self, local_parens_nesting, payload);
                    return;
                }
                '&' if !mask_macro => {
                    let (is_macro_amp, amp_count) = is_macro_amp(self.cursor.chars());

                    if is_macro_amp {
                        // Hit a macro var expr in the string expression => emit the text token
                        let payload = self.resolve_string_literal_payload(
                            lit_start_idx,
                            lit_end_idx,
                            last_lit_end_byte_offset,
                            None, // will use the current byte offset
                        );

                        emit_token_update_nesting(self, local_parens_nesting, payload);

                        return;
                    }

                    // Just amps in the text, consume and continue
                    self.cursor.advance_by(amp_count);
                }
                '%' => {
                    // Check if this is a quote char
                    if matches!(self.cursor.peek_next(), '"' | '\'' | '%' | '(' | ')') {
                        // Quoted char

                        // First, store the literal section before the escape percent
                        let (new_start, new_end) =
                            self.add_string_literal(last_lit_end_byte_offset, None);
                        lit_start_idx = min(lit_start_idx, new_start);
                        lit_end_idx = new_end;

                        // Now advance the cursor past the percent
                        self.cursor.advance();

                        // And update the last byte offset - this will ensure that the
                        // following escaped char will be incuded in the next literal section
                        last_lit_end_byte_offset = self.cur_byte_offset();

                        // Finally, advance the cursor past the quoted char
                        self.cursor.advance();
                        continue;
                    }

                    if !mask_macro && is_macro_percent(self.cursor.peek_next(), false) {
                        // Hit a macro call or statment in/after the string expression => emit the text token
                        let payload = self.resolve_string_literal_payload(
                            lit_start_idx,
                            lit_end_idx,
                            last_lit_end_byte_offset,
                            None, // will use the current byte offset
                        );

                        emit_token_update_nesting(self, local_parens_nesting, payload);

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
                ')' if parens_nesting_level.wrapping_add_signed(local_parens_nesting) != 0 => {
                    // Decrease the local parens nesting level
                    local_parens_nesting -= 1;
                    self.cursor.advance();
                }
                ')' if parens_nesting_level.wrapping_add_signed(local_parens_nesting) == 0 => {
                    // Found the terminator, emit the token, pop the mode and return
                    let payload = self.resolve_string_literal_payload(
                        lit_start_idx,
                        lit_end_idx,
                        last_lit_end_byte_offset,
                        None, // will use the current byte offset
                    );

                    self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, payload);
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
        // Reached EOF
        // Emit the text token and return

        let payload = self.resolve_string_literal_payload(
            lit_start_idx,
            lit_end_idx,
            last_lit_end_byte_offset,
            None, // will use the current byte offset
        );

        emit_token_update_nesting(self, local_parens_nesting, payload);
    }

    fn lex_ws(&mut self) {
        debug_assert!(self.cursor.peek().map_or(false, char::is_whitespace));

        loop {
            if let Some('\n') = self.cursor.advance() {
                self.add_line();
            }

            if !self.cursor.peek().map_or(false, char::is_whitespace) {
                break;
            }
        }
        self.emit_token(TokenChannel::HIDDEN, TokenType::WS, Payload::None);
    }

    fn lex_cstyle_comment(&mut self) {
        debug_assert_eq!(self.cursor.peek(), Some('/'));
        debug_assert_eq!(self.cursor.peek_next(), '*');

        // Eat the opening comment
        self.cursor.advance();
        self.cursor.advance();

        while let Some(c) = self.cursor.advance() {
            if c == '*' && self.cursor.peek() == Some('/') {
                self.cursor.advance();
                self.emit_token(
                    TokenChannel::COMMENT,
                    TokenType::CStyleComment,
                    Payload::None,
                );
                return;
            }

            if c == '\n' {
                self.add_line();
            }
        }
        // EOF reached without a closing comment
        // Emit an error token and return
        self.emit_token(
            TokenChannel::COMMENT,
            TokenType::CStyleComment,
            Payload::None,
        );
        self.emit_error(ErrorType::UnterminatedComment);
    }

    #[inline]
    fn lex_string_expression_start(&mut self, allow_stat: bool) {
        debug_assert_eq!(self.cursor.peek(), Some('"'));

        self.cursor.advance();
        self.emit_token(
            TokenChannel::DEFAULT,
            TokenType::StringExprStart,
            Payload::None,
        );
        self.push_mode(LexerMode::StringExpr { allow_stat });
    }

    fn lex_single_quoted_str(&mut self) {
        debug_assert_eq!(self.cursor.peek(), Some('\''));

        // Eat the opening single quote
        self.cursor.advance();

        // When lexing the string, if we encounter a double quote,
        // i.e. an escaped quote, we'll need to store the
        // unescaped string literal in the buufer, so we need
        // a number of vars to track the start, the end of the literal
        // in the buffer as well as the byte offset in the cursor as we go.
        // The common case is that we'll not need to store the literal
        // as we won't see any escaped quotes, hence they are not always
        // used in the end.

        // This var stores the true start of the literal in the buffer
        let mut lit_start_idx = self.buffer.next_string_literal_start();
        // This var stores the true end of the literal in the buffer.
        // We are adding multiple "sections" of the literal to the buffer
        // moving the end as we go.
        let mut lit_end_idx = lit_start_idx;
        // This var stores the byte offset of the end of the source range
        // for the last literal section added to the buffer. Basically this
        // allows "skipping" parts of the source text that are quote characters
        let mut last_lit_end_byte_offset = self.cur_byte_offset();

        // Now lex the string
        loop {
            if let Some(c) = self.cursor.advance() {
                match c {
                    '\'' => {
                        if self.cursor.peek() == Some('\'') {
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
        let tok_type = self.resolve_string_literal_ending();

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
    fn resolve_string_literal_ending(&mut self) -> TokenType {
        #[cfg(debug_assertions)]
        debug_assert!(['"', '\''].contains(&self.cursor.prev_char()));

        let tok_type = if let Some(c) = self.cursor.peek() {
            match c {
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
            }
        } else {
            TokenType::StringLiteral
        };

        // If we found a literal, advance the cursor
        if tok_type != TokenType::StringLiteral {
            self.cursor.advance();
        }

        tok_type
    }

    fn dispatch_mode_str_expr(&mut self, next_char: char, allow_stat: bool) {
        debug_assert!(
            matches!(self.mode(), LexerMode::StringExpr { allow_stat: s } if s == allow_stat)
        );

        self.start_token();

        match next_char {
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
                    last_tok_info.token_type == TokenType::StringExprStart
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
                let tok_type = if let Some(c) = self.cursor.peek() {
                    match c {
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
                    }
                } else {
                    TokenType::StringExprEnd
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
                match self.cursor.peek_next() {
                    c if is_valid_sas_name_start(c) => {
                        if allow_stat {
                            self.lex_macro_identifier();
                        } else {
                            // In macro context, nested statements cause open code recursion error
                            // but it seems like they are still half-handled. I.e. they are yanked
                            // from the string expression like in open code, but not actually
                            // executed. Still from lexing perspective, it is more robust to
                            // lex them.
                            // We have to first save the error info at the last byte offset,
                            // and only then lex the macro identifier to report at the correct position
                            let err_info =
                                self.prep_error_info_at_cur_offset(OPEN_CODE_RECURSION_ERR);

                            self.lex_macro_identifier();

                            if self
                                .buffer
                                .last_token_info()
                                .is_some_and(|tok_info| is_macro_stat_tok_type(tok_info.token_type))
                            {
                                self.emit_error_info(err_info);
                            }
                        }
                    }
                    _ => {
                        // just a percent. consume and continue
                        self.cursor.advance();
                        self.lex_str_expr_text();
                    }
                }
            }
            _ => {
                // Not a macro var, not a macro call and not an ending => lex the middle
                self.lex_str_expr_text();
            }
        }
    }

    fn lex_str_expr_text(&mut self) {
        // See `lex_single_quoted_str` for in-depth comments on the logic
        // of lexing possibly escaped text in a string expression
        let mut lit_start_idx = self.buffer.next_string_literal_start();
        let mut lit_end_idx = lit_start_idx;
        let mut last_lit_end_byte_offset = self.cur_byte_offset();

        // Now lex the string
        while let Some(c) = self.cursor.peek() {
            match c {
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
                            last_tok_info.token_type == TokenType::StringExprStart
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
        // EOF reached without a closing double quote

        // Also calculate the payload (will differ whether we had escaped quotes or not).
        let payload = self.resolve_string_literal_payload(
            lit_start_idx,
            lit_end_idx,
            last_lit_end_byte_offset,
            None, // will use the current byte offset
        );

        self.handle_unterminated_str_expr(payload);
    }

    fn handle_unterminated_str_expr(&mut self, payload: Payload) {
        debug_assert_eq!(self.cursor.peek(), None);

        // This will handle the unterminated string expression
        // Both the case of a real string expression and a string literal
        // emitting the correct "missing" token and an error

        let last_tok_is_start = if let Some(last_tok_info) = self.buffer.last_token_info() {
            last_tok_info.token_type == TokenType::StringExprStart
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
        debug_assert_eq!(self.cursor.peek(), Some('"'));

        // This is a regular literal. We need to consume the char, figure
        // out which type of literal is this, similar to single quoted
        // string, replace the last token and exit the string expression mode
        self.cursor.advance();

        let tok_type = self.resolve_string_literal_ending();

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
        debug_assert_eq!(self.cursor.peek(), Some('&'));

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

        while let Some(c) = self.cursor.peek() {
            match c {
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

        // Reached the end of the macro var expr

        // Add the token without consuming the following character
        self.emit_token(
            TokenChannel::DEFAULT,
            TokenType::MacroVarExpr,
            Payload::None,
        );

        // Report we lexed a token
        true
    }

    fn lex_identifier(&mut self) {
        debug_assert!(self
            .cursor
            .peek()
            .map_or(false, |c| c == '_' || is_xid_start(c)));

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
            } else if is_xid_continue(c) {
                is_ascii = false;
                true
            } else {
                false
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
        let ident = self
            .pending_token_text()
            .chars()
            .map(|c| c.to_ascii_uppercase())
            .collect::<SmolStr>();

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
        if let Some(tok_info) = self.buffer.last_token_info_on_default_channel() {
            if tok_info.token_type != TokenType::SEMI {
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
        let (ending, ending_len) = if is_datalines4 { (";;;;", 4) } else { (";", 1) };

        loop {
            match self.cursor.peek() {
                Some('\n') => {
                    self.cursor.advance();
                    self.add_line();
                }
                Some(';') | None => {
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

    fn lex_numeric_literal(&mut self, seen_dot: bool) {
        debug_assert!(self.cursor.peek().map_or(false, |c| c.is_ascii_digit()));
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
        let mut expect_hex = !seen_dot;
        let mut expect_dot = !seen_dot;
        let mut expect_exp = true;
        let mut seen_num_after_exp = false;

        loop {
            match self.cursor.peek() {
                Some('0'..='9') => {
                    // can be any notation
                    self.cursor.advance();

                    // If we have seen the E, also mark that we have seen a number after it
                    if !expect_exp {
                        seen_num_after_exp = true;
                    }
                }
                Some('a'..='d' | 'A'..='D' | 'f' | 'F') if expect_hex => {
                    // must be HEX notation
                    self.lex_numeric_hex_literal();
                    return;
                }
                Some('x' | 'X') if expect_hex => {
                    // complete literal in HEX notation
                    // do not advance, such that the `x` is consumed by the HEX parser
                    self.lex_numeric_hex_literal();
                    return;
                }
                Some('e' | 'E') => {
                    if !expect_hex {
                        // If we already seen the dot and now got E => Scientific notation
                        // consume as lex_numeric_exp_literal() assumes it is working past the E
                        self.cursor.advance();
                        self.lex_numeric_exp_literal(seen_num_after_exp);
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
                Some('.') => {
                    if expect_dot {
                        // Can be standard decimal or scientific notation
                        // but not HEX now
                        self.cursor.advance();
                        expect_hex = false;
                        expect_dot = false;
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
                        self.lex_numeric_exp_literal(seen_num_after_exp);
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
                        self.lex_numeric_exp_literal(seen_num_after_exp);
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
        debug_assert!(!self.cursor.peek().map_or(false, |c| c.is_ascii_digit()));

        let ((tok_type, payload), error) = parse_numeric(self.pending_token_text());

        self.emit_token(TokenChannel::DEFAULT, tok_type, payload);

        if let Some(error) = error {
            self.emit_error(error);
        }
    }

    /// Consumes the remaining tail of a HEX literal and emits the token
    fn lex_numeric_hex_literal(&mut self) {
        #[cfg(debug_assertions)]
        debug_assert!(self.cursor.prev_char().is_ascii_hexdigit());

        // Eat until the end of the literal (x or X) or identify a missing x/X
        loop {
            match self.cursor.peek() {
                Some('0'..='9' | 'a'..='f' | 'A'..='F') => {
                    self.cursor.advance();
                }
                Some('x' | 'X') => {
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

    fn lex_numeric_exp_literal(&mut self, mut seen_exp_sign_or_num: bool) {
        #[cfg(debug_assertions)]
        debug_assert!(matches!(self.cursor.prev_char(), '0'..='9' | 'e' | 'E'));

        loop {
            match self.cursor.peek() {
                Some('0'..='9') => {
                    self.cursor.advance();
                    seen_exp_sign_or_num = true;
                }
                Some('-' | '+') if !seen_exp_sign_or_num => {
                    self.cursor.advance();
                    seen_exp_sign_or_num = true;
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
                    (Some('\'' | '"'), ';') => {
                        self.cursor.advance();
                        self.cursor.advance();
                        self.emit_token(TokenChannel::HIDDEN, TokenType::TermQuote, Payload::None);
                    }
                    (Some('*'), _) => {
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
                    Some('!') => {
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
                    Some('¦') => {
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
                    Some('|') => {
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
                    Some('=') => {
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
                    Some('=') => {
                        self.cursor.advance();
                        self.emit_token(TokenChannel::DEFAULT, TokenType::LE, Payload::None);
                    }
                    Some('>') => {
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
                    Some('=') => {
                        self.cursor.advance();
                        self.emit_token(TokenChannel::DEFAULT, TokenType::GE, Payload::None);
                    }
                    Some('<') => {
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
                    Some('0'..='9') => {
                        // `.N`
                        self.lex_numeric_literal(true);
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
                    Some('*') => {
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

        let Some(next_char) = la_cursor.peek() else {
            // EOF
            return false;
        };

        // Start by trying to eat a possible start char of a SAS name
        // Unicode IS allowed in custom formats...
        if is_valid_sas_name_start(next_char) {
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

    /// Similar to `lex_macro_any`, but allowing only macro calls and
    /// auto-emitting error on macro statements. Use in contexts
    /// where only macro calls are appropriate.
    ///
    /// Performs a lookeahed to check if % starts a macro call,
    /// and lexes it if so.
    ///
    /// Arguments:
    /// - `allow_quote_call`: bool - if `false`, macro quote functions will
    ///     not be lexed as macro calls.
    /// - `allow_stat_to_follow`: bool - if `false`, automatically emits an
    ///     error if a statement is encountered, without consuming the statement itself.
    ///
    /// Returns `MacroKwType`, which will indicate if the token was a macro call,
    /// a macro statement keyword is following (not lexed) or something else.
    ///
    /// NOTE: If `allow_quote_call` is `false` the return will be `MacroKwType::None`!
    fn lex_macro_call(
        &mut self,
        allow_quote_call: bool,
        allow_stat_to_follow: bool,
    ) -> MacroKwType {
        debug_assert_eq!(self.cursor.peek(), Some('%'));

        if !is_valid_sas_name_start(self.cursor.peek_next()) {
            // Not followed by an identifier char
            return MacroKwType::None;
        }

        // Pass a clone of the actual cursor to perform lookahead,
        // as we are only allowing macro calls and not macro statements
        let (tok_type, advance_by) = lex_macro_call_stat_or_label(&mut self.cursor.clone())
            .unwrap_or_else(|err| {
                self.emit_error(ErrorType::InternalError(err));
                (TokenTypeMacroCallOrStat::MacroIdentifier, 0)
            });

        if !is_macro_stat_tok_type(tok_type.into()) {
            // A macro call (or technically a label, but see the docs for
            // `lex_macro_call_stat_or_label` for the explanation)
            if !allow_quote_call && is_macro_quote_call_tok_type(tok_type.into()) {
                // As of today this checked for macro text expressions that
                // are in places of identifiers. SAS emits an error in this case
                // as quote functions create invisible quote chars that are not valid
                // in identifiers. But for now we do not emit error here unlike
                // for statements...just lazy.
                return MacroKwType::None;
            }

            self.cursor.advance_by(advance_by);

            self.dispatch_macro_call_or_stat(tok_type);

            return MacroKwType::MacroCallOrLabel;
        }

        // Must be a macro statement

        if !allow_stat_to_follow {
            // This would lead to breaking SAS session with:
            // ERROR: Open code statement recursion detected.
            // so we emit an error here in addition to missing )
            // that will emit during mode stack pop
            self.emit_error(OPEN_CODE_RECURSION_ERR);
        }

        MacroKwType::MacroStat
    }

    fn lex_macro_comment(&mut self) {
        debug_assert_eq!(self.cursor.peek(), Some('%'));
        debug_assert_eq!(self.cursor.peek_next(), '*');

        // Consume the opener
        self.cursor.advance();
        self.cursor.advance();

        // And now simply eat until first semi and semi too
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
        debug_assert_eq!(self.cursor.peek(), Some('%'));
        debug_assert!(is_valid_sas_name_start(self.cursor.peek_next()));

        // Pass the actual cursor so it will not only be lexed into
        // a token type but also consumed
        let (kw_tok_type, _) =
            lex_macro_call_stat_or_label(&mut self.cursor).unwrap_or_else(|err| {
                self.emit_error(ErrorType::InternalError(err));
                (TokenTypeMacroCallOrStat::MacroIdentifier, 0)
            });

        self.dispatch_macro_call_or_stat(kw_tok_type);
    }

    /// Performs look-ahead to disambiguate between:
    /// - %do;
    /// - %do %while(...);
    /// - %do %until(...);
    /// - %do var=... %to ... <%do ...>;
    ///
    /// Sets the appropriate mode stack for the following tokens.
    fn dispatch_macro_do(&mut self, next_char: char) {
        debug_assert!(self
            .buffer
            .last_token_info_on_default_channel()
            .is_some_and(|ti| ti.token_type == TokenType::KwmDo));

        // Whatever goes next, this mode is done
        self.pop_mode();

        // We need to look ahead to determine the type of the %do statement.
        // We are already past all WS and comments after the %do keyword
        match next_char {
            ';' => {
                // %do;
                self.start_token();
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::SEMI, Payload::None);
            }
            '%' if is_valid_sas_name_start(self.cursor.peek_next()) => {
                self.start_token();
                self.lex_macro_identifier();

                // This may be both %do %while/until or %do %mcall_that_creates_iter_var
                // so we need to fork on the type of the last token. For %while/until
                // we do nothing because lexer above has already set the mode stack,
                // for the macro call we do the same as for all other symbols - push the,
                // name expression mode, except that we know we've found at least the start
                if self.buffer.last_token_info().is_some_and(|ti| {
                    ![TokenType::KwmUntil, TokenType::KwmWhile].contains(&ti.token_type)
                }) {
                    self.push_mode(LexerMode::MacroEval {
                        macro_eval_flags: MacroEvalExprFlags::new(
                            MacroEvalNumericMode::Integer,
                            MacroEvalNextArgumentMode::None,
                            true,
                            true,
                        ),
                        pnl: 0,
                    });
                    self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                    self.push_mode(LexerMode::ExpectSymbol(
                        '=',
                        TokenType::ASSIGN,
                        TokenChannel::DEFAULT,
                    ));
                    self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                    // Note the difference from below. We aready lexed one part of the var name expr,
                    // so we pass `true` and do not pass error, since it won't ever be emitted anyway
                    self.push_mode(LexerMode::MacroVarNameExpr(true, None));
                }
            }
            _ => {
                // %do var=...; A mix of %let and %if expression
                self.push_mode(LexerMode::MacroEval {
                    macro_eval_flags: MacroEvalExprFlags::new(
                        MacroEvalNumericMode::Integer,
                        MacroEvalNextArgumentMode::None,
                        true,
                        true,
                    ),
                    pnl: 0,
                });
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                self.push_mode(LexerMode::ExpectSymbol(
                    '=',
                    TokenType::ASSIGN,
                    TokenChannel::DEFAULT,
                ));
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                self.push_mode(LexerMode::MacroVarNameExpr(
                    false,
                    Some(ErrorType::MissingExpected(
                        "ERROR: An unexpected semicolon occurred in the %DO statement.\n\
                        ERROR: A dummy macro will be compiled.",
                    )),
                ));
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn dispatch_macro_call_or_stat(&mut self, kw_tok_type: TokenTypeMacroCallOrStat) {
        // Emit the token for the keyword itself

        // We use hidden channel for the %str/%nrstr and the wrapping parens
        // since this is a pure compile time directive in SAS which just allows
        // having a macro text expression with things that would otherwise be
        // interpreted as macro calls or removed (like spaces). For all
        // other cases we emit on default channel.
        self.emit_token(
            if [
                TokenTypeMacroCallOrStat::KwmStr,
                TokenTypeMacroCallOrStat::KwmNrStr,
            ]
            .contains(&kw_tok_type)
            {
                TokenChannel::HIDDEN
            } else {
                TokenChannel::DEFAULT
            },
            kw_tok_type.into(),
            Payload::None,
        );

        // Now populate the following mode stack
        match kw_tok_type {
            // Built-in Macro functions go first, then statements
            TokenTypeMacroCallOrStat::KwmStr | TokenTypeMacroCallOrStat::KwmNrStr => {
                self.expect_macro_str_call_args(kw_tok_type == TokenTypeMacroCallOrStat::KwmNrStr);
            }
            TokenTypeMacroCallOrStat::KwmEval | TokenTypeMacroCallOrStat::KwmSysevalf => {
                self.expect_eval_call_args(kw_tok_type == TokenTypeMacroCallOrStat::KwmSysevalf);
            }
            TokenTypeMacroCallOrStat::KwmScan
            | TokenTypeMacroCallOrStat::KwmQScan
            | TokenTypeMacroCallOrStat::KwmKScan
            | TokenTypeMacroCallOrStat::KwmQKScan => {
                self.expect_scan_or_substr_call_args(true);
            }
            TokenTypeMacroCallOrStat::KwmSubstr
            | TokenTypeMacroCallOrStat::KwmQSubstr
            | TokenTypeMacroCallOrStat::KwmKSubstr
            | TokenTypeMacroCallOrStat::KwmQKSubstr => {
                self.expect_scan_or_substr_call_args(false);
            }
            // The "simple" built-ins, that are lexed as macro calls without any special handling
            // Even though we know that some of them have speecific types or number of arguments.
            // E.g. the last 6 are really one argument of var name expr only, but
            // decided to not do too much parsing-like validation here
            TokenTypeMacroCallOrStat::KwmIndex
            | TokenTypeMacroCallOrStat::KwmKIndex
            | TokenTypeMacroCallOrStat::KwmLength
            | TokenTypeMacroCallOrStat::KwmKLength
            | TokenTypeMacroCallOrStat::KwmLowcase
            | TokenTypeMacroCallOrStat::KwmKLowcase
            | TokenTypeMacroCallOrStat::KwmQLowcase
            | TokenTypeMacroCallOrStat::KwmQKLowcase
            | TokenTypeMacroCallOrStat::KwmUpcase
            | TokenTypeMacroCallOrStat::KwmKUpcase
            | TokenTypeMacroCallOrStat::KwmQUpcase
            | TokenTypeMacroCallOrStat::KwmQKUpcase
            | TokenTypeMacroCallOrStat::KwmSysmexecname
            | TokenTypeMacroCallOrStat::KwmSysprod
            | TokenTypeMacroCallOrStat::KwmKCmpres
            | TokenTypeMacroCallOrStat::KwmQKCmpres
            | TokenTypeMacroCallOrStat::KwmKLeft
            | TokenTypeMacroCallOrStat::KwmQKLeft
            | TokenTypeMacroCallOrStat::KwmQuote
            | TokenTypeMacroCallOrStat::KwmNrQuote
            | TokenTypeMacroCallOrStat::KwmBquote
            | TokenTypeMacroCallOrStat::KwmNrBquote
            | TokenTypeMacroCallOrStat::KwmSuperq
            | TokenTypeMacroCallOrStat::KwmUnquote
            | TokenTypeMacroCallOrStat::KwmSymExist
            | TokenTypeMacroCallOrStat::KwmSymGlobl
            | TokenTypeMacroCallOrStat::KwmSymLocal
            | TokenTypeMacroCallOrStat::KwmSysget
            | TokenTypeMacroCallOrStat::KwmSysmacexec
            | TokenTypeMacroCallOrStat::KwmSysmacexist => {
                self.expect_builtin_macro_call_args();
            }
            // The special built-in beast, that allows named arguments
            TokenTypeMacroCallOrStat::KwmValidchs => {
                self.expect_validchs_call_args();
            }
            // Custom macro or label
            TokenTypeMacroCallOrStat::MacroIdentifier => {
                self.maybe_expect_macro_call_args();
            }
            // No argument built-in calls
            #[allow(clippy::match_same_arms)]
            TokenTypeMacroCallOrStat::KwmSysmexecdepth => {}
            // Macro statements
            TokenTypeMacroCallOrStat::KwmInclude
            | TokenTypeMacroCallOrStat::KwmList
            | TokenTypeMacroCallOrStat::KwmThen
            | TokenTypeMacroCallOrStat::KwmElse => {
                // Super easy, they effectively do nothing to mode stack
            }
            TokenTypeMacroCallOrStat::KwmEnd
            | TokenTypeMacroCallOrStat::KwmReturn
            | TokenTypeMacroCallOrStat::KwmRun => {
                // Almost super easy, just expect the closing semi
                self.push_mode(LexerMode::ExpectSemiOrEOF);
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            TokenTypeMacroCallOrStat::KwmAbort
            | TokenTypeMacroCallOrStat::KwmDisplay
            | TokenTypeMacroCallOrStat::KwmGoto
            | TokenTypeMacroCallOrStat::KwmInput
            | TokenTypeMacroCallOrStat::KwmMend
            | TokenTypeMacroCallOrStat::KwmPut
            | TokenTypeMacroCallOrStat::KwmSysexec => {
                // These we just lex as macro text expressions until the semi
                self.push_mode(LexerMode::ExpectSemiOrEOF);
                self.push_mode(LexerMode::MacroSemiTerminatedTextExpr);
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            TokenTypeMacroCallOrStat::KwmDo => {
                // First skip WS and comments, then put lexer into do dispatch mode
                self.push_mode(LexerMode::MacroDo);
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            TokenTypeMacroCallOrStat::KwmTo | TokenTypeMacroCallOrStat::KwmBy => {
                self.push_mode(LexerMode::ExpectSemiOrEOF);
                // The handler fo arguments will push the mode for the comma, etc.
                self.push_mode(LexerMode::MacroEval {
                    macro_eval_flags: MacroEvalExprFlags::new(
                        MacroEvalNumericMode::Integer,
                        MacroEvalNextArgumentMode::None,
                        kw_tok_type == TokenTypeMacroCallOrStat::KwmTo,
                        true,
                    ),
                    pnl: 0,
                });
                // Leading insiginificant WS before opening parenthesis
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            TokenTypeMacroCallOrStat::KwmUntil | TokenTypeMacroCallOrStat::KwmWhile => {
                self.push_mode(LexerMode::ExpectSemiOrEOF);
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                self.push_mode(LexerMode::ExpectSymbol(
                    ')',
                    TokenType::RPAREN,
                    TokenChannel::DEFAULT,
                ));
                self.push_mode(LexerMode::MacroEval {
                    macro_eval_flags: MacroEvalExprFlags::new(
                        MacroEvalNumericMode::Integer,
                        MacroEvalNextArgumentMode::None,
                        false,
                        false,
                    ),
                    pnl: 0,
                });
                // Leading insiginificant WS before the first argument
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                self.push_mode(LexerMode::ExpectSymbol(
                    '(',
                    TokenType::LPAREN,
                    TokenChannel::DEFAULT,
                ));
                // Leading insiginificant WS before opening parenthesis
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            TokenTypeMacroCallOrStat::KwmLet => {
                // following let we must have name, equal sign and expression.
                // All maybe surrounded by insignificant whitespace! + the closing semi
                // Also, SAS happily recovers after missing equal sign, with just a note
                // Hence we pre-feed all the expected states to the mode stack in reverse order,
                // and it will unwind as we lex tokens
                // We do not handle the trailing WS for the initialized, instead defer it to the
                // parser, to avoid excessive lookahead
                self.push_mode(LexerMode::ExpectSemiOrEOF);
                self.push_mode(LexerMode::MacroSemiTerminatedTextExpr);
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                self.push_mode(LexerMode::ExpectSymbol(
                    '=',
                    TokenType::ASSIGN,
                    TokenChannel::DEFAULT,
                ));
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                self.push_mode(LexerMode::MacroVarNameExpr(
                    false,
                    Some(ErrorType::MissingExpected(
                        "ERROR: Expecting a variable name after %LET.",
                    )),
                ));
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            TokenTypeMacroCallOrStat::KwmIf => {
                self.push_mode(LexerMode::MacroEval {
                    macro_eval_flags: MacroEvalExprFlags::new(
                        MacroEvalNumericMode::Integer,
                        MacroEvalNextArgumentMode::None,
                        true,
                        // A semi in %if will cause all kinds of SAS errors,
                        // but lexer will indeed end the expression and lex
                        // semi as semi etc.
                        true,
                    ),
                    pnl: 0,
                });
                // Leading insiginificant WS before expression
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            _ => {
                // TODO!!!!!! PLACEHOLDER - should be exhaustive
                self.push_mode(LexerMode::ExpectSemiOrEOF);
                self.push_mode(LexerMode::MacroSemiTerminatedTextExpr);
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
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
    #[inline]
    fn maybe_expect_macro_call_args(&mut self) {
        // Checkpoint the current state
        self.checkpoint();

        // Push the mode to check if this is a call with parameters.
        // This is as usual in reverse order, first any ws/comments,
        // and then our special mode that will check for the opening parenthesis
        // and possibly rollback to the checkpoint
        self.push_mode(LexerMode::MaybeMacroCallArgs);
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
    }

    /// A helper to populate the expected states for the %str/%nrstr call
    ///
    /// Should be called after adding the %str/%nrstr token
    #[inline]
    fn expect_macro_str_call_args(&mut self, mask_macro: bool) {
        // Populate the expected states for the %str/%nrstr call
        // in reverse order, as the lexer will unwind the stack
        // as it lexes the tokens

        // We use hidden channel for the %str/%nrstr and the wrapping parens
        // since this is a pure compile time directive in SAS which just allows
        // having a macro text expression with things that would otherwise be
        // interpreted as macro calls or removed (like spaces)

        self.push_mode(LexerMode::ExpectSymbol(
            ')',
            TokenType::RPAREN,
            TokenChannel::HIDDEN,
        ));
        self.push_mode(LexerMode::MacroStrQuotedExpr { mask_macro, pnl: 0 });
        self.push_mode(LexerMode::ExpectSymbol(
            '(',
            TokenType::LPAREN,
            TokenChannel::HIDDEN,
        ));
        // Leading insiginificant WS before opening parenthesis
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
    }

    /// A helper to populate the expected states for the %eval/%sysevalf call
    #[inline]
    fn expect_eval_call_args(&mut self, is_sysevalf: bool) {
        self.push_mode(LexerMode::ExpectSymbol(
            ')',
            TokenType::RPAREN,
            TokenChannel::DEFAULT,
        ));
        // The handler for this will push the mode for the comma and following
        // argument as needed
        self.push_mode(LexerMode::MacroEval {
            macro_eval_flags: MacroEvalExprFlags::new(
                if is_sysevalf {
                    MacroEvalNumericMode::Float
                } else {
                    MacroEvalNumericMode::Integer
                },
                if is_sysevalf {
                    MacroEvalNextArgumentMode::MacroArg
                } else {
                    MacroEvalNextArgumentMode::None
                },
                false,
                false,
            ),
            pnl: 0,
        });
        // Leading insiginificant WS before the first argument
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::ExpectSymbol(
            '(',
            TokenType::LPAREN,
            TokenChannel::DEFAULT,
        ));
        // Leading insiginificant WS before opening parenthesis
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
    }

    /// A helper to populate the expected states for the %scan/%substr call
    /// and their quoted versions
    #[inline]
    fn expect_scan_or_substr_call_args(&mut self, is_scan: bool) {
        self.push_mode(LexerMode::ExpectSymbol(
            ')',
            TokenType::RPAREN,
            TokenChannel::DEFAULT,
        ));
        // Second, expression argument. The handler for this will push the mode for the comma
        // and the following of the correct type
        self.push_mode(LexerMode::MacroEval {
            macro_eval_flags: MacroEvalExprFlags::new(
                MacroEvalNumericMode::Integer,
                if is_scan {
                    MacroEvalNextArgumentMode::MacroArg
                } else {
                    MacroEvalNextArgumentMode::EvalExpr
                },
                false,
                false,
            ),
            pnl: 0,
        });
        // First argument and following comma + WS
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::ExpectSymbol(
            ',',
            TokenType::COMMA,
            TokenChannel::DEFAULT,
        ));
        self.push_mode(LexerMode::MacroCallValue {
            populate_next_arg_stack: false,
            pnl: 0,
        });
        // Leading insiginificant WS before the first argument
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::ExpectSymbol(
            '(',
            TokenType::LPAREN,
            TokenChannel::DEFAULT,
        ));
        // Leading insiginificant WS before opening parenthesis
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
    }

    /// A helper to populate the expected states for the built-in macro calls
    /// that have no special handling - just arguments
    #[inline]
    fn expect_builtin_macro_call_args(&mut self) {
        // All built-ins have arguments, so we may avoid the `maybe` version
        self.push_mode(LexerMode::ExpectSymbol(
            ')',
            TokenType::RPAREN,
            TokenChannel::DEFAULT,
        ));
        // The handler fo arguments will push the mode for the comma, etc.
        // Built-ins do not allow named arguments, so we pass `MacroCallValue`
        // right away
        self.push_mode(LexerMode::MacroCallValue {
            populate_next_arg_stack: true,
            pnl: 0,
        });
        // Leading insiginificant WS before the first argument
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::ExpectSymbol(
            '(',
            TokenType::LPAREN,
            TokenChannel::DEFAULT,
        ));
        // Leading insiginificant WS before opening parenthesis
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
    }

    /// A helper to populate the expected states for the %validchs call
    /// that allows named arguments. The only built-in that does so.
    #[inline]
    fn expect_validchs_call_args(&mut self) {
        self.push_mode(LexerMode::ExpectSymbol(
            ')',
            TokenType::RPAREN,
            TokenChannel::DEFAULT,
        ));
        // The handler fo arguments will push the mode for the comma, etc.
        self.push_mode(LexerMode::MacroCallArgOrValue { pnl: 0 });
        // Leading insiginificant WS before the first argument
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::ExpectSymbol(
            '(',
            TokenType::LPAREN,
            TokenChannel::DEFAULT,
        ));
        // Leading insiginificant WS before opening parenthesis
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
    }
}

/// Lex the source code and return the tokenized buffer.
///
/// Known differences from the SAS lexer:
/// - String expressions in macro context are lexed as in open code,
///     for example literals will be lexed as literals, also SAS lexes them
///     as macro text expressions, verbatim.
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
