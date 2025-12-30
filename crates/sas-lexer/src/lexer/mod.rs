pub(crate) mod buffer;
pub(crate) mod channel;
mod cursor;
pub mod error;
mod hex;
mod lexer_mode;
mod r#macro;
mod numeric;
mod sas_lang;
#[cfg(test)]
mod tests;
mod text;
pub(crate) mod token_type;

use bit_vec::BitVec;
use buffer::{
    LineIdx, Payload, TokenInfo, TokenizedBuffer, WorkBufferCheckpoint, WorkTokenizedBuffer,
};
use channel::TokenChannel;
use error::{ErrorInfo, ErrorKind};
use hex::parse_sas_hex_string;
use lexer_mode::{
    LexerMode, MacroArgContext, MacroArgNameValueFlags, MacroEvalExprFlags,
    MacroEvalNextArgumentMode, MacroEvalNumericMode,
};

use numeric::{try_parse_decimal, try_parse_hex_integer, NumericParserResult};
#[cfg(feature = "macro_sep")]
use r#macro::needs_macro_sep;
use r#macro::{
    get_macro_resolve_ops_from_amps, is_macro_amp, is_macro_eval_logical_op,
    is_macro_eval_mnemonic, is_macro_eval_quotable_op, is_macro_percent,
    is_macro_quote_call_tok_type, is_macro_stat, is_macro_stat_tok_type,
    lex_macro_call_stat_or_label,
};
use sas_lang::{
    is_valid_sas_name_continue, is_valid_sas_name_start, is_valid_unicode_sas_name_start,
    StringLiteralQuote,
};
use std::{cmp::min, num::NonZeroUsize};
use text::{ByteOffset, CharOffset};
use token_type::{
    parse_keyword, MacroKwType, TokenType, TokenTypeMacroCallOrStat, MAX_KEYWORDS_LEN,
};
use unicode_ident::{is_xid_continue, is_xid_start};

const MAX_EXPECTED_STACK_DEPTH: usize = 40;

const BOM: char = '\u{feff}';

#[derive(Debug)]
struct LexerCheckpoint<'src> {
    cursor: cursor::Cursor<'src>,
    cur_token_byte_offset: ByteOffset,
    cur_token_start: CharOffset,
    cur_token_line: LineIdx,
    mode_stack_len: usize,
    buffer_checkpoint: WorkBufferCheckpoint,
}

#[derive(Debug)]
struct Lexer<'src> {
    /// Source text being lexed
    source: &'src str,

    /// Length of the source text. Stored for faster access over `source.len()`
    source_len: u32,

    /// The "work" token buffer being built
    buffer: WorkTokenizedBuffer,

    /// Cursor for the source text
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

    /// Macro nesting level. Tracks the nesting of macro definitions,
    /// as lexing inside macro definitions is different from the
    /// open code
    macro_nesting_level: u32,

    /// A stack of boolean values that tracks if an open code statement
    /// started but is not yet terminated by semi.
    /// This is used in start comments and datalines predictions.
    pending_stat_stack: BitVec,

    /// Stores the last state for debug assertions to check
    /// against infinite loops. It is the remaining input length
    /// and the mode stack.
    #[cfg(debug_assertions)]
    last_state: (u32, Vec<LexerMode>),
}

/// Result of lexing
#[derive(Debug)]
pub struct LexResult {
    pub buffer: TokenizedBuffer,
    pub errors: Vec<ErrorInfo>,

    #[cfg(any(feature = "opti_stats", test))]
    pub max_mode_stack_depth: usize,
}

impl Lexer<'_> {
    fn new(
        source: &'_ str,
        init_mode: Option<LexerMode>,
        macro_nesting_level: Option<u32>,
    ) -> Result<Lexer<'_>, ErrorKind> {
        let Ok(source_len) = u32::try_from(source.len()) else {
            return Err(ErrorKind::FileTooLarge);
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

        // Calculate default macro nesting level which is 0, meaning we are not in a macro
        let macro_nesting_level = macro_nesting_level.unwrap_or(0);

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
            macro_nesting_level,
            pending_stat_stack: BitVec::from_elem(1, false),
        })
    }

    #[allow(clippy::print_stdout)]
    #[cfg(debug_assertions)]
    fn dump_lexer_state_to_console(&self) {
        println!("Infinite loop detected! Lexer data:");

        println!(
            "- cur_token_byte_offset: {}",
            self.cur_token_byte_offset.get()
        );
        println!("- cur_token_start: {}", self.cur_token_start.get());
        println!("- cur_token_line: {:?}", self.cur_token_line);
        println!("- mode stack: {:?}", self.mode_stack);
        println!("- checkpoint is set: {}", self.checkpoint.is_some());
    }

    /// Create a checkpoint for the lexer.
    ///
    /// Checkpoint only stores the mode stack length at the moment of the call,
    /// and hence assumes that modes will not be popped beyond that point before
    /// rollback.
    ///
    /// Make sure to always clear the checkpoint via `clear_checkpoint` if not rolling back
    fn checkpoint(&mut self) {
        // We should always make sure to clear any checkpoints
        debug_assert!(self.checkpoint.is_none());

        self.checkpoint = Some(LexerCheckpoint {
            cursor: self.cursor.clone(),
            cur_token_byte_offset: self.cur_token_byte_offset,
            cur_token_start: self.cur_token_start,
            cur_token_line: self.cur_token_line,
            mode_stack_len: self.mode_stack.len(),
            buffer_checkpoint: self.buffer.checkpoint(),
        });
    }

    /// Clear the checkpoint, without rolling back
    fn clear_checkpoint(&mut self) {
        self.checkpoint = None;
    }

    /// Rollback the lexer to the last checkpoint, clearing it in the process.
    fn rollback(&mut self) {
        if let Some(checkpoint) = self.checkpoint.take() {
            self.cursor = checkpoint.cursor;
            self.cur_token_byte_offset = checkpoint.cur_token_byte_offset;
            self.cur_token_start = checkpoint.cur_token_start;
            self.cur_token_line = checkpoint.cur_token_line;
            self.mode_stack.truncate(checkpoint.mode_stack_len);
            self.buffer.rollback(checkpoint.buffer_checkpoint);
        } else {
            #[cfg(debug_assertions)]
            {
                self.dump_lexer_state_to_console();
            }
            // Emit an error, we should not be here
            self.emit_error(ErrorKind::InternalErrorMissingCheckpoint);
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
                self.emit_error(ErrorKind::InternalErrorNoTokenText);
                ""
            })
    }

    fn add_string_literal_from_src(
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
                self.emit_error(ErrorKind::InternalErrorOutOfBounds);
                ""
            });

        self.buffer.add_string_literal(text)
    }

    // Lexer mode stack manipulation
    #[inline]
    fn push_mode(&mut self, mode: LexerMode) {
        self.mode_stack.push(mode);
    }

    fn pop_mode(&mut self) {
        if self.mode_stack.pop().is_none() {
            self.emit_error(ErrorKind::InternalErrorEmptyModeStack);
            self.push_mode(LexerMode::default());
        }
    }

    fn mode(&mut self) -> LexerMode {
        match self.mode_stack.last() {
            Some(mode) => mode.clone(),
            None => {
                self.emit_error(ErrorKind::InternalErrorEmptyModeStack);
                self.push_mode(LexerMode::default());
                LexerMode::default()
            }
        }
    }

    // Pending stat stack manipulation
    fn push_pending_stat(&mut self, value: bool) {
        self.pending_stat_stack.push(value);
    }

    fn pop_pending_stat(&mut self) {
        // Only pop if there is more than one element. Otherwise
        // our lexer will fail on extraneous `%end` and `%mend`
        // statements
        if self.pending_stat_stack.len() > 1 {
            self.pending_stat_stack.pop();
        }
    }

    fn pending_stat(&mut self) -> bool {
        match self.pending_stat_stack.len() {
            0 => {
                self.emit_error(ErrorKind::InternalErrorEmptyPendingStatStack);
                self.pending_stat_stack.push(false);
                false
            }
            i => self.pending_stat_stack.get(i - 1).unwrap_or_default(),
        }
    }

    fn set_pending_stat(&mut self, value: bool) {
        match self.pending_stat_stack.len() {
            0 => {
                self.emit_error(ErrorKind::InternalErrorEmptyPendingStatStack);
                self.pending_stat_stack.push(value);
            }
            i => self.pending_stat_stack.set(i - 1, value),
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

    /// Emits token at a previously locally saved mark. In contrast to the
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
        if !self.buffer.last_token_info_mut().is_some_and(|t| {
            t.channel = channel;
            t.token_type = token_type;
            t.payload = payload;
            true
        }) {
            // This is an internal error, we should always have a token to replace
            self.emit_error(ErrorKind::InternalErrorNoTokenToReplace);

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

    fn prep_error_info_at_cur_offset(&self, error: ErrorKind) -> ErrorInfo {
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
    fn emit_error(&mut self, error: ErrorKind) {
        self.errors.push(self.prep_error_info_at_cur_offset(error));
    }

    #[inline]
    fn emit_error_info(&mut self, error_info: ErrorInfo) {
        self.errors.push(error_info);
    }

    /// Main lexing loop, responsible for driving the lexing forwards
    /// as well as finalizing it with a mandatroy EOF roken.
    fn lex(mut self) -> LexResult {
        #[cfg(any(feature = "opti_stats", test))]
        let mut max_mode_stack_depth = 0usize;

        while let Some(next_char) = self.cursor.peek() {
            self.lex_token(next_char);

            #[cfg(any(feature = "opti_stats", test))]
            {
                max_mode_stack_depth = max_mode_stack_depth.max(self.mode_stack.len());
            }

            #[allow(clippy::print_stdout)]
            #[cfg(debug_assertions)]
            {
                let new_state = (self.cursor.remaining_len(), self.mode_stack.clone());
                if self.last_state == new_state {
                    println!("Infinite loop detected!");

                    self.dump_lexer_state_to_console();

                    self.emit_error(ErrorKind::InternalErrorInfiniteLoop);

                    #[cfg(any(feature = "opti_stats", test))]
                    {
                        return LexResult {
                            buffer: self.buffer.into_detached(self.source),
                            errors: self.errors,
                            max_mode_stack_depth,
                        };
                    }

                    #[cfg(not(any(feature = "opti_stats", test)))]
                    {
                        return LexResult {
                            buffer: self.buffer.into_detached(self.source),
                            errors: self.errors,
                        };
                    }
                }
                self.last_state = new_state;
            }
        }

        self.finalize_lexing();

        #[cfg(any(feature = "opti_stats", test))]
        {
            LexResult {
                buffer: self.buffer.into_detached(self.source),
                errors: self.errors,
                max_mode_stack_depth,
            }
        }

        #[cfg(not(any(feature = "opti_stats", test)))]
        {
            LexResult {
                buffer: self.buffer.into_detached(self.source),
                errors: self.errors,
            }
        }
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
                LexerMode::ExpectSymbol(tok_type, tok_channel) => {
                    // If we were expecting a token - call lexing that will effectively
                    // emit an error and the token
                    self.lex_expected_token(None, tok_type, tok_channel);
                }
                LexerMode::ExpectSemiOrEOF | LexerMode::MacroDo => {
                    // If we were expecting a semicolon or EOF - emit a virtual semicolon for parser convenience.
                    // The trailing %DO behaves the same way
                    self.start_token();
                    self.emit_token(TokenChannel::DEFAULT, TokenType::SEMI, Payload::None);
                }
                LexerMode::Default
                | LexerMode::MakeCheckpoint
                | LexerMode::WsOrCStyleCommentOnly
                | LexerMode::MacroLocalGlobal { .. }
                | LexerMode::MaybeMacroCallArgsOrLabel { .. }
                | LexerMode::MaybeMacroCallArgAssign { .. }
                | LexerMode::MacroCallArgOrValue { .. }
                | LexerMode::MaybeMacroDefArgs
                | LexerMode::MaybeTailMacroArgValue
                | LexerMode::MacroDefArg
                | LexerMode::MacroDefNextArgOrDefaultValue
                | LexerMode::MacroSemiTerminatedTextExpr
                | LexerMode::MacroStatOptionsTextExpr => {
                    // These are optional modes, meaning there can be no actual token lexed in it
                    // so we can safely pop them
                }
                LexerMode::MacroStrQuotedExpr { pnl, .. }
                | LexerMode::MacroCallValue { pnl, .. }
                | LexerMode::MacroEval { pnl, .. } => {
                    // If the parens nesting level is > 0, we should emit the missing number of
                    // closing parens to balance it out

                    if pnl > 0 {
                        self.emit_error(ErrorKind::MissingExpectedRParen);

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
                LexerMode::MacroNameExpr(_, err) => {
                    // This may happen if we have %let without a variable name in the end or similar
                    if let Some(err) = err {
                        self.emit_error(err);
                    }
                }
                LexerMode::MacroDefName => {
                    // This would trigger an error in SAS. For simplicity we use the same
                    // error text for macro name and args, as it is close enough approximation
                    self.emit_error(ErrorKind::InvalidMacroDefName);
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
            LexerMode::MakeCheckpoint => {
                // used as a marker for the lexer to create a checkpoint.
                // Pop the mode, checkpoint without it and continue
                self.pop_mode();
                self.checkpoint();
            }
            LexerMode::Default => self.dispatch_mode_default(next_char),
            LexerMode::ExpectSymbol(tok_type, tok_channel) => {
                self.start_token();
                self.lex_expected_token(Some(next_char), tok_type, tok_channel);
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
                    self.emit_error(ErrorKind::MissingExpectedSemiOrEOF);
                }

                self.emit_token(TokenChannel::DEFAULT, TokenType::SEMI, Payload::None);
                self.pop_mode();
            }
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
            LexerMode::MaybeMacroCallArgsOrLabel { check_macro_label } => {
                self.lex_maybe_macro_call_args_or_label(next_char, check_macro_label);
            }
            LexerMode::MaybeMacroCallArgAssign { flags } => {
                self.lex_maybe_macro_call_arg_assign(next_char, flags);
            }
            LexerMode::MaybeTailMacroArgValue => {
                self.lex_maybe_tail_macro_call_arg_value(next_char);
            }
            LexerMode::MacroCallArgOrValue { flags } => {
                self.dispatch_macro_call_arg_or_value(next_char, flags);
            }
            LexerMode::MacroCallValue { flags, pnl } => {
                self.dispatch_macro_call_arg_value(next_char, flags, pnl);
            }
            LexerMode::MaybeMacroDefArgs => {
                self.lex_maybe_macro_def_args(next_char);
            }
            LexerMode::MacroDefArg => {
                self.dispatch_macro_def_arg(next_char);
            }
            LexerMode::MacroDefNextArgOrDefaultValue => {
                self.lex_macro_def_next_arg_or_default_value(next_char);
            }
            LexerMode::MacroDo => {
                self.dispatch_macro_do(next_char);
            }
            LexerMode::MacroLocalGlobal { is_local } => {
                self.dispatch_macro_local_global(next_char, is_local);
            }
            LexerMode::MacroNameExpr(found_name, err) => {
                self.dispatch_macro_name_expr(next_char, !found_name, err);
            }
            LexerMode::MacroSemiTerminatedTextExpr => {
                self.dispatch_macro_semi_term_text_expr(next_char);
            }
            LexerMode::MacroStatOptionsTextExpr => {
                self.dispatch_macro_stat_opts_text_expr(next_char);
            }
            LexerMode::MacroDefName => {
                self.start_token();

                self.lex_macro_def_identifier(next_char, false);

                // If we didn't have an expected ascii identifier.
                // The call has already emitted an error.

                // SAS will actually skip the entire macro definition including the body,
                // not just the macro statement. But we'll try to salvage at least smth.
                // It is as easy as just popping current mode, since we have
                // `LexerMode::MacroStatOptionsTextExpr` on the stack as well as args
                // and they will catch the rest of the macro definition

                // Thus no matter the return value, we'll pop the mode
                self.pop_mode();
            }
        }
    }

    fn lex_expected_token(
        &mut self,
        next_char: Option<char>,
        tok_type: TokenType,
        tok_channel: TokenChannel,
    ) {
        debug_assert!(
            self.mode() == LexerMode::ExpectSymbol(tok_type, tok_channel)
                || self.cursor.peek().is_none() // EOF
        );
        debug_assert!(matches!(
            tok_type,
            TokenType::RPAREN
                | TokenType::ASSIGN
                | TokenType::LPAREN
                | TokenType::COMMA
                | TokenType::FSLASH
        ));

        let (expected_char, error_kind) = match tok_type {
            TokenType::RPAREN => (')', ErrorKind::MissingExpectedRParen),
            TokenType::ASSIGN => ('=', ErrorKind::MissingExpectedAssign),
            TokenType::LPAREN => ('(', ErrorKind::MissingExpectedLParen),
            TokenType::COMMA => (',', ErrorKind::MissingExpectedComma),
            TokenType::FSLASH => ('/', ErrorKind::MissingExpectedFSlash),
            _ => {
                // This is an internal error, we should not have this token type here
                self.emit_error(ErrorKind::InternalErrorUnexpectedTokenType);
                self.pop_mode();
                return;
            }
        };

        if next_char == Some(expected_char) {
            // Consume the expected content
            self.cursor.advance();
        } else {
            // Expected token not found. Emit an error which will point at previous token
            // The token itself is emitted below
            self.emit_error(error_kind);
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
            '\'' => {
                self.lex_single_quoted_str();
                self.set_pending_stat(true);
            }
            '"' => {
                self.lex_string_expression_start(true);
                self.set_pending_stat(true);
            }
            ';' => {
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::SEMI, Payload::None);
                self.set_pending_stat(false);
            }
            '/' => {
                if self.cursor.peek_next() == '*' {
                    self.lex_cstyle_comment();
                } else {
                    self.cursor.advance();
                    self.emit_token(TokenChannel::DEFAULT, TokenType::FSLASH, Payload::None);
                    self.set_pending_stat(true);
                }
            }
            '&' => {
                if !self.lex_macro_var_expr() {
                    // Regular AMP sequence, consume as token
                    self.cursor.eat_while(|c| c == '&');
                    self.emit_token(TokenChannel::DEFAULT, TokenType::AMP, Payload::None);
                }
                self.set_pending_stat(true);
            }
            '%' => {
                match self.cursor.peek_next() {
                    '*' => {
                        self.lex_macro_comment();
                    }
                    c if is_valid_unicode_sas_name_start(c) => {
                        self.lex_macro_identifier(true);
                    }
                    _ => {
                        // Not a macro, just a percent
                        self.cursor.advance();
                        self.emit_token(TokenChannel::DEFAULT, TokenType::PERCENT, Payload::None);
                        self.set_pending_stat(true);
                    }
                }
            }
            '0'..='9' => {
                // Numeric literal
                self.lex_numeric_literal(false);
                self.set_pending_stat(true);
            }
            c if is_valid_unicode_sas_name_start(c) => {
                self.lex_identifier();
                self.set_pending_stat(true);
            }
            _ => {
                // Something else must be a symbol or some unknown character
                self.lex_symbols(next_char);
                if self
                    .buffer
                    .last_token_info()
                    .is_some_and(|t| t.token_type != TokenType::PredictedCommentStat)
                {
                    self.set_pending_stat(true);
                }
            }
        }
    }

    #[allow(clippy::too_many_lines)]
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

        let terminate_on_comma = macro_eval_flags.terminate_on_comma()
            && (parens_nesting_level == 0 || !macro_eval_flags.parens_mask_comma());

        // Dispatch the "big" categories
        match next_char {
            '\'' => self.lex_single_quoted_str(),
            '"' => self.lex_string_expression_start(false),
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
                            self.lex_macro_string_in_macro_eval_context(
                                macro_eval_flags,
                                terminate_on_comma,
                            );
                        }
                    }
                    MacroKwType::MacroCall => {}
                }
            }
            ')' if parens_nesting_level == 0 => {
                // Found the end of the expression, pop the mode and return
                self.maybe_emit_empty_macro_string_in_eval(None);
                self.pop_mode();
            }
            ',' if terminate_on_comma => {
                // Found the end of the expression, pop the mode and return
                self.maybe_emit_empty_macro_string_in_eval(None);
                self.pop_mode();

                // Now push modes for the next argument
                match macro_eval_flags.follow_arg_mode() {
                    MacroEvalNextArgumentMode::None => {}
                    MacroEvalNextArgumentMode::SingleEvalExpr => {
                        self.push_mode(LexerMode::MacroEval {
                            macro_eval_flags: MacroEvalExprFlags::new(
                                macro_eval_flags.numeric_mode(),
                                MacroEvalNextArgumentMode::None,
                                false,
                                false,
                                // doesn't matter really since comma is not special in tail arguments
                                false,
                            ),
                            pnl: 0,
                        });
                    }
                    MacroEvalNextArgumentMode::EvalExpr => {
                        self.push_mode(LexerMode::MacroEval {
                            macro_eval_flags: MacroEvalExprFlags::new(
                                macro_eval_flags.numeric_mode(),
                                MacroEvalNextArgumentMode::EvalExpr,
                                false,
                                false,
                                // in reality it is always true for this mode, but
                                // it seems more robust to forward the flag
                                macro_eval_flags.parens_mask_comma(),
                            ),
                            pnl: 0,
                        });
                    }
                    MacroEvalNextArgumentMode::MacroArg => {
                        self.push_mode(LexerMode::MacroCallValue {
                            flags: MacroArgNameValueFlags::new(
                                MacroArgContext::BuiltInMacro,
                                true,
                                true,
                            ),
                            pnl: 0,
                        });
                    }
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
                    self.lex_macro_string_in_macro_eval_context(
                        macro_eval_flags,
                        terminate_on_comma,
                    );
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
                    *parens_nesting_level = parens_nesting_level.saturating_sub(1);
                }
            }
        };

        let (tok_type, extra_advance_by) = match next_char {
            // '/' is not here, because it is handled in the caller with c-style comment
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
    fn lex_macro_string_in_macro_eval_context(
        &mut self,
        macro_eval_flags: MacroEvalExprFlags,
        terminate_on_comma: bool,
    ) {
        debug_assert!(matches!(
           self.mode(),
           LexerMode::MacroEval { macro_eval_flags: f, .. }
                if f == macro_eval_flags
        ));

        // In eval context, the leading/trailing whitespace may or may not be
        // part of a macro string depending on the preceding/following characters.
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
        // all contexts except `%sysevalf` where it will also recognize floats).
        // But it is very clever. `%eval(1+1)` here both `1` and `1` are recognized
        // as integers. But in `%eval(1.0+1)` the `1.0` is lexed as string because
        // it is not a valid integer. Or in `%eval(1 2=12)` the lhs will be a macro
        // string `1 2`, not integers `1` and `2`.
        //
        // It is super hard to implement this correctly, e.g. in `%eval(1/*comment*/2)`
        // SAS will first yank the comment out of the char stream and then lex the
        // 12 as an actual integer! But we only support continuous tokens, so we must
        // break 1 and 2 into separate tokens with a comment in-between.
        // Thus we do our best, but may not be 100% correct for every edge case.

        let mut ws_mark: Option<(ByteOffset, CharOffset, LineIdx)> = None;
        let mut try_lexing_numeric = true;
        let mut may_precede_mnemonic = true;

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
                ',' if terminate_on_comma => {
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
                        // Hence preceding WS is significant and we should not
                        // try lexing the preceding as a numeric literal
                        try_lexing_numeric = false;
                        ws_mark = None;
                    }
                    // Stop consuming char and break for standard logic.
                    break;
                }
                '%' => {
                    if is_macro_percent(self.cursor.peek_next(), true) {
                        // Hit a macro call or statement in/after the string expression
                        // Stop consuming char and break for standard logic.

                        // NOTE: this is super expensive look-ahead. If we push down
                        // trailing WS trimming to the parser, it can be avoided.
                        if !is_macro_stat(self.cursor.as_str()) {
                            // Not a delimiting statement, but a macro call
                            // Hence preceding WS is significant and we should not
                            // try lexing the preceding as a numeric literal
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
                    may_precede_mnemonic = true;
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
                    if ws_mark.is_some() || may_precede_mnemonic =>
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
                    may_precede_mnemonic = !is_xid_continue(c);
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
                self.emit_error(ErrorKind::InternalErrorNoTokenText);
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
                    match try_parse_hex_integer(
                        macro_string.get(..macro_string.len() - 1).unwrap_or(""),
                    ) {
                        Some(NumericParserResult {
                            token: (tok_type, payload),
                            length,
                            error,
                        }) => {
                            if error.is_none() && length.get() == macro_string.len() - 1 {
                                self.emit_token(TokenChannel::DEFAULT, tok_type, payload);
                            } else {
                                // Emit as macro string
                                self.emit_token(
                                    TokenChannel::DEFAULT,
                                    TokenType::MacroString,
                                    Payload::None,
                                );
                            }
                        }
                        None => {
                            // Emit as macro string
                            self.emit_token(
                                TokenChannel::DEFAULT,
                                TokenType::MacroString,
                                Payload::None,
                            );
                        }
                    }
                } else {
                    // Try integer/float depending on the flag
                    match try_parse_decimal(macro_string, true, macro_eval_flags.float_mode()) {
                        Some(NumericParserResult {
                            token: (tok_type, payload),
                            length,
                            error,
                        }) => {
                            if error.is_none() && length.get() == macro_string.len() {
                                self.emit_token(TokenChannel::DEFAULT, tok_type, payload);
                            } else {
                                // Emit as macro string
                                self.emit_token(
                                    TokenChannel::DEFAULT,
                                    TokenType::MacroString,
                                    Payload::None,
                                );
                            }
                        }
                        None => {
                            // Emit as macro string
                            self.emit_token(
                                TokenChannel::DEFAULT,
                                TokenType::MacroString,
                                Payload::None,
                            );
                        }
                    }
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
                // if we have a preceedning logical operator, we should emit empty string
                // no matter the next token type
                if is_macro_eval_logical_op(prev_tok_type)
                    // or if we are at the start of expression, or start of parenthesized subexpression
                    || matches!(
                        prev_tok_type,
                        // These are all possible "starts", tokens preceding
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
                    )
                {
                    self.emit_empty_macro_string_token();
                }
            }
        }
    }

    fn dispatch_macro_name_expr(
        &mut self,
        next_char: char,
        first_token: bool,
        err: Option<ErrorKind>,
    ) {
        debug_assert!(
            matches!(self.mode(), LexerMode::MacroNameExpr(f, e) if f != first_token && e == err)
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
            if let Some(LexerMode::MacroNameExpr(found_name, _)) =
                lexer.mode_stack.get_mut(start_mode_index)
            {
                *found_name = true;
            } else {
                lexer.emit_error(ErrorKind::InternalErrorUnexpectedModeStack);
            }
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
                }
            }
            '%' => {
                match self.lex_macro_call(false, false) {
                    MacroKwType::MacroStat | MacroKwType::None => {
                        // Hit a following macro statement or just a percent => pop mode and exit.
                        // Error for statement case has already been emitted by `lex_macro_call`
                        pop_mode_and_check(self);
                    }
                    MacroKwType::MacroCall => {
                        if first_token {
                            update_mode(self);
                        }
                    }
                }
            }
            c if is_valid_unicode_sas_name_start(c) || (!first_token && is_xid_continue(c)) => {
                // A macro string in place of macro identifier
                // Consume as identifier, no reserved words here,
                // so we do not need the full lex_identifier logic
                self.cursor.eat_while(is_xid_continue);

                // Add token, but do not pop the mode, as we may have a full macro text expression
                // that generates an identifier
                self.emit_token(
                    TokenChannel::DEFAULT,
                    // True identifier is only possible if this is the first (and only) token.
                    TokenType::MacroString,
                    Payload::None,
                );

                if first_token {
                    update_mode(self);
                }
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
                    MacroKwType::MacroCall => {}
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
                        // Hit a macro call or statement in/after the string expression => emit the text token
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

    fn dispatch_macro_stat_opts_text_expr(&mut self, next_char: char) {
        debug_assert!(matches!(self.mode(), LexerMode::MacroStatOptionsTextExpr));

        self.start_token();

        // Dispatch the "big" categories
        match next_char {
            '\'' => self.lex_single_quoted_str(),
            '"' => self.lex_string_expression_start(false),
            '/' => {
                if self.cursor.peek_next() == '*' {
                    self.lex_cstyle_comment();
                } else {
                    // not a comment, a slash. Lex as a FSLASH token
                    self.cursor.advance();
                    self.emit_token(TokenChannel::DEFAULT, TokenType::FSLASH, Payload::None);
                }
            }
            '&' => {
                if !self.lex_macro_var_expr() {
                    // Not a macro var, just a sequence of ampersands
                    // consume the sequence and continue lexing the string
                    self.cursor.eat_while(|c| c == '&');
                    self.lex_macro_string_stat_opts();
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
                    MacroKwType::MacroCall => {}
                }
            }
            ';' => {
                // Found the terminator, pop the mode and return
                self.pop_mode();
            }
            c if c.is_whitespace() => {
                // Lex whitespace
                self.lex_ws();
            }
            '=' => {
                // Lex the assignment
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::ASSIGN, Payload::None);
            }
            _ => {
                // Not a terminator, just a regular character in the string
                // consume and continue lexing the string
                self.cursor.advance();
                self.lex_macro_string_stat_opts();
            }
        }
    }

    fn lex_macro_string_stat_opts(&mut self) {
        debug_assert!(matches!(self.mode(), LexerMode::MacroStatOptionsTextExpr));

        while let Some(c) = self.cursor.peek() {
            match c {
                '\'' | '"' | '/' | '=' => {
                    // Reached the end of the section of a macro string
                    // Emit the text token and return
                    self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);
                    return;
                }
                c if c.is_whitespace() => {
                    // Also emit the text token and return
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
                        // Hit a macro call or statement in/after the string expression => emit the text token
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

    /// Checks next char for `(` and either emits the LPAREN token with
    /// the necessary following modes or rolls back to the checkpoint
    #[inline]
    fn lex_maybe_macro_call_args_or_label(&mut self, next_char: char, check_macro_label: bool) {
        debug_assert!(
            matches!(self.mode(), LexerMode::MaybeMacroCallArgsOrLabel { check_macro_label: c } if c == check_macro_label)
        );

        match next_char {
            '(' => {
                // Add the LPAREN token
                self.start_token();
                self.cursor.advance();

                self.emit_token(TokenChannel::DEFAULT, TokenType::LPAREN, Payload::None);

                // Clear the checkpoint
                self.clear_checkpoint();

                // Pop the `MaybeMacroCallArgsOrLabel` mode
                self.pop_mode();

                // Populate the remaining expected states for the macro call
                self.push_mode(LexerMode::ExpectSymbol(
                    TokenType::RPAREN,
                    TokenChannel::DEFAULT,
                ));
                // The handler for arguments will push the mode for the comma, etc.
                self.push_mode(LexerMode::MacroCallArgOrValue {
                    flags: MacroArgNameValueFlags::new(MacroArgContext::MacroCall, true, true),
                });
                // Leading insiginificant WS before the first argument
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            ':' if check_macro_label => {
                // Update the previous token, which must be the macro identifier
                if !self
                    .buffer
                    .last_token_info_on_default_channel_mut()
                    .is_some_and(|t| {
                        if t.token_type != TokenType::MacroIdentifier {
                            // This would mean a bug in the lexer
                            return false;
                        }

                        t.token_type = TokenType::MacroLabel;
                        true
                    })
                {
                    // This is an internal error, we should always have a token to replace
                    self.emit_error(ErrorKind::InternalErrorNoTokenToReplace);
                }

                #[cfg(feature = "macro_sep")]
                {
                    // Emit the macro separator token before the label if necessary
                    let last_two_tokens = self
                        .buffer
                        .iter_token_infos()
                        .rev()
                        .filter(|(_, tok_info)| tok_info.channel == TokenChannel::DEFAULT)
                        .take(2)
                        .collect::<Vec<_>>();

                    let last_token = last_two_tokens.first();
                    let second_last_token_tok_type = last_two_tokens
                        .get(1)
                        .map(|(_, tok_info)| tok_info.token_type);

                    // Logically this may not be None, but we'll be defensive
                    if let Some((last_token_idx, last_ti)) = last_token {
                        if needs_macro_sep(second_last_token_tok_type, last_ti.token_type) {
                            self.buffer.insert_token(
                                *last_token_idx,
                                TokenChannel::DEFAULT,
                                TokenType::MacroSep,
                                last_ti.byte_offset,
                                last_ti.start,
                                last_ti.line,
                                Payload::None,
                            );
                        }
                    }
                }

                // Add the COLON token on hidden channel
                self.start_token();
                self.cursor.advance();

                self.emit_token(TokenChannel::HIDDEN, TokenType::COLON, Payload::None);

                // Clear the checkpoint
                self.clear_checkpoint();

                // Pop the MaybeMacroCallArgsOrLabel mode
                self.pop_mode();
            }
            _ => {
                // Not a macro call with arguments, rollback (which will implicitly pop the mode)
                debug_assert!(self.checkpoint.is_some());

                // Rollback to the checkpoint, which should be before any WS and comments
                // and right after macro identifier
                self.rollback();
            }
        }
    }

    /// Checks next char for `=` and either emits the ASSIGN token with
    /// the necessary following modes or rolls back to the checkpoint
    /// and still emits the macro arg value modes.
    #[inline]
    fn lex_maybe_macro_call_arg_assign(&mut self, next_char: char, flags: MacroArgNameValueFlags) {
        debug_assert!(matches!(
            self.mode(),
            LexerMode::MaybeMacroCallArgAssign { flags: f } if f == flags
        ));

        // Pop this mode no matter what
        self.pop_mode();

        if next_char == '=' {
            // Add the token
            self.start_token();
            self.cursor.advance();

            self.emit_token(TokenChannel::DEFAULT, TokenType::ASSIGN, Payload::None);

            // Clear the checkpoint
            self.clear_checkpoint();

            self.push_mode(LexerMode::MacroCallValue { flags, pnl: 0 });
            // Leading insiginificant WS before the argument
            self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        } else {
            // Not a macro call with arguments, rollback (which will implicitly pop the mode)
            debug_assert!(self.checkpoint.is_some());

            // Rollback to the checkpoint, which should be before any WS and comments
            // and right after macro identifier
            self.rollback();

            // In this case any WS before the value is significant
            self.push_mode(LexerMode::MacroCallValue { flags, pnl: 0 });
        }
    }

    /// Checks next char for `,` and either emits the token with
    /// the necessary following modes for exactly one macro argument
    /// or does nothing.
    #[inline]
    fn lex_maybe_tail_macro_call_arg_value(&mut self, next_char: char) {
        debug_assert!(matches!(self.mode(), LexerMode::MaybeTailMacroArgValue));

        // Pop this mode no matter what
        self.pop_mode();

        if next_char == ',' {
            // Add the token
            self.start_token();
            self.cursor.advance();

            self.emit_token(TokenChannel::DEFAULT, TokenType::COMMA, Payload::None);

            self.push_mode(LexerMode::MacroCallValue {
                flags: MacroArgNameValueFlags::new(MacroArgContext::BuiltInMacro, false, false),
                pnl: 0,
            });
            // Leading insiginificant WS before the argument
            self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        }
    }

    #[allow(clippy::too_many_lines)]
    fn dispatch_macro_call_arg_or_value(&mut self, next_char: char, flags: MacroArgNameValueFlags) {
        debug_assert!(matches!(
            self.mode(),
            LexerMode::MacroCallArgOrValue { flags: f } if f == flags
        ));

        // We may set checkpoints in this mode, but when popping the mode
        // we should always ensure that the checkpoint is cleared.
        #[allow(clippy::items_after_statements)]
        fn safe_pop_mode(lexer: &mut Lexer) {
            // Ensure no checkpoint remains
            lexer.clear_checkpoint();

            // Pop the mode and switch to the value mode
            lexer.pop_mode();
        }

        self.start_token();

        // Helper to bail out of this mode and switch to the value mode instead
        let switch_to_value_mode = |lexer: &mut Lexer| {
            // If checkpoint was set, we rollback to it so that macro string
            // could be re-lexed with whatever symbol that triggered this call
            if lexer.checkpoint.is_some() {
                lexer.rollback();
            } else {
                // Otherwise, just pop the mode
                lexer.pop_mode();
            }

            // and switch to the value mode
            lexer.push_mode(LexerMode::MacroCallValue { flags, pnl: 0 });
        };

        // If we hit what is possibly the "end" of the macro arg name,
        // we checkpoint, emit a mode stack that will check if after optional
        // comments/WS we see a `=`. This would mean, yes, it was a macro arg name.
        // Otherwise it will rollback to the checkpoint and re-lex the string as a value.
        // Checkpoint may or may not be set by now, depending on the path taken to get here.
        //
        // Two cases possible:
        // - For macro strings we set checkpoint before starting to lex it, which
        // allows extending it with following WS in case of rollback. => checkpoint
        // will already be set.
        // - For macro var expressions and macro calls checkpoint must be set
        // after they are lexed, so when they call this closure, it will not be set yet.
        let push_check_assign = |lexer: &mut Lexer| {
            if lexer.checkpoint.is_none() {
                lexer.checkpoint();
            }

            lexer.push_mode(LexerMode::MaybeMacroCallArgAssign { flags });
            lexer.push_mode(LexerMode::WsOrCStyleCommentOnly);
        };

        // Dispatch the "big" categories
        match next_char {
            // All symbols, including string literal starts is handled below, since
            // they all share the same logic here - not an arg name.
            '/' => {
                if self.cursor.peek_next() == '*' {
                    // Ok, time to check if this is a macro arg name
                    push_check_assign(self);
                } else {
                    // Symbol - can't be a macro arg name
                    switch_to_value_mode(self);
                }
            }
            '&' => {
                if self.lex_macro_var_expr() {
                    // Clear checkpoint if we had. E.g. in case of `arg&mv`,
                    // `arg` on the previous iteration would have set the checkpoint
                    // but now we know that no WS follows `arg` so we won't need
                    // to rollback so far back.
                    self.clear_checkpoint();
                } else {
                    // Symbol - can't be a macro arg name
                    switch_to_value_mode(self);
                }
            }
            '%' => {
                match self.cursor.peek_next() {
                    '*' => {
                        self.start_token();
                        self.lex_macro_comment();
                    }
                    c if is_valid_unicode_sas_name_start(c) => {
                        // Either a nested macro stat or macro call

                        // We need to clear the checkpoint, for two reasons.
                        // One, macro call lexing uses checkpoint for MaybeMacroCallArgs mode,
                        // we allow only one checkpoint at a time, hence we need to clear
                        // possible previous checkpoint from this mode before trying the macro call lexing.
                        // Second, there is no point to revert back to before the macro,
                        // whether this is arg name or value - the macro will be lexed anyway
                        self.clear_checkpoint();

                        // Save position of the current mode in the stack,
                        // before macro lexing kicks in, which may add new modes.
                        // The the "else" branch below for why this is necessary
                        let mode_stack_len = self.mode_stack.len();

                        self.start_token();
                        self.lex_macro_identifier(false);

                        // Now check whether it was a statement or a macro call
                        if self
                            .buffer
                            .last_token_info()
                            .is_some_and(|ti| is_macro_stat_tok_type(ti.token_type))
                        {
                            // Hit a following macro statement. This is allowed in SAS,
                            // and we can't really "see through" - i.e. we may not know
                            // if the code inside the following macro is part of arg value,
                            // or maybe even terminates the current argument and starts a new one.
                            // So we kinda pretend as if nothing happened in terms of argument lexing
                            // states. The macro stat will parse based on
                            // it's logic and as soon as mandatory stat parts are done,
                            // it will go back to the current state...
                            // Theoretically this should work correctly in valid code, like:
                            //
                            // `%m(1 %if %mi(=a) %then %do; ,arg=value %end;)`
                        } else {
                            // Macro call is only viable in arg name in the last
                            // position => when we see the first one,
                            // we can move to check for assignment to disambiguate arg name vs value.
                            // But only after the macro call is fully lexed - meaning after MaybeMacroCallArgs
                            // mode which uses the checkpoint! And we only allowed one at a time.
                            // Hence the following trick...we push the following modes,
                            // "after" (meaning with index below the stack top) the newly
                            // populated modes by the macro call lexing.
                            // Unlike pushing, this goes in the sasme order we expect them
                            // to be handled, not reverse

                            // This piece will ensure that checkpoint related to this logic
                            // is set only after the macro call is fully lexed.
                            self.mode_stack
                                .insert(mode_stack_len, LexerMode::MakeCheckpoint);
                            self.mode_stack
                                .insert(mode_stack_len, LexerMode::WsOrCStyleCommentOnly);
                            self.mode_stack.insert(
                                mode_stack_len,
                                LexerMode::MaybeMacroCallArgAssign { flags },
                            );
                        }
                    }
                    _ => {
                        // Not a macro, just a percent. switch to value mode
                        switch_to_value_mode(self);
                    }
                }
            }
            ',' if flags.terminate_on_comma() => {
                // Found the terminator, pop the mode and push new modes
                // to expect stuff then return
                safe_pop_mode(self);

                if flags.populate_next_arg_stack() {
                    // Lex the `,` right away
                    self.start_token();
                    self.cursor.advance();
                    self.emit_token(TokenChannel::DEFAULT, TokenType::COMMA, Payload::None);

                    let new_mode = match flags.context() {
                        MacroArgContext::MacroCall => LexerMode::MacroCallArgOrValue { flags },
                        MacroArgContext::BuiltInMacro => {
                            LexerMode::MacroCallValue { flags, pnl: 0 }
                        }
                        MacroArgContext::MacroDef => LexerMode::MacroDefArg,
                    };

                    self.push_mode(new_mode);
                    // Leading insiginificant WS before the argument
                    self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                }
            }
            ')' => {
                // Found the terminator of the entire macro call arguments,
                // just pop the mode and return
                safe_pop_mode(self);
            }
            c if c.is_whitespace() => {
                // WS means we are done with the arg name
                // and should check for assignment
                push_check_assign(self);
            }
            c => {
                // We need to figure out if this is the first token in arg name or not.
                // Non-first char can only follow a macro var expression, a macro call
                // or a macro string in this mode => the list of possible first tokens types used.
                let first_token = !self.buffer.last_token_info().is_some_and(|ti| {
                    [
                        TokenType::MacroVarTerm,
                        TokenType::MacroIdentifier,
                        TokenType::MacroString,
                        TokenType::RPAREN,
                    ]
                    .contains(&ti.token_type)
                });

                if is_valid_unicode_sas_name_start(c) || (!first_token && is_xid_continue(c)) {
                    // A macro string in place of macro identifier
                    // First checkpoint BEFORE consuming! See above why.
                    // If we do not have a bug, it may not be set yet, so this call
                    // is safe.
                    self.checkpoint();

                    // Consume as identifier, no reserved words here,
                    // so we do not need the full lex_identifier logic
                    self.cursor.eat_while(is_xid_continue);

                    // Add token, but do not pop the mode, as we may have a full macro text expression
                    // that generates an identifier
                    self.emit_token(
                        TokenChannel::DEFAULT,
                        // True identifier is only possible if this is the first (and only) token.
                        TokenType::MacroString,
                        Payload::None,
                    );
                } else if c == '=' && !first_token {
                    // Add the token
                    self.start_token();
                    self.cursor.advance();

                    self.emit_token(TokenChannel::DEFAULT, TokenType::ASSIGN, Payload::None);

                    // Found the terminator, pop the mode and push new modes
                    // to expect stuff then return
                    safe_pop_mode(self);

                    self.push_mode(LexerMode::MacroCallValue { flags, pnl: 0 });
                    // Leading insiginificant WS before the argument
                    self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                } else {
                    // Not an arg name, switch to value mode
                    switch_to_value_mode(self);
                }
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn dispatch_macro_call_arg_value(
        &mut self,
        next_char: char,
        flags: MacroArgNameValueFlags,
        parens_nesting_level: u32,
    ) {
        debug_assert!(matches!(
            self.mode(),
            LexerMode::MacroCallValue { flags: f, pnl: l }
                if l == parens_nesting_level && f == flags
        ));

        self.start_token();

        // Dispatch the "big" categories
        match next_char {
            '\'' => self.lex_single_quoted_str(),
            '"' => self.lex_string_expression_start(true),
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
                    self.lex_macro_string_in_macro_call_arg_value(flags, parens_nesting_level);
                }
            }
            '&' => {
                if !self.lex_macro_var_expr() {
                    // Not a macro var, just a sequence of ampersands
                    // consume the sequence and continue lexing the string
                    self.cursor.eat_while(|c| c == '&');
                    self.lex_macro_string_in_macro_call_arg_value(flags, parens_nesting_level);
                }
            }
            '%' => {
                match self.cursor.peek_next() {
                    '*' => {
                        self.start_token();
                        self.lex_macro_comment();
                    }
                    c if is_valid_unicode_sas_name_start(c) => {
                        // We allow both macro call & stats inside macro call args, so...
                        self.start_token();
                        self.lex_macro_identifier(false);
                    }
                    _ => {
                        // Not a macro, just a percent
                        self.cursor.advance();
                        self.lex_macro_string_in_macro_call_arg_value(flags, parens_nesting_level);
                    }
                }
            }
            '\n' => {
                // Special case to catch newline
                // We could have not consumed it and let the
                // string lexing handle it, but this way we
                // we avoid one extra check
                self.cursor.advance();
                self.add_line();
                self.lex_macro_string_in_macro_call_arg_value(flags, parens_nesting_level);
            }
            ',' if parens_nesting_level == 0 && flags.terminate_on_comma() => {
                // Found the terminator, pop the mode and push new modes
                // to expect stuff then return
                self.pop_mode();

                if flags.populate_next_arg_stack() {
                    // Lex the `,` right away
                    self.start_token();
                    self.cursor.advance();
                    self.emit_token(TokenChannel::DEFAULT, TokenType::COMMA, Payload::None);

                    let new_mode = match flags.context() {
                        MacroArgContext::MacroCall => LexerMode::MacroCallArgOrValue { flags },
                        MacroArgContext::BuiltInMacro => {
                            LexerMode::MacroCallValue { flags, pnl: 0 }
                        }
                        MacroArgContext::MacroDef => LexerMode::MacroDefArg,
                    };

                    self.push_mode(new_mode);
                    // Leading insiginificant WS before the argument
                    self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                }
            }
            ')' if parens_nesting_level == 0 => {
                // Found the terminator of the entire macro call arguments,
                // just pop the mode and return
                self.pop_mode();
            }
            _ => {
                // Not a terminator, just a regular character in the string
                // Do NOT consume - macro string tracks parens, and this
                // maybe a paren. Continue lexing the string
                self.lex_macro_string_in_macro_call_arg_value(flags, parens_nesting_level);
            }
        }
    }

    fn lex_macro_string_in_macro_call_arg_value(
        &mut self,
        flags: MacroArgNameValueFlags,
        parens_nesting_level: u32,
    ) {
        debug_assert!(matches!(
            self.mode(), LexerMode::MacroCallValue{ flags: f, .. } if f == flags
        ));

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
                        LexerMode::MacroCallValue { pnl, .. } => {
                            *pnl = pnl.wrapping_add_signed(local_parens_nesting);
                        }
                        _ => unreachable!(),
                    }
                }
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
                        // Hit a macro call or statement in/after the string expression => emit the text token
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
                ',' if parens_nesting_level.wrapping_add_signed(local_parens_nesting) == 0
                    && flags.terminate_on_comma() =>
                {
                    // Found the terminator, pop the mode and push new modes
                    // to expect stuff, emit token then return
                    self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);
                    self.pop_mode();

                    if flags.populate_next_arg_stack() {
                        // Lex the `,` right away
                        self.start_token();
                        self.cursor.advance();
                        self.emit_token(TokenChannel::DEFAULT, TokenType::COMMA, Payload::None);

                        let new_mode = match flags.context() {
                            MacroArgContext::MacroCall => LexerMode::MacroCallArgOrValue { flags },
                            MacroArgContext::BuiltInMacro => {
                                LexerMode::MacroCallValue { flags, pnl: 0 }
                            }
                            MacroArgContext::MacroDef => LexerMode::MacroDefArg,
                        };

                        self.push_mode(new_mode);
                        // Leading insiginificant WS before the argument
                        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                    }
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

    /// Checks next char for `(` and either emits the LPAREN token with
    /// the necessary following modes or just pops the mode
    #[inline]
    fn lex_maybe_macro_def_args(&mut self, next_char: char) {
        // Whatever the next char is, we can pop the mode
        self.pop_mode();

        if next_char == '(' {
            // Add the LPAREN token
            self.start_token();
            self.cursor.advance();

            self.emit_token(TokenChannel::DEFAULT, TokenType::LPAREN, Payload::None);

            // Populate the remaining expected states for the macro definition arguments list
            self.push_mode(LexerMode::ExpectSymbol(
                TokenType::RPAREN,
                TokenChannel::DEFAULT,
            ));
            // The handler for arguments will push the mode for the comma, etc.
            self.push_mode(LexerMode::MacroDefArg);
            // Leading insiginificant WS before the first argument
            self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        }
    }

    fn dispatch_macro_def_arg(&mut self, next_char: char) {
        debug_assert!(matches!(self.mode(), LexerMode::MacroDefArg));

        if next_char == ')' {
            // Found the terminator of the entire macro definition arguments,
            // just pop the mode and return
            self.pop_mode();
            return;
        }

        self.start_token();

        // In correct code only ascii identifier may follow
        if !self.lex_macro_def_identifier(next_char, true) {
            // We didn't have an expected ascii identifier.
            // The call has already emitted an error.

            // SAS will actually skip the entire macro definition including the body,
            // not just the macro statement. But we'll try to salvage at least smth.

            // We do this by delegating to macro call arg lexer which allows
            // more stuff...
            self.pop_mode();
            self.push_mode(LexerMode::MacroCallArgOrValue {
                flags: MacroArgNameValueFlags::new(MacroArgContext::MacroDef, true, true),
            });
            return;
        }

        // If we got the arg name, populate the proper expected states
        self.pop_mode();

        self.push_mode(LexerMode::MacroDefNextArgOrDefaultValue);
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
    }

    /// Checks the next char, lexes `=` or `,` and populates the relevant
    /// following modes if any needed.
    #[inline]
    fn lex_macro_def_next_arg_or_default_value(&mut self, next_char: char) {
        // Whatever the next char is, we can pop the mode
        self.pop_mode();

        match next_char {
            '=' => {
                // Add the token
                self.start_token();
                self.cursor.advance();

                self.emit_token(TokenChannel::DEFAULT, TokenType::ASSIGN, Payload::None);

                // Populate the remaining expected states for the macro call

                // The default value. It will add the next arg mode to stack
                // if `,` is found
                self.push_mode(LexerMode::MacroCallValue {
                    flags: MacroArgNameValueFlags::new(MacroArgContext::MacroDef, true, true),
                    pnl: 0,
                });
                // Leading insiginificant WS before the argument default value
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            ',' => {
                // Add the token
                self.start_token();
                self.cursor.advance();

                self.emit_token(TokenChannel::DEFAULT, TokenType::COMMA, Payload::None);

                // Populate the remaining expected states for the macro call

                // The next argument.
                self.push_mode(LexerMode::MacroDefArg);
                // Leading insiginificant WS before the argument
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            _ => {
                // Not a valid char for the next arg or default value
                // We'll just pop the mode and let the next mode handle it.
                // If it is `)` - it will be consumed by the next mode (which
                // should expect it). Otherwise lexer will recover.
            }
        }
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
            '"' => self.lex_string_expression_start(true),
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

                // Otherwise similar to other macro calls with caveats
                match self.cursor.peek_next() {
                    // Macro comments do not seems to lex as comments inside `%str` calls
                    c if is_valid_unicode_sas_name_start(c) => {
                        // We allow both macro call & stats inside macro call args, so...
                        // One difference with other macro calls though, is that %str()
                        // will efectivaly execute before the embedded statement, hence
                        // the inner statement itself may not be correct really.
                        // E.g. `%str( %let v=1;);` fails with:
                        // `ERROR: Symbolic variable name V1 must contain only letters,
                        // digits, and underscores.`. But we do not handle this really,
                        // as we output str-quoted macro strings as the same `MacroString`
                        // token type and not a special "quoted" version.
                        self.start_token();
                        self.lex_macro_identifier(false);
                    }
                    _ => {
                        // Not a macro, just a percent
                        self.cursor.advance();
                        self.lex_macro_string_in_str_call(mask_macro, parens_nesting_level);
                    }
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
                    }
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
                            self.add_string_literal_from_src(last_lit_end_byte_offset, None);
                        lit_start_idx = min(lit_start_idx, new_start);
                        lit_end_idx = new_end;

                        // Now advance the cursor past the percent
                        self.cursor.advance();

                        // And update the last byte offset - this will ensure that the
                        // following escaped char will be included in the next literal section
                        last_lit_end_byte_offset = self.cur_byte_offset();

                        // Finally, advance the cursor past the quoted char
                        self.cursor.advance();
                        continue;
                    }

                    if !mask_macro && is_macro_percent(self.cursor.peek_next(), false) {
                        // Hit a macro call or statement in/after the string expression => emit the text token
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
        debug_assert!(self.cursor.peek().is_some_and(char::is_whitespace));

        loop {
            if let Some('\n') = self.cursor.advance() {
                self.add_line();
            }

            if !self.cursor.peek().is_some_and(char::is_whitespace) {
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
        self.emit_error(ErrorKind::UnterminatedComment);
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
                                self.add_string_literal_from_src(last_lit_end_byte_offset, None);
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

                self.emit_error(ErrorKind::UnterminatedStringLiteral);
                return;
            }
        }

        // Calculate the byte offset of the end of the string text, which is -1 of the current
        // cursor position, as we've already advanced the cursor to the closing single quote
        let str_text_end_byte_offset = Some(self.cur_byte_offset() - 1);

        // Now check if this is a single quoted string or one of the other literals
        let tok_type = self.resolve_string_literal_ending();

        let (payload, error) = match tok_type {
            TokenType::HexStringLiteral => {
                // Try parsing the hex string
                let parsed_value = parse_sas_hex_string(self.pending_token_text());

                match parsed_value {
                    Ok(value) => {
                        let (lit_start_idx, lit_end_idx) = self.buffer.add_string_literal(value);

                        (
                            Some(Payload::StringLiteral(lit_start_idx, lit_end_idx)),
                            None,
                        )
                    }
                    // Remember the error and revert to a regular string literal logic
                    Err(e) => (None, Some(e)),
                }
            }
            _ => (None, None),
        };

        let payload = payload.unwrap_or_else(|| {
            self.resolve_string_literal_payload(
                lit_start_idx,
                lit_end_idx,
                last_lit_end_byte_offset,
                str_text_end_byte_offset,
            )
        });

        self.emit_token(TokenChannel::DEFAULT, tok_type, payload);

        if let Some(e) = error {
            // Emit the error after the token to properly attribute it to the token
            self.emit_error(e);
        }
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
    ///   It is used to determine if a payload is needed
    /// * `last_lit_end_byte_offset` - The byte offset of the end of the last literal
    ///   section already added to the buffer
    /// * `str_text_end_byte_offset` - The byte offset of the end of the string text
    ///   that is being lexed. Not including closing quote or anything after it.
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
            let (_, final_end) = self
                .add_string_literal_from_src(last_lit_end_byte_offset, str_text_end_byte_offset);

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
                    c if is_valid_unicode_sas_name_start(c) => {
                        if allow_stat {
                            self.lex_macro_identifier(false);
                        } else {
                            // In macro context, nested statements cause open code recursion error
                            // but it seems like they are still half-handled. I.e. they are yanked
                            // from the string expression like in open code, but not actually
                            // executed. Still from lexing perspective, it is more robust to
                            // lex them.
                            // We have to first save the error info at the last byte offset,
                            // and only then lex the macro identifier to report at the correct position
                            let err_info = self
                                .prep_error_info_at_cur_offset(ErrorKind::OpenCodeRecursionError);

                            self.lex_macro_identifier(false);

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
                            self.add_string_literal_from_src(last_lit_end_byte_offset, None);
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
        self.emit_error(ErrorKind::UnterminatedStringLiteral);
        self.pop_mode();
    }

    fn lex_double_quoted_literal(&mut self, mut payload: Payload) {
        debug_assert_eq!(self.cursor.peek(), Some('"'));

        // This is a regular literal. We need to consume the char, figure
        // out which type of literal is this, similar to single quoted
        // string, replace the last token and exit the string expression mode
        self.cursor.advance();

        let tok_type = self.resolve_string_literal_ending();

        if matches!(tok_type, TokenType::HexStringLiteral) {
            // Try parsing the hex string. In order to get the full string slice for parsing
            // we actually need to move one byte back from the current token position, because the
            // opening quote has been lexed as part of the previous token we'll replace below.
            // Thus we can't use `pending_token_text` here.

            let full_token_text = self
                .source
                .get(
                    // Logically we know it can't overflow, but doing a safe sub just in case
                    usize::from(self.cur_token_byte_offset).saturating_sub(1)
                        ..self.cur_byte_offset().into(),
                )
                .unwrap_or_else(|| {
                    // This is an internal error, we should always have a token text
                    self.emit_error(ErrorKind::InternalErrorNoTokenText);
                    ""
                });

            let parsed_value = parse_sas_hex_string(full_token_text);

            match parsed_value {
                Ok(value) => {
                    let (lit_start_idx, lit_end_idx) = self.buffer.add_string_literal(value);

                    payload = Payload::StringLiteral(lit_start_idx, lit_end_idx);
                }
                // Here we can immediately emit the error, as the last token will be the correct one
                Err(e) => {
                    self.emit_error(e);
                }
            }
        }

        self.update_last_token(TokenChannel::DEFAULT, tok_type, payload);
        self.pop_mode();
    }

    /// Tries lexing a macro variable expression
    ///
    /// If it is one indeed, consumes input and generates 1+ tokens
    /// constituting the macro var expr. Yes, this is another case
    /// where the lexer can generate multiple tokens on a single
    /// iteration.
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

        // First we extract resolve "operations" from the ampersands, see
        // `get_macro_resolve_ops_from_amps` for details
        let mut resolve_op_stack = get_macro_resolve_ops_from_amps(amp_count);

        // Advance & emit the resolve ops
        for rev_prec in resolve_op_stack.clone() {
            self.cursor.advance_by(2u32.pow(u32::from(rev_prec)));

            self.emit_token(
                TokenChannel::DEFAULT,
                TokenType::MacroVarResolve,
                Payload::Integer(rev_prec.into()),
            );
            self.start_token();
        }

        // Now, we are looking at the start of the actual identifier that
        // will be resolved, 0+ dot terminators and a possible tail of
        // further macro var exprs (same thing recursively).

        // Why we need to keep the stack of resolve ops? Because on the
        // other end of the entire resolusion chain, we need to recognize
        // matching termination dots and distinguish them from actual dots.
        // E.g. a typical `&&prefix&counter..._` can be used to dynamically
        // reolve into `&prefix1`, `&prefix2`, etc. macro vars in a do-loop
        // and generate the eventual two part name `[value from prefix1]._`
        while !resolve_op_stack.is_empty() {
            // Check the follower to dispatch the next step
            match self.cursor.peek() {
                // Even though unicode macro vars doesn't seem possible, it is
                // not a hard error, so we lex unicode anyway
                Some(c) if is_valid_unicode_sas_name_start(c) => {
                    // Eat the identifier portion
                    self.cursor.eat_while(is_xid_continue);

                    // Emit the identifier portion as macro string
                    self.emit_token(TokenChannel::DEFAULT, TokenType::MacroString, Payload::None);
                    self.start_token();
                }
                Some('.') => {
                    // a dot, terminates the current resolve op on the stack.
                    // Eat the dot and emit the term token
                    self.cursor.advance();

                    self.emit_token(
                        TokenChannel::DEFAULT,
                        TokenType::MacroVarTerm,
                        Payload::None,
                    );
                    self.start_token();

                    // Pop the resolve op from the stack
                    resolve_op_stack.pop();
                }
                Some('&') => {
                    // Ok, we got at least some portion of the macro var expr. But the next amp
                    // can either be the continuation (like in `&&var&c``)
                    //                                               ^^
                    // which we lex in this same iteration
                    // or a trailing amp (like in `&&var&& other stuff``)
                    //                                  ^^
                    // What happens is we basically repeat the same logic as in the beginning
                    let (is_macro, amp_count) = is_macro_amp(self.cursor.chars());

                    if !is_macro {
                        // The following & characters are not part of the macro var expr
                        // Return immediately, as we've already emitted the identifier
                        return true;
                    }

                    // Extract following resolve "operations" from the ampersands
                    let following_resolves = get_macro_resolve_ops_from_amps(amp_count);

                    // Advance & emit the resolve ops
                    for rev_prec in following_resolves.clone() {
                        self.cursor.advance_by(2u32.pow(u32::from(rev_prec)));

                        self.emit_token(
                            TokenChannel::DEFAULT,
                            TokenType::MacroVarResolve,
                            Payload::Integer(rev_prec.into()),
                        );
                        self.start_token();
                    }

                    // And here comes the magic ;-) We need to pop all resolves from
                    // the stack that are <= of the max (lowest precedence) of the following
                    // resolves.
                    let Some(&max_following_prec) = following_resolves.first() else {
                        // SAFETY: we only get into this arm if the follower is an amp,
                        // so there must be at least one resolve op
                        unreachable!()
                    };

                    resolve_op_stack.retain(|&prec| prec > max_following_prec);
                    resolve_op_stack.extend(following_resolves);
                }
                _ => {
                    // Reached the end of the macro var expr.
                    // Just return immediately
                    return true;
                }
            }
        }

        // Report we lexed a token
        true
    }

    fn lex_identifier(&mut self) {
        debug_assert!(self
            .cursor
            .peek()
            .is_some_and(|c| c == '_' || is_xid_start(c)));

        // Start tracking whether the identifier is ASCII
        // It is necessary, as we need to upper case the identifier if it is ASCII
        // for dispatching, and if it is not ASCII, we know it is not a keyword and can
        // skip the dispatching
        let mut is_ascii = true;

        // Eat the identifier. We can safely use `is_xid_continue` because the caller
        // already checked that the first character is a valid start of an identifier
        self.cursor.eat_while(|c| {
            if c.is_ascii() {
                is_valid_sas_name_continue(c)
            } else if is_xid_continue(c) {
                is_ascii = false;
                true
            } else {
                false
            }
        });

        // Now the fun part - dispatch all kinds of identifiers
        let pending_ident = self.pending_token_text();
        let pending_ident_len = pending_ident.len();

        if !is_ascii || pending_ident_len > MAX_KEYWORDS_LEN {
            // Easy case - not ASCII or longer than any of the keywords, just emit the identifier token
            self.emit_token(TokenChannel::DEFAULT, TokenType::Identifier, Payload::None);
            return;
        }

        // This is much quicker than capturing the value as we consume the cursor.
        // Using fixed size buffer, similar to SmolStr crate and others
        let mut buf = [0u8; MAX_KEYWORDS_LEN];

        #[allow(clippy::indexing_slicing)]
        for (i, c) in pending_ident.as_bytes().iter().enumerate() {
            buf[i] = c.to_ascii_uppercase();
        }

        #[allow(unsafe_code, clippy::indexing_slicing)]
        let ident = unsafe { ::core::str::from_utf8_unchecked(&buf[..pending_ident_len]) };

        if let Some(kw_tok_type) = parse_keyword(ident) {
            self.emit_token(TokenChannel::DEFAULT, kw_tok_type, Payload::None);
            return;
        }

        match ident {
            "DATALINES" | "CARDS" | "LINES" => {
                if !self.lex_datalines(false) {
                    self.emit_token(TokenChannel::DEFAULT, TokenType::Identifier, Payload::None);
                }
            }
            "DATALINES4" | "CARDS4" | "LINES4" => {
                if !self.lex_datalines(true) {
                    self.emit_token(TokenChannel::DEFAULT, TokenType::Identifier, Payload::None);
                }
            }
            _ => {
                // genuine user defined identifier
                self.emit_token(TokenChannel::DEFAULT, TokenType::Identifier, Payload::None);
            }
        }
    }

    /// A special lexer that allows strictly ascii identifiers.
    ///
    /// It is used in %macro definitions, as both macro names and
    /// argument names may only be ASCII identifiers, and not even
    /// text expressions.
    ///
    /// Emits a SAS-like error if the `next_char` is not ASCII. Otherwise
    /// consumes the identifier (ascii only) and emits the token.
    ///
    /// Assumes caller has called `self.start_token()` and handles
    /// the mode stack, as well as error recovery.
    fn lex_macro_def_identifier(&mut self, next_char: char, is_argument: bool) -> bool {
        debug_assert!(matches!(
            self.mode(),
            LexerMode::MacroDefName | LexerMode::MacroDefArg
        ));

        if is_valid_sas_name_start(next_char) {
            // Eat the identifier. We can safely use all continuation chars because
            // we already checked that the first character is a valid start of an identifier
            self.cursor.eat_while(is_valid_sas_name_continue);

            self.emit_token(TokenChannel::DEFAULT, TokenType::Identifier, Payload::None);

            // Report that we lexed a token
            true
        } else {
            // This would trigger an error in SAS. For simplicity we use the same
            // error text for macro name and args, as it is close enough approximation
            if is_argument {
                self.emit_error(ErrorKind::InvalidMacroDefArgName);
            } else {
                self.emit_error(ErrorKind::InvalidMacroDefName);
            }

            // Report that we did not lex a token
            false
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
        // if it may be the start of the datalines (must be preceded by `;` on default channel),
        // then we need to peek forward to find a `;`. Only if we find it, we can be sure
        // that this is indeed a datalines start token.
        if let Some(tok_info) = self.buffer.last_token_info_on_default_channel() {
            if tok_info.token_type != TokenType::SEMI {
                // the previous character is not a semicolon
                return false;
            }
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

        // Few! Now we now that this is indeed a datalines! TBH technically we do not know...
        // In SAS, it is context sensitive and will only trigger inside a data step
        // but otherwise it is theoretically possible to have smth. like `datalines;` in
        // a macro...but I refuse to support this madness. Hopefully no such code exists

        // A rare case, where we emit multiple tokens and avoid state/modes
        // Seemed too complicated to allow looping in the lexer for the sake of
        // of this very special language construct

        // Now advance to the semi-colon for real before emitting the datalines start token
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
                        self.emit_error(ErrorKind::UnterminatedDatalines);
                        break;
                    }

                    if self.cursor.as_str().get(..ending_len).unwrap_or("") == ending {
                        // Found the ending. Do not consume as it will be a separate token
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

    #[allow(clippy::cast_possible_truncation)]
    fn lex_numeric_literal(&mut self, seen_dot: bool) {
        debug_assert!(self
            .cursor
            .peek()
            .is_some_and(|c| c.is_ascii_digit() || c == '.'));
        // First, SAS supports 3 notations for numeric literals:
        // 1. Standard decimal notation (base 10)
        // 2. Hexadecimal notation (base 16)
        // 3. Scientific notation (base 10)

        // For HEX notation, the string must start with a number (0-9)
        // and can have up-to 16 total HEX digits (including the starting one)
        // due to 8 bytes of storage for a number in SAS. It must be
        // followed by an `x` or `X` character

        let source_view = self.cursor.as_str();

        // Now we need to try parsing viable notations and
        // later disambiguate between them

        let hex_result = if seen_dot {
            // We have seen a dot, so this can't be a HEX notation
            None
        } else {
            try_parse_hex_integer(source_view)
        };

        let decimal_result = try_parse_decimal(source_view, !seen_dot, true);

        let mut check_trailing_hex_terminator = false;

        let result = match (decimal_result, hex_result) {
            (Some(decimal_result), Some(hex_result)) => {
                // If both are present, we need to compare the lengths
                // to see which one is longer. And if tied, also check for trailing x/X
                if decimal_result.length > hex_result.length {
                    // Clearly decimal as it is longer (x/X can't even be part of the decimal)
                    decimal_result
                } else if hex_result.length > decimal_result.length {
                    // Clearly hex as it is longer
                    check_trailing_hex_terminator = true;
                    hex_result
                } else {
                    // Tied lengths, we need to check for trailing x/X
                    if [b'x', b'X'].contains(
                        source_view
                            .as_bytes()
                            .get(hex_result.length.get())
                            .unwrap_or(&b'_'),
                    ) {
                        // We actually checked for trailing x/X here,
                        // but to keep the code simpler and re-use shared
                        // logic, we set a flag to re-check and consume it later
                        check_trailing_hex_terminator = true;
                        hex_result
                    } else {
                        decimal_result
                    }
                }
            }
            (Some(decimal_result), None) => decimal_result,
            (None, Some(hex_result)) => {
                // We need to check for trailing x/X
                check_trailing_hex_terminator = true;
                hex_result
            }
            (None, None) => {
                // This may happen if the source has invalid numeric that `lexical`
                // parser catches and we do not. Thus we emmitate a result by
                // assuming that all numbers are eaten
                let len = source_view
                    .as_bytes()
                    .iter()
                    .position(|c| !c.is_ascii_digit())
                    .unwrap_or(source_view.len());

                NonZeroUsize::new(len).map_or_else(
                    // SAFETY: len can't be 0, as we have at least one digit
                    || unreachable!(),
                    |length| NumericParserResult {
                        token: (TokenType::FloatLiteral, Payload::Float(0.0)),
                        length,
                        error: Some(ErrorKind::InvalidNumericLiteral),
                    },
                )
            }
        };

        // First advance by the length of the parsed number
        // SAFETY: we can't have length > u32::MAX
        self.cursor.advance_by(result.length.get() as u32);

        let mut missing_trailing_x = false;

        // If this is a possible HEX literal, we need to check for the trailing x/X
        if check_trailing_hex_terminator {
            if self.cursor.peek().is_some_and(|c| matches!(c, 'x' | 'X')) {
                // Ok, as expected, we have a trailing x/X. Consume it
                self.cursor.advance();
            } else {
                // We expected a trailing x/X, but it is not there
                missing_trailing_x = true;
            }
        }

        // Now emit token
        let (tok_type, payload) = result.token;

        self.emit_token(TokenChannel::DEFAULT, tok_type, payload);

        // If there was a parsing error, emit it
        if let Some(error) = result.error {
            self.emit_error(error);
        }

        // And if there was a missing trailing x/X, emit the error
        if missing_trailing_x {
            self.emit_error(ErrorKind::UnterminatedHexNumericLiteral);
        }
    }

    /// Lexes a predicted comment statement in open and macro code.
    ///
    /// We only predict two types of start comments:
    ///
    /// 1) Open code
    ///
    /// Despite the documentation, it seems that macro is not executed
    /// in open code at all, e.g. macro calls do not run and thus do not mask
    /// semicolons. Hence it is actually important to predict comments in open code
    /// on lexer level, otherwise it would be pretty hard for the parser to
    /// handle something like this: `* %mcall(doesn't mask ; semi);`. In open
    /// code SAS will terminate comment at the first semicolon!
    ///
    /// 2) Within macro definitions without nested macro code
    ///
    /// This means that `* comment;` inside a macro definition will be predicted,
    /// but `* %let this will execute;` will not. This is because
    /// SAS behaves differently and all macro is actually executed which leads
    /// to various unexpected things => the downstream parser should properly
    /// parse comment statements within macro definitions.
    ///
    /// More on the topic in official SAS documentation:
    /// <https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/mcrolref/n17rxjs5x93mghn1mdxesvg78drx.htm>
    /// and
    /// <https://sas.service-now.com/csm?id=kb_article_view&sysparm_article=KB0036213>
    fn lex_predicted_comment(&mut self) -> bool {
        // If we are mid open code statement, it can't be a comment
        if self.pending_stat() {
            return false;
        }

        // Check if we are in open code or not
        if self.macro_nesting_level == 0 {
            // Non-macro code - easy
            while let Some(c) = self.cursor.advance() {
                match c {
                    '\n' => {
                        self.add_line();
                    }
                    ';' => {
                        // Found the end of the comment
                        break;
                    }
                    _ => {}
                }
            }

            // Emit the comment token
            self.emit_token(
                TokenChannel::COMMENT,
                TokenType::PredictedCommentStat,
                Payload::None,
            );

            // Report that we lexed a token
            return true;
        }

        // We are in a macro definition, this is harder. We need to checkpoint
        // start consuming the comment, but rollback if we hit any macro code
        // Checkpoint to be able to rollback
        self.checkpoint();

        while let Some(c) = self.cursor.advance() {
            match c {
                '\n' => {
                    self.add_line();
                }
                '%' if self
                    .cursor
                    .peek()
                    .is_some_and(is_valid_unicode_sas_name_start) =>
                {
                    // Ok, we hit a macro call or a macro stat. Rollback and return
                    // We assume the following usage possible:
                    // ```sas
                    // macro m; * 2 %mend;
                    // data _null_; a = 1 %m();
                    // ```
                    // hence we rollback, and tell the caller "no, this is not a comment".
                    //
                    // Even if this is a semi-legit, non-recommended start comment, like:
                    // ```sas
                    // %macro m; *let a=b; comment tail; %mend;
                    // ```
                    // we still defer to the parser to handle it.
                    self.rollback();

                    return false;
                }
                ';' => {
                    // Found the end of the comment
                    break;
                }
                _ => {}
            }
        }

        // EOF/semi reached
        // Clear the checkpoint
        self.clear_checkpoint();

        // Emit the comment token
        self.emit_token(
            TokenChannel::COMMENT,
            TokenType::PredictedCommentStat,
            Payload::None,
        );

        // Report that we lexed a token
        true
    }

    #[allow(clippy::too_many_lines)]
    fn lex_symbols(&mut self, c: char) {
        match c {
            '*' => {
                self.cursor.advance();

                if !self.lex_predicted_comment() {
                    match self.cursor.peek() {
                        Some('*') => {
                            self.cursor.advance();
                            self.emit_token(TokenChannel::DEFAULT, TokenType::STAR2, Payload::None);
                        }
                        _ => {
                            self.emit_token(TokenChannel::DEFAULT, TokenType::STAR, Payload::None);
                        }
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
                match self.cursor.peek_next() {
                    '0'..='9' => {
                        // `.N`
                        self.lex_numeric_literal(true);
                    }
                    _ => {
                        self.cursor.advance();
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
                // All other characters. This can be arbitrary things. For example `x` statement
                // reads the following as a plain string to execute on the host and thus can
                // have anything. There are also proc ds2, python, iml, execute in sql and many
                // others that accept other language => we can't reasonably restrict anything.

                // consume the character, emit a catch all token
                self.cursor.advance();
                self.emit_token(TokenChannel::HIDDEN, TokenType::CatchAll, Payload::None);
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
        if is_valid_unicode_sas_name_start(next_char) {
            la_cursor.advance();
            la_cursor.eat_while(is_xid_continue);
        }

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

    /// Similar to `lex_macro_identifier`, but allowing only macro calls and
    /// optionally auto-emitting error on macro statements. Use in contexts
    /// where only macro calls are appropriate.
    ///
    /// Performs a lookeahed to check if % starts a macro call,
    /// and lexes it if so.
    ///
    /// Arguments:
    /// - `allow_quote_call`: bool - if `false`, macro quote functions will
    ///   not be lexed as macro calls.
    /// - `allow_stat_to_follow`: bool - if `false`, automatically emits an
    ///   error if a statement is encountered, without consuming the statement itself.
    ///
    /// Returns `MacroKwType`, which will indicate which token type was lexed:
    /// a macro call or a macro statement keyword. `MacroKwType::None` means
    /// nothing was lexed.
    ///
    /// NOTE: If `allow_quote_call` is `false` the return will be `MacroKwType::None`!
    fn lex_macro_call(
        &mut self,
        allow_quote_call: bool,
        allow_stat_to_follow: bool,
    ) -> MacroKwType {
        debug_assert_eq!(self.cursor.peek(), Some('%'));

        if !is_valid_unicode_sas_name_start(self.cursor.peek_next()) {
            // Not followed by an identifier char
            return MacroKwType::None;
        }

        // Pass a clone of the actual cursor to perform lookahead,
        // as we are only allowing macro calls and not macro statements
        let mut la_cursor = self.cursor.clone();
        // Move past the % to the actual identifier
        la_cursor.advance();

        let (tok_type, advance_by) =
            lex_macro_call_stat_or_label(&mut la_cursor).unwrap_or_else(|err| {
                self.emit_error(err);
                (TokenTypeMacroCallOrStat::MacroIdentifier, 0)
            });

        if !is_macro_stat_tok_type(tok_type.into()) {
            // A macro call since we assume this is only called in contexts
            // where macro labels are not possible
            if !allow_quote_call && is_macro_quote_call_tok_type(tok_type.into()) {
                // As of today this checked for macro text expressions that
                // are in places of identifiers. SAS emits an error in this case
                // as quote functions create invisible quote chars that are not valid
                // in identifiers. But for now we do not emit error here unlike
                // for statements...just lazy.
                return MacroKwType::None;
            }

            // Advance the actual cursor to the end of the macro call
            // which is the length of the identifier + 1 for the %
            self.cursor.advance_by(advance_by + 1);

            self.dispatch_macro_call_or_stat(tok_type, false);

            return MacroKwType::MacroCall;
        }

        // Must be a macro statement
        if !allow_stat_to_follow {
            // This would lead to breaking SAS session with:
            // ERROR: Open code statement recursion detected.
            // so we emit an error here in addition to missing )
            // that will emit during mode stack pop
            self.emit_error(ErrorKind::OpenCodeRecursionError);
        }

        MacroKwType::MacroStat
    }

    fn lex_macro_comment(&mut self) {
        debug_assert_eq!(self.cursor.peek(), Some('%'));
        debug_assert_eq!(self.cursor.peek_next(), '*');

        // Consume the opener
        self.cursor.advance();
        self.cursor.advance();

        // eat until first semi and semi too.
        // However, macro comments kind-a lex single/double quoted
        // string literals and semi is masked within them, so we need
        // some additional logic to handle this
        let mut cur_quote: Option<StringLiteralQuote> = None;

        while let Some(c) = self.cursor.advance() {
            match c {
                ';' if cur_quote.is_none() => break,
                '\n' => {
                    self.add_line();
                }
                '\'' if cur_quote.is_none() => {
                    // Start of a single quote string
                    cur_quote = Some(StringLiteralQuote::Single);
                }
                '\'' if cur_quote
                    .as_ref()
                    .is_some_and(StringLiteralQuote::is_single) =>
                {
                    // End of a single quote string
                    cur_quote = None;
                }
                '"' if cur_quote.is_none() => {
                    // Start of a double quote string
                    cur_quote = Some(StringLiteralQuote::Double);
                }
                '"' if cur_quote
                    .as_ref()
                    .is_some_and(StringLiteralQuote::is_double) =>
                {
                    // End of a double quote string
                    cur_quote = None;
                }
                _ => {}
            }
        }

        self.emit_token(
            TokenChannel::COMMENT,
            TokenType::MacroComment,
            Payload::None,
        );
    }

    fn lex_macro_identifier(&mut self, allow_macro_label: bool) {
        debug_assert_eq!(self.cursor.peek(), Some('%'));
        debug_assert!(is_valid_unicode_sas_name_start(self.cursor.peek_next()));

        // Pass the actual cursor so it will not only be lexed into
        // a token type but also consumed

        // Consume the % before the identifier
        self.cursor.advance();

        let (kw_tok_type, _) =
            lex_macro_call_stat_or_label(&mut self.cursor).unwrap_or_else(|err| {
                self.emit_error(err);
                (TokenTypeMacroCallOrStat::MacroIdentifier, 0)
            });

        self.dispatch_macro_call_or_stat(kw_tok_type, allow_macro_label);
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

                // We know that the following WS may not be significant
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            '%' if is_valid_unicode_sas_name_start(self.cursor.peek_next()) => {
                self.start_token();
                self.lex_macro_identifier(false);

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
                            false, // doesn't matter really
                        ),
                        pnl: 0,
                    });
                    self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                    self.push_mode(LexerMode::ExpectSymbol(
                        TokenType::ASSIGN,
                        TokenChannel::DEFAULT,
                    ));
                    self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                    // Note the difference from below. We already lexed one part of the var name expr,
                    // so we pass `true` and do not pass error, since it won't ever be emitted anyway
                    self.push_mode(LexerMode::MacroNameExpr(true, None));
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
                        false, // doesn't matter really
                    ),
                    pnl: 0,
                });
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                self.push_mode(LexerMode::ExpectSymbol(
                    TokenType::ASSIGN,
                    TokenChannel::DEFAULT,
                ));
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                self.push_mode(LexerMode::MacroNameExpr(
                    false,
                    Some(ErrorKind::UnexpectedSemiInDoLoop),
                ));
            }
        }
    }

    /// Performs look-ahead to disambiguate between:
    /// - %local/%global var1 var2 ... varN;
    /// - %local/%global / readonly var=...;
    ///
    /// Sets the appropriate mode stack for the following tokens.
    fn dispatch_macro_local_global(&mut self, next_char: char, is_local: bool) {
        debug_assert!(self
            .buffer
            .last_token_info_on_default_channel()
            .is_some_and(
                |ti| ti.token_type == TokenType::KwmLocal || ti.token_type == TokenType::KwmGlobal
            ));

        // Whatever goes next, this mode is done
        self.pop_mode();

        // We need to look ahead to determine the type of the %do statement.
        // We are already past all WS and comments after the %do keyword
        match next_char {
            '/' => {
                // readonly;
                self.start_token();
                self.cursor.advance();
                self.emit_token(TokenChannel::DEFAULT, TokenType::FSLASH, Payload::None);

                // In SAS smth. like `%local / &mv_that_resolves_to_readonly real_var=real_value;`
                // is perfectly valid and possible. So we can't just expect mandatory `readonly`
                // keyword when lexing. Hence we do what we can - push one name expr mode and
                // then follow with the full let statement mode stack. This is as close as we can
                // get to the correct behavior in static lexing.

                // States are pushed in reverse order
                self.expect_macro_let_stat(ErrorKind::InvalidMacroLocalGlobalReadonlyVarName);

                self.push_mode(LexerMode::MacroNameExpr(
                    false,
                    Some(if is_local {
                        ErrorKind::MissingMacroLocalReadonlyKw
                    } else {
                        ErrorKind::MissingMacroGlobalReadonlyKw
                    }),
                ));
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            _ => {
                // var list
                self.push_mode(LexerMode::ExpectSemiOrEOF);
                self.push_mode(LexerMode::MacroStatOptionsTextExpr);
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn dispatch_macro_call_or_stat(
        &mut self,
        kw_tok_type: TokenTypeMacroCallOrStat,
        allow_macro_label: bool,
    ) {
        // If we have macro separator virtual token enabled, we need to
        // start by checking if we should emit it before the keyword token
        #[cfg(feature = "macro_sep")]
        {
            // This will handle all cases except before the macro label.
            // Macro label is disambiguated via a separate mode, see below
            // how MacroIdentifier is handled.
            // Also, we do not emit macro sep in string expressions and macro
            // args, which can't have labels, so the logic is directly here
            // and not checked by the shared `needs_macro_sep` fn
            if needs_macro_sep(
                self.buffer
                    .last_token_info_on_default_channel()
                    .map(|ti| ti.token_type),
                kw_tok_type.into(),
            ) && self.mode_stack.last().map_or(true, |m| {
                !matches!(
                    m,
                    LexerMode::StringExpr { .. }
                        | LexerMode::MacroCallArgOrValue { .. }
                        | LexerMode::MacroCallValue { .. }
                )
            }) {
                self.emit_token(TokenChannel::DEFAULT, TokenType::MacroSep, Payload::None);
            }
        }
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
            // Even though we know that most of them are single arg, SAS won't mask commas
            // and just emit error if 2+ args are passed
            TokenTypeMacroCallOrStat::KwmDatatyp
            | TokenTypeMacroCallOrStat::KwmLowcase
            | TokenTypeMacroCallOrStat::KwmKLowcase
            | TokenTypeMacroCallOrStat::KwmCmpres
            | TokenTypeMacroCallOrStat::KwmQCmpres
            | TokenTypeMacroCallOrStat::KwmKCmpres
            | TokenTypeMacroCallOrStat::KwmQKCmpres
            | TokenTypeMacroCallOrStat::KwmLeft
            | TokenTypeMacroCallOrStat::KwmQLeft
            | TokenTypeMacroCallOrStat::KwmKLeft
            | TokenTypeMacroCallOrStat::KwmQKLeft
            | TokenTypeMacroCallOrStat::KwmTrim
            | TokenTypeMacroCallOrStat::KwmQTrim
            | TokenTypeMacroCallOrStat::KwmKTrim
            | TokenTypeMacroCallOrStat::KwmQKTrim => {
                self.expect_builtin_macro_call_args();
            }
            // For these "simple" built-ins SAS will mask commas, so we need handle
            // them as strict single arg functions
            TokenTypeMacroCallOrStat::KwmIndex
            | TokenTypeMacroCallOrStat::KwmKIndex
            | TokenTypeMacroCallOrStat::KwmLength
            | TokenTypeMacroCallOrStat::KwmKLength
            | TokenTypeMacroCallOrStat::KwmQLowcase
            | TokenTypeMacroCallOrStat::KwmQKLowcase
            | TokenTypeMacroCallOrStat::KwmUpcase
            | TokenTypeMacroCallOrStat::KwmKUpcase
            | TokenTypeMacroCallOrStat::KwmQUpcase
            | TokenTypeMacroCallOrStat::KwmQKUpcase
            | TokenTypeMacroCallOrStat::KwmSysmexecname
            | TokenTypeMacroCallOrStat::KwmSysprod
            | TokenTypeMacroCallOrStat::KwmQuote
            | TokenTypeMacroCallOrStat::KwmNrQuote
            | TokenTypeMacroCallOrStat::KwmBquote
            | TokenTypeMacroCallOrStat::KwmNrBquote
            // Superq is in a league of it's own. in reality it expects valid macro var
            // name, and comma inside will cause a different error, but whatever
            | TokenTypeMacroCallOrStat::KwmSuperq
            | TokenTypeMacroCallOrStat::KwmUnquote
            | TokenTypeMacroCallOrStat::KwmSymExist
            | TokenTypeMacroCallOrStat::KwmSymGlobl
            | TokenTypeMacroCallOrStat::KwmSymLocal
            | TokenTypeMacroCallOrStat::KwmSysget
            | TokenTypeMacroCallOrStat::KwmSysmacexec
            | TokenTypeMacroCallOrStat::KwmSysmacexist => {
                self.expect_builtin_macro_call_one_arg_masking();
            }
            // The special built-in beast, that allows named arguments
            TokenTypeMacroCallOrStat::KwmCompstor
            | TokenTypeMacroCallOrStat::KwmValidchs
            | TokenTypeMacroCallOrStat::KwmVerify
            | TokenTypeMacroCallOrStat::KwmKVerify => {
                self.expect_builtin_macro_call_named_args();
            }
            // Custom macro or label
            TokenTypeMacroCallOrStat::MacroIdentifier => {
                self.maybe_expect_macro_call_args_or_label(allow_macro_label);
            }
            // No argument built-in calls
            TokenTypeMacroCallOrStat::KwmSysmexecdepth => {}
            // The most special of them all - %sysfunc
            TokenTypeMacroCallOrStat::KwmSysfunc | TokenTypeMacroCallOrStat::KwmQSysfunc => {
                self.expect_sysfunc_macro_call_args();
            }
            // Macro statements
            TokenTypeMacroCallOrStat::KwmInclude
            | TokenTypeMacroCallOrStat::KwmList
            | TokenTypeMacroCallOrStat::KwmThen
            | TokenTypeMacroCallOrStat::KwmElse => {
                // Super easy, they effectively do nothing to mode stack.
                // Although they all kinda expect a semi after some arbitrary
                // stuff, but enforcing this here is an overkill. Parser will
                // handle it just fine.
                // One thing we do add though - is lexing of WS. Significant WS
                // (macro string) may not follow any of these, but since they
                // may legitimately pop inside non-default context (at least
                // %then-%else may be inside macro call args in real-life code),
                // hence this way we ensure that at least leading WS/comments
                // are lexed on hidden channel
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            TokenTypeMacroCallOrStat::KwmReturn
            | TokenTypeMacroCallOrStat::KwmRun
            | TokenTypeMacroCallOrStat::KwmSysmstoreclear => {
                // Almost super easy, just expect the closing semi
                self.push_mode(LexerMode::ExpectSemiOrEOF);
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            TokenTypeMacroCallOrStat::KwmEnd => {
                // Like the previous once, just expect the closing semi
                self.push_mode(LexerMode::ExpectSemiOrEOF);
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);

                // But also pop the pending stat value
                self.pop_pending_stat();
            }
            TokenTypeMacroCallOrStat::KwmPut | TokenTypeMacroCallOrStat::KwmSysexec => {
                // These we just lex as macro text expressions until the semi
                self.push_mode(LexerMode::ExpectSemiOrEOF);
                self.push_mode(LexerMode::MacroSemiTerminatedTextExpr);
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            TokenTypeMacroCallOrStat::KwmAbort
            | TokenTypeMacroCallOrStat::KwmDisplay
            | TokenTypeMacroCallOrStat::KwmGoto
            | TokenTypeMacroCallOrStat::KwmInput
            | TokenTypeMacroCallOrStat::KwmSymdel
            | TokenTypeMacroCallOrStat::KwmSyslput
            | TokenTypeMacroCallOrStat::KwmSysrput
            | TokenTypeMacroCallOrStat::KwmWindow => {
                // These we just lex as macro stat opts text expressions until the semi
                self.push_mode(LexerMode::ExpectSemiOrEOF);
                self.push_mode(LexerMode::MacroStatOptionsTextExpr);
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            TokenTypeMacroCallOrStat::KwmMend => {
                // These we just lex as macro stat opts text expressions until the semi
                self.push_mode(LexerMode::ExpectSemiOrEOF);
                self.push_mode(LexerMode::MacroStatOptionsTextExpr);
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);

                // For %mend we also need to pop the macro nesting level
                self.macro_nesting_level = self.macro_nesting_level.saturating_sub(1);

                // And also pop the pending stat value
                self.pop_pending_stat();
            }
            TokenTypeMacroCallOrStat::KwmDo => {
                // First skip WS and comments, then put lexer into do dispatch mode
                self.push_mode(LexerMode::MacroDo);
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);

                // Also we need to push the current pending stat value to the stack.
                // This is done to handle the code like this:
                // ```sas
                // data;
                // a = 1
                // %if &condition %do;
                //     * 42;
                // %end;
                // %else %do;
                //     * 2;
                // %end;
                //```
                // What happens here is that in both %do branches we have the tail of
                // the statement. But we do not want the first one `* 42;` to modify
                // the pending statement status for the second one, or otherwise we
                // would predict that that the second `* 2;` is a comment.
                // So we push the current pending stat value to the stack and pop it
                // when we are done with the %do statement (%end).
                let cur_pending_stat = self.pending_stat();
                self.push_pending_stat(cur_pending_stat);
            }
            TokenTypeMacroCallOrStat::KwmTo | TokenTypeMacroCallOrStat::KwmBy => {
                self.push_mode(LexerMode::ExpectSemiOrEOF);
                // The handler for arguments will push the mode for the comma, etc.
                self.push_mode(LexerMode::MacroEval {
                    macro_eval_flags: MacroEvalExprFlags::new(
                        MacroEvalNumericMode::Integer,
                        MacroEvalNextArgumentMode::None,
                        kw_tok_type == TokenTypeMacroCallOrStat::KwmTo,
                        true,
                        false, // doesn't matter really
                    ),
                    pnl: 0,
                });
                // Leading insiginificant WS before opening parenthesis
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            TokenTypeMacroCallOrStat::KwmUntil | TokenTypeMacroCallOrStat::KwmWhile => {
                self.expect_macro_until_while_stat_args();
            }
            TokenTypeMacroCallOrStat::KwmLet => {
                self.expect_macro_let_stat(ErrorKind::InvalidMacroLetVarName);
            }
            TokenTypeMacroCallOrStat::KwmLocal | TokenTypeMacroCallOrStat::KwmGlobal => {
                // First skip WS and comments, then put lexer into dispatch mode
                self.push_mode(LexerMode::MacroLocalGlobal {
                    is_local: kw_tok_type == TokenTypeMacroCallOrStat::KwmLocal,
                });
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
                        false, // doesn't matter really
                    ),
                    pnl: 0,
                });
                // Leading insiginificant WS before expression
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
            }
            TokenTypeMacroCallOrStat::KwmCopy | TokenTypeMacroCallOrStat::KwmSysmacdelete => {
                self.expect_macro_name_then_opts();
            }
            TokenTypeMacroCallOrStat::KwmMacro => {
                self.push_mode(LexerMode::ExpectSemiOrEOF);
                self.push_mode(LexerMode::MacroStatOptionsTextExpr);
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                self.push_mode(LexerMode::MaybeMacroDefArgs);
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);
                self.push_mode(LexerMode::MacroDefName);
                self.push_mode(LexerMode::WsOrCStyleCommentOnly);

                // Increment the macro nesting level
                self.macro_nesting_level += 1;

                // Also we need to push the current pending stat value to the stack.
                //
                // Unlike the %do statement, for the macro definitions we assume that
                // there is no pending stat to begin with. This is because we can't
                // really know how this macro will be used, it may generate something
                // that's inline in an open code statement, or it may generate full
                // statements. This can only be resolved by the parser (or to be
                // more accurate interpreter).
                //
                // What this heuristic means in real-life, is that in a code like the
                // following:
                // ```sas
                // macro m();
                // * 2;
                // %mend;
                //
                // data _null_;
                //     a = %m()
                // run;
                //```
                // We will assume that the `* 2;` is a comment, and not a part of the
                // assignment statement. But this should be exceedingly rare in real-life
                self.push_pending_stat(false);
            }
            TokenTypeMacroCallOrStat::KwmSyscall => {
                self.expect_syscall_call_and_args();
            }
        }
    }

    /// A special helper that allows us to do a complex "parsing" look-ahead
    /// to distinguish between an argument-less macro call, the one
    /// with arguments or macro label (if in a context that allows them).
    ///
    /// E.g. in `"&m /*comment*/ ()suffix"` `&m /*comment*/ ()` is a macro call
    /// with arguments. Notice that there is WS & comment between the macro identifier
    /// and the opening parenthesis. It is insignificant and should be lexed as such.
    /// While in `"&m /*comment*/ suffix"` `&m` is a macro call without arguments,
    /// and ` /*comment*/ suffix` is a single token of remaining text!
    ///
    /// In reality, in SAS, this is even more complex and impossible to statically
    /// determine, as SAS looks for () only if the macro was defined with parameters!
    /// So in theory, in `"&m /*comment*/ ()suffix"`, the entire ` /*comment*/ ()suffix`
    /// may be text!
    ///
    /// We obviously can't do that, so we will assume that the macro call is with arguments.
    #[inline]
    fn maybe_expect_macro_call_args_or_label(&mut self, allow_macro_label: bool) {
        // Checkpoint the current state
        self.checkpoint();

        // Push the mode to check if this is a call with parameters.
        // This is as usual in reverse order, first any ws/comments,
        // and then our special mode that will check for the opening parenthesis,
        // colon and possibly rollback to the checkpoint
        self.push_mode(LexerMode::MaybeMacroCallArgsOrLabel {
            check_macro_label: allow_macro_label,
        });
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
            TokenType::RPAREN,
            TokenChannel::HIDDEN,
        ));
        self.push_mode(LexerMode::MacroStrQuotedExpr { mask_macro, pnl: 0 });
        self.push_mode(LexerMode::ExpectSymbol(
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
                // %eval has only one arg, so comma is not special.
                // and %sysevalf apparently doesn't mask commas inside
                // parens! the first comma terminates the first argument
                false,
            ),
            pnl: 0,
        });
        // Leading insiginificant WS before the first argument
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::ExpectSymbol(
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
                    MacroEvalNextArgumentMode::SingleEvalExpr
                },
                false,
                false,
                true,
            ),
            pnl: 0,
        });
        // First argument and following comma + WS
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::ExpectSymbol(
            TokenType::COMMA,
            TokenChannel::DEFAULT,
        ));
        self.push_mode(LexerMode::MacroCallValue {
            flags: MacroArgNameValueFlags::new(MacroArgContext::BuiltInMacro, false, true),
            pnl: 0,
        });
        // Leading insiginificant WS before the first argument
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::ExpectSymbol(
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
            TokenType::RPAREN,
            TokenChannel::DEFAULT,
        ));
        // The handler for arguments will push the mode for the comma, etc.
        // Built-ins do not allow named arguments, so we pass `MacroCallValue`
        // right away
        self.push_mode(LexerMode::MacroCallValue {
            flags: MacroArgNameValueFlags::new(MacroArgContext::BuiltInMacro, true, true),
            pnl: 0,
        });
        // Leading insiginificant WS before the first argument
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::ExpectSymbol(
            TokenType::LPAREN,
            TokenChannel::DEFAULT,
        ));
        // Leading insiginificant WS before opening parenthesis
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
    }

    /// A helper to populate the expected states for the built-in macro calls
    /// that mask comma, and thus have exactly one argument
    #[inline]
    fn expect_builtin_macro_call_one_arg_masking(&mut self) {
        // All built-ins have arguments, so we may avoid the `maybe` version
        self.push_mode(LexerMode::ExpectSymbol(
            TokenType::RPAREN,
            TokenChannel::DEFAULT,
        ));
        // Push the mode for the sole expected argument
        self.push_mode(LexerMode::MacroCallValue {
            flags: MacroArgNameValueFlags::new(MacroArgContext::BuiltInMacro, false, false),
            pnl: 0,
        });
        // Leading insiginificant WS before the first argument
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::ExpectSymbol(
            TokenType::LPAREN,
            TokenChannel::DEFAULT,
        ));
        // Leading insiginificant WS before opening parenthesis
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
    }

    /// A helper to populate the expected states for a couple of builints
    /// allowing named arguments.
    #[inline]
    fn expect_builtin_macro_call_named_args(&mut self) {
        self.push_mode(LexerMode::ExpectSymbol(
            TokenType::RPAREN,
            TokenChannel::DEFAULT,
        ));
        // The handler for arguments will push the mode for the comma, etc.
        self.push_mode(LexerMode::MacroCallArgOrValue {
            flags: MacroArgNameValueFlags::new(MacroArgContext::MacroCall, true, true),
        });
        // Leading insiginificant WS before the first argument
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::ExpectSymbol(
            TokenType::LPAREN,
            TokenChannel::DEFAULT,
        ));
        // Leading insiginificant WS before opening parenthesis
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
    }

    /// A helper to populate the expected states for the %[q]sysfunc call.
    /// It starts with a mandatory name text expression (similar to `%let`
    /// var name expression), then mandatory arguments in parens.
    ///
    /// The inner function arguments are evaluated in `sysevalf` mode
    /// contextually - when the requested base function expects numeric.
    /// Since we do not want to maintain a full list of all functions
    /// that expect numeric arguments, we always lex as eval expr.
    ///
    /// And then an optional comma + format is allowed.
    #[inline]
    fn expect_sysfunc_macro_call_args(&mut self) {
        // Outer closing parenthesis
        self.push_mode(LexerMode::ExpectSymbol(
            TokenType::RPAREN,
            TokenChannel::DEFAULT,
        ));
        self.push_mode(LexerMode::MaybeTailMacroArgValue);
        // Leading insiginificant WS before the last argument
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        // Inner closing parenthesis
        self.push_mode(LexerMode::ExpectSymbol(
            TokenType::RPAREN,
            TokenChannel::DEFAULT,
        ));
        // The handler for arguments will push the mode for the comma, etc.
        self.push_mode(LexerMode::MacroEval {
            macro_eval_flags: MacroEvalExprFlags::new(
                MacroEvalNumericMode::Float,
                MacroEvalNextArgumentMode::EvalExpr,
                false,
                false,
                true,
            ),
            pnl: 0,
        });
        // Leading insiginificant WS before the first argument
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        // Inner openinig parenthesis
        self.push_mode(LexerMode::ExpectSymbol(
            TokenType::LPAREN,
            TokenChannel::DEFAULT,
        ));
        // Leading insiginificant WS before opening parenthesis
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        // Function name
        self.push_mode(LexerMode::MacroNameExpr(
            false,
            Some(ErrorKind::MissingSysfuncFuncName),
        ));
        // Leading insiginificant WS before the function name
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        // Outer openinig parenthesis
        self.push_mode(LexerMode::ExpectSymbol(
            TokenType::LPAREN,
            TokenChannel::DEFAULT,
        ));
        // Leading insiginificant WS before opening parenthesis
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
    }

    /// A helper to populate the expected states for the [%do] %until/%while statements
    /// starting at the opening parenthesis. It is similar to %eval and
    /// followed by expected semicolon or EOF.
    #[inline]
    fn expect_macro_until_while_stat_args(&mut self) {
        self.push_mode(LexerMode::ExpectSemiOrEOF);
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::ExpectSymbol(
            TokenType::RPAREN,
            TokenChannel::DEFAULT,
        ));
        self.push_mode(LexerMode::MacroEval {
            macro_eval_flags: MacroEvalExprFlags::new(
                MacroEvalNumericMode::Integer,
                MacroEvalNextArgumentMode::None,
                false,
                false,
                false, // doesn't matter really
            ),
            pnl: 0,
        });
        // Leading insiginificant WS before the first argument
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::ExpectSymbol(
            TokenType::LPAREN,
            TokenChannel::DEFAULT,
        ));
        // Leading insiginificant WS before opening parenthesis
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
    }

    /// A helper to populate the expected states for the %let statement.
    /// Following let we must have name, equal sign and expression.
    /// All maybe surrounded by insignificant whitespace! + the closing semi
    /// Also, SAS happily recovers after missing equal sign, with just a note
    /// Hence we pre-feed all the expected states to the mode stack in reverse order,
    /// and it will unwind as we lex tokens
    /// We do not handle the trailing WS for the initializer, instead defer it to the
    /// parser, to avoid excessive lookahead
    #[inline]
    fn expect_macro_let_stat(&mut self, err_type: ErrorKind) {
        self.push_mode(LexerMode::ExpectSemiOrEOF);
        self.push_mode(LexerMode::MacroSemiTerminatedTextExpr);
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::ExpectSymbol(
            TokenType::ASSIGN,
            TokenChannel::DEFAULT,
        ));
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::MacroNameExpr(false, Some(err_type)));
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
    }

    /// A helper to populate the expected states for the %copy/%SYSMACDELETE statements.
    /// It is similar to %let, starts with a mandatory name expr (macro here),
    /// then a mandatory `/` followed by options that we just lex as
    /// stat options text expression.
    #[inline]
    fn expect_macro_name_then_opts(&mut self) {
        self.push_mode(LexerMode::ExpectSemiOrEOF);
        self.push_mode(LexerMode::MacroStatOptionsTextExpr);
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::ExpectSymbol(
            TokenType::FSLASH,
            TokenChannel::DEFAULT,
        ));
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::MacroNameExpr(
            false,
            Some(ErrorKind::InvalidOrOutOfOrderStatement),
        ));
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
    }

    /// A helper to populate the expected states for the `%syscall` statement.
    /// It is somewhat similar to the `%sysfunc` call in that it is followed
    /// by a mandatory name text expression (call routine name). And
    /// then by a mandatory parentesized arguments. Despite the docs saying
    /// parens are optional, even argument-less `CALL STREAMREWIND` fails
    /// when no parens are present.
    ///
    /// The arguments are evaluated with the same caveats as in the `%sysfunc`,
    /// see above for more details.
    #[inline]
    fn expect_syscall_call_and_args(&mut self) {
        self.push_mode(LexerMode::ExpectSemiOrEOF);
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        // Outer closing parenthesis
        self.push_mode(LexerMode::ExpectSymbol(
            TokenType::RPAREN,
            TokenChannel::DEFAULT,
        ));
        // The handler for arguments will push the mode for the comma, etc.
        self.push_mode(LexerMode::MacroEval {
            macro_eval_flags: MacroEvalExprFlags::new(
                MacroEvalNumericMode::Float,
                MacroEvalNextArgumentMode::EvalExpr,
                false,
                false,
                true,
            ),
            pnl: 0,
        });
        // Leading insiginificant WS before the first argument
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        // Outer openinig parenthesis
        self.push_mode(LexerMode::ExpectSymbol(
            TokenType::LPAREN,
            TokenChannel::DEFAULT,
        ));
        // Leading insiginificant WS before opening parenthesis
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
        self.push_mode(LexerMode::MacroNameExpr(
            false,
            Some(ErrorKind::MissingSyscallRoutineName),
        ));
        self.push_mode(LexerMode::WsOrCStyleCommentOnly);
    }
}

/// Lex the source code of an entire program.
///
/// This is the most common way to use the lexer. This function assumes that a standalone
/// program is being lexed. I.e. the code is not partial code snippet or
/// file that is meant to be included in a parent program.
///
/// # Arguments
/// * `source: &str` - The source code to lex
///
/// # Returns
/// * `Result<LexResult, ErrorKind>` - The lexed tokens, errors and string literals buffer
///
/// # Errors
/// If the source code is larger than 4GB, an error message is returned
///
/// # Examples
/// ```
/// use sas_lexer::{lex_program, LexResult, TokenIdx};
/// let source = "%let x = 42;";
/// let LexResult { buffer, .. } = lex_program(&source).unwrap();
/// let tokens: Vec<TokenIdx> = buffer.iter_tokens().collect();
/// assert_eq!(tokens.len(), 9);
///
/// for token in tokens {
///     println!("{:?}", buffer.get_token_raw_text(token, &source));
/// }
/// ```
pub fn lex_program<S: AsRef<str>>(source: &S) -> Result<LexResult, ErrorKind> {
    let lexer = Lexer::new(source.as_ref(), None, None)?;
    Ok(lexer.lex())
}
