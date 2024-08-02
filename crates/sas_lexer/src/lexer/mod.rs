pub(crate) mod buffer;
pub(crate) mod channel;
mod cursor;
pub mod error;
pub mod print;
mod sas_lang;
mod text;
pub(crate) mod token_type;

use buffer::{DetachedTokenizedBuffer, LineIdx, Payload, TokenizedBuffer};
use channel::TokenChannel;
use error::{ErrorInfo, ErrorType};
use sas_lang::is_valid_sas_name_start;
use text::{ByteOffset, CharOffset};
use token_type::TokenType;

const BOM: char = '\u{feff}';

/// The lexer mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum LexerMode {
    /// The default mode
    #[default]
    Default,
    /// The macro variable expression mode
    DoubleQuotedString,
}

#[derive(Debug)]
struct Lexer<'src> {
    // source: &'src str,
    source_len: u32,
    buffer: TokenizedBuffer<'src>,
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
    fn new(source: &str, init_mode: Option<LexerMode>) -> Result<Lexer, &str> {
        let Ok(source_len) = u32::try_from(source.len()) else {
            return Err("Lexing of files larger than 4GB is not supported");
        };

        let mut cursor = cursor::Cursor::new(source);
        let mut buffer = TokenizedBuffer::new(source);

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

    fn add_token(&mut self, channel: TokenChannel, token_type: TokenType) {
        self.buffer.add_token(
            channel,
            token_type,
            self.cur_token_byte_offset,
            self.cur_token_start,
            self.cur_token_line,
            Payload::None,
        );
    }

    fn emit_error(&mut self, error: ErrorType) {
        let last_line_char_offset = if let Some(line_info) = self.buffer.get_line_infos().last() {
            line_info.get_start_char_offset().get()
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
            self.buffer.into_iter().last(),
        ));
    }

    fn lex(mut self) -> (TokenizedBuffer<'src>, Box<[ErrorInfo]>) {
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
        self.start_token();

        let c = self.cursor.peek();

        if c.is_whitespace() {
            self.lex_ws();
            return;
        }

        if c.is_ascii() {
            match c {
                '\'' => self.lex_single_quoted_str(),
                '"' => self.lex_double_quoted_str(),
                '/' => {
                    if self.cursor.peek_next() == '*' {
                        self.lex_cstyle_comment();
                    } else {
                        // TODO: this is not done
                        self.cursor.advance();
                        self.add_token(TokenChannel::DEFAULT, TokenType::BaseCode);
                    }
                }
                '&' => {
                    if !self.lex_macro_var_expr() {
                        // We figured this was not a macro variable expression
                        // Apparently any number of consecutive & characters are treated as a single one
                        // so we need to consume the rest of the & characters
                        self.cursor.eat_while(|c| c == '&');

                        self.add_token(TokenChannel::DEFAULT, TokenType::AMP);
                    }
                }
                '*' => {
                    // TODO: this is not done
                    self.cursor.advance();
                    match (self.cursor.peek(), self.cursor.peek_next()) {
                        ('\'' | '"', ';') => {
                            self.cursor.advance();
                            self.cursor.advance();
                            self.add_token(TokenChannel::HIDDEN, TokenType::TermQuote);
                        }
                        _ => {
                            self.add_token(TokenChannel::DEFAULT, TokenType::BaseCode);
                        }
                    }
                }
                _ => {
                    self.cursor.advance();
                    self.add_token(TokenChannel::DEFAULT, TokenType::BaseCode);
                }
            }
        } else {
            self.cursor.advance();
            self.add_token(TokenChannel::DEFAULT, TokenType::BaseCode);
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
        self.add_token(TokenChannel::HIDDEN, TokenType::WS);
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
                self.add_token(TokenChannel::COMMENT, TokenType::CStyleComment);
                self.emit_error(ErrorType::UnterminatedComment);
                return;
            }
        }

        self.add_token(TokenChannel::COMMENT, TokenType::CStyleComment);
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
                self.add_token(TokenChannel::DEFAULT, TokenType::SingleQuotedStringLiteral);
                self.emit_error(ErrorType::UnterminatedStringLiteral);
                return;
            }
        }

        // Now check if this is a single quoted string or one of the other literals
        let tok_type = match self.cursor.peek() {
            'b' => TokenType::SingleQuotedBitTestingLiteral,
            'd' => {
                if self.cursor.peek_next() == 't' {
                    self.cursor.advance();

                    TokenType::SingleQuotedDateTimeLiteral
                } else {
                    TokenType::SingleQuotedDateLiteral
                }
            }
            'n' => TokenType::SingleQuotedNameLiteral,
            't' => TokenType::SingleQuotedTimeLiteral,
            'x' => TokenType::SingleQuotedHexStringLiteral,
            _ => TokenType::SingleQuotedStringLiteral,
        };

        // If we found a literal, advance the cursor
        if tok_type != TokenType::SingleQuotedStringLiteral {
            self.cursor.advance();
        }

        self.add_token(TokenChannel::DEFAULT, tok_type);
    }

    fn lex_double_quoted_str(&mut self) {
        debug_assert_eq!(self.cursor.peek(), '"');

        self.push_mode(LexerMode::DoubleQuotedString);

        !todo!();

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
                self.add_token(TokenChannel::DEFAULT, TokenType::SingleQuotedStringLiteral);
                self.emit_error(ErrorType::UnterminatedStringLiteral);
                return;
            }
        }

        // Now check if this is a regular double quoted string or one of the literals
        let tok_type = match self.cursor.peek() {
            'b' => TokenType::DoubleQuotedBitTestingExprEnd,
            'd' => {
                if self.cursor.peek_next() == 't' {
                    self.cursor.advance();

                    TokenType::DoubleQuotedDateTimeExprEnd
                } else {
                    TokenType::DoubleQuotedDateExprEnd
                }
            }
            'n' => TokenType::DoubleQuotedNameExprEnd,
            't' => TokenType::DoubleQuotedTimeExprEnd,
            'x' => TokenType::DoubleQuotedHexStringExprEnd,
            _ => TokenType::DoubleQuotedStringExprEnd,
        };

        // If we found a literal, advance the cursor
        if tok_type != TokenType::DoubleQuotedStringExprEnd {
            self.cursor.advance();
        }

        self.add_token(TokenChannel::DEFAULT, tok_type);
        self.pop_mode();
    }

    fn lex_macro_var_expr(&mut self) -> bool {
        #[derive(Clone, Copy)]
        enum State {
            Amp,
            Name,
        }

        debug_assert_eq!(self.cursor.peek(), '&');

        // Unfortunately, arbitrary number of lookahead is needed to
        // confirm that this is a macro variable expression
        // as arbitrary number of & characters are treated as a single one.
        // Thus we lexing on a cloned iterator to avoid consuming the original
        // until we are sure that this is a macro variable expression
        // And since we can have something like &&expr&&&9 where &&&9 is NOT a macro var expr
        // we not only need a mini state machine but we also need to gradually
        // advance our real cursor when we have fully formed part like &&expr
        // and then continue the loop until we hit non macro var expr
        let mut la_view = self.cursor.clone();
        let mut state = State::Amp;
        let mut lexed_macro_var_expr = false;

        loop {
            match (state, la_view.advance()) {
                (State::Amp, Some(c)) => {
                    if is_valid_sas_name_start(c) {
                        // Ok, this is a macro expr for sure
                        // and we got the first character of the name
                        state = State::Name;
                        lexed_macro_var_expr = true;
                        continue;
                    }

                    if c != '&' {
                        // Not a macro var expr
                        break;
                    }
                }
                (State::Amp, None) => {
                    // Not a macro var expr and EOF reached
                    break;
                }
                (State::Name, Some(c)) => {
                    if c.is_ascii_alphanumeric() || c == '_' {
                        // Still a name
                        continue;
                    }

                    // Ok, at least some portion of the macro var expr
                    // has been found. Advance the real cursor.

                    // If the last character is a dot, which terminates a macro var expr
                    // we need to consume it as well, but otherwise not,
                    // so we substract 1 from the skip_bytes
                    let skip_chars =
                        la_view.char_offset() - self.cursor.char_offset() - u32::from(c != '.');

                    self.cursor.advance_by(skip_chars);

                    if c == '&' {
                        // Possibly more macro var expr
                        state = State::Amp;
                        continue;
                    }

                    break;
                }
                (State::Name, None) => {
                    // Reached end of file, implicitly end of macro var expr
                    self.cursor.advance_to_eof();
                }
            }
        }

        if lexed_macro_var_expr {
            self.add_token(TokenChannel::DEFAULT, TokenType::MacroVarExpr);
        }

        lexed_macro_var_expr
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
/// let result = lex(source);
/// assert!(result.is_ok());
/// ```
pub fn lex(source: &str) -> Result<(DetachedTokenizedBuffer, Box<[ErrorInfo]>), String> {
    let lexer = Lexer::new(source, None)?;
    let (buffer, errors) = lexer.lex();
    Ok((buffer.into_detached()?, errors))
}
