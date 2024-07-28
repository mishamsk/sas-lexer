pub(crate) mod buffer;
pub(crate) mod channel;
mod cursor;
pub mod error;
pub mod print;
mod sas_lang;
pub(crate) mod token_type;

use buffer::{LineIdx, Payload, TokenizedBuffer};
use channel::TokenChannel;
use error::LexerError;
use sas_lang::is_valid_sas_name_start;
use token_type::TokenType;

const BOM: char = '\u{feff}';

#[derive(Debug)]
struct Lexer<'src> {
    // source: &'src str,
    source_len: u32,
    buffer: TokenizedBuffer<'src>,
    cursor: cursor::Cursor<'src>,
    /// Start byte offset of the token being lexed
    cur_token_start: u32,
    /// Start line index of the token being lexed
    cur_token_line: LineIdx,
}

impl<'src> Lexer<'src> {
    fn new(source: &str) -> Result<Lexer, &str> {
        let Ok(source_len) = u32::try_from(source.len()) else {
            return Err("Lexing of files larger than 4GB is not supported");
        };

        let cursor = cursor::Cursor::new(source);
        let mut buffer = TokenizedBuffer::new(source, Some(source.len()));

        // Add the first line
        let cur_line = buffer.add_line(0);

        Ok(Lexer {
            // source,
            source_len,
            buffer,
            cursor,
            cur_token_start: 0,
            cur_token_line: cur_line,
        })
    }

    #[inline]
    fn cur_byte_offset(&self) -> u32 {
        self.source_len - self.cursor.text_len()
    }

    #[inline]
    fn add_line(&mut self) {
        self.buffer.add_line(self.cur_byte_offset());
    }

    fn start_token(&mut self) {
        self.cur_token_start = self.cur_byte_offset();
        self.cur_token_line = (self.buffer.line_count() - 1).into();
    }

    fn add_token(&mut self, channel: TokenChannel, token_type: TokenType) {
        self.buffer.add_token(
            channel,
            token_type,
            self.cur_token_start,
            self.cur_token_line,
            Payload::None,
        );
    }

    fn emit_error(&mut self, error: LexerError) {
        self.buffer.add_token(
            TokenChannel::DEFAULT,
            TokenType::ERROR,
            self.cur_byte_offset(),
            (self.buffer.line_count() - 1).into(),
            Payload::Error(error),
        );
    }

    fn lex(mut self) -> TokenizedBuffer<'src> {
        fn _add_eof(buffer: &mut TokenizedBuffer, source_len: u32, cur_line: LineIdx) {
            buffer.add_token(
                TokenChannel::DEFAULT,
                TokenType::EOF,
                source_len,
                cur_line,
                Payload::None,
            );
        }

        // If the source is empty, add EOF and return
        if self.source_len == 0 {
            _add_eof(&mut self.buffer, self.source_len, self.cur_token_line);
            return self.buffer;
        }

        // Skip BOM if present
        if self.cursor.peek() == BOM {
            self.cursor.advance();
            self.cur_token_start += 1;
        }

        while !self.cursor.is_eof() {
            self.lex_token();
        }

        _add_eof(&mut self.buffer, self.source_len, self.cur_token_line);

        self.buffer
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
            self.add_token(TokenChannel::DEFAULT, TokenType::ERROR);
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
                self.emit_error(LexerError::UnterminatedComment);
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
                self.add_token(TokenChannel::DEFAULT, TokenType::SingleQuotedString);
                self.emit_error(LexerError::UnterminatedStringLiteral);
                return;
            }
        }

        // Now check if this is a single quoted string or one of the literals
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
            _ => TokenType::SingleQuotedString,
        };

        // If we found a literal, advance the cursor
        if tok_type != TokenType::SingleQuotedString {
            self.cursor.advance();
        }

        self.add_token(TokenChannel::DEFAULT, tok_type);
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
        let mut la_view = self.cursor.chars();
        let mut state = State::Amp;
        let mut lexed_macro_var_expr = false;

        loop {
            match (state, la_view.next()) {
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
                    let skip_bytes = self.cursor.text_len() as usize
                        - la_view.as_str().len()
                        - usize::from(c != '.');

                    // SAFETY: we only match ascii characters when we got here, so
                    // this the bytes offset can't hit in the middle of a multi-byte character
                    self.cursor.skip_bytes(skip_bytes);

                    if c == '&' {
                        // Possibly more macro var expr
                        state = State::Amp;
                        continue;
                    }

                    break;
                }
                (State::Name, None) => {
                    // Reached end of file, implicitly end of macro var expr
                    self.cursor
                        .skip_bytes(self.cursor.text_len() as usize - la_view.as_str().len());
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
pub fn lex(source: &str) -> Result<TokenizedBuffer, &str> {
    let lexer = Lexer::new(source)?;

    Ok(lexer.lex())
}
