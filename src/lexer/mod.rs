pub(crate) mod buffer;
pub(crate) mod channel;
mod cursor;
pub mod print;
pub(crate) mod token_type;

use buffer::{LineIdx, Payload, TokenizedBuffer};
use channel::TokenChannel;
use token_type::TokenType;

const BOM: char = '\u{feff}';

#[derive(Debug)]
struct Lexer<'src> {
    source: &'src str,
    source_len: u32,
    buffer: TokenizedBuffer<'src>,
    cursor: cursor::Cursor<'src>,
    /// Start byte offset of the token being lexed
    cur_token_start: u32,
    /// Start line index of the token being lexed
    cur_token_line: LineIdx,
}

impl<'src> Lexer<'src> {
    fn new(source: &str) -> Lexer {
        let cursor = cursor::Cursor::new(source);
        let mut buffer = TokenizedBuffer::new(source, Some(source.len()));

        // Add the first line
        let cur_line = buffer.add_line(0);

        Lexer {
            source,
            source_len: source.len() as u32,
            buffer,
            cursor,
            cur_token_start: 0,
            cur_token_line: cur_line,
        }
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
                '*' => {
                    // TODO: this is not done
                    self.cursor.advance();
                    match (self.cursor.peek(), self.cursor.peek_next()) {
                        ('\'', ';') | ('"', ';') => {
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

        while let Some(c) = self.cursor.advance() {
            if c == '*' && self.cursor.peek() == '/' {
                self.cursor.advance();
                break;
            }

            if c == '\n' {
                self.add_line();
            }
        }

        self.add_token(TokenChannel::COMMENT, TokenType::CStyleComment);
    }

    fn lex_single_quoted_str(&mut self) {
        debug_assert_eq!(self.cursor.peek(), '\'');

        // Eat the opening single quote
        self.cursor.advance();

        while let Some(c) = self.cursor.advance() {
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

        self.add_token(TokenChannel::DEFAULT, tok_type)
    }
}

pub fn lex(source: &str) -> TokenizedBuffer {
    let lexer = Lexer::new(source);

    lexer.lex()
}
