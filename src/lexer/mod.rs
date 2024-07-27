pub(crate) mod buffer;
mod channel;
mod cursor;
pub mod print;
mod token_type;

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
    /// Current line index
    cur_line: LineIdx,
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
            cur_line,
        }
    }

    #[inline]
    fn cur_byte_offset(&self) -> u32 {
        self.source_len - self.cursor.text_len()
    }

    fn add_line(&mut self, start: u32) -> LineIdx {
        self.cur_line = self.buffer.add_line(start);
        self.cur_line
    }

    fn add_token(&mut self, channel: TokenChannel, token_type: TokenType) {
        self.buffer.add_token(
            channel,
            token_type,
            self.cur_token_start,
            self.cur_line,
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

        if self.source_len == 0 {
            _add_eof(&mut self.buffer, self.source_len, self.cur_line);
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

        _add_eof(&mut self.buffer, self.source_len, self.cur_line);

        self.buffer
    }

    fn lex_token(&mut self) {
        self.cur_token_start = self.cur_byte_offset();

        match self.cursor.advance() {
            Some(c) => {
                if c.is_ascii() {
                    match c {
                        '\n' => {
                            self.add_token(TokenChannel::HIDDEN, TokenType::WS);
                            self.add_line(self.cur_byte_offset());
                        }
                        '*' => match (self.cursor.peek(), self.cursor.peek_next()) {
                            ('\'', ';') | ('"', ';') => {
                                self.cursor.advance();
                                self.cursor.advance();
                                self.add_token(TokenChannel::HIDDEN, TokenType::TermQuote);
                            }
                            _ => {
                                self.add_token(TokenChannel::DEFAULT, TokenType::BaseCode);
                            }
                        },
                        _ => {
                            self.add_token(TokenChannel::DEFAULT, TokenType::BaseCode);
                        }
                    }
                }
            }
            None => {}
        }
    }
}

pub fn lex(source: &str) -> TokenizedBuffer {
    let lexer = Lexer::new(source);

    lexer.lex()
}
