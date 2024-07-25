mod buffer;
mod channel;
mod cursor;
pub mod print;
mod token_type;

pub use buffer::TokenIdx;

pub fn lex(source: &str) -> buffer::TokenizedBuffer {
    let mut cursor = cursor::Cursor::new(source);
    let chars_total = source.len() as u32;

    let mut buffer = buffer::TokenizedBuffer::new(source, Some(source.len()));
    let mut line = buffer.add_line(0);

    let channel = channel::TokenChannel::DEFAULT;
    let token_type = token_type::TokenType::BaseCode;

    while !cursor.is_eof() {
        let mut start = chars_total - cursor.text_len();

        // FIXME: newline handling is not correct
        match cursor.peek() {
            '\n' => {
                line = buffer.add_line(start + 1);
                // skip over newline for now
                cursor.advance();
                start = start + 1;
            }
            _ => {}
        }
        cursor.advance();

        buffer.add_token(channel, token_type, start, line);
    }

    buffer
}
