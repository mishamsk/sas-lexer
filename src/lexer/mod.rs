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
        let start = chars_total - cursor.text_len();

        match cursor.peek() {
            '\n' => {
                cursor.advance();
                line = buffer.add_line(start + 1);
            }
            _ => {
                cursor.advance();
            }
        }

        buffer.add_token(channel, token_type, start, line);
    }

    buffer
}
