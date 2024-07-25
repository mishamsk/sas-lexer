/// Function to print the token
use crate::lexer::buffer::{TokenIdx, TokenizedBuffer};

pub fn print_token(token: TokenIdx, buffer: &TokenizedBuffer) -> String {
    let start_line = buffer.get_token_start_line(token);
    let end_line = buffer.get_token_end_line(token);
    let start_column = buffer.get_token_start_column(token);
    let end_column = buffer.get_token_end_column(token);
    let token_start = buffer.get_token_start(token);
    let token_end = buffer.get_token_end(token);
    let token_text = buffer.get_token_text(token).unwrap_or("<no text>");
    let token_type = buffer.get_token_type(token);
    let token_channel = buffer.get_token_channel(token);

    // Constructing the string representation of the token
    let token_repr = format!(
        "[@{},{}:{}={:?},<{}>,L{}:C{}-L{}:C{},chl={}]",
        token,
        token_start,
        token_end,
        token_text,
        token_type,
        start_line,
        start_column,
        end_line,
        end_column,
        token_channel
    );

    token_repr
}

pub fn print_tokens(tokens: Vec<TokenIdx>, buffer: &TokenizedBuffer) {
    for token in tokens.into_iter() {
        println!("{}", print_token(token, buffer));
    }
}
