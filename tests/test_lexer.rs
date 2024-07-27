use rstest::rstest;

use sas_lexer::{lex, Payload, TokenChannel, TokenIdx, TokenType};

fn check_single_lexeme(
    contents: &str,
    token_type: TokenType,
    token_channel: TokenChannel,
    payload: Payload,
) {
    let buffer = lex(contents);
    let tokens: Vec<TokenIdx> = buffer.into_iter().collect();

    assert_eq!(
        tokens.len(),
        2,
        "Expected a 2 tokens (test and EOF), got {}",
        tokens.len()
    );
    assert_eq!(
        buffer.get_token_type(tokens[1]),
        TokenType::EOF,
        "Expected EOF token, got {:?}",
        buffer.get_token_type(tokens[1])
    );

    let token = tokens[0];

    // for single tests line should always be 1
    assert_eq!(
        buffer.get_token_start_line(token),
        1,
        "Expected line 1, got {}",
        buffer.get_token_start_line(token)
    );

    // compare line count with the number of lines in the contents
    // for singles we should have consumed all the lines
    assert_eq!(
        buffer.get_token_end_line(token),
        contents.lines().count() as u32,
        "Expected last line, got {}",
        buffer.get_token_end_line(token)
    );

    // for single tests column should always be 0
    assert_eq!(
        buffer.get_token_start_column(token),
        0,
        "Expected column 0, got {}",
        buffer.get_token_start_column(token)
    );

    // to compare the end column, we need to calculate the offset from the last line
    // lines iterator doesn't include the newline character, so we need to add 1
    // if the contents ends with a newline
    let last_line_end_column = contents.lines().last().unwrap().len() as u32
        + if contents.ends_with('\n') { 1 } else { 0 };

    assert_eq!(
        buffer.get_token_end_column(token),
        last_line_end_column,
        "Expected last column {}, got {}",
        last_line_end_column,
        buffer.get_token_end_column(token)
    );

    // compare start and end byte offsets with the length of the contents
    assert_eq!(
        buffer.get_token_start(token),
        0,
        "Expected start offset 0, got {}",
        buffer.get_token_start(token)
    );
    assert_eq!(
        buffer.get_token_end(token),
        contents.len() as u32,
        "Expected end  offset {}, got {}",
        contents.len() as u32,
        buffer.get_token_end(token)
    );

    // we are not testing ephemeral tokens here, so we should always have text
    // and match the entire contents
    assert_eq!(
        buffer.get_token_text(token).unwrap(),
        contents,
        "Expected text {:?}, got {:?}",
        contents,
        buffer.get_token_text(token).unwrap()
    );

    assert_eq!(
        buffer.get_token_type(token),
        token_type,
        "Expected token type {:?}, got {:?}",
        token_type,
        buffer.get_token_type(token)
    );
    assert_eq!(
        buffer.get_token_channel(token),
        token_channel,
        "Expected token channel {:?}, got {:?}",
        token_channel,
        buffer.get_token_channel(token)
    );
    assert_eq!(
        buffer.get_token_payload(token),
        payload,
        "Expected token payload {:?}, got {:?}",
        payload,
        buffer.get_token_payload(token)
    );
}

#[rstest]
#[case::mixed_ws("\n\t \n", TokenType::WS, TokenChannel::HIDDEN, Payload::None)]
fn test_single_lexemes(
    #[case] contents: &str,
    #[case] token_type: TokenType,
    #[case] token_channel: TokenChannel,
    #[case] payload: Payload,
) {
    check_single_lexeme(contents, token_type, token_channel, payload);
}

#[rstest]
#[case::empty("''")]
#[case::escaped("'some''other'")]
#[case::with_newline("'some\nother'")]
#[case::with_unicode("'some\n🔥\n'")]
#[case::with_macro("'%some() and &mvar'")]
fn test_single_quoted_string(#[case] contents: &str) {
    check_single_lexeme(
        contents,
        TokenType::SingleQuotedString,
        TokenChannel::DEFAULT,
        Payload::None,
    );
}

#[rstest]
#[case::bit_testing("'1'b", TokenType::SingleQuotedBitTestingLiteral)]
#[case::bit_testing("'4may2022'd", TokenType::SingleQuotedDateLiteral)]
#[case::bit_testing("'01may2021:12:30'dt", TokenType::SingleQuotedDateTimeLiteral)]
#[case::bit_testing("'unicode 🙏 col'n", TokenType::SingleQuotedNameLiteral)]
#[case::bit_testing("'00:42't", TokenType::SingleQuotedTimeLiteral)]
#[case::bit_testing("'FF'x", TokenType::SingleQuotedHexStringLiteral)]
fn test_single_quoted_literals(#[case] contents: &str, #[case] token_type: TokenType) {
    check_single_lexeme(contents, token_type, TokenChannel::DEFAULT, Payload::None);
}
