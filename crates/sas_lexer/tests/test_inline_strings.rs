#![allow(clippy::cast_possible_truncation)]
mod util;

use rstest::rstest;
use util::{check_error, check_token};

use sas_lexer::{
    error::ErrorType, lex, DetachedTokenizedBuffer, Payload, TokenChannel, TokenIdx, TokenType,
};

/// Helper function to check the properties of a single token that is supposed
/// to span the entire contents.
fn check_single_real_token(
    source: &str,
    token: TokenIdx,
    buffer: &DetachedTokenizedBuffer,
    token_type: TokenType,
    token_channel: TokenChannel,
    payload: Payload,
) {
    // for single tests line should always be 1
    let start_line = 1;

    // compare line count with the number of lines in the contents
    // for singles we should have consumed all the lines
    let end_line = source.lines().count() as u32;

    // for single tests column should always be 0
    let start_column = 0;

    // to compare the end column, we need to calculate the offset from the last line
    // lines iterator doesn't include the newline character, so we need to add 1
    // if the contents ends with a newline
    let last_line_end_column =
        source.lines().last().unwrap().chars().count() as u32 + u32::from(source.ends_with('\n'));

    // compare start and end byte offsets with the length of the contents
    let start_offset = 0;
    let end_byte_offset = source.len() as u32;
    let end_char_offset = source.chars().count() as u32;

    // we are not testing ephemeral tokens here, so we should always have text
    // and match the entire contents
    let token_text = Some(source);

    check_token(
        source,
        token,
        buffer,
        start_offset,
        end_byte_offset,
        start_offset,
        end_char_offset,
        start_line,
        end_line,
        start_column,
        last_line_end_column,
        token_type,
        token_channel,
        payload,
        token_text,
    );
}

fn check_single_lexeme(
    source: &str,
    token_type: TokenType,
    token_channel: TokenChannel,
    payload: Payload,
) {
    let (buffer, errors) = lex(source).unwrap();

    assert_eq!(errors.len(), 0, "Expected no errors, got {}", errors.len());

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

    check_single_real_token(
        source,
        tokens[0],
        &buffer,
        token_type,
        token_channel,
        payload,
    );
}

#[test]
fn test_unicode_char_offset() {
    let source = "'🔥'";
    let (buffer, errors) = lex(source).unwrap();

    assert_eq!(errors.len(), 0, "Expected no errors, got {}", errors.len());

    let token = buffer.into_iter().next().unwrap();

    assert_eq!(
        buffer.get_token_end(token).get(),
        3,
        "Expected a char offset of 3, got {}",
        buffer.get_token_end(token).get()
    );

    assert_eq!(
        buffer.get_token_end_byte_offset(token).get(),
        source.len() as u32,
        "Expected a byte offset of {}, got {}",
        source.len(),
        buffer.get_token_end_byte_offset(token).get()
    );

    // Now test the ubiquotous Hoe many characters is 🤦🏼‍♂️ case.
    // We want python compatibility here. So 5 characters.
    let source = "'🤦🏼‍♂️'";
    let (buffer, errors) = lex(source).unwrap();

    assert_eq!(errors.len(), 0, "Expected no errors, got {}", errors.len());

    let token = buffer.into_iter().next().unwrap();

    assert_eq!(
        buffer.get_token_end(token).get(),
        7, // 5 characters + 2 quotes
        "Expected a char offset of 7, got {}",
        buffer.get_token_end(token).get()
    );
}

#[test]
fn test_column_count_with_bom() {
    let source = "\u{FEFF}/* this is comment */";

    let (buffer, errors) = lex(source).unwrap();

    assert_eq!(errors.len(), 0, "Expected no errors, got {}", errors.len());

    let token = buffer.into_iter().next().unwrap();

    assert_eq!(
        buffer.get_token_start_column(token),
        0,
        "Expected a start column 0, got {}",
        buffer.get_token_start_column(token)
    );
}

#[rstest]
#[case::mixed_ws("\n\t \n", TokenType::WS, TokenChannel::HIDDEN, Payload::None)]
#[case::cstyle_comment_single_line(
    "/* this is comment */",
    TokenType::CStyleComment,
    TokenChannel::COMMENT,
    Payload::None
)]
#[case::cstyle_comment_multi_line(
    "/* this is 🔥\n comment */",
    TokenType::CStyleComment,
    TokenChannel::COMMENT,
    Payload::None
)]
fn test_single_lexemes(
    #[case] contents: &str,
    #[case] token_type: TokenType,
    #[case] token_channel: TokenChannel,
    #[case] payload: Payload,
) {
    check_single_lexeme(contents, token_type, token_channel, payload);
}

#[test]
fn test_unterminated_cstyle_comment() {
    let source = "/* unterminated comment*";

    let (buffer, errors) = lex(source).unwrap();

    let tokens: Vec<TokenIdx> = buffer.into_iter().collect();

    assert_eq!(
        tokens.len(),
        2,
        "Expected a 2 tokens (comment & EOF), got {}",
        tokens.len()
    );

    assert_eq!(
        buffer.get_token_type(tokens[1]),
        TokenType::EOF,
        "Expected EOF token, got {:?}",
        buffer.get_token_type(tokens[1])
    );

    // check the comment token
    check_single_real_token(
        source,
        tokens[0],
        &buffer,
        TokenType::CStyleComment,
        TokenChannel::COMMENT,
        Payload::None,
    );

    // check the error
    assert_eq!(errors.len(), 1, "Expected 1 errors, got {}", errors.len());

    check_error(
        &errors[0],
        ErrorType::UnterminatedComment,
        24,
        24,
        1,
        24,
        Some(tokens[0]),
    );
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
        TokenType::SingleQuotedStringLiteral,
        TokenChannel::DEFAULT,
        Payload::None,
    );
}

#[rstest]
#[case::bit_testing("'1'b", TokenType::SingleQuotedBitTestingLiteral)]
#[case::date("'4may2022'd", TokenType::SingleQuotedDateLiteral)]
#[case::datetime("'01may2021:12:30'dt", TokenType::SingleQuotedDateTimeLiteral)]
#[case::name("'unicode 🙏 col'n", TokenType::SingleQuotedNameLiteral)]
#[case::time("'00:42't", TokenType::SingleQuotedTimeLiteral)]
#[case::hex("'FF'x", TokenType::SingleQuotedHexStringLiteral)]
fn test_single_quoted_literals(#[case] contents: &str, #[case] token_type: TokenType) {
    check_single_lexeme(contents, token_type, TokenChannel::DEFAULT, Payload::None);
}

#[test]
fn test_unterminated_string_literal() {
    let source = "'unterminated string";

    let (buffer, errors) = lex(source).unwrap();

    let tokens: Vec<TokenIdx> = buffer.into_iter().collect();

    assert_eq!(
        tokens.len(),
        2,
        "Expected 2 tokens (string & EOF), got {}",
        tokens.len()
    );

    assert_eq!(
        buffer.get_token_type(tokens[1]),
        TokenType::EOF,
        "Expected EOF token, got {:?}",
        buffer.get_token_type(tokens[1])
    );

    // check the string literal token
    check_single_real_token(
        source,
        tokens[0],
        &buffer,
        TokenType::SingleQuotedStringLiteral,
        TokenChannel::DEFAULT,
        Payload::None,
    );

    // check the error
    assert_eq!(errors.len(), 1, "Expected 1 errors, got {}", errors.len());

    check_error(
        &errors[0],
        ErrorType::UnterminatedStringLiteral,
        20,
        20,
        1,
        20,
        Some(tokens[0]),
    );
}
