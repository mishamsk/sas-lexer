#![allow(clippy::cast_possible_truncation)]
mod util;

use rstest::rstest;
use util::{check_error, check_token};

use sas_lexer::{
    error::ErrorType, lex, Payload, TokenChannel, TokenIdx, TokenType, TokenizedBuffer,
};

/// Helper function to check the properties of a single token that is supposed
/// to span the entire contents.
fn check_single_real_token(
    source: &str,
    token: TokenIdx,
    buffer: &TokenizedBuffer,
    token_type: TokenType,
    token_channel: TokenChannel,
    payload: Payload,
) {
    // for single tests line should always be 1
    let start_line = 1;

    // compare line count with the number of lines in the contents
    // for singles we should have consumed all the lines
    let line_count = source.lines().count() as u32;

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
        line_count,
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
    let (buffer, errors) = lex(&source).unwrap();

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
    let (buffer, errors) = lex(&source).unwrap();

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
    let (buffer, errors) = lex(&source).unwrap();

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

    let (buffer, errors) = lex(&source).unwrap();

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

    let (buffer, errors) = lex(&source).unwrap();

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
#[case::empty("")]
#[case::escaped("some#other")] // we use the #, to let the test function put the right quotes
#[case::with_newline("some\nother")]
#[case::with_unicode("some\n🔥\n")]
#[case::with_macro_chars("&9 and &&&, 9% and %%%")]
#[case::with_real_macro("%some() and &mvar")]
fn test_string_literal(#[case] contents: &str, #[values('\'', '"')] quote: char) {
    if contents == "%some() and &mvar" && quote == '"' {
        return; // Skip this case, as it is not a string literal
    }

    let contents = if contents.contains('#') {
        contents.replace('#', format!("{quote}{quote}").as_str())
    } else {
        contents.to_string()
    };

    check_single_lexeme(
        format!("{quote}{contents}{quote}").as_str(),
        TokenType::StringLiteral,
        TokenChannel::DEFAULT,
        Payload::None,
    );
}

#[rstest]
#[case::bit_testing("1", "b", TokenType::BitTestingLiteral)]
#[case::date("4may2022", "d", TokenType::DateLiteral)]
#[case::datetime("01may2021:12:30", "dt", TokenType::DateTimeLiteral)]
#[case::name("unicode 🙏 col", "n", TokenType::NameLiteral)]
#[case::time("00:42", "t", TokenType::TimeLiteral)]
#[case::hex("FF", "x", TokenType::HexStringLiteral)]
fn test_non_string_literals(
    #[case] contents: &str,
    #[case] suffix: &str,
    #[case] token_type: TokenType,
    #[values('\'', '"')] quote: char,
) {
    // lowercase suffix
    check_single_lexeme(
        format!("{quote}{contents}{quote}{suffix}").as_str(),
        token_type,
        TokenChannel::DEFAULT,
        Payload::None,
    );

    // uppercase suffix
    let suffix = suffix.to_uppercase();

    check_single_lexeme(
        format!("{quote}{contents}{quote}{suffix}").as_str(),
        token_type,
        TokenChannel::DEFAULT,
        Payload::None,
    );
}

#[rstest]
fn test_unterminated_string_literal(
    #[values("'unterminated string", "\"unterminated string")] source: &str,
) {
    let (buffer, errors) = lex(&source).unwrap();

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
        TokenType::StringLiteral,
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

#[rstest]
#[case::escaped("some\"\"other")]
#[case::with_newline("some\nother")]
#[case::with_unicode("some\n🔥\n")]
#[case::with_macro_chars("&9 and &&&, 9% and %%%")]
fn test_string_expr(
    #[case] contents: &str,
    // todo: add macro call & statement
    #[values("&&var&c", "&var")] macro_content: &str,
    #[values(
        TokenType::StringExprEnd,
        TokenType::BitTestingLiteralExprEnd,
        TokenType::DateLiteralExprEnd,
        TokenType::DateTimeLiteralExprEnd,
        TokenType::NameLiteralExprEnd,
        TokenType::TimeLiteralExprEnd,
        TokenType::HexStringLiteralExprEnd
    )]
    end_type: TokenType,
) {
    // get the suffix
    let suffix = match end_type {
        TokenType::StringExprEnd => "",
        TokenType::BitTestingLiteralExprEnd => "b",
        TokenType::DateLiteralExprEnd => "d",
        TokenType::DateTimeLiteralExprEnd => "dt",
        TokenType::NameLiteralExprEnd => "n",
        TokenType::TimeLiteralExprEnd => "t",
        TokenType::HexStringLiteralExprEnd => "x",
        _ => unreachable!(),
    };

    // Test both lowercase and uppercase suffixes
    for suffix in [suffix, suffix.to_ascii_uppercase().as_str()] {
        // Construct the source string
        let source = format!("\"{contents} {macro_content}\"{suffix}");

        let (buffer, errors) = lex(&source).unwrap();

        assert_eq!(errors.len(), 0, "Expected no errors, got {}", errors.len());

        let tokens: Vec<TokenIdx> = buffer.into_iter().collect();

        // Must be 5 tokens: start, text, macro, end of corresponding type, EOF
        assert_eq!(
            tokens.len(),
            5,
            "Expected 5 tokens (start, text, macro, end, EOF), got {}",
            tokens.len()
        );

        // Check the start token
        check_token(
            &source,
            tokens[0],
            &buffer,
            0,
            1,
            0,
            1,
            1,
            1,
            0,
            1,
            TokenType::StringExprStart,
            TokenChannel::DEFAULT,
            Payload::None,
            Some("\""),
        );

        // Check the text token

        // extra 1 for the space
        let text_tok_bytes = 1 + contents.len() as u32;
        let text_tok_chars = 1 + contents.chars().count() as u32;
        // we add space after the newline, so it will extend the line count
        let text_tok_lines = contents.lines().count() as u32 + u32::from(contents.ends_with('\n'));
        let text_tok_end_column = if text_tok_lines > 1 {
            if contents.ends_with('\n') {
                1 // because of the space after the newline
            } else {
                1 + contents.lines().last().unwrap().chars().count() as u32
            }
        } else {
            text_tok_chars + 1
        };

        check_token(
            &source,
            tokens[1],
            &buffer,
            1,
            1 + text_tok_bytes,
            1,
            1 + text_tok_chars,
            1,
            text_tok_lines,
            1,
            text_tok_end_column,
            TokenType::StringExprText,
            TokenChannel::DEFAULT,
            Payload::None,
            Some(&format!("{contents} ")),
        );

        // Check the macro token
        let macro_tok_bytes = macro_content.len() as u32;

        check_token(
            &source,
            tokens[2],
            &buffer,
            1 + text_tok_bytes,
            1 + text_tok_bytes + macro_tok_bytes,
            1 + text_tok_chars,
            1 + text_tok_chars + macro_tok_bytes,
            text_tok_lines,
            1,
            text_tok_end_column,
            text_tok_end_column + macro_tok_bytes,
            TokenType::MacroVarExpr,
            TokenChannel::DEFAULT,
            Payload::None,
            Some(macro_content),
        );

        // Check the end token
        let end_tok_bytes = 1 + suffix.len() as u32;

        check_token(
            &source,
            tokens[3],
            &buffer,
            1 + text_tok_bytes + macro_tok_bytes,
            1 + text_tok_bytes + macro_tok_bytes + end_tok_bytes,
            1 + text_tok_chars + macro_tok_bytes,
            1 + text_tok_chars + macro_tok_bytes + end_tok_bytes,
            text_tok_lines,
            1,
            text_tok_end_column + macro_tok_bytes,
            text_tok_end_column + macro_tok_bytes + end_tok_bytes,
            end_type,
            TokenChannel::DEFAULT,
            Payload::None,
            Some(&format!("\"{suffix}")),
        );
    }
}
