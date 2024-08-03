#![allow(clippy::cast_possible_truncation)]
mod util;

use std::vec;

use rstest::rstest;
use sas_lexer::{error::ErrorType, lex, TokenChannel, TokenType};

use util::{assert_lexing, TokenTestCase};

const NO_ERRORS: Vec<ErrorType> = vec![];

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
#[case::mixed_ws("\n\t \n", TokenType::WS)]
#[case::cstyle_comment_single_line(
    "/* this is comment */",
    (TokenType::CStyleComment,
    TokenChannel::COMMENT)
)]
#[case::cstyle_comment_multi_line(
    "/* this is 🔥\n comment */",
    (TokenType::CStyleComment,
    TokenChannel::COMMENT)
)]
fn test_single_comment_ws(#[case] contents: &str, #[case] expected_token: impl TokenTestCase) {
    assert_lexing(contents, vec![expected_token], NO_ERRORS);
}

#[test]
fn test_unterminated_cstyle_comment() {
    let source = "/* unterminated comment*";

    assert_lexing(
        source,
        vec![(TokenType::CStyleComment, TokenChannel::COMMENT)],
        vec![ErrorType::UnterminatedComment],
    );
}

#[rstest]
#[case::empty("")]
#[case::escaped("some#other")] // we use the #, to let the test function put the right quotes
#[case::with_newline("some\nother")]
#[case::with_crlf("some\r\nother")]
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

    assert_lexing(
        format!("{quote}{contents}{quote}").as_str(),
        vec![TokenType::StringLiteral],
        NO_ERRORS,
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
    // Test both lowercase and uppercase suffixes
    for suffix in [suffix, suffix.to_ascii_uppercase().as_str()] {
        assert_lexing(
            format!("{quote}{contents}{quote}{suffix}").as_str(),
            vec![token_type],
            NO_ERRORS,
        );
    }
}

#[rstest]
fn test_unterminated_string_literal(
    #[values("'unterminated string", "\"unterminated string")] source: &str,
) {
    assert_lexing(
        source,
        vec![(TokenType::StringLiteral, TokenChannel::DEFAULT)],
        vec![ErrorType::UnterminatedStringLiteral],
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
        let source = format!("\"{contents} {macro_content} {contents} {macro_content}\"{suffix}");

        assert_lexing(
            source.as_str(),
            vec![
                ("\"", TokenType::StringExprStart),
                (&format!("{contents} "), TokenType::StringExprText),
                (macro_content, TokenType::MacroVarExpr),
                (&format!(" {contents} "), TokenType::StringExprText),
                (macro_content, TokenType::MacroVarExpr),
                (&format!("\"{suffix}"), end_type),
            ],
            NO_ERRORS,
        );
    }
}

#[rstest]
#[case::simple("body,1,2")]
#[case::with_macro("&mv,%mcall()")]
#[case::with_newline("datalines\nother")]
#[case::with_unicode("some\n🔥\n")]
#[case::with_semi("; datalines; other")]
fn test_datalines(#[values("", ";", ";\n\t/*comment*/  ")] prefix: &str, #[case] body: &str) {
    // choose the right keywords to test
    let (starts, ending) = if body.contains(";") {
        (
            ["dAtALiNeS4", "lInEs4", "cArDs4", "cArDs4  ", "cArDs4\n"],
            ";;;;",
        )
    } else {
        (["dAtALiNeS", "lInEs", "cArDs", "cArDs  ", "cArDs\n"], ";")
    };

    let (buffer, _) = lex(&prefix).unwrap();

    let prefix_expected_tokens = if prefix.is_empty() {
        vec![]
    } else {
        buffer
            .into_iter()
            .filter(|t| buffer.get_token_type(*t) != TokenType::EOF)
            .map(|t| {
                (
                    buffer.get_token_text(t, &prefix).unwrap(),
                    buffer.get_token_type(t),
                    buffer.get_token_channel(t),
                )
            })
            .collect()
    };

    for start_sans_semi in starts {
        // Construct the source string
        let full_start = format!("{start_sans_semi};");

        let source = format!("{prefix}{full_start}{body}{ending}");

        let mut expected_tokens = prefix_expected_tokens.clone();

        expected_tokens.extend([
            (
                full_start.as_ref(),
                TokenType::DatalinesStart,
                TokenChannel::DEFAULT,
            ),
            (body, TokenType::DatalinesData, TokenChannel::DEFAULT),
            (ending, TokenType::DatalinesEnd, TokenChannel::DEFAULT),
        ]);

        assert_lexing(source.as_str(), expected_tokens, NO_ERRORS);
    }
}

#[test]
fn test_not_datalines() {
    let source = "input datalines4;";
    assert_lexing(
        source,
        vec![
            // this is temporary, will fail when we add keywords
            ("input", TokenType::BaseIdentifier),
            (" ", TokenType::WS),
            ("datalines4", TokenType::BaseIdentifier),
            (";", TokenType::SEMI),
        ],
        NO_ERRORS,
    );
}
