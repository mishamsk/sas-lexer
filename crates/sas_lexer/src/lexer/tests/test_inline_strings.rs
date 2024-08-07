use std::vec;

use crate::Payload;
use crate::{error::ErrorType, lex, TokenChannel, TokenType};
use rstest::rstest;

use super::super::token_type::KEYWORDS;
use super::util::{assert_lexing, ErrorTestCase, TokenTestCase};

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
#[case::cstyle_comment_multi_line(
    "%* this is 🔥\n macro comment;",
    (TokenType::MacroComment,
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
    let (starts, ending) = if body.contains(';') {
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
            (ending, TokenType::SEMI, TokenChannel::DEFAULT),
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
            ("input", TokenType::KwInput),
            (" ", TokenType::WS),
            ("datalines4", TokenType::Identifier),
            (";", TokenType::SEMI),
        ],
        NO_ERRORS,
    );
}

#[rstest]
#[case(";", TokenType::SEMI)]
#[case("&", TokenType::AMP)]
#[case("&&&", TokenType::AMP)]
#[case("%", TokenType::PERCENT)]
#[case("(", TokenType::LPAREN)]
#[case(")", TokenType::RPAREN)]
#[case("{", TokenType::LCURLY)]
#[case("}", TokenType::RCURLY)]
#[case("[", TokenType::LBRACK)]
#[case("]", TokenType::RBRACK)]
#[case("*", TokenType::STAR)]
#[case("!", TokenType::EXCL)]
#[case("!!", TokenType::EXCL2)]
#[case("¦", TokenType::BPIPE)]
#[case("¦¦", TokenType::BPIPE2)]
#[case("||", TokenType::PIPE2)]
#[case("**", TokenType::STAR2)]
#[case("¬", TokenType::NOT)]
#[case("^", TokenType::NOT)]
#[case("~", TokenType::NOT)]
#[case("∘", TokenType::NOT)]
#[case("/", TokenType::FSLASH)]
#[case("+", TokenType::PLUS)]
#[case("-", TokenType::MINUS)]
#[case("><", TokenType::GTLT)]
#[case("<>", TokenType::LTGT)]
#[case("<", TokenType::LT)]
#[case("<=", TokenType::LE)]
#[case("¬=", TokenType::NE)]
#[case("^=", TokenType::NE)]
#[case("~=", TokenType::NE)]
#[case("∘=", TokenType::NE)]
#[case(">", TokenType::GT)]
#[case(">=", TokenType::GE)]
#[case("=*", TokenType::SoundsLike)]
#[case("|", TokenType::PIPE)]
#[case(".", TokenType::DOT)]
#[case(",", TokenType::COMMA)]
#[case(":", TokenType::COLON)]
#[case("=", TokenType::ASSIGN)]
#[case("$", TokenType::DOLLAR)]
#[case("@", TokenType::AT)]
#[case("#", TokenType::HASH)]
#[case("?", TokenType::QUESTION)]
#[case("*';", (TokenType::TermQuote, TokenChannel::HIDDEN))]
#[case("*\";", (TokenType::TermQuote, TokenChannel::HIDDEN))]
fn test_all_single_symbols(#[case] contents: &str, #[case] expected_token: impl TokenTestCase) {
    assert_lexing(contents, vec![expected_token], NO_ERRORS);
}

#[test]
fn test_all_single_keywords() {
    KEYWORDS.keys().for_each(|keyword| {
        // Change every odd character to uppercase, and every even character to lowercase
        let mangled_keyword = keyword
            .chars()
            .enumerate()
            .map(|(i, c)| {
                if i % 2 == 0 {
                    c.to_ascii_lowercase()
                } else {
                    c.to_ascii_uppercase()
                }
            })
            .collect::<String>();

        assert_lexing(
            mangled_keyword.as_str(),
            vec![(KEYWORDS.get(keyword).copied().unwrap())],
            NO_ERRORS,
        );
    });
}

#[rstest]
#[case::simple("myvar", vec![TokenType::Identifier] , NO_ERRORS)]
#[case::underscore("_myvar",vec![TokenType::Identifier], NO_ERRORS)]
#[case::unicode("тест",vec![TokenType::Identifier], NO_ERRORS)]
#[case::with_num("_myvar9", vec![TokenType::Identifier], NO_ERRORS)]
#[case::err_copy("_myvar©", 
    vec![
        ("_myvar", TokenType::Identifier, TokenChannel::DEFAULT),
        ("©", TokenType::UNKNOWN, TokenChannel::HIDDEN)
        ], 
    vec![ErrorType::UnknownCharacter('©')]
)]
#[case::num_start("9_9myvar", 
    vec![
        ("9", TokenType::IntegerLiteral, Payload::Integer(9)),
        ("_9myvar", TokenType::Identifier, Payload::None),
        ], 
    NO_ERRORS
)]
fn test_identifier(
    #[case] contents: &str,
    #[case] expected_token: Vec<impl TokenTestCase>,
    #[case] expected_error: Vec<impl ErrorTestCase>,
) {
    assert_lexing(contents, expected_token, expected_error);
}

#[rstest]
#[case::char10("$CHAR10.", vec![TokenType::CharFormat])]
#[case::wd_none("$.", vec![TokenType::CharFormat])]
#[case::wd_width("$5.", vec![TokenType::CharFormat])]
// This is really not valid in SAS, but whatever
#[case::wd_prec("$.2", vec![TokenType::CharFormat])]
#[case::wd_all("$5.2", vec![TokenType::CharFormat])]
#[case::cust_unicode("$тест.", vec![TokenType::CharFormat])]
#[case::cust_end_underscore("$CCM_Phys_TempT3_.", vec![TokenType::CharFormat])]
#[case::not_format_num("$9", 
    vec![
        ("$", TokenType::DOLLAR, Payload::None),        
        ("9", TokenType::IntegerLiteral, Payload::Integer(9)),
        ]
)]
#[case::not_format_char("$f", 
    vec![
        ("$", TokenType::DOLLAR, Payload::None),        
        ("f", TokenType::Identifier, Payload::None),
        ]
)]
fn test_char_format(
    #[case] contents: &str,
    #[case] expected_token: Vec<impl TokenTestCase>,    
) {
    assert_lexing(contents, expected_token, NO_ERRORS);
}

#[rstest]
// Decimal notation
#[case::int1("1", (TokenType::IntegerLiteral, 1))]
#[case::int001("001", (TokenType::IntegerLiteral, 1))]
#[case::int001_plus("+001", (TokenType::IntegerLiteral, 1))]
#[case::int001_minus("-001", (TokenType::IntegerLiteral, -1))]
#[case::int1_dot("1.", (TokenType::IntegerDotLiteral, 1))]
#[case::int_dot_0(".0", (TokenType::IntegerDotLiteral, 0))]
#[case::int1_dot00("1.00", (TokenType::IntegerDotLiteral, 1))]
#[case::int01_dot("01.", (TokenType::IntegerLiteral, 1))]
#[case::int01_dot00("01.00", (TokenType::IntegerLiteral, 1))]
#[case::int1_minus_dot("-1.", (TokenType::IntegerLiteral, -1))]
#[case::int01_minus_dot("-01.", (TokenType::IntegerLiteral, -1))]
#[case::int01_plus_dot00("+01.00", (TokenType::IntegerLiteral, 1))]
#[case::pos_dec_only("+.1", (TokenType::FloatLiteral, 0.1))]
#[case::neg_dec_only("-.1", (TokenType::FloatLiteral, -0.1))]
// one more than i64::MAX
#[case::i64_overlow("9223372036854775808", (TokenType::FloatLiteral, 9223372036854775808.0))]
// Hexadecimal notation
#[case::hex("02Ax", (TokenType::IntegerLiteral, 42))]
#[case::hex_one_digit("9X", (TokenType::IntegerLiteral, 9))]
#[case::hex_max("-9ffFFffFFffFFffFx", (TokenType::FloatLiteral, -1.152921504606847e19))]
// Scientific notation
#[case::sci("1e3", (TokenType::FloatLiteral, 1000.0))]
#[case::sci_plus("1E+3", (TokenType::FloatLiteral, 1000.0))]
#[case::sci_minus("1e-3", (TokenType::FloatLiteral, 0.001))]
#[case::sci_neg("-1E3", (TokenType::FloatLiteral, -1000.0))]
#[case::sci_pos_plus("+1e+3", (TokenType::FloatLiteral, 1000.0))]
#[case::sci_neg_minus("-1E-3", (TokenType::FloatLiteral, -0.001))]
#[case::sci_dot("4.2e3", (TokenType::FloatLiteral, 4200.0))]
#[case::sci_dot_only(".1E3", (TokenType::FloatLiteral, 100.0))]
#[case::sci_pos_dot("+4.2e3", (TokenType::FloatLiteral, 4200.0))]
#[case::sci_neg_dot_only("-.1E3", (TokenType::FloatLiteral, -100.0))]
fn test_numeric_literal(#[case] contents: &str, #[case] expected_token: impl TokenTestCase) {
    assert_lexing(contents, vec![expected_token], NO_ERRORS);
}

#[rstest]
#[case(".1-", (TokenType::FloatLiteral, 0.0), ErrorType::InvalidNumericLiteral)]
#[case(".1e", (TokenType::FloatLiteral, 0.0), ErrorType::InvalidNumericLiteral)]
#[case(".1E", (TokenType::FloatLiteral, 0.0), ErrorType::InvalidNumericLiteral)]
#[case("02A", (TokenType::IntegerLiteral, 42), ErrorType::UnterminatedHexNumericLiteral)]
#[case("-9ffFFffFFffFFffF123AFx", (TokenType::FloatLiteral, -1.152921504606847e19), ErrorType::InvalidNumericLiteral)]
fn test_numeric_literal_error_recovery(
    #[case] contents: &str,
    #[case] expected_token: impl TokenTestCase,
    #[case] expected_error: ErrorType,
) {
    assert_lexing(contents, vec![expected_token], vec![expected_error]);
}

#[rstest]
#[case::empty("%nrstr()")]
#[case::with_ws_before_paren("%nrstr \t()")]
#[case::two_precent("%nrstr(%%)")]
#[case::three_precent("%nrstr(%%%))")]
#[case::all_quote_types("%nrstr(%(%)%%)")]
#[case::with_newline("%nrstr(some\nother)")]
#[case::with_crlf("%nrstr(some\r\nother)")]
#[case::with_unicode("%nrstr(some\n🔥\n)")]
#[case::with_macro_chars("%nrstr(&9 and &&&, 9% and % )")]
#[case::with_real_macro("%nrstr(%some() and &mvar)")]
fn test_nrstr_quoted_string_literal(#[case] contents: &str) {
    assert_lexing(
        contents,
        vec![TokenType::NrStrLiteral],
        NO_ERRORS,
    );
}

#[rstest]
// TODO: the following requires adding ErrorTestCase implementation with all params
// #[case::missing_open_paren("%nrstr  )", ErrorType::MissingExpectedCharacter('('))]
#[case::missing_closing_paren("%nrstr(%%%)", ErrorType::MissingExpected(")"))]
fn test_nrstr_quoted_str_error_recovery(#[case] contents: &str, #[case] expected_error: ErrorType) {
    assert_lexing(
        contents,
        vec![TokenType::NrStrLiteral],
        vec![expected_error],
    );
}