use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::vec;

use crate::Payload;
use crate::{error::ErrorType, lex, TokenChannel, TokenType};
use rstest::{fixture, rstest};

use super::super::token_type::{KEYWORDS, MKEYWORDS};
use super::util::{assert_lexing, mangle_case, ErrorTestCase, TokenTestCase};

const NO_ERRORS: Vec<ErrorType> = vec![];

impl Hash for TokenType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (*self as u16).hash(state);
    }
}

#[fixture]
fn kwm_to_str_map() -> HashMap<TokenType, String> {
    HashMap::from_iter(MKEYWORDS.into_iter().map(|(k, v)| (*v, k.to_string())))
}

#[test]
fn test_unicode_char_offset() {
    let source = "'🔥'";
    let (buffer, errors) = lex(&source).unwrap();

    assert_eq!(errors.len(), 0, "Expected no errors, got {}", errors.len());

    let token = buffer.into_iter().next().unwrap();

    assert_eq!(
        buffer.get_token_end(token).expect("wrong token").get(),
        3,
        "Expected a char offset of 3, got {}",
        buffer.get_token_end(token).expect("wrong token").get()
    );

    assert_eq!(
        buffer
            .get_token_end_byte_offset(token)
            .expect("wrong token")
            .get(),
        source.len() as u32,
        "Expected a byte offset of {}, got {}",
        source.len(),
        buffer
            .get_token_end_byte_offset(token)
            .expect("wrong token")
            .get()
    );

    // Now test the ubiquotous Hoe many characters is 🤦🏼‍♂️ case.
    // We want python compatibility here. So 5 characters.
    let source = "'🤦🏼‍♂️'";
    let (buffer, errors) = lex(&source).unwrap();

    assert_eq!(errors.len(), 0, "Expected no errors, got {}", errors.len());

    let token = buffer.into_iter().next().unwrap();

    assert_eq!(
        buffer.get_token_end(token).expect("wrong token").get(),
        7, // 5 characters + 2 quotes
        "Expected a char offset of 7, got {}",
        buffer.get_token_end(token).expect("wrong token").get()
    );
}

#[test]
fn test_column_count_with_bom() {
    let source = "\u{FEFF}/* this is comment */";

    let (buffer, errors) = lex(&source).unwrap();

    assert_eq!(errors.len(), 0, "Expected no errors, got {}", errors.len());

    let token = buffer.into_iter().next().unwrap();

    assert_eq!(
        buffer.get_token_start_column(token).expect("wrong token"),
        0,
        "Expected a start column 0, got {}",
        buffer.get_token_start_column(token).expect("wrong token")
    );
}

#[test]
fn test_end_line_with_empty_tok() {
    let source = "%eval(\nne 1)";

    let expected_tokens = vec![
        ("%eval", TokenType::KwmEval, Payload::None),
        ("(", TokenType::LPAREN, Payload::None),
        ("\n", TokenType::WS, Payload::None),
        ("", TokenType::MacroStringEmpty, Payload::None),
        ("ne", TokenType::KwNE, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("1", TokenType::IntegerLiteral, Payload::Integer(1)),
        (")", TokenType::RPAREN, Payload::None),
    ];

    assert_lexing(source, expected_tokens, NO_ERRORS);

    let (buffer, _) = lex(&source).unwrap();

    // Get the empty token MacroStringEmpty and check the end line
    let token = buffer.into_iter().nth(3).unwrap();

    assert_eq!(
        buffer.get_token_end_line(token).expect("wrong token"),
        2,
        "Expected an end line 2, got {}",
        buffer.get_token_end_line(token).expect("wrong token")
    );
}

#[rstest]
#[case::mixed_ws("\n\t \n", TokenType::WS)]
#[case::cstyle_comment_single_line(
    "/* this is comment */",
    (TokenType::CStyleComment,
    TokenChannel::COMMENT)
)]
#[case::cstyle_comment_multi_line_and_unicode(
    "/* this is 🔥\n comment */",
    (TokenType::CStyleComment,
    TokenChannel::COMMENT)
)]
#[case::macro_comment_multi_line_and_unicode(
    "%* this is 🔥\n macro comment;",
    (TokenType::MacroComment,
    TokenChannel::COMMENT)
)]
fn test_single_comment_ws(#[case] contents: &str, #[case] expected_token: impl TokenTestCase) {
    assert_lexing(contents, vec![expected_token], NO_ERRORS);
}

#[rstest]
#[case::empty_macro_comment_with_follower(
    "%*;after",
    vec![("%*;", TokenType::MacroComment), ("after", TokenType::Identifier)]
)]
#[case::cstyle_double_comment(
    "%put /* nestest /* comment */  yes*/;",
    vec![
        ("%put", TokenType::KwmPut),
        (" ", TokenType::WS),
        ("/* nestest /* comment */", TokenType::CStyleComment),
        ("  ", TokenType::WS),
        ("yes*/", TokenType::MacroString),
        (";", TokenType::SEMI),
    ]
)]
fn test_comments(#[case] contents: &str, #[case] expected_token: Vec<impl TokenTestCase>) {
    assert_lexing(contents, expected_token, NO_ERRORS);
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
#[case::with_newline("some\nother")]
// Comment is part of the string, for double quoted too!
#[case::with_comment("some/*b*/other")]
#[case::with_crlf("some\r\nother")]
#[case::with_unicode("some\n🔥\n")]
#[case::with_macro_chars("&9 and &&&, 9% and %%%")]
#[case::with_real_macro("%some() and &mvar")]
fn test_string_literal_no_escape(#[case] contents: &str, #[values('\'', '"')] quote: char) {
    if contents == "%some() and &mvar" && quote == '"' {
        return; // Skip this case, as it is not a string literal
    }

    assert_lexing(
        format!("{quote}{contents}{quote}").as_str(),
        vec![TokenType::StringLiteral],
        NO_ERRORS,
    );
}

#[rstest]
// we use the #, to let the test function put the right quotes
#[case::only_escape("#")]
#[case::mid("some#other")]
#[case::start_end("#some#")]
#[case::consecutive("#some#")]
#[case::with_newline("some#\n#other")]
#[case::with_crlf("some#\r\n#other")]
#[case::with_unicode("some#🔥#")]
fn test_string_literal_with_escape(#[case] contents: &str, #[values('\'', '"')] quote: char) {
    let test_contents = contents.replace('#', format!("{quote}{quote}").as_str());
    let expected_contents = contents.replace('#', format!("{quote}").as_str());

    assert_lexing(
        format!("{quote}{test_contents}{quote}").as_str(),
        vec![(
            TokenType::StringLiteral,
            Payload::StringLiteral(0, expected_contents.len() as u32),
            expected_contents.as_ref(),
        )],
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
    #[values("'", "'unterminated string", "\"", "\"unterminated string")] source: &str,
) {
    assert_lexing(
        source,
        vec![(TokenType::StringLiteral, TokenChannel::DEFAULT)],
        vec![ErrorType::UnterminatedStringLiteral],
    );
}

#[rstest]
#[case::with_newline("some\nother")]
#[case::with_unicode("some\n🔥\n")]
#[case::with_macro_chars("&9 and &&&, 9% and %%%")]
fn test_string_expr_with_macro_no_escape(
    #[case] contents: &str,
    #[values(
        ("&&var&c", TokenType::MacroVarExpr),
        ("&var", TokenType::MacroVarExpr),
        ("%mcall", TokenType::MacroIdentifier)
    )]
    macro_content: (&str, TokenType),
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
        let source = format!(
            "\"{contents} {} {contents} {}\"{suffix}",
            macro_content.0, macro_content.0
        );

        assert_lexing(
            source.as_str(),
            vec![
                ("\"", TokenType::StringExprStart),
                (&format!("{contents} "), TokenType::StringExprText),
                macro_content,
                (&format!(" {contents} "), TokenType::StringExprText),
                macro_content,
                (&format!("\"{suffix}"), end_type),
            ],
            NO_ERRORS,
        );
    }
}

#[rstest]
#[case::with_macro_and_escapes("\"pre\"\"%t(\"\")\"\"p💪st\n&mv\"\"tail\"",
    vec![
        ("\"", TokenType::StringExprStart, Payload::None, "\""),
        ("pre\"\"", TokenType::StringExprText, Payload::StringLiteral(0, 4), "pre\""),
        ("%t", TokenType::MacroIdentifier, Payload::None, "%t"),
        ("(", TokenType::LPAREN, Payload::None, "("),
        ("\"\"", TokenType::StringLiteral, Payload::None, "\"\""),
        (")", TokenType::RPAREN, Payload::None, ")"),
        ("\"\"p💪st\n", TokenType::StringExprText, Payload::StringLiteral(4, 13), "\"p💪st\n"),
        ("&mv", TokenType::MacroVarExpr, Payload::None, "&mv"),
        ("\"\"tail", TokenType::StringExprText, Payload::StringLiteral(13, 18), "\"tail"),
        ("\"", TokenType::StringExprEnd, Payload::None, "\""),
    ]
)]
#[case::with_unlexed_macro_comment("\"%* not a comment;\"",
    vec![
        ("\"%* not a comment;\"", TokenType::StringLiteral),        
    ]
)]
#[case::with_unlexed_macro_comment_in_macro("%put \"%* not a comment;\"",
    vec![
        ("%put", TokenType::KwmPut),
        (" ", TokenType::WS),
        ("\"%* not a comment;\"", TokenType::StringLiteral),        
        ("", TokenType::SEMI),
    ]
)]
#[case::with_unlexed_cstyle_comment("\"/*not a comment*/\"",
    vec![
        ("\"/*not a comment*/\"", TokenType::StringLiteral),        
    ]
)]
fn test_complex_string_expr(
    #[case] contents: &str,
    #[case] expected_token: Vec<impl TokenTestCase>,
) {
    assert_lexing(contents, expected_token, NO_ERRORS);
}

#[rstest]
#[case::macro_stat_inside("%put \"%let v=1;\";",
    vec![
        ("%put", TokenType::KwmPut),
        (" ", TokenType::WS),
        ("\"", TokenType::StringExprStart),        
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("v", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ("\"", TokenType::StringExprEnd),
        (";", TokenType::SEMI),        
        ],
    vec![
        (ErrorType::OpenCodeRecursionError, 6),
        ]
)]
fn test_string_expr_error_recovery(
    #[case] contents: &str,
    #[case] expected_token: Vec<impl TokenTestCase>,
    #[case] expected_error: Vec<impl ErrorTestCase>,
) {
    assert_lexing(contents, expected_token, expected_error);
}

#[rstest]
fn test_string_expr_with_embedded_stat(
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
        let source = format!("\"this %let a=1; is possible\"{suffix}");

        assert_lexing(
            source.as_str(),
            vec![
                ("\"", TokenType::StringExprStart),
                ("this ", TokenType::StringExprText),
                ("%let", TokenType::KwmLet),
                (" ", TokenType::WS),
                ("a", TokenType::MacroString),
                ("=", TokenType::ASSIGN),
                ("1", TokenType::MacroString),
                (";", TokenType::SEMI),
                (" is possible", TokenType::StringExprText),
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
            .filter(|t| buffer.get_token_type(*t).expect("wrong token") != TokenType::EOF)
            .map(|t| {
                (
                    buffer
                        .get_token_raw_text(t, &prefix)
                        .expect("wrong token")
                        .unwrap(),
                    buffer.get_token_type(t).expect("wrong token"),
                    buffer.get_token_channel(t).expect("wrong token"),
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
// This is now predicted as a comment stat missing the closing ; at EOF
// #[case("*", TokenType::STAR)]
#[case("!", TokenType::EXCL)]
#[case("!!", TokenType::EXCL2)]
#[case("¦", TokenType::BPIPE)]
#[case("¦¦", TokenType::BPIPE2)]
#[case("||", TokenType::PIPE2)]
// This is now predicted as a comment stat missing the closing ; at EOF
// #[case("**", TokenType::STAR2)]
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
#[case("*';", TokenType::PredictedCommentStat)]
#[case("*\";", TokenType::PredictedCommentStat)]
fn test_all_single_symbols(#[case] contents: &str, #[case] expected_token: impl TokenTestCase) {
    assert_lexing(contents, vec![expected_token], NO_ERRORS);
}

#[test]
fn test_all_single_non_macro_keywords() {
    KEYWORDS.into_iter().for_each(|(keyword, tok_type)| {
        // Change every odd character to uppercase, and every even character to lowercase
        let mangled_keyword = mangle_case(keyword);

        assert_lexing(mangled_keyword.as_str(), vec![(*tok_type)], NO_ERRORS);
    });
}

/// This tests that we are correctly identifying ascii only keywords
/// and thus handling case insensitivity correctly.
#[rstest]
fn test_keywords_followed_by_unicode(
    #[values(("-", TokenType::MINUS), ("+", TokenType::PLUS))] (keyword, keyword_tok): (
        &str,
        TokenType,
    ),
) {
    // Change every odd character to uppercase, and every even character to lowercase
    let mut mangled_keyword = mangle_case(keyword);
    mangled_keyword.push('🔥');

    assert_lexing(
        mangled_keyword.as_str(),
        vec![
            (keyword, keyword_tok, TokenChannel::DEFAULT),
            ("🔥", TokenType::UNKNOWN, TokenChannel::HIDDEN),
        ],
        vec![ErrorType::UnexpectedCharacter],
    );
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
    vec![ErrorType::UnexpectedCharacter]
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
fn test_char_format(#[case] contents: &str, #[case] expected_token: Vec<impl TokenTestCase>) {
    assert_lexing(contents, expected_token, NO_ERRORS);
}

#[rstest]
// Decimal notation
#[case::int1("1", (TokenType::IntegerLiteral, 1))]
#[case::int001("001", (TokenType::IntegerLiteral, 1))]
#[case::int1_dot("1.", (TokenType::FloatLiteral, 1.))]
#[case::int_dot_0(".0", (TokenType::FloatLiteral, 0.))]
#[case::int1_dot00("1.00", (TokenType::FloatLiteral, 1.))]
#[case::int01_dot("01.", (TokenType::FloatLiteral, 1.))]
#[case::int01_dot00("01.00", (TokenType::FloatLiteral, 1.))]
// one more than u64::MAX
#[case::u64_overlow("18446744073709551616", (TokenType::FloatLiteral, 18446744073709551616.0))]
// Hexadecimal notation
#[case::hex("02Ax", (TokenType::IntegerLiteral, 42))]
#[case::hex_one_digit("9X", (TokenType::IntegerLiteral, 9))]
// Scientific notation
#[case::sci("1e3", (TokenType::FloatExponentLiteral, 1000.0))]
#[case::sci_plus("1E+3", (TokenType::FloatExponentLiteral, 1000.0))]
#[case::sci_minus("1e-3", (TokenType::FloatExponentLiteral, 0.001))]
#[case::sci_dot("4.2e3", (TokenType::FloatExponentLiteral, 4200.0))]
#[case::sci_dot_only(".1E3", (TokenType::FloatExponentLiteral, 100.0))]
#[case::sci_dot_only_no_decimal("1.E3", (TokenType::FloatExponentLiteral, 1000.0))]
// Almost ambiguous cases
#[case::sci_or_hex("1E1", (TokenType::FloatExponentLiteral, 10.0))]
fn test_numeric_literal(#[case] contents: &str, #[case] expected_token: impl TokenTestCase) {
    assert_lexing(contents, vec![expected_token], NO_ERRORS);
}

/// First numeric lexing implementation was also attaching the leading sign to the numeric literal.
/// But this would not play well with things like integer ranges and input statement mini-language.
/// Hence these tests.
#[rstest]
// Decimal notation
#[case::int001_minus("001", ("001", TokenType::IntegerLiteral, Payload::Integer(1)))]
#[case::int1_minus_dot("1.", ("1.", TokenType::FloatLiteral, Payload::Float(1.)))]
#[case::int01_minus_dot("01.", ("01.", TokenType::FloatLiteral, Payload::Float(1.)))]
#[case::neg_dec_only(".1", (".1", TokenType::FloatLiteral, Payload::Float(0.1)))]
#[case::hex_max("9ffFFffFFffFFffFx", ("9ffFFffFFffFFffFx", TokenType::IntegerLiteral, Payload::Integer(11529215046068469759)))]
#[case::sci_neg("1E3", ("1E3", TokenType::FloatExponentLiteral, Payload::Float(1000.0)))]
#[case::sci_neg_minus("1E-3", ("1E-3", TokenType::FloatExponentLiteral, Payload::Float(0.001)))]
#[case::sci_neg_dot_only(".1E3", (".1E3", TokenType::FloatExponentLiteral, Payload::Float(100.0)))]
fn test_numeric_literal_with_leading_sign(
    #[case] contents: &str,
    #[case] expected_token: (&str, TokenType, Payload),
    #[values(("-", TokenType::MINUS), ("+", TokenType::PLUS))] (sign_str, sign_tok): (
        &str,
        TokenType,
    ),
) {
    assert_lexing(
        format!("{sign_str}{contents}").as_str(),
        vec![(sign_str, sign_tok, Payload::None), expected_token],
        NO_ERRORS,
    );
}

// Makes sure that traiing sings are not causing erros in scientific notation
#[rstest]
#[case::sci("1e3", (TokenType::FloatExponentLiteral, 1000.0))]
#[case::sci_plus("1E+3", (TokenType::FloatExponentLiteral, 1000.0))]
#[case::sci_minus("1e-3", (TokenType::FloatExponentLiteral, 0.001))]
#[case::sci_dot("4.2e3", (TokenType::FloatExponentLiteral, 4200.0))]
#[case::sci_dot_only(".1E3", (TokenType::FloatExponentLiteral, 100.0))]
fn test_scientific_numeric_literal_with_suffix(
    #[case] contents: &str,
    #[case] expected_token: (TokenType, f64),
    #[values(("-", TokenType::MINUS), ("+", TokenType::PLUS))] (sign_str, sign_tok): (
        &str,
        TokenType,
    ),
) {
    let (token_type, payload) = expected_token;

    assert_lexing(
        format!("{contents}{sign_str}").as_str(),
        vec![
            (contents, token_type, Payload::Float(payload)),
            (sign_str, sign_tok, Payload::None),
        ],
        NO_ERRORS,
    );
}

#[rstest]
#[case(".1e", (TokenType::FloatLiteral, 0.0), ErrorType::InvalidNumericLiteral)]
#[case(".1E", (TokenType::FloatLiteral, 0.0), ErrorType::InvalidNumericLiteral)]
#[case("02A", (TokenType::IntegerLiteral, 42), ErrorType::UnterminatedHexNumericLiteral)]
#[case("9ffFFffFFffFFffF123AFx", (TokenType::FloatLiteral, 1.2089258196146292e25), ErrorType::InvalidNumericLiteral)]
fn test_numeric_literal_error_recovery(
    #[case] contents: &str,
    #[case] expected_token: impl TokenTestCase,
    #[case] expected_error: ErrorType,
) {
    assert_lexing(contents, vec![expected_token], vec![expected_error]);
}

#[test]
fn test_numeric_literal_range() {
    assert_lexing(
        "1-2",
        vec![
            ("1", TokenType::IntegerLiteral, Payload::Integer(1)),
            ("-", TokenType::MINUS, Payload::None),
            ("2", TokenType::IntegerLiteral, Payload::Integer(2)),
        ],
        NO_ERRORS,
    );
}

#[rstest]
#[case::open_brace_val("%LeT a=(;",
    vec![
        ("%LeT", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("a", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("(", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::close_brace_val("%lEt a=);",
    vec![
        ("%lEt", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("a", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        (")", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::text_expr_name("%let a&mv=&mv.b=;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("a", TokenType::MacroString),
        ("&mv", TokenType::MacroVarExpr),
        ("=", TokenType::ASSIGN),
        ("&mv.", TokenType::MacroVarExpr),
        ("b=", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::text_expr_name_xid_cont_after_macro("%let &mv.9=1;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("&mv.", TokenType::MacroVarExpr),
        ("9", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),        
        (";", TokenType::SEMI),
        ]
)]
#[case::dot_delim_mvar_in_name("%let a&b.c=2;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("a", TokenType::MacroString),
        ("&b.", TokenType::MacroVarExpr),
        ("c", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("2", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::lead_trail_ws("%let a \n=   1 1  ;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("a", TokenType::MacroString),
        (" \n", TokenType::WS),
        ("=", TokenType::ASSIGN),
        ("   ", TokenType::WS),
        // Trailing whitespace is not significant in SAS, but
        // we defer to parser to trim it          
        ("1 1  ", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::mcall_parens_trail("%let a&a1%t()=1;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("a", TokenType::MacroString),
        ("&a1", TokenType::MacroVarExpr),
        ("%t", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        (")", TokenType::RPAREN),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::macro_call_sequence("%let %t()%t()=1;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("%t", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        (")", TokenType::RPAREN),
        ("%t", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        (")", TokenType::RPAREN),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::nested_macro_calls("%let a&a1%t(%t())=1;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("a", TokenType::MacroString),
        ("&a1", TokenType::MacroVarExpr),
        ("%t", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        ("%t", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        (")", TokenType::RPAREN),
        (")", TokenType::RPAREN),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::tail_macro_call_no_parens("%let a&a1%t=1;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("a", TokenType::MacroString),
        ("&a1", TokenType::MacroVarExpr),
        ("%t", TokenType::MacroIdentifier),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::double_tail_macro_call_no_parens("%let a&a1%t%t=1;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("a", TokenType::MacroString),
        ("&a1", TokenType::MacroVarExpr),
        ("%t", TokenType::MacroIdentifier),
        ("%t", TokenType::MacroIdentifier),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::macro_call_in_the_middle("%let a%t()a2=1;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("a", TokenType::MacroString),
        ("%t", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        (")", TokenType::RPAREN),
        ("a2", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::underscore_start("%let _9v=1;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("_9v", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::leading_comment("%let /*com*/_9v=1;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("/*com*/", TokenType::CStyleComment),
        ("_9v", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::inline_comment("%let _9/*com*/v=1;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("_9", TokenType::MacroString),
        ("/*com*/", TokenType::CStyleComment),
        ("v", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::eval_macro_call("%let ev%eval(1+1)=2;",
    vec![
        ("%let", TokenType::KwmLet, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("ev", TokenType::MacroString, Payload::None),
        ("%eval", TokenType::KwmEval, Payload::None),
        ("(", TokenType::LPAREN, Payload::None),
        ("1", TokenType::IntegerLiteral, Payload::Integer(1)),
        ("+", TokenType::PLUS, Payload::None),
        ("1", TokenType::IntegerLiteral, Payload::Integer(1)),
        (")", TokenType::RPAREN, Payload::None),
        ("=", TokenType::ASSIGN, Payload::None),
        ("2", TokenType::MacroString, Payload::None),
        (";", TokenType::SEMI, Payload::None),
        ]
)]
#[case::symexist_macro_call("%let ev%symexist(a1)=2;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("ev", TokenType::MacroString),
        ("%symexist", TokenType::KwmSymExist),
        ("(", TokenType::LPAREN),
        ("a1", TokenType::MacroString),
        (")", TokenType::RPAREN),
        ("=", TokenType::ASSIGN),
        ("2", TokenType::MacroString),
        (";", TokenType::SEMI),
       ]
)]
#[case::unquote_macro_call("%let ev%unquote(%quote(&a1))=2;",
    vec![
       ("%let",TokenType::KwmLet),
       (" " ,TokenType::WS),
       ("ev" ,TokenType::MacroString),
       ("%unquote" ,TokenType::KwmUnquote),
       ("(" ,TokenType::LPAREN),
       ("%quote" ,TokenType::KwmQuote),
       ("(" ,TokenType::LPAREN),
       ("&a1" ,TokenType::MacroVarExpr),
       (")" ,TokenType::RPAREN),
       (")" ,TokenType::RPAREN),
       ("=" ,TokenType::ASSIGN),
       ("2" ,TokenType::MacroString),
       (";" ,TokenType::SEMI),
       ]
)]
#[case::ws_in_inline_macro_call("%let pre_%t /*c*/ ()_post=v;",
    vec![
       ("%let" ,TokenType::KwmLet),
       (" " ,TokenType::WS),
       ("pre_" ,TokenType::MacroString),
       ("%t" ,TokenType::MacroIdentifier),
       (" " ,TokenType::WS),
       ("/*c*/" ,TokenType::CStyleComment),
       (" " ,TokenType::WS),
       ("(" ,TokenType::LPAREN),
       (")" ,TokenType::RPAREN),
       ("_post" ,TokenType::MacroString),
       ("=" ,TokenType::ASSIGN),
       ("v" ,TokenType::MacroString),
       (";" ,TokenType::SEMI),
       ]
)]
#[case::complex_value("%let a='%t();' \"%t();\";",
    vec![
       ("%let" ,TokenType::KwmLet),
       (" " ,TokenType::WS),
       ("a" ,TokenType::MacroString),
       ("=" ,TokenType::ASSIGN),
       ("'%t();'" ,TokenType::StringLiteral),
       (" " ,TokenType::MacroString),
       ("\"" ,TokenType::StringExprStart),
       ("%t" ,TokenType::MacroIdentifier),
       ("(" ,TokenType::LPAREN),
       (")" ,TokenType::RPAREN),
       (";" ,TokenType::StringExprText),
       ("\"" ,TokenType::StringExprEnd),
       (";" ,TokenType::SEMI),
      ]
)]
fn test_macro_let(#[case] contents: &str, #[case] expected_token: Vec<impl TokenTestCase>) {
    assert_lexing(contents, expected_token, NO_ERRORS);
}

#[rstest]
#[case::dot_delim_macro_call("%let a%m.b=1;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("a", TokenType::MacroString),
        ("%m", TokenType::MacroIdentifier),
        // Recovered from missing assign hence empty string
        ("", TokenType::ASSIGN),
        (".b=1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ],
    // Real SAS error will be: 
    // /* ERROR: Symbolic variable name a[resolved %m call].b must contain only letters, digits, and underscores. */
    vec![(ErrorType::MissingExpectedAssign, 8)]
)]
#[case::miss_assign("%let a b=1;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("a", TokenType::MacroString),
        (" ", TokenType::WS),
        // Recovered from missing assign hence empty string
        ("", TokenType::ASSIGN),
        ("b=1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ],
    vec![(ErrorType::MissingExpectedAssign, 7)]
)]
#[case::miss_assign_2("%let a &mv=1;",
vec![
    ("%let", TokenType::KwmLet),
    (" ", TokenType::WS),
    ("a", TokenType::MacroString),
    (" ", TokenType::WS),
    // Recovered from missing assign hence empty string
    ("", TokenType::ASSIGN),
    ("&mv", TokenType::MacroVarExpr),
    ("=1", TokenType::MacroString),
    (";", TokenType::SEMI),
    ],
    vec![(ErrorType::MissingExpectedAssign, 7)]
)]
#[case::miss_name_str_literal("%let 'v'=v;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        // Recovered from missing assign hence empty string
        ("", TokenType::ASSIGN),
        ("'v'", TokenType::StringLiteral),
        ("=v", TokenType::MacroString),
        (";", TokenType::SEMI),
        ],
    vec![
        (ErrorType::InvalidMacroLetVarName, 5),
        (ErrorType::MissingExpectedAssign, 5)
        ]
)]
#[case::miss_name_at_end("%let",
    vec![
        ("%let", TokenType::KwmLet),
        // Recovered from missing assign hence empty string
        ("", TokenType::ASSIGN),        
        // Recovered, hence empty string
        // But doesn't produce an error!!!
        ("", TokenType::SEMI),        
        ],
    vec![
        (ErrorType::InvalidMacroLetVarName, 4),
        (ErrorType::MissingExpectedAssign, 4)
        ]
)]
#[case::miss_name_wrong_ident_start("%let 9v=1;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        // Recovered from missing assign hence empty string
        ("", TokenType::ASSIGN),
        ("9v=1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ],
    vec![
        (ErrorType::InvalidMacroLetVarName, 5),
        (ErrorType::MissingExpectedAssign, 5)
        ]
)]
// For the following tests, the real SAS error is something like:
// /* ERROR: Symbolic variable name a must contain only letters, digits, and underscores. */
// The underlying reason is that all quote calls add invisible characters to the name
// which are not allowed in SAS variable names
#[case::quote_call_in_name_1("%let a%nrstr(a)=2;",
vec![
    ("%let", TokenType::KwmLet, TokenChannel::DEFAULT),
    (" ", TokenType::WS, TokenChannel::HIDDEN),
    ("a", TokenType::MacroString, TokenChannel::DEFAULT),
    // Recovered from missing assign hence empty string
    ("", TokenType::ASSIGN, TokenChannel::DEFAULT),
    ("%nrstr", TokenType::KwmNrStr, TokenChannel::HIDDEN),
    ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
    ("a", TokenType::MacroString, TokenChannel::DEFAULT),
    (")", TokenType::RPAREN, TokenChannel::HIDDEN),
    ("=2", TokenType::MacroString, TokenChannel::DEFAULT),
    (";", TokenType::SEMI, TokenChannel::DEFAULT),
    ],
    vec![(ErrorType::MissingExpectedAssign, 6)]
)]
#[case::quote_call_in_name_2("%let a%quote(a)=2;",
vec![
    ("%let", TokenType::KwmLet),
    (" ", TokenType::WS),
    ("a", TokenType::MacroString),
    // Recovered from missing assign hence empty string
    ("", TokenType::ASSIGN),
    ("%quote", TokenType::KwmQuote),
    ("(", TokenType::LPAREN),
    ("a", TokenType::MacroString),
    (")", TokenType::RPAREN),
    ("=2", TokenType::MacroString),
    (";", TokenType::SEMI)
    ],
    vec![(ErrorType::MissingExpectedAssign, 6)]
)]
#[case::quote_call_in_name_3("%let a%str(%inner())=2;",
vec![
    ("%let", TokenType::KwmLet, TokenChannel::DEFAULT),
    (" ", TokenType::WS, TokenChannel::HIDDEN),
    ("a", TokenType::MacroString, TokenChannel::DEFAULT),
    // Recovered from missing assign hence empty string
    ("", TokenType::ASSIGN, TokenChannel::DEFAULT),
    ("%str", TokenType::KwmStr, TokenChannel::HIDDEN),
    ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
    ("%inner", TokenType::MacroIdentifier, TokenChannel::DEFAULT),
    ("(", TokenType::LPAREN, TokenChannel::DEFAULT),
    (")", TokenType::RPAREN, TokenChannel::DEFAULT),
    (")", TokenType::RPAREN, TokenChannel::HIDDEN),
    ("=2", TokenType::MacroString, TokenChannel::DEFAULT),
    (";", TokenType::SEMI, TokenChannel::DEFAULT)
    ],
    vec![(ErrorType::MissingExpectedAssign, 6)]
)]
fn test_macro_let_error_recovery(
    #[case] contents: &str,
    #[case] expected_token: Vec<impl TokenTestCase>,
    #[case] expected_error: Vec<impl ErrorTestCase>,
) {
    assert_lexing(contents, expected_token, expected_error);
}

#[rstest]
#[case::basic_call("%t(1,2);",
    vec![
        ("%t", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        ("1", TokenType::MacroString),
        (",", TokenType::COMMA),
        ("2", TokenType::MacroString),
        (")", TokenType::RPAREN),
        (";", TokenType::SEMI),
        ]
)]
#[case::nested_call("%t(some(),2);",
    vec![
        ("%t", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        ("some()", TokenType::MacroString),
        (",", TokenType::COMMA),
        ("2", TokenType::MacroString),
        (")", TokenType::RPAREN),
        (";", TokenType::SEMI),
    ]
)]
#[case::nested_call_with_comment("%t/*c*/(/*c*/some()/*c*/,/*c*/2/*c*/)/*c*/;",
    vec![
        ("%t", TokenType::MacroIdentifier),
        ("/*c*/", TokenType::CStyleComment),
        ("(", TokenType::LPAREN),
        ("/*c*/", TokenType::CStyleComment),
        ("some()", TokenType::MacroString),
        ("/*c*/", TokenType::CStyleComment),
        (",", TokenType::COMMA),
        ("/*c*/", TokenType::CStyleComment),
        ("2", TokenType::MacroString),
        ("/*c*/", TokenType::CStyleComment),
        (")", TokenType::RPAREN),
        ("/*c*/", TokenType::CStyleComment),
        (";", TokenType::SEMI),
    ]
)]
#[case::balanced_parens_with_comment("%t((/*c*/))",
    vec![
        ("%t", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        ("(", TokenType::MacroString),
        ("/*c*/", TokenType::CStyleComment),
        (")", TokenType::MacroString),
        (")", TokenType::RPAREN),        
    ]
)]
#[case::balanced_parens_after_slash_with_comment("%t(/(/*c*/))",
    vec![
        ("%t", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        ("/(", TokenType::MacroString),
        ("/*c*/", TokenType::CStyleComment),
        (")", TokenType::MacroString),
        (")", TokenType::RPAREN),        
    ]
)]
#[case::bare_nested_parens("%t(some(),());",
    vec![
        ("%t", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        ("some()", TokenType::MacroString),
        (",", TokenType::COMMA),
        ("()", TokenType::MacroString),
        (")", TokenType::RPAREN),
        (";", TokenType::SEMI),
    ]
)]
#[case::comma_inside_nested_parens("%t(some(,),());",
    vec![
        ("%t", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        ("some(,)", TokenType::MacroString),
        (",", TokenType::COMMA),
        ("()", TokenType::MacroString),
        (")", TokenType::RPAREN),
        (";", TokenType::SEMI),
    ]
)]
#[case::kw_call("%tk(arg1=some(,));",
    vec![
        ("%tk", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        ("arg1", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("some(,)", TokenType::MacroString),
        (")", TokenType::RPAREN),
        (";", TokenType::SEMI),
    ]
)]
#[case::comma_inside_nested_parens_in_val("%tk(arg1=some(,),arg2=());",
    vec![
        ("%tk", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        ("arg1", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("some(,)", TokenType::MacroString),
        (",", TokenType::COMMA),
        ("arg2", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("()", TokenType::MacroString),
        (")", TokenType::RPAREN),
        (";", TokenType::SEMI),
    ]
)]
#[case::full_nested_call_like("%tk(arg1=some(,arg2=()),arg2=());",
    vec![
        ("%tk", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        ("arg1", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("some(,arg2=())", TokenType::MacroString),
        (",", TokenType::COMMA),
        ("arg2", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("()", TokenType::MacroString),
        (")", TokenType::RPAREN),
        (";", TokenType::SEMI),
    ]
)]
fn test_macro_call(#[case] contents: &str, #[case] expected_token: Vec<impl TokenTestCase>) {
    assert_lexing(contents, expected_token, NO_ERRORS);
}

#[rstest]
#[case::basic_call("%sTr( );",
    vec![
        ("%sTr", TokenType::KwmStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        (" ", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        (";", TokenType::SEMI, TokenChannel::DEFAULT),
        ]
)]
#[case::empty("%sTr()",
    vec![
        ("%sTr", TokenType::KwmStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),        
        ]
)]
#[case::with_macro_chars("%str(&9 and &&&, 9% and % )",
    vec![
        ("%str", TokenType::KwmStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("&9 and &&&, 9% and % ", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::with_unicode_newline_crf("%str(some\r\n🔥\n)",
    vec![
        ("%str", TokenType::KwmStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("some\r\n🔥\n", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::nested_str_call("%str(-->/*c*/ %str(/*c*/ )<--)",
    vec![
        ("%str", TokenType::KwmStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("-->", TokenType::MacroString, TokenChannel::DEFAULT),
        ("/*c*/", TokenType::CStyleComment, TokenChannel::COMMENT),
        (" ", TokenType::MacroString, TokenChannel::DEFAULT),
        ("%str", TokenType::KwmStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("/*c*/", TokenType::CStyleComment, TokenChannel::COMMENT),
        (" ", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ("<--", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::all_quotes("%sTr(%\"v %'v %%call %( %) );",
    vec![
        (
            "%sTr",
            TokenType::KwmStr,
            TokenChannel::HIDDEN,
            Payload::None,
            "%sTr",
        ),
        (
            "(",
            TokenType::LPAREN,
            TokenChannel::HIDDEN,
            Payload::None,
            "(",
        ),
        (
            "%\"v %'v %%call %( %) ",
            TokenType::MacroString,
            TokenChannel::DEFAULT,
            Payload::StringLiteral(0, 16),
            "\"v 'v %call ( ) ",
        ),
        (
            ")",
            TokenType::RPAREN,
            TokenChannel::HIDDEN,
            Payload::None,
            ")",
        ),
        (
            ";",
            TokenType::SEMI,
            TokenChannel::DEFAULT,
            Payload::None,
            ";",
        ),
    ]
)]
#[case::nested_parens("%sTr((1(2)3));",
    vec![
        ("%sTr", TokenType::KwmStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("(1(2)3)", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        (";", TokenType::SEMI, TokenChannel::DEFAULT),
        ]
)]
#[case::balanced_unquoted_parens("%str((/*c*/))",
    vec![
        ("%str", TokenType::KwmStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("(", TokenType::MacroString, TokenChannel::DEFAULT),
        ("/*c*/", TokenType::CStyleComment, TokenChannel::COMMENT),
        (")", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::balanced_unquoted_parens_after_slash("%str(/(/*c*/))",
    vec![
        ("%str", TokenType::KwmStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("/(", TokenType::MacroString, TokenChannel::DEFAULT),
        ("/*c*/", TokenType::CStyleComment, TokenChannel::COMMENT),
        (")", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::inline_str_expr("%StR(\");%t()\")",
    vec![
        ("%StR", TokenType::KwmStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("\"", TokenType::StringExprStart, TokenChannel::DEFAULT),
        (");", TokenType::StringExprText, TokenChannel::DEFAULT),
        ("%t", TokenType::MacroIdentifier, TokenChannel::DEFAULT),
        ("(", TokenType::LPAREN, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::DEFAULT),
        ("\"", TokenType::StringExprEnd, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::inline_str_expr_with_comment("%str(/*c*/\");/*c*/%t()\" a/*c*/a)",
    vec![
        ("%str", TokenType::KwmStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("/*c*/", TokenType::CStyleComment, TokenChannel::COMMENT),
        ("\"", TokenType::StringExprStart, TokenChannel::DEFAULT),
        (");/*c*/", TokenType::StringExprText, TokenChannel::DEFAULT),
        ("%t", TokenType::MacroIdentifier, TokenChannel::DEFAULT),
        ("(", TokenType::LPAREN, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::DEFAULT),
        ("\"", TokenType::StringExprEnd, TokenChannel::DEFAULT),
        (" a", TokenType::MacroString, TokenChannel::DEFAULT),
        ("/*c*/", TokenType::CStyleComment, TokenChannel::COMMENT),
        ("a", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::inline_str_lit_with_comment("%str(');/*c*/%t()' a)",
    vec![
        ("%str", TokenType::KwmStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("');/*c*/%t()'", TokenType::StringLiteral, TokenChannel::DEFAULT),
        (" a", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::nested_call_with_ws_comment("%str(pre;%t /*c*/ ()post)",
    vec![
        ("%str", TokenType::KwmStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("pre;", TokenType::MacroString, TokenChannel::DEFAULT),
        ("%t", TokenType::MacroIdentifier, TokenChannel::DEFAULT),
        (" ", TokenType::WS, TokenChannel::HIDDEN),
        ("/*c*/", TokenType::CStyleComment, TokenChannel::COMMENT),
        (" ", TokenType::WS, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::DEFAULT),
        ("post", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::nested_call_with_ws_comment_no_paren("%str(pre;%t /*c*/ post)",
    vec![
        ("%str", TokenType::KwmStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("pre;", TokenType::MacroString, TokenChannel::DEFAULT),
        ("%t", TokenType::MacroIdentifier, TokenChannel::DEFAULT),
        (" ", TokenType::MacroString, TokenChannel::DEFAULT),
        ("/*c*/", TokenType::CStyleComment, TokenChannel::COMMENT),
        (" post", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
fn test_macro_str_call(#[case] contents: &str, #[case] expected_token: Vec<impl TokenTestCase>) {
    assert_lexing(contents, expected_token, NO_ERRORS);
}

#[rstest]
#[case::macro_stat_inside("%str( %let v=1;);",
    vec![
        ("%str", TokenType::KwmStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        (" ", TokenType::MacroString, TokenChannel::DEFAULT),
        // Recovered from missing hence empty string        
        ("", TokenType::RPAREN, TokenChannel::HIDDEN),
        ("%let", TokenType::KwmLet, TokenChannel::DEFAULT),
        (" ", TokenType::WS, TokenChannel::HIDDEN),
        ("v", TokenType::MacroString, TokenChannel::DEFAULT),
        ("=", TokenType::ASSIGN, TokenChannel::DEFAULT),
        ("1", TokenType::MacroString, TokenChannel::DEFAULT),
        (";", TokenType::SEMI, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::DEFAULT),
        (";", TokenType::SEMI, TokenChannel::DEFAULT),
        ],
    vec![
        (ErrorType::OpenCodeRecursionError, 6),
        (ErrorType::MissingExpectedRParen, 6)
        ]
)]
fn test_macro_str_call_error_recovery(
    #[case] contents: &str,
    #[case] expected_token: Vec<impl TokenTestCase>,
    #[case] expected_error: Vec<impl ErrorTestCase>,
) {
    assert_lexing(contents, expected_token, expected_error);
}

#[rstest]
#[case::basic_call("%nrsTr( );",
    vec![
        ("%nrsTr", TokenType::KwmNrStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        (" ", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        (";", TokenType::SEMI, TokenChannel::DEFAULT),
        ]
)]
#[case::empty("%nrsTr()",
    vec![
        ("%nrsTr", TokenType::KwmNrStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),        
        ]
)]
#[case::with_macro_chars("%nrstr(&9 and &&&, 9% and % )",
    vec![
        ("%nrstr", TokenType::KwmNrStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("&9 and &&&, 9% and % ", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::with_unicode_newline_crf("%nrstr(some\r\n🔥\n)",
    vec![
        ("%nrstr", TokenType::KwmNrStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("some\r\n🔥\n", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::nested_str_call("%nrstr(-->/*c*/ %str(/*c*/ )<--)",
    vec![
        ("%nrstr", TokenType::KwmNrStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("-->", TokenType::MacroString, TokenChannel::DEFAULT),
        ("/*c*/", TokenType::CStyleComment, TokenChannel::COMMENT),
        (" %str(", TokenType::MacroString, TokenChannel::DEFAULT),
        ("/*c*/", TokenType::CStyleComment, TokenChannel::COMMENT),
        (" )<--", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::all_quotes("%nrsTr(%\"v %'v %%call %( %) );",
    vec![
        (
            "%nrsTr",
            TokenType::KwmNrStr,
            TokenChannel::HIDDEN,
            Payload::None,
            "%nrsTr",
        ),
        (
            "(",
            TokenType::LPAREN,
            TokenChannel::HIDDEN,
            Payload::None,
            "(",
        ),
        (
            "%\"v %'v %%call %( %) ",
            TokenType::MacroString,
            TokenChannel::DEFAULT,
            Payload::StringLiteral(0, 16),
            "\"v 'v %call ( ) ",
        ),
        (
            ")",
            TokenType::RPAREN,
            TokenChannel::HIDDEN,
            Payload::None,
            ")",
        ),
        (
            ";",
            TokenType::SEMI,
            TokenChannel::DEFAULT,
            Payload::None,
            ";",
        ),
    ]
)]
#[case::nested_parens("%nrsTr((1(2)3));",
    vec![
        ("%nrsTr", TokenType::KwmNrStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("(1(2)3)", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        (";", TokenType::SEMI, TokenChannel::DEFAULT),
        ]
)]
#[case::balanced_unquoted_parens("%nrstr((/*c*/))",
    vec![
        ("%nrstr", TokenType::KwmNrStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("(", TokenType::MacroString, TokenChannel::DEFAULT),
        ("/*c*/", TokenType::CStyleComment, TokenChannel::COMMENT),
        (")", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::balanced_unquoted_parens_after_slash("%nrstr(/(/*c*/))",
    vec![
        ("%nrstr", TokenType::KwmNrStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("/(", TokenType::MacroString, TokenChannel::DEFAULT),
        ("/*c*/", TokenType::CStyleComment, TokenChannel::COMMENT),
        (")", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::inline_str_expr("%nrStR(\");%t()\")",
    vec![
        ("%nrStR", TokenType::KwmNrStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("\"", TokenType::StringExprStart, TokenChannel::DEFAULT),
        (");", TokenType::StringExprText, TokenChannel::DEFAULT),
        ("%t", TokenType::MacroIdentifier, TokenChannel::DEFAULT),
        ("(", TokenType::LPAREN, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::DEFAULT),
        ("\"", TokenType::StringExprEnd, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::inline_str_expr_with_comment("%nrstr(/*c*/\");/*c*/%t()\" a/*c*/a)",
    vec![
        ("%nrstr", TokenType::KwmNrStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("/*c*/", TokenType::CStyleComment, TokenChannel::COMMENT),
        ("\"", TokenType::StringExprStart, TokenChannel::DEFAULT),
        (");/*c*/", TokenType::StringExprText, TokenChannel::DEFAULT),
        ("%t", TokenType::MacroIdentifier, TokenChannel::DEFAULT),
        ("(", TokenType::LPAREN, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::DEFAULT),
        ("\"", TokenType::StringExprEnd, TokenChannel::DEFAULT),
        (" a", TokenType::MacroString, TokenChannel::DEFAULT),
        ("/*c*/", TokenType::CStyleComment, TokenChannel::COMMENT),
        ("a", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::inline_str_lit_with_comment("%nrstr(');/*c*/%t()' a)",
    vec![
        ("%nrstr", TokenType::KwmNrStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("');/*c*/%t()'", TokenType::StringLiteral, TokenChannel::DEFAULT),
        (" a", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::nested_call_with_ws_comment("%nrstr(pre;%t /*c*/ ()post)",
    vec![
        ("%nrstr", TokenType::KwmNrStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("pre;%t ", TokenType::MacroString, TokenChannel::DEFAULT),
        ("/*c*/", TokenType::CStyleComment, TokenChannel::COMMENT),
        (" ()post", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
#[case::nested_call_with_ws_comment_no_paren("%nrstr(pre;%t /*c*/ post)",
    vec![
        ("%nrstr", TokenType::KwmNrStr, TokenChannel::HIDDEN),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
        ("pre;%t ", TokenType::MacroString, TokenChannel::DEFAULT),
        ("/*c*/", TokenType::CStyleComment, TokenChannel::COMMENT),
        (" post", TokenType::MacroString, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ]
)]
fn test_macro_nrstr_call(#[case] contents: &str, #[case] expected_token: Vec<impl TokenTestCase>) {
    assert_lexing(contents, expected_token, NO_ERRORS);
}

#[rstest]
#[case::missing_open_paren("%nrstr  )",
    vec![
        ("%nrstr", TokenType::KwmNrStr, TokenChannel::HIDDEN),
        ("  ", TokenType::WS, TokenChannel::HIDDEN),
        // Recovered from missing hence empty string
        ("", TokenType::LPAREN, TokenChannel::HIDDEN),
        (")", TokenType::RPAREN, TokenChannel::HIDDEN),
        ],
    vec![
        (ErrorType::MissingExpectedLParen, 8)
        ]
)]
#[case::missing_closing_paren("%nrstr(%%%)",
    vec![
        ("%nrstr", TokenType::KwmNrStr, TokenChannel::HIDDEN, Payload::None, "%nrstr"),
        ("(", TokenType::LPAREN, TokenChannel::HIDDEN, Payload::None, "("),
        ("%%%)", TokenType::MacroString, TokenChannel::DEFAULT, Payload::StringLiteral(0, 2), "%)"),
        // Recovered from missing hence empty string
        ("", TokenType::RPAREN, TokenChannel::HIDDEN, Payload::None, ""),
        ],
    vec![
        (ErrorType::MissingExpectedRParen, 11)
        ]
)]
fn test_macro_nrstr_call_error_recovery(
    #[case] contents: &str,
    #[case] expected_token: Vec<impl TokenTestCase>,
    #[case] expected_error: Vec<impl ErrorTestCase>,
) {
    assert_lexing(contents, expected_token, expected_error);
}

#[rstest]
#[case::macro_include(TokenType::KwmInclude)]
#[case::macro_list(TokenType::KwmList)]
#[case::macro_then(TokenType::KwmThen)]
#[case::macro_else(TokenType::KwmElse)]
fn test_macro_stats_with_no_expected_tail(
    #[case] tok_type: TokenType,
    kwm_to_str_map: HashMap<TokenType, String>,
) {
    // Get the string representation of the token type
    let tok_str = kwm_to_str_map.get(&tok_type).unwrap();

    // Prepend % and mangle case
    let tok_str = format!("%{}", mangle_case(tok_str));

    assert_lexing(tok_str.as_str(), vec![(tok_type)], NO_ERRORS);
}

/// These are super simple - just a keyword followed by a mandatory SEMI
#[rstest]
#[case::macro_end(TokenType::KwmEnd)]
#[case::macro_return(TokenType::KwmReturn)]
#[case::macro_run(TokenType::KwmRun)]
#[case::macro_sysmstoreclear(TokenType::KwmSysmstoreclear)]
fn test_macro_simple_stats(
    #[case] tok_type: TokenType,
    kwm_to_str_map: HashMap<TokenType, String>,
) {
    // Get the string representation of the token type
    let tok_str = kwm_to_str_map.get(&tok_type).unwrap();

    // Prepend % and mangle case
    let tok_str = format!("%{}", mangle_case(tok_str));

    // First test the correct case
    assert_lexing(
        format!("{tok_str};").as_str(),
        vec![(tok_str.as_str(), tok_type), (";", TokenType::SEMI)],
        NO_ERRORS,
    );

    // Recover from missing semicolon at EOF
    assert_lexing(
        format!("{tok_str}").as_str(),
        vec![(tok_str.as_str(), tok_type), ("", TokenType::SEMI)],
        NO_ERRORS,
    );

    // Recover from missing semicolon not at EOF, with error
    assert_lexing(
        format!("{tok_str}+").as_str(),
        vec![
            (tok_str.as_str(), tok_type),
            ("", TokenType::SEMI),
            ("+", TokenType::PLUS),
        ],
        vec![(ErrorType::MissingExpectedSemiOrEOF, tok_str.len())],
    );
}

/// These are simple - just a keyword, then unrestricted text expr
/// followed by a mandatory SEMI
#[rstest]
#[case::macro_put(TokenType::KwmPut)]
#[case::macro_sysexec(TokenType::KwmSysexec)]
fn test_macro_stats_with_semi_term_tail(
    #[case] tok_type: TokenType,
    kwm_to_str_map: HashMap<TokenType, String>,
) {
    // Get the string representation of the token type
    let tok_str = kwm_to_str_map.get(&tok_type).unwrap();

    // Prepend % and mangle case
    let tok_str = format!("%{}", mangle_case(tok_str));

    // Create a shared string without the ending semicolon
    let test_str = format!("{tok_str}/*c*/&mv-%mc() + / \"\"\"some&suf\" and 'lit'd /*t*/");
    let expected_tokens = vec![
        (tok_str.as_str(), tok_type, Payload::None, tok_str.as_str()),
        ("/*c*/", TokenType::CStyleComment, Payload::None, "/*c*/"),
        ("&mv", TokenType::MacroVarExpr, Payload::None, "&mv"),
        ("-", TokenType::MacroString, Payload::None, "-"),
        ("%mc", TokenType::MacroIdentifier, Payload::None, "%mc"),
        ("(", TokenType::LPAREN, Payload::None, "("),
        (")", TokenType::RPAREN, Payload::None, ")"),
        (" + / ", TokenType::MacroString, Payload::None, " + / "),
        ("\"", TokenType::StringExprStart, Payload::None, "\""),
        (
            "\"\"some",
            TokenType::StringExprText,
            Payload::StringLiteral(0, 5),
            "\"some",
        ),
        ("&suf", TokenType::MacroVarExpr, Payload::None, "&suf"),
        ("\"", TokenType::StringExprEnd, Payload::None, "\""),
        (" and ", TokenType::MacroString, Payload::None, " and "),
        ("'lit'd", TokenType::DateLiteral, Payload::None, "'lit'd"),
        (" ", TokenType::MacroString, Payload::None, " "),
        ("/*t*/", TokenType::CStyleComment, Payload::None, "/*t*/"),
    ];

    // First test the correct case
    assert_lexing(
        format!("{test_str};").as_str(),
        {
            let mut v = expected_tokens.clone();
            v.push((";", TokenType::SEMI, Payload::None, ";"));
            v
        },
        NO_ERRORS,
    );

    // Recover from missing semicolon at EOF
    assert_lexing(
        format!("{test_str}").as_str(),
        {
            let mut v = expected_tokens.clone();
            v.push(("", TokenType::SEMI, Payload::None, ""));
            v
        },
        NO_ERRORS,
    );

    // Recover from missing semicolon not at EOF, with error
    assert_lexing(
        format!("{test_str}%then").as_str(),
        {
            let mut v = expected_tokens.clone();
            v.push(("", TokenType::SEMI, Payload::None, ""));
            v.push(("%then", TokenType::KwmThen, Payload::None, "%then"));
            v
        },
        vec![
            (ErrorType::OpenCodeRecursionError, test_str.len()),
            (ErrorType::MissingExpectedSemiOrEOF, test_str.len()),
        ],
    );
}

/// These are more involved. They have various detailed syntaxes,
/// but we used shared mode that is between unrestricted and eval.
/// This test ignores the syntax and test the mode in general on
/// not so correct string. A separate test covers one realistical
/// example for each.
#[rstest]
#[case::macro_abort(TokenType::KwmAbort)]
#[case::macro_display(TokenType::KwmDisplay)]
#[case::macro_goto(TokenType::KwmGoto)]
#[case::macro_input(TokenType::KwmInput)]
#[case::macro_mend(TokenType::KwmMend)]
#[case::macro_symdel(TokenType::KwmSymdel)]
#[case::macro_syslput(TokenType::KwmSyslput)]
#[case::macro_sysrput(TokenType::KwmSysrput)]
#[case::macro_window(TokenType::KwmWindow)]
fn test_macro_stats_with_stat_opts_tail(
    #[case] tok_type: TokenType,
    kwm_to_str_map: HashMap<TokenType, String>,
) {
    // Get the string representation of the token type
    let tok_str = kwm_to_str_map.get(&tok_type).unwrap();

    // Prepend % and mangle case
    let tok_str = format!("%{}", mangle_case(tok_str));

    // Create a shared string without the ending semicolon
    let test_str = format!(
        "{tok_str}/*c*/var1 &pre.var%suf() / NOWARN opt=\"\"\"some&suf\" #5 a='lit'd /*t*/"
    );
    let expected_tokens = vec![
        (tok_str.as_str(), tok_type, Payload::None, tok_str.as_str()),
        ("/*c*/", TokenType::CStyleComment, Payload::None, "/*c*/"),
        ("var1", TokenType::MacroString, Payload::None, "var1"),
        (" ", TokenType::WS, Payload::None, " "),
        ("&pre.", TokenType::MacroVarExpr, Payload::None, "&pre."),
        ("var", TokenType::MacroString, Payload::None, "var"),
        ("%suf", TokenType::MacroIdentifier, Payload::None, "%suf"),
        ("(", TokenType::LPAREN, Payload::None, "("),
        (")", TokenType::RPAREN, Payload::None, ")"),
        (" ", TokenType::WS, Payload::None, " "),
        ("/", TokenType::FSLASH, Payload::None, "/"),
        (" ", TokenType::WS, Payload::None, " "),
        ("NOWARN", TokenType::MacroString, Payload::None, "NOWARN"),
        (" ", TokenType::WS, Payload::None, " "),
        ("opt", TokenType::MacroString, Payload::None, "opt"),
        ("=", TokenType::ASSIGN, Payload::None, "="),
        ("\"", TokenType::StringExprStart, Payload::None, "\""),
        (
            "\"\"some",
            TokenType::StringExprText,
            Payload::StringLiteral(0, 5),
            "\"some",
        ),
        ("&suf", TokenType::MacroVarExpr, Payload::None, "&suf"),
        ("\"", TokenType::StringExprEnd, Payload::None, "\""),
        (" ", TokenType::WS, Payload::None, " "),
        ("#5", TokenType::MacroString, Payload::None, "#5"),
        (" ", TokenType::WS, Payload::None, " "),
        ("a", TokenType::MacroString, Payload::None, "a"),
        ("=", TokenType::ASSIGN, Payload::None, "="),
        ("'lit'd", TokenType::DateLiteral, Payload::None, "'lit'd"),
        (" ", TokenType::WS, Payload::None, " "),
        ("/*t*/", TokenType::CStyleComment, Payload::None, "/*t*/"),
    ];

    // First test the correct case
    assert_lexing(
        format!("{test_str};").as_str(),
        {
            let mut v = expected_tokens.clone();
            v.push((";", TokenType::SEMI, Payload::None, ";"));
            v
        },
        NO_ERRORS,
    );

    // Recover from missing semicolon at EOF
    assert_lexing(
        format!("{test_str}").as_str(),
        {
            let mut v = expected_tokens.clone();
            v.push(("", TokenType::SEMI, Payload::None, ""));
            v
        },
        NO_ERRORS,
    );

    // Recover from missing semicolon not at EOF, with error
    assert_lexing(
        format!("{test_str}%then").as_str(),
        {
            let mut v = expected_tokens.clone();
            v.push(("", TokenType::SEMI, Payload::None, ""));
            v.push(("%then", TokenType::KwmThen, Payload::None, "%then"));
            v
        },
        vec![
            (ErrorType::OpenCodeRecursionError, test_str.len()),
            (ErrorType::MissingExpectedSemiOrEOF, test_str.len()),
        ],
    );
}

#[rstest]
#[case::open_parens_and_lead_trail_ws("%PuT \n  a=( ;",
    vec![
        ("%PuT", TokenType::KwmPut),
        (" \n  ", TokenType::WS),
        ("a=( ", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::close_parens_crlf_unicode_mvar("%pUt some\r\n🔥\nother&mv;",
vec![
    ("%pUt", TokenType::KwmPut),
    (" ", TokenType::WS),
    ("some\r\n🔥\nother", TokenType::MacroString),
    ("&mv", TokenType::MacroVarExpr),        
    (";", TokenType::SEMI),
    ]
)]
#[case::atypical_symbols("%put \nv=[&m_s.];",
    vec![
        ("%put", TokenType::KwmPut),
        (" \n", TokenType::WS),
        ("v=[", TokenType::MacroString),
        ("&m_s.", TokenType::MacroVarExpr),
        ("]", TokenType::MacroString),        
        (";", TokenType::SEMI),
        ]
)]
#[case::with_numeric_expr_symbols("%put\t\n \npre <s>=[&mv];",
    vec![
        ("%put", TokenType::KwmPut),
        ("\t\n \n", TokenType::WS),
        ("pre <s>=[", TokenType::MacroString),
        ("&mv", TokenType::MacroVarExpr),
        ("]", TokenType::MacroString),        
        (";", TokenType::SEMI),
        ]
)]
#[case::keywords_mixed_with_other("%put  _AlL_ /*c*/ pre post
                        _AuTOMATIC_ /*c*/ pre post
                        _GlOBAL_ /*c*/ pre post
                        _LoCAL_ /*c*/ pre post
                        _ReADONLY_ /*c*/ pre post
                        _UsER_ /*c*/ pre post
                        _WrITABLE_ /*c*/ pre post;",
    vec![
        ("%put", TokenType::KwmPut),
        ("  ", TokenType::WS),
        ("_AlL_ ", TokenType::MacroString),
        ("/*c*/", TokenType::CStyleComment),
        (" pre post\n                        _AuTOMATIC_ ", TokenType::MacroString),        
        ("/*c*/", TokenType::CStyleComment),
        (" pre post\n                        _GlOBAL_ ", TokenType::MacroString),        
        ("/*c*/", TokenType::CStyleComment),
        (" pre post\n                        _LoCAL_ ", TokenType::MacroString),        
        ("/*c*/", TokenType::CStyleComment),
        (" pre post\n                        _ReADONLY_ ", TokenType::MacroString),        
        ("/*c*/", TokenType::CStyleComment),
        (" pre post\n                        _UsER_ ", TokenType::MacroString),        
        ("/*c*/", TokenType::CStyleComment),
        (" pre post\n                        _WrITABLE_ ", TokenType::MacroString),
        ("/*c*/", TokenType::CStyleComment),
        (" pre post", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::string_literals("%put a='%t();' \"%t();\";",
    vec![
       ("%put" ,TokenType::KwmPut),
       (" " ,TokenType::WS),
       ("a=" ,TokenType::MacroString),
       ("'%t();'" ,TokenType::StringLiteral),
       (" " ,TokenType::MacroString),
       ("\"" ,TokenType::StringExprStart),
       ("%t" ,TokenType::MacroIdentifier),
       ("(" ,TokenType::LPAREN),
       (")" ,TokenType::RPAREN),
       (";" ,TokenType::StringExprText),
       ("\"" ,TokenType::StringExprEnd),
       (";" ,TokenType::SEMI),
       ]
    )]
#[case::macro_call_with_ws_comment("%put (pre%t /*c*/ ()post);",
    vec![
        ("%put", TokenType::KwmPut),
        (" ", TokenType::WS),
        ("(pre", TokenType::MacroString),
        ("%t", TokenType::MacroIdentifier),
        (" ", TokenType::WS),
        ("/*c*/", TokenType::CStyleComment),
        (" ", TokenType::WS),
        ("(", TokenType::LPAREN),
        (")", TokenType::RPAREN),
        ("post)", TokenType::MacroString),        
        (";" ,TokenType::SEMI),
        ]
)]
fn test_macro_put(#[case] contents: &str, #[case] expected_token: Vec<impl TokenTestCase>) {
    assert_lexing(contents, expected_token, NO_ERRORS);
}

#[rstest]
#[case::static_label("%goTO findit;",
    vec![
        ("%goTO", TokenType::KwmGoto),
        (" ", TokenType::WS),
        ("findit", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::text_expr("%goto &home;",
    vec![
        ("%goto", TokenType::KwmGoto),
        (" ", TokenType::WS),
        ("&home", TokenType::MacroVarExpr),
        (";", TokenType::SEMI),
        ]
)]
fn test_macro_goto(#[case] contents: &str, #[case] expected_token: Vec<impl TokenTestCase>) {
    assert_lexing(contents, expected_token, NO_ERRORS);
}

#[rstest]
#[case::star2_op("**", TokenType::STAR2)]
#[case::plus_op("+", TokenType::PLUS)]
#[case::minus_op("-", TokenType::MINUS)]
#[case::not_op("¬", TokenType::NOT)]
#[case::not_op("^", TokenType::NOT)]
#[case::not_op("~", TokenType::NOT)]
#[case::kwnot_op("nOt", TokenType::KwNOT)]
#[case::star_op("*", TokenType::STAR)]
#[case::fslash_op("/", TokenType::FSLASH)]
#[case::lt_op("<", TokenType::LT)]
#[case::kwlt_op("Lt", TokenType::KwLT)]
#[case::le_op("<=", TokenType::LE)]
#[case::kwle_op("Le", TokenType::KwLE)]
#[case::assign_op("=", TokenType::ASSIGN)]
#[case::kweq_op("Eq", TokenType::KwEQ)]
#[case::hash_op("#", TokenType::HASH)] // IN op in sas macro
#[case::kwin_op("In", TokenType::KwIN)]
#[case::ne_op("¬=", TokenType::NE)]
#[case::ne_op("^=", TokenType::NE)]
#[case::ne_op("~=", TokenType::NE)]
#[case::kwne_op("Ne", TokenType::KwNE)]
#[case::gt_op(">", TokenType::GT)]
#[case::kwgt_op("Gt", TokenType::KwGT)]
#[case::ge_op(">=", TokenType::GE)]
#[case::kwge_op("Ge", TokenType::KwGE)]
#[case::amp_op("&", TokenType::AMP)] // AND op in sas macro
#[case::amp_op("&&&", TokenType::AMP)] // multiples should also work
#[case::kwand_op("AnD", TokenType::KwAND)]
#[case::pipe_op("|", TokenType::PIPE)] // OR op in sas macro
#[case::kwor_op("Or", TokenType::KwOR)]
fn test_macro_integer_eval_expr_all_ops_simple(
    #[case] op_str: &str,
    #[case] expected_op_token: TokenType,
) {
    assert_lexing(
        format!("%eval(1 {op_str} \n2)").as_str(),
        vec![
            ("%eval", TokenType::KwmEval, Payload::None),
            ("(", TokenType::LPAREN, Payload::None),
            ("1", TokenType::IntegerLiteral, Payload::Integer(1)),
            (" ", TokenType::WS, Payload::None),
            (op_str, expected_op_token, Payload::None),
            (" \n", TokenType::WS, Payload::None),
            ("2", TokenType::IntegerLiteral, Payload::Integer(2)),
            (")", TokenType::RPAREN, Payload::None),
        ],
        NO_ERRORS,
    );
}

#[rstest]
#[case(";")]
#[case("%")]
#[case("{")]
#[case("}")]
#[case("[")]
#[case("]")]
#[case("!")]
#[case("!!")]
#[case("¦")]
#[case("¦¦")]
#[case("∘")]
#[case(".")]
#[case(",")]
#[case(":")]
#[case("$")]
#[case("@")]
fn test_macro_integer_eval_expr_not_real_ops(#[case] op_str: &str) {
    assert_lexing(
        format!("%eval(1 {op_str} \n2)").as_str(),
        vec![
            ("%eval", TokenType::KwmEval, Payload::None),
            ("(", TokenType::LPAREN, Payload::None),
            (
                format!("1 {op_str} \n2").as_ref(),
                TokenType::MacroString,
                Payload::None,
            ),
            (")", TokenType::RPAREN, Payload::None),
        ],
        NO_ERRORS,
    );
}

#[rstest]
#[case::not_mnemonic("not")]
#[case::lt_mnemonic("lt")]
#[case::le_mnemonic("le")]
#[case::eq_mnemonic("eq")]
#[case::in_mnemonic("in")]
#[case::ne_mnemonic("ne")]
#[case::gt_mnemonic("gt")]
#[case::ge_mnemonic("ge")]
#[case::and_mnemonic("and")]
#[case::or_mnemonic("or")]
fn test_macro_strings_with_mnemonics_eval_expr(#[case] op_str: &str) {
    assert_lexing(
        format!("%eVaL(pre{op_str} gt {op_str}post and p{op_str}e)").as_str(),
        vec![
            ("%eVaL", TokenType::KwmEval, Payload::None),
            ("(", TokenType::LPAREN, Payload::None),
            (
                format!("pre{op_str}").as_ref(),
                TokenType::MacroString,
                Payload::None,
            ),
            (" ", TokenType::WS, Payload::None),
            ("gt", TokenType::KwGT, Payload::None),
            (" ", TokenType::WS, Payload::None),
            (
                format!("{op_str}post").as_ref(),
                TokenType::MacroString,
                Payload::None,
            ),
            (" ", TokenType::WS, Payload::None),
            ("and", TokenType::KwAND, Payload::None),
            (" ", TokenType::WS, Payload::None),
            (
                format!("p{op_str}e").as_ref(),
                TokenType::MacroString,
                Payload::None,
            ),
            (")", TokenType::RPAREN, Payload::None),
        ],
        NO_ERRORS,
    );
}

#[rstest]
#[case::comma_and_semi(",eQ;", 
    vec![
        (",", TokenType::MacroString, Payload::None),
        ("eQ", TokenType::KwEQ, Payload::None),
        (";", TokenType::MacroString, Payload::None),
    ]
)]
#[case::not_integer_1("1 2 ~= 3 4", 
    vec![
        ("1 2", TokenType::MacroString, Payload::None),
        (" ", TokenType::WS, Payload::None),            
        ("~=", TokenType::NE, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("3 4", TokenType::MacroString, Payload::None),
    ]
)]
#[case::not_integer_2("1.2 ~= 3.4", 
    vec![
        ("1.2", TokenType::MacroString, Payload::None),
        (" ", TokenType::WS, Payload::None),            
        ("~=", TokenType::NE, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("3.4", TokenType::MacroString, Payload::None),
    ]
)]
#[case::looks_like_but_not_integer("12s ~= 01m3", 
    vec![
        ("12s", TokenType::MacroString, Payload::None),
        (" ", TokenType::WS, Payload::None),            
        ("~=", TokenType::NE, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("01m3", TokenType::MacroString, Payload::None),
    ]
)]
#[case::hex_integer_literals("0FFx < 9ffX",
    vec![
        ("0FFx", TokenType::IntegerLiteral, Payload::Integer(255)),
        (" ", TokenType::WS, Payload::None),            
        ("<", TokenType::LT, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("9ffX", TokenType::IntegerLiteral, Payload::Integer(2559)),        
    ]
)]
#[case::not_hex_integer_literals("0F_Fx eq fffX",
    vec![
        ("0F_Fx", TokenType::MacroString, Payload::None),
        (" ", TokenType::WS, Payload::None),            
        ("eq", TokenType::KwEQ, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("fffX", TokenType::MacroString, Payload::None),        
    ]
)]
#[case::ws_handling_str_literals("'a' 'b' = 'c'",
    vec![
        ("'a'", TokenType::StringLiteral, Payload::None),
        (" ", TokenType::MacroString, Payload::None),            
        ("'b'", TokenType::StringLiteral, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("=", TokenType::ASSIGN, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("'c'", TokenType::StringLiteral, Payload::None),
    ]
)]
#[case::ws_handling_and_str_expr("\"a&mv%m (/*c*/)\" &mv = a",
    vec![
        ("\"", TokenType::StringExprStart, Payload::None),
        ("a", TokenType::StringExprText, Payload::None),
        ("&mv", TokenType::MacroVarExpr, Payload::None),
        ("%m", TokenType::MacroIdentifier, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("(", TokenType::LPAREN, Payload::None),
        ("/*c*/", TokenType::CStyleComment, Payload::None),
        (")", TokenType::RPAREN, Payload::None),
        ("\"", TokenType::StringExprEnd, Payload::None),
        (" ", TokenType::MacroString, Payload::None),
        ("&mv", TokenType::MacroVarExpr, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("=", TokenType::ASSIGN, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("a", TokenType::MacroString, Payload::None),
    ]
)]
#[case::int_broken_by_comment("1/*c*/2",
    vec![
        ("1", TokenType::MacroString, Payload::None),
        ("/*c*/", TokenType::CStyleComment, Payload::None),
        ("2", TokenType::IntegerLiteral, Payload::Integer(2)),
    ]
)]
#[case::all_in_expr(" (\n🔥>  &m.v)*/*c*/(\t3 * ( 5-0ffx) ) eq %m.suf \"\"", 
    vec![
        (" ", TokenType::WS, Payload::None),
        ("(", TokenType::LPAREN, Payload::None),
        ("\n", TokenType::WS, Payload::None),
        ("🔥", TokenType::MacroString, Payload::None),
        (">", TokenType::GT, Payload::None),
        ("  ", TokenType::WS, Payload::None),
        ("&m.", TokenType::MacroVarExpr, Payload::None),
        ("v", TokenType::MacroString, Payload::None),
        (")", TokenType::RPAREN, Payload::None),
        ("*", TokenType::STAR, Payload::None),
        ("/*c*/", TokenType::CStyleComment, Payload::None),
        ("(", TokenType::LPAREN, Payload::None),
        ("\t", TokenType::WS, Payload::None),
        ("3", TokenType::IntegerLiteral, Payload::Integer(3)),
        (" ", TokenType::WS, Payload::None),
        ("*", TokenType::STAR, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("(", TokenType::LPAREN, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("5", TokenType::IntegerLiteral, Payload::Integer(5)),
        ("-", TokenType::MINUS, Payload::None),
        ("0ffx", TokenType::IntegerLiteral, Payload::Integer(255)),
        (")", TokenType::RPAREN, Payload::None),
        (" ", TokenType::WS, Payload::None),
        (")", TokenType::RPAREN, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("eq", TokenType::KwEQ, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("%m", TokenType::MacroIdentifier, Payload::None),
        (".suf ", TokenType::MacroString, Payload::None),        
        ("\"\"", TokenType::StringLiteral, Payload::None),
    ]
)]
fn test_macro_eval_expr(
    #[case] expr_str: &str,
    #[case] expected_tokens: Vec<(&str, TokenType, Payload)>,
) {
    let mut all_expected_tokens = vec![
        ("%eval", TokenType::KwmEval, Payload::None),
        ("(", TokenType::LPAREN, Payload::None),
    ];

    all_expected_tokens.extend(expected_tokens);
    all_expected_tokens.push((")", TokenType::RPAREN, Payload::None));

    assert_lexing(
        format!("%eval({expr_str})").as_str(),
        all_expected_tokens,
        NO_ERRORS,
    );
}

#[rstest]
#[case::comma_and_semi("1,eQ;", 
    vec![
        ("1", TokenType::IntegerLiteral, Payload::Integer(1)),
        (",", TokenType::COMMA, Payload::None),        
        ("eQ;", TokenType::MacroString, Payload::None),
    ]
)]
#[case::not_integer_1("1 2 ~= 3 4", 
    vec![
        ("1 2", TokenType::MacroString, Payload::None),
        (" ", TokenType::WS, Payload::None),            
        ("~=", TokenType::NE, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("3 4", TokenType::MacroString, Payload::None),
    ]
)]
#[case::float("1.2 ~= 3.4", 
    vec![
        ("1.2", TokenType::FloatLiteral, Payload::Float(1.2)),
        (" ", TokenType::WS, Payload::None),            
        ("~=", TokenType::NE, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("3.4", TokenType::FloatLiteral, Payload::Float(3.4)),
    ]
)]
#[case::hex_integer_literals("0FFx < 9ffX",
    vec![
        ("0FFx", TokenType::IntegerLiteral, Payload::Integer(255)),
        (" ", TokenType::WS, Payload::None),            
        ("<", TokenType::LT, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("9ffX", TokenType::IntegerLiteral, Payload::Integer(2559)),        
    ]
)]
#[case::not_hex_integer_literals("0F_Fx eq fffX",
    vec![
        ("0F_Fx", TokenType::MacroString, Payload::None),
        (" ", TokenType::WS, Payload::None),            
        ("eq", TokenType::KwEQ, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("fffX", TokenType::MacroString, Payload::None),        
    ]
)]
#[case::ws_handling_str_literals("'a' 'b' = 'c'",
    vec![
        ("'a'", TokenType::StringLiteral, Payload::None),
        (" ", TokenType::MacroString, Payload::None),            
        ("'b'", TokenType::StringLiteral, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("=", TokenType::ASSIGN, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("'c'", TokenType::StringLiteral, Payload::None),
    ]
)]
#[case::ws_handling_and_str_expr("\"a&mv%m (/*c*/)\" &mv = a",
    vec![
        ("\"", TokenType::StringExprStart, Payload::None),
        ("a", TokenType::StringExprText, Payload::None),
        ("&mv", TokenType::MacroVarExpr, Payload::None),
        ("%m", TokenType::MacroIdentifier, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("(", TokenType::LPAREN, Payload::None),
        ("/*c*/", TokenType::CStyleComment, Payload::None),
        (")", TokenType::RPAREN, Payload::None),
        ("\"", TokenType::StringExprEnd, Payload::None),
        (" ", TokenType::MacroString, Payload::None),
        ("&mv", TokenType::MacroVarExpr, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("=", TokenType::ASSIGN, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("a", TokenType::MacroString, Payload::None),
    ]
)]
#[case::int_broken_by_comment("1/*c*/2",
    vec![
        ("1", TokenType::MacroString, Payload::None),
        ("/*c*/", TokenType::CStyleComment, Payload::None),
        ("2", TokenType::IntegerLiteral, Payload::Integer(2)),
    ]
)]
#[case::all_in_expr(" (\n🔥>  &m.v)*/*c*/(\t3 * ( 5.1-0ffx) ) eq %m.suf \"\"", 
    vec![
        (" ", TokenType::WS, Payload::None),
        ("(", TokenType::LPAREN, Payload::None),
        ("\n", TokenType::WS, Payload::None),
        ("🔥", TokenType::MacroString, Payload::None),
        (">", TokenType::GT, Payload::None),
        ("  ", TokenType::WS, Payload::None),
        ("&m.", TokenType::MacroVarExpr, Payload::None),
        ("v", TokenType::MacroString, Payload::None),
        (")", TokenType::RPAREN, Payload::None),
        ("*", TokenType::STAR, Payload::None),
        ("/*c*/", TokenType::CStyleComment, Payload::None),
        ("(", TokenType::LPAREN, Payload::None),
        ("\t", TokenType::WS, Payload::None),
        ("3", TokenType::IntegerLiteral, Payload::Integer(3)),
        (" ", TokenType::WS, Payload::None),
        ("*", TokenType::STAR, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("(", TokenType::LPAREN, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("5.1", TokenType::FloatLiteral, Payload::Float(5.1)),
        ("-", TokenType::MINUS, Payload::None),
        ("0ffx", TokenType::IntegerLiteral, Payload::Integer(255)),
        (")", TokenType::RPAREN, Payload::None),
        (" ", TokenType::WS, Payload::None),
        (")", TokenType::RPAREN, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("eq", TokenType::KwEQ, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("%m", TokenType::MacroIdentifier, Payload::None),
        (".suf ", TokenType::MacroString, Payload::None),        
        ("\"\"", TokenType::StringLiteral, Payload::None),
    ]
)]
fn test_macro_eval_float_expr(
    #[case] expr_str: &str,
    #[case] expected_tokens: Vec<(&str, TokenType, Payload)>,
) {
    let mut all_expected_tokens = vec![
        ("%sysevalf", TokenType::KwmSysevalf, Payload::None),
        ("(", TokenType::LPAREN, Payload::None),
    ];

    all_expected_tokens.extend(expected_tokens);
    all_expected_tokens.push((",", TokenType::COMMA, Payload::None));
    all_expected_tokens.push(("ceil", TokenType::MacroString, Payload::None));
    all_expected_tokens.push((")", TokenType::RPAREN, Payload::None));

    assert_lexing(
        format!("%sysevalf({expr_str},ceil)").as_str(),
        all_expected_tokens,
        NO_ERRORS,
    );
}

#[rstest]
fn test_macro_eval_empty_logical_operand(
    #[values(
        vec![
            ("%eval", TokenType::KwmEval),
            ("(", TokenType::LPAREN),
            ("#", TokenType::UNKNOWN),
            (")", TokenType::RPAREN),
        ],
        vec![
            ("%sysevalf", TokenType::KwmSysevalf),
            ("(", TokenType::LPAREN),
            ("#", TokenType::UNKNOWN),
            (",", TokenType::COMMA),
            ("ceil", TokenType::MacroString),
            (")", TokenType::RPAREN),
        ],
        vec![
            ("%scan", TokenType::KwmScan),
            ("(", TokenType::LPAREN),
            ("a", TokenType::MacroString),
            (",", TokenType::COMMA),
            ("#", TokenType::UNKNOWN),
            (")", TokenType::RPAREN),
        ],
        vec![
            ("%scan", TokenType::KwmScan),
            ("(", TokenType::LPAREN),
            ("a", TokenType::MacroString),
            (",", TokenType::COMMA),
            ("#", TokenType::UNKNOWN),
            (",", TokenType::COMMA),
            ("=", TokenType::MacroString),
            (")", TokenType::RPAREN),
        ],
        vec![
            ("%qscan", TokenType::KwmQScan),
            ("(", TokenType::LPAREN),
            ("a", TokenType::MacroString),
            (",", TokenType::COMMA),
            ("#", TokenType::UNKNOWN),
            (")", TokenType::RPAREN),
        ],
        vec![
            ("%qscan", TokenType::KwmQScan),
            ("(", TokenType::LPAREN),
            ("a", TokenType::MacroString),
            (",", TokenType::COMMA),
            ("#", TokenType::UNKNOWN),
            (",", TokenType::COMMA),
            ("=", TokenType::MacroString),
            (")", TokenType::RPAREN),
        ],
        vec![
            ("%kscan", TokenType::KwmKScan),
            ("(", TokenType::LPAREN),
            ("a", TokenType::MacroString),
            (",", TokenType::COMMA),
            ("#", TokenType::UNKNOWN),
            (")", TokenType::RPAREN),
        ],
        vec![
            ("%kscan", TokenType::KwmKScan),
            ("(", TokenType::LPAREN),
            ("a", TokenType::MacroString),
            (",", TokenType::COMMA),
            ("#", TokenType::UNKNOWN),
            (",", TokenType::COMMA),
            ("=", TokenType::MacroString),
            (")", TokenType::RPAREN),
        ],
        vec![
            ("%qKscan", TokenType::KwmQKScan),
            ("(", TokenType::LPAREN),
            ("a", TokenType::MacroString),
            (",", TokenType::COMMA),
            ("#", TokenType::UNKNOWN),
            (")", TokenType::RPAREN),
        ],
        vec![
            ("%qKscan", TokenType::KwmQKScan),
            ("(", TokenType::LPAREN),
            ("a", TokenType::MacroString),
            (",", TokenType::COMMA),
            ("#", TokenType::UNKNOWN),
            (",", TokenType::COMMA),
            ("=", TokenType::MacroString),
            (")", TokenType::RPAREN),
        ],
        // Didn't add q/k substr variations, unlikely to have bugs
        // if the regular ones work
        vec![
            ("%substr", TokenType::KwmSubstr),
            ("(", TokenType::LPAREN),
            ("a", TokenType::MacroString),
            (",", TokenType::COMMA),
            ("#", TokenType::UNKNOWN),
            (")", TokenType::RPAREN),
        ],
        vec![
            ("%substr", TokenType::KwmSubstr),
            ("(", TokenType::LPAREN),
            ("a", TokenType::MacroString),
            (",", TokenType::COMMA),
            ("#", TokenType::UNKNOWN),
            (",", TokenType::COMMA),
            ("#", TokenType::UNKNOWN),            
            (")", TokenType::RPAREN),
        ],
        vec![
            ("%do", TokenType::KwmDo),
            (" ", TokenType::WS),
            ("i", TokenType::MacroString),
            ("=", TokenType::ASSIGN),
            ("#", TokenType::UNKNOWN),
            ("%to", TokenType::KwmTo),
            (" ", TokenType::WS),
            ("#", TokenType::UNKNOWN),
            ("%by", TokenType::KwmBy),
            (" ", TokenType::WS),
            ("#", TokenType::UNKNOWN),
            (";", TokenType::SEMI),
        ],
        vec![
            ("%do", TokenType::KwmDo),
            (" ", TokenType::WS),
            ("i", TokenType::MacroString),
            ("=", TokenType::ASSIGN),
            ("#", TokenType::UNKNOWN),
            ("%to", TokenType::KwmTo),
            (" ", TokenType::WS),
            ("#", TokenType::UNKNOWN),
            (";", TokenType::SEMI),            
        ],
        vec![
            ("%do", TokenType::KwmDo),
            (" ", TokenType::WS),
            ("%until", TokenType::KwmUntil),
            ("(", TokenType::LPAREN),
            ("#", TokenType::UNKNOWN),
            (")", TokenType::RPAREN),
            (";", TokenType::SEMI),
        ],
        vec![
            ("%do", TokenType::KwmDo),
            (" ", TokenType::WS),
            ("%WhIle", TokenType::KwmWhile),
            ("(", TokenType::LPAREN),
            ("#", TokenType::UNKNOWN),
            (")", TokenType::RPAREN),
            (";", TokenType::SEMI),
        ],
        vec![
            ("%if", TokenType::KwmIf),
            (" ", TokenType::WS),
            ("#", TokenType::UNKNOWN),
            ("%tHeN", TokenType::KwmThen),            
        ],
    )]
    template: Vec<(&str, TokenType)>,
    #[values(
        ("<", TokenType::LT),
        ("lt", TokenType::KwLT),
        ("<=", TokenType::LE),
        ("le", TokenType::KwLE),
        ("=", TokenType::ASSIGN),
        ("eq", TokenType::KwEQ),
        ("#", TokenType::HASH),
        ("in", TokenType::KwIN),
        ("~=", TokenType::NE),
        ("ne", TokenType::KwNE),
        (">", TokenType::GT),
        ("gt", TokenType::KwGT),
        (">=", TokenType::GE),
        ("ge", TokenType::KwGE),
    )]
    (op_str, op_tok): (&str, TokenType),
    #[values(
        // LHS missing
        vec![
            ("", TokenType::MacroStringEmpty, Payload::None),
            ("#", TokenType::UNKNOWN, Payload::None),
            (" ", TokenType::WS, Payload::None),
            ("1", TokenType::IntegerLiteral, Payload::Integer(1)),
        ],
        // RHS missing
        vec![
            ("1", TokenType::IntegerLiteral, Payload::Integer(1)),
            (" ", TokenType::WS, Payload::None),
            ("#", TokenType::UNKNOWN, Payload::None),
            ("", TokenType::MacroStringEmpty, Payload::None),
        ],
        // Both missing
        vec![
            ("", TokenType::MacroStringEmpty, Payload::None),
            ("#", TokenType::UNKNOWN, Payload::None),
            ("", TokenType::MacroStringEmpty, Payload::None),
        ],
        // Two ops consecutive
        vec![
            ("", TokenType::MacroStringEmpty, Payload::None),
            ("#", TokenType::UNKNOWN, Payload::None),
            (" ", TokenType::WS, Payload::None),
            ("", TokenType::MacroStringEmpty, Payload::None),
            ("#", TokenType::UNKNOWN, Payload::None),
            ("", TokenType::MacroStringEmpty, Payload::None),
        ],
        // Parentheses
        vec![
            ("(", TokenType::LPAREN, Payload::None),
            ("", TokenType::MacroStringEmpty, Payload::None),
            ("#", TokenType::UNKNOWN, Payload::None),
            (" ", TokenType::WS, Payload::None),
            ("1", TokenType::IntegerLiteral, Payload::Integer(1)),
            (")", TokenType::RPAREN, Payload::None),
            ("and", TokenType::KwAND, Payload::None),
            ("(", TokenType::LPAREN, Payload::None),
            ("1", TokenType::IntegerLiteral, Payload::Integer(1)),
            (" ", TokenType::WS, Payload::None),
            ("#", TokenType::UNKNOWN, Payload::None),
            ("", TokenType::MacroStringEmpty, Payload::None),
            (")", TokenType::RPAREN, Payload::None),
            ("and", TokenType::KwAND, Payload::None),
            ("(", TokenType::LPAREN, Payload::None),
            ("", TokenType::MacroStringEmpty, Payload::None),
            ("#", TokenType::UNKNOWN, Payload::None),
            ("", TokenType::MacroStringEmpty, Payload::None),
            (")", TokenType::RPAREN, Payload::None),
        ],
        // No parentheses, split with logical
        vec![
            ("", TokenType::MacroStringEmpty, Payload::None),
            ("#", TokenType::UNKNOWN, Payload::None),
            (" ", TokenType::WS, Payload::None),            
            ("", TokenType::MacroStringEmpty, Payload::None),
            ("and", TokenType::KwAND, Payload::None),            
            (" ", TokenType::WS, Payload::None),            
            ("", TokenType::MacroStringEmpty, Payload::None),
            ("#", TokenType::UNKNOWN, Payload::None),
            ("", TokenType::MacroStringEmpty, Payload::None),            
        ],
    )]
    expr_template: Vec<(&str, TokenType, Payload)>,
) {
    // we construct a complex expression, with lhs, rhs and both operands missing
    // then replace the placeholders in the test template with this expression and test
    let expr_expected_tokens = expr_template
        .iter()
        .map(|(snip, tok_type, payload)| {
            (
                match tok_type {
                    TokenType::UNKNOWN => op_str,
                    _ => *snip,
                },
                match tok_type {
                    TokenType::UNKNOWN => op_tok,
                    _ => *tok_type,
                },
                *payload,
            )
        })
        .collect::<Vec<_>>();

    let all_expected_tokens = template
        .iter()
        .flat_map(|(snip, tok_type)| match tok_type {
            TokenType::UNKNOWN => {
                // Placeholder, here comes the expression
                expr_expected_tokens.clone()
            }
            _ => {
                // Regular template part, add payload
                vec![(*snip, *tok_type, Payload::None)]
            }
        })
        .collect::<Vec<_>>();

    assert_lexing(
        all_expected_tokens
            .iter()
            .map(|(snip, _, _)| *snip)
            .collect::<String>()
            .as_str(),
        all_expected_tokens,
        NO_ERRORS,
    );
}

#[rstest]
#[case::do_to_no_by("%do i=1 %to 2  ;",
    vec![
        ("%do", TokenType::KwmDo, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("i", TokenType::MacroString, Payload::None),
        ("=", TokenType::ASSIGN, Payload::None),
        ("1", TokenType::IntegerLiteral, Payload::Integer(1)),
        (" ", TokenType::WS, Payload::None),
        ("%to", TokenType::KwmTo, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("2", TokenType::IntegerLiteral, Payload::Integer(2)),
        ("  ", TokenType::WS, Payload::None),        
        (";", TokenType::SEMI, Payload::None),
        ]
)]
#[case::do_to_no_by_trailing_commnet("%do i=1 %to 2  /*c*/  ;",
    vec![
        ("%do", TokenType::KwmDo, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("i", TokenType::MacroString, Payload::None),
        ("=", TokenType::ASSIGN, Payload::None),
        ("1", TokenType::IntegerLiteral, Payload::Integer(1)),
        (" ", TokenType::WS, Payload::None),
        ("%to", TokenType::KwmTo, Payload::None),
        (" ", TokenType::WS, Payload::None),
        // Known limitation of our "predictive" lexer
        ("2  ", TokenType::MacroString, Payload::None),        
        ("/*c*/", TokenType::CStyleComment, Payload::None),
        ("  ", TokenType::WS, Payload::None),
        (";", TokenType::SEMI, Payload::None),
        ]
)]
#[case::do_to_by("%do i=1 %to &mv %by 0ffx;",
    vec![
        ("%do", TokenType::KwmDo, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("i", TokenType::MacroString, Payload::None),
        ("=", TokenType::ASSIGN, Payload::None),
        ("1", TokenType::IntegerLiteral, Payload::Integer(1)),
        (" ", TokenType::WS, Payload::None),
        ("%to", TokenType::KwmTo, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("&mv", TokenType::MacroVarExpr, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("%by", TokenType::KwmBy, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("0ffx", TokenType::IntegerLiteral, Payload::Integer(255)),
        (";", TokenType::SEMI, Payload::None),
    ]
)]
fn test_macro_do(#[case] contents: &str, #[case] expected_token: Vec<impl TokenTestCase>) {
    assert_lexing(contents, expected_token, NO_ERRORS);
}

#[rstest]
#[case::no_args_or_opts("%MAcro m;",
    vec![
        ("%MAcro", TokenType::KwmMacro, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("m", TokenType::Identifier, Payload::None),
        (";", TokenType::SEMI, Payload::None),
    ],
    NO_ERRORS
)]
#[case::empty_args_no_opts("%macro m();",
    vec![
        ("%macro", TokenType::KwmMacro, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("m", TokenType::Identifier, Payload::None),
        ("(", TokenType::LPAREN, Payload::None),
        (")", TokenType::RPAREN, Payload::None),
        (";", TokenType::SEMI, Payload::None),
    ],
    NO_ERRORS
)]
#[case::no_args_with_opts("%macro m /STore;",
    vec![
        ("%macro", TokenType::KwmMacro, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("m", TokenType::Identifier, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("/", TokenType::FSLASH, Payload::None),
        ("STore", TokenType::MacroString, Payload::None),
        (";", TokenType::SEMI, Payload::None),
    ],
    NO_ERRORS
)]
#[case::empty_args_with_opts("%macro m()/MINDELIMITER='''z';",
    vec![
        ("%macro", TokenType::KwmMacro, Payload::None, "%macro"),
        (" ", TokenType::WS, Payload::None, " "),
        ("m", TokenType::Identifier, Payload::None, "m"),
        ("(", TokenType::LPAREN, Payload::None, "("),
        (")", TokenType::RPAREN, Payload::None, ")"),
        ("/", TokenType::FSLASH, Payload::None, "/"),
        ("MINDELIMITER", TokenType::MacroString, Payload::None, "MINDELIMITER"),
        ("=", TokenType::ASSIGN, Payload::None, "="),
        ("'''z'", TokenType::StringLiteral, Payload::StringLiteral(0, 2), "'z"),
        (";", TokenType::SEMI, Payload::None, ";"),
    ],
    NO_ERRORS
)]
#[case::all_in_one("%macro /*c*/ m /*c*/ ( /*c*/ no_def, \
    \n\t/*c*/ arg1 /*c*/ = \"%qscan(&mv,3,&&&)\" , \
	\n\t/*c*/ arg2 /*c*/ =(,) and 🔥) / DES=\"&mv\" ;",
    vec![
        ("%macro", TokenType::KwmMacro, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("/*c*/", TokenType::CStyleComment, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("m", TokenType::Identifier, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("/*c*/", TokenType::CStyleComment, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("(", TokenType::LPAREN, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("/*c*/", TokenType::CStyleComment, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("no_def", TokenType::Identifier, Payload::None),
        (",", TokenType::COMMA, Payload::None),
        (" \n\t", TokenType::WS, Payload::None),
        ("/*c*/", TokenType::CStyleComment, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("arg1", TokenType::Identifier, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("/*c*/", TokenType::CStyleComment, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("=", TokenType::ASSIGN, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("\"", TokenType::StringExprStart, Payload::None),
        ("%qscan", TokenType::KwmQScan, Payload::None),
        ("(", TokenType::LPAREN, Payload::None),
        ("&mv", TokenType::MacroVarExpr, Payload::None),
        (",", TokenType::COMMA, Payload::None),
        ("3", TokenType::IntegerLiteral, Payload::Integer(3)),
        (",", TokenType::COMMA, Payload::None),
        ("&&&", TokenType::MacroString, Payload::None),
        (")", TokenType::RPAREN, Payload::None),
        ("\"", TokenType::StringExprEnd, Payload::None),
        // Trailing ws, not recognized as WS...
        (" ", TokenType::MacroString, Payload::None),
        (",", TokenType::COMMA, Payload::None),
        (" \n\t", TokenType::WS, Payload::None),
        ("/*c*/", TokenType::CStyleComment, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("arg2", TokenType::Identifier, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("/*c*/", TokenType::CStyleComment, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("=", TokenType::ASSIGN, Payload::None),
        ("(,) and 🔥", TokenType::MacroString, Payload::None),
        (")", TokenType::RPAREN, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("/", TokenType::FSLASH, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("DES", TokenType::MacroString, Payload::None),
        ("=", TokenType::ASSIGN, Payload::None),
        ("\"", TokenType::StringExprStart, Payload::None),
        ("&mv", TokenType::MacroVarExpr, Payload::None),
        ("\"", TokenType::StringExprEnd, Payload::None),
        (" ", TokenType::WS, Payload::None),
        (";", TokenType::SEMI, Payload::None),
    ],
    NO_ERRORS
)]
#[case::single_arg_error_recovery("%macro m(&err = val, \
    \n\t good );",
    vec![
        ("%macro", TokenType::KwmMacro, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("m", TokenType::Identifier, Payload::None),
        ("(", TokenType::LPAREN, Payload::None),
        ("&err", TokenType::MacroVarExpr, Payload::None),
        // lexed as macro call arg => ws not recognized
        (" ", TokenType::MacroString, Payload::None),
        ("=", TokenType::ASSIGN, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("val", TokenType::MacroString, Payload::None),
        (",", TokenType::COMMA, Payload::None),
        (" \n\t ", TokenType::WS, Payload::None),        
        ("good", TokenType::Identifier, Payload::None),
        (" ", TokenType::WS, Payload::None),
        (")", TokenType::RPAREN, Payload::None),
        (";", TokenType::SEMI, Payload::None),
    ],
    vec![(
        ErrorType::InvalidMacroDefArgName, 9
    )]
)]
#[case::macro_name_error_recovery("%macro &err (arg = val);",
    vec![
        ("%macro", TokenType::KwmMacro, Payload::None),
        (" ", TokenType::WS, Payload::None),        
        ("&err", TokenType::MacroVarExpr, Payload::None),
        (" ", TokenType::WS, Payload::None),        
        ("(arg", TokenType::MacroString, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("=", TokenType::ASSIGN, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("val)", TokenType::MacroString, Payload::None),        
        (";", TokenType::SEMI, Payload::None),
    ],
    vec![(
        ErrorType::InvalidMacroDefName, 7
    )]
)]
fn test_macro_def(
    #[case] contents: &str,
    #[case] expected_token: Vec<impl TokenTestCase>,
    #[case] expected_errors: Vec<impl ErrorTestCase>,
) {
    assert_lexing(contents, expected_token, expected_errors);
}

/// This test doesn't follow true semantics of built-ins. Most accept
/// only one arg, but we have a test case with two regardless.
#[rstest]
#[case::ints_not_evaluated(
    // Integers are not evaluated
    vec![
        ("1+1", TokenType::MacroString)            
    ]
)]
#[case::look_like_named_args_and_comma(
    // No named args, and comma in balanced parens is not a terminator
    vec![
        ("arg1=some(,arg2=())", TokenType::MacroString)            
    ],
)]
#[case::balanced_parens_after_slash_with_comment(
    vec![
        ("/(", TokenType::MacroString),
        ("/*c*/", TokenType::CStyleComment),
        (")", TokenType::MacroString),
    ]
)]
#[case::nested_call_with_comment(
    vec![
        ("/*c*/", TokenType::CStyleComment),
        ("some()", TokenType::MacroString),
        ("/*c*/", TokenType::CStyleComment),
        (",", TokenType::COMMA),
        ("/*c*/", TokenType::CStyleComment),
        ("2", TokenType::MacroString),
        ("/*c*/", TokenType::CStyleComment),        
    ]
)]
fn test_macro_simple_builtins(
    #[values(
        TokenType::KwmDatatyp,
        TokenType::KwmIndex,
        TokenType::KwmKIndex,
        TokenType::KwmLength,
        TokenType::KwmKLength,
        TokenType::KwmLowcase,
        TokenType::KwmKLowcase,
        TokenType::KwmQLowcase,
        TokenType::KwmQKLowcase,
        TokenType::KwmUpcase,
        TokenType::KwmKUpcase,
        TokenType::KwmQUpcase,
        TokenType::KwmQKUpcase,
        TokenType::KwmSysmexecname,
        TokenType::KwmSysprod,
        TokenType::KwmCmpres,
        TokenType::KwmQCmpres,
        TokenType::KwmKCmpres,
        TokenType::KwmQKCmpres,
        TokenType::KwmLeft,
        TokenType::KwmQLeft,
        TokenType::KwmKLeft,
        TokenType::KwmQKLeft,
        TokenType::KwmTrim,
        TokenType::KwmQTrim,
        TokenType::KwmKTrim,
        TokenType::KwmQKTrim,
        TokenType::KwmQuote,
        TokenType::KwmNrQuote,
        TokenType::KwmBquote,
        TokenType::KwmNrBquote,
        TokenType::KwmSuperq,
        TokenType::KwmUnquote,
        TokenType::KwmSymExist,
        TokenType::KwmSymGlobl,
        TokenType::KwmSymLocal,
        TokenType::KwmSysget,
        TokenType::KwmSysmacexec,
        TokenType::KwmSysmacexist,
        TokenType::KwmVerify,
        TokenType::KwmKVerify
    )]
    tok_type: TokenType,
    #[case] inner_expr_tokens: Vec<(&str, TokenType)>,
    kwm_to_str_map: HashMap<TokenType, String>,
) {
    let func_name = mangle_case(kwm_to_str_map.get(&tok_type).unwrap());
    let func_name = format!("%{func_name}");

    let expr_str = inner_expr_tokens
        .iter()
        .map(|(snip, _)| *snip)
        .collect::<String>();

    let mut expected_tokens = Vec::with_capacity(inner_expr_tokens.len() + 3);

    expected_tokens.push((func_name.as_str(), tok_type, Payload::None));
    expected_tokens.push(("(", TokenType::LPAREN, Payload::None));
    expected_tokens.extend(
        inner_expr_tokens
            .iter()
            .map(|(snip, tok)| (*snip, *tok, Payload::None)),
    );
    expected_tokens.push((")", TokenType::RPAREN, Payload::None));

    assert_lexing(
        format!("{func_name}({expr_str})").as_str(),
        expected_tokens,
        NO_ERRORS,
    );
}

#[rstest]
// One of the two built-ins that has named args
#[case::validchs("%VALIDchs(dsnm=sashelp.class, encoding=utf-8)",
    vec![
        ("%VALIDchs", TokenType::KwmValidchs),
        ("(", TokenType::LPAREN),
        ("dsnm", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("sashelp.class", TokenType::MacroString),
        (",", TokenType::COMMA),
        (" ", TokenType::WS),
        ("encoding", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("utf-8", TokenType::MacroString),
        (")", TokenType::RPAREN),
    ]
)]
#[case::compstor("%COMPstor(PATHNAME=lib)",
    vec![
        ("%COMPstor", TokenType::KwmCompstor),
        ("(", TokenType::LPAREN),
        ("PATHNAME", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("lib", TokenType::MacroString),        
        (")", TokenType::RPAREN),
    ]
)]
// This one is argument-less, so val should be an identifier in open code
#[case::sysmexecdepth("%SYSmexecdepth(val)",
    vec![
        ("%SYSmexecdepth", TokenType::KwmSysmexecdepth),
        ("(", TokenType::LPAREN),
        ("val", TokenType::Identifier),
        (")", TokenType::RPAREN),        
    ]
)]
// Tests that we correctly populate mode stack for 3+ argument built-ins
// Note this is not a valid call, the 4th argument can't be `=` as it
// must be one of the modifiers, but for testing purposes it's fine
#[case::four_arg_scan("%scan(a=b=c,&mv,=,=)",
    vec![
        ("%scan", TokenType::KwmScan),
        ("(", TokenType::LPAREN),
        ("a=b=c", TokenType::MacroString),
        (",", TokenType::COMMA),
        ("&mv", TokenType::MacroVarExpr),
        (",", TokenType::COMMA),
        ("=", TokenType::MacroString),
        (",", TokenType::COMMA),
        ("=", TokenType::MacroString),
        (")", TokenType::RPAREN),
    ]
)]
fn test_macro_special_builtins(
    #[case] contents: &str,
    #[case] expected_token: Vec<impl TokenTestCase>,
) {
    assert_lexing(contents, expected_token, NO_ERRORS);
}

/// A collection of non-exhaustive tests for rare macro stats.
/// %window is handled only in file test, too long to include here.
#[rstest]
#[case::copy(
    "%coPY %pre/*c*/()macroname&suf //*c*/ OUT='ext' lib=%mc SOURCE;",
    vec![
        ("%coPY", TokenType::KwmCopy),
        (" ", TokenType::WS),
        ("%pre", TokenType::MacroIdentifier),
        ("/*c*/", TokenType::CStyleComment),
        ("(", TokenType::LPAREN),
        (")", TokenType::RPAREN),
        ("macroname", TokenType::MacroString),
        ("&suf", TokenType::MacroVarExpr),
        (" ", TokenType::WS),
        ("/", TokenType::FSLASH),
        ("/*c*/", TokenType::CStyleComment),
        (" ", TokenType::WS),
        ("OUT", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("'ext'", TokenType::StringLiteral),
        (" ", TokenType::WS),
        ("lib", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("%mc", TokenType::MacroIdentifier),
        (" ", TokenType::WS),
        ("SOURCE", TokenType::MacroString),
        (";", TokenType::SEMI),
    ]
)]
#[case::input(
    "%Input first &second;",
    vec![
        ("%Input", TokenType::KwmInput),        
        (" ", TokenType::WS),
        ("first", TokenType::MacroString),
        (" ", TokenType::WS),
        ("&second", TokenType::MacroVarExpr),
        (";", TokenType::SEMI),
    ]
)]
#[case::mend(
    "%Mend disc;",
    vec![
        ("%Mend", TokenType::KwmMend),
        (" ", TokenType::WS),        
        ("disc", TokenType::MacroString),
        (";", TokenType::SEMI),
    ]
)]
#[case::symdel(
    "%SYMdel var1 &var2 / nowarn;",
    vec![
        ("%SYMdel", TokenType::KwmSymdel),
        (" ", TokenType::WS),
        ("var1", TokenType::MacroString),
        (" ", TokenType::WS),
        ("&var2", TokenType::MacroVarExpr),
        (" ", TokenType::WS),
        ("/", TokenType::FSLASH),
        (" ", TokenType::WS),
        ("nowarn", TokenType::MacroString),
        (";", TokenType::SEMI),
    ]
)]
#[case::syslput(
    "%sysLput path=%bquote(&path);",
    vec![
        ("%sysLput", TokenType::KwmSyslput),
        (" ", TokenType::WS),
        ("path", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("%bquote", TokenType::KwmBquote),
        ("(", TokenType::LPAREN),
        ("&path", TokenType::MacroVarExpr),
        (")", TokenType::RPAREN),
        (";", TokenType::SEMI),        
    ]
)]
#[case::sysmacdelete(
    "%SYSmacDELETE macro_name / nowarn;",
    vec![
        ("%SYSmacDELETE", TokenType::KwmSysmacdelete),
        (" ", TokenType::WS),
        ("macro_name", TokenType::MacroString),
        (" ", TokenType::WS),
        ("/", TokenType::FSLASH),
        (" ", TokenType::WS),
        ("nowarn", TokenType::MacroString),
        (";", TokenType::SEMI),
    ]
)]
#[case::sysrput(
    "%SYSrput retcode=&sysinfo;",
    vec![
        ("%SYSrput", TokenType::KwmSysrput),
        (" ", TokenType::WS),
        ("retcode", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("&sysinfo", TokenType::MacroVarExpr),
        (";", TokenType::SEMI),
    ]
)]
#[case::local_regular(
    "%LOcal var1 pre&mv %m()suf;",
    vec![
        ("%LOcal", TokenType::KwmLocal),
        (" ", TokenType::WS),
        ("var1", TokenType::MacroString),
        (" ", TokenType::WS),
        ("pre", TokenType::MacroString),
        ("&mv", TokenType::MacroVarExpr),
        (" ", TokenType::WS),
        ("%m", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        (")", TokenType::RPAREN),
        ("suf", TokenType::MacroString),
        (";", TokenType::SEMI),
    ]
)]
#[case::global_regular(
    "%GLobal var1 pre&mv %m()suf;",
    vec![
        ("%GLobal", TokenType::KwmGlobal),
        (" ", TokenType::WS),
        ("var1", TokenType::MacroString),
        (" ", TokenType::WS),
        ("pre", TokenType::MacroString),
        ("&mv", TokenType::MacroVarExpr),
        (" ", TokenType::WS),
        ("%m", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        (")", TokenType::RPAREN),
        ("suf", TokenType::MacroString),
        (";", TokenType::SEMI),
    ]
)]
#[case::local_readonly_static(
    "%local / readonly mv=1;",
    vec![
        ("%local", TokenType::KwmLocal),
        (" ", TokenType::WS),
        ("/", TokenType::FSLASH),
        (" ", TokenType::WS),
        ("readonly", TokenType::MacroString),
        (" ", TokenType::WS),
        ("mv", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),
        (";", TokenType::SEMI),
    ]
)]
#[case::local_readonly_dynamic(
    "%local / read&only mv=1;",
    vec![
        ("%local", TokenType::KwmLocal),
        (" ", TokenType::WS),
        ("/", TokenType::FSLASH),
        (" ", TokenType::WS),
        ("read", TokenType::MacroString),
        ("&only", TokenType::MacroVarExpr),
        (" ", TokenType::WS),
        ("mv", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),
        (";", TokenType::SEMI),
    ]
)]
#[case::global_readonly_static(
    "%global / readonly mv=1;",
    vec![
        ("%global", TokenType::KwmGlobal),
        (" ", TokenType::WS),
        ("/", TokenType::FSLASH),
        (" ", TokenType::WS),
        ("readonly", TokenType::MacroString),
        (" ", TokenType::WS),
        ("mv", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),
        (";", TokenType::SEMI),
    ]
)]
#[case::global_readonly_dynamic(
    "%global / read&only mv=1;",
    vec![
        ("%global", TokenType::KwmGlobal),
        (" ", TokenType::WS),
        ("/", TokenType::FSLASH),
        (" ", TokenType::WS),
        ("read", TokenType::MacroString),
        ("&only", TokenType::MacroVarExpr),
        (" ", TokenType::WS),
        ("mv", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),
        (";", TokenType::SEMI),
    ]
)]
fn test_macro_rare_stats(#[case] contents: &str, #[case] expected_token: Vec<impl TokenTestCase>) {
    assert_lexing(contents, expected_token, NO_ERRORS);
}

#[rstest]
// No error cases
#[case::first_line_simple("*--comment--;",
    vec![
        ("*--comment--;", TokenType::PredictedCommentStat),
    ],
    NO_ERRORS,
)]
#[case::last_line_simple("data;*--comment--",
    vec![
        ("data", TokenType::KwData),
        (";", TokenType::SEMI),
        ("*--comment--", TokenType::PredictedCommentStat),
    ],
    NO_ERRORS,
)]
#[case::first_line_with_macro("*c&mv.%mc(;,arg=some(;));",
    vec![
        ("*c&mv.%mc(;,arg=some(;));", TokenType::PredictedCommentStat),
    ],
    NO_ERRORS,
)]
#[case::after_stat_simple("data;*--comment--;",
    vec![
        ("data", TokenType::KwData),
        (";", TokenType::SEMI),
        ("*--comment--;", TokenType::PredictedCommentStat),
    ],
    NO_ERRORS,
)]
#[case::after_stat_with_macro("data;*c&mv.%mc(;,arg=some(;));",
    vec![
        ("data", TokenType::KwData),
        (";", TokenType::SEMI),
        ("*c&mv.%mc(;,arg=some(;));", TokenType::PredictedCommentStat),
    ],
    NO_ERRORS,
)]
#[case::do_end_comment("%do; * /*c*/ 2; %end;",
vec![
    ("%do", TokenType::KwmDo),
    (";", TokenType::SEMI),
    (" ", TokenType::WS),
    ("* /*c*/ 2;", TokenType::PredictedCommentStat),
    (" ", TokenType::WS),
    ("%end", TokenType::KwmEnd),
    (";", TokenType::SEMI),
    ],
    NO_ERRORS,
)]
// Three following stats that we "allow" for * to be recognized as math op
#[case::do_end_not_comment("%do; * /*c*/ 2 %end;",
    vec![
        ("%do", TokenType::KwmDo, Payload::None),
        (";", TokenType::SEMI, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("*", TokenType::STAR, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("/*c*/", TokenType::CStyleComment, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("2", TokenType::IntegerLiteral, Payload::Integer(2)),
        (" ", TokenType::WS, Payload::None),
        ("%end", TokenType::KwmEnd, Payload::None),
        (";", TokenType::SEMI, Payload::None),
    ],
    NO_ERRORS,
)]
#[case::before_mend_not_comment("* /*c*/ 2 %mend;",
    vec![
        ("*", TokenType::STAR, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("/*c*/", TokenType::CStyleComment, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("2", TokenType::IntegerLiteral, Payload::Integer(2)),
        (" ", TokenType::WS, Payload::None),
        ("%mend", TokenType::KwmMend, Payload::None),
        (";", TokenType::SEMI, Payload::None),
    ],
    NO_ERRORS,
)]
#[case::before_else_not_comment("* /*c*/ 2 %else",
    vec![
        ("*", TokenType::STAR, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("/*c*/", TokenType::CStyleComment, Payload::None),
        (" ", TokenType::WS, Payload::None),
        ("2", TokenType::IntegerLiteral, Payload::Integer(2)),
        (" ", TokenType::WS, Payload::None),
        ("%else", TokenType::KwmElse, Payload::None),        
    ],
    NO_ERRORS,
)]
// Error cases. We "predict" a comment before %let and all other stats
// so we recover missing semicolon
#[case::before_let_stat("* /*c*/ 2 %let v=;",
    vec![
        ("* /*c*/ 2 ", TokenType::PredictedCommentStat),
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("v", TokenType::MacroString),
        ("=", TokenType::ASSIGN),
        (";", TokenType::SEMI),
    ],
    vec![
        (ErrorType::InvalidOrOutOfOrderStatement, 10)
    ]
)]
fn test_comment_prediction(
    #[case] contents: &str,
    #[case] expected_token: Vec<impl TokenTestCase>,
    #[case] expected_error: Vec<impl ErrorTestCase>,
) {
    assert_lexing(contents, expected_token, expected_error);
}
