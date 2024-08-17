use std::vec;

use crate::Payload;
use crate::{error::ErrorType, lex, TokenChannel, TokenType};
use rstest::rstest;

use super::super::token_type::KEYWORDS;
use super::util::{assert_lexing, mangle_case, ErrorTestCase, TokenTestCase};

const NO_ERRORS: Vec<ErrorType> = vec![];

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
fn test_complex_string_expr(
    #[case] contents: &str,
    #[case] expected_token: Vec<impl TokenTestCase>,
) {
    assert_lexing(contents, expected_token, NO_ERRORS);
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
                ("a", TokenType::Identifier),
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
        let mangled_keyword = mangle_case(keyword);

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
fn test_char_format(#[case] contents: &str, #[case] expected_token: Vec<impl TokenTestCase>) {
    assert_lexing(contents, expected_token, NO_ERRORS);
}

#[rstest]
// Decimal notation
#[case::int1("1", (TokenType::IntegerLiteral, 1))]
#[case::int001("001", (TokenType::IntegerLiteral, 1))]
#[case::int1_dot("1.", (TokenType::IntegerDotLiteral, 1))]
#[case::int_dot_0(".0", (TokenType::IntegerDotLiteral, 0))]
#[case::int1_dot00("1.00", (TokenType::IntegerDotLiteral, 1))]
#[case::int01_dot("01.", (TokenType::IntegerLiteral, 1))]
#[case::int01_dot00("01.00", (TokenType::IntegerLiteral, 1))]
// one more than u64::MAX
#[case::i64_overlow("18446744073709551616", (TokenType::FloatLiteral, 18446744073709551616.0))]
// Hexadecimal notation
#[case::hex("02Ax", (TokenType::IntegerLiteral, 42))]
#[case::hex_one_digit("9X", (TokenType::IntegerLiteral, 9))]
// Scientific notation
#[case::sci("1e3", (TokenType::FloatLiteral, 1000.0))]
#[case::sci_plus("1E+3", (TokenType::FloatLiteral, 1000.0))]
#[case::sci_minus("1e-3", (TokenType::FloatLiteral, 0.001))]
#[case::sci_dot("4.2e3", (TokenType::FloatLiteral, 4200.0))]
#[case::sci_dot_only(".1E3", (TokenType::FloatLiteral, 100.0))]
fn test_numeric_literal(#[case] contents: &str, #[case] expected_token: impl TokenTestCase) {
    assert_lexing(contents, vec![expected_token], NO_ERRORS);
}

/// First numeric lexing implementation was also attaching the leading sign to the numeric literal.
/// But this would not play well with things like integer ranges and input statement mini-language.
/// Hence these tests.
#[rstest]
// Decimal notation
#[case::int001_minus("001", ("001", TokenType::IntegerLiteral, Payload::Integer(1)))]
#[case::int1_minus_dot("1.", ("1.", TokenType::IntegerDotLiteral, Payload::Integer(1)))]
#[case::int01_minus_dot("01.", ("01.", TokenType::IntegerLiteral, Payload::Integer(1)))]
#[case::neg_dec_only(".1", (".1", TokenType::FloatLiteral, Payload::Float(0.1)))]
#[case::hex_max("9ffFFffFFffFFffFx", ("9ffFFffFFffFFffFx", TokenType::IntegerLiteral, Payload::Integer(11529215046068469759)))]
#[case::sci_neg("1E3", ("1E3", TokenType::FloatLiteral, Payload::Float(1000.0)))]
#[case::sci_neg_minus("1E-3", ("1E-3", TokenType::FloatLiteral, Payload::Float(0.001)))]
#[case::sci_neg_dot_only(".1E3", (".1E3", TokenType::FloatLiteral, Payload::Float(100.0)))]
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

#[rstest]
#[case(".1e", (TokenType::FloatLiteral, 0.0), ErrorType::InvalidNumericLiteral)]
#[case(".1E", (TokenType::FloatLiteral, 0.0), ErrorType::InvalidNumericLiteral)]
#[case("02A", (TokenType::IntegerLiteral, 42), ErrorType::UnterminatedHexNumericLiteral)]
#[case("9ffFFffFFffFFffF123AFx", (TokenType::FloatLiteral, 1.152921504606847e19), ErrorType::InvalidNumericLiteral)]
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
        ("a", TokenType::Identifier),
        ("=", TokenType::ASSIGN),
        ("(", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::close_brace_val("%lEt a=);",
    vec![
        ("%lEt", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("a", TokenType::Identifier),
        ("=", TokenType::ASSIGN),
        (")", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::text_expr_name("%let a&mv=&mv.b=;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("a", TokenType::Identifier),
        ("&mv", TokenType::MacroVarExpr),
        ("=", TokenType::ASSIGN),
        ("&mv.", TokenType::MacroVarExpr),
        ("b=", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::dot_delim_mvar_in_name("%let a&b.c=2;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("a", TokenType::Identifier),
        ("&b.", TokenType::MacroVarExpr),
        ("c", TokenType::Identifier),
        ("=", TokenType::ASSIGN),
        ("2", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::lead_trail_ws("%let a \n=   1 1  ;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("a", TokenType::Identifier),
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
        ("a", TokenType::Identifier),
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
        ("a", TokenType::Identifier),
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
        ("a", TokenType::Identifier),
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
        ("a", TokenType::Identifier),
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
        ("a", TokenType::Identifier),
        ("%t", TokenType::MacroIdentifier),
        ("(", TokenType::LPAREN),
        (")", TokenType::RPAREN),
        ("a2", TokenType::Identifier),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::underscore_start("%let _9v=1;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("_9v", TokenType::Identifier),
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
        ("_9v", TokenType::Identifier),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::inline_comment("%let _9/*com*/v=1;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("_9", TokenType::Identifier),
        ("/*com*/", TokenType::CStyleComment),
        ("v", TokenType::Identifier),
        ("=", TokenType::ASSIGN),
        ("1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::eval_macro_call("%let ev%eval(1+1)=2;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("ev", TokenType::Identifier),
        ("%eval", TokenType::KwmEval),
        ("(", TokenType::LPAREN),
        ("1+1", TokenType::MacroString),
        (")", TokenType::RPAREN),
        ("=", TokenType::ASSIGN),
        ("2", TokenType::MacroString),
        (";", TokenType::SEMI),
        ]
)]
#[case::symexist_macro_call("%let ev%symexist(a1)=2;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("ev", TokenType::Identifier),
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
       ("ev" ,TokenType::Identifier),
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
       ("pre_" ,TokenType::Identifier),
       ("%t" ,TokenType::MacroIdentifier),
       (" " ,TokenType::WS),
       ("/*c*/" ,TokenType::CStyleComment),
       (" " ,TokenType::WS),
       ("(" ,TokenType::LPAREN),
       (")" ,TokenType::RPAREN),
       ("_post" ,TokenType::Identifier),
       ("=" ,TokenType::ASSIGN),
       ("v" ,TokenType::MacroString),
       (";" ,TokenType::SEMI),
       ]
)]
#[case::complex_value("%let a='%t();' \"%t();\";",
    vec![
       ("%let" ,TokenType::KwmLet),
       (" " ,TokenType::WS),
       ("a" ,TokenType::Identifier),
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
        ("a", TokenType::Identifier),
        ("%m", TokenType::MacroIdentifier),
        // Recovered from missing assign hence empty string
        ("", TokenType::ASSIGN),
        (".b=1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ],
    // Real SAS error will be: 
    // /* ERROR: Symbolic variable name a[resolved %m call].b must contain only letters, digits, and underscores. */
    vec![(ErrorType::MissingExpected("="), 8)]
)]
#[case::miss_assign("%let a b=1;",
    vec![
        ("%let", TokenType::KwmLet),
        (" ", TokenType::WS),
        ("a", TokenType::Identifier),
        (" ", TokenType::WS),
        // Recovered from missing assign hence empty string
        ("", TokenType::ASSIGN),
        ("b=1", TokenType::MacroString),
        (";", TokenType::SEMI),
        ],
    vec![(ErrorType::MissingExpected("="), 7)]
)]
#[case::miss_assign_2("%let a &mv=1;",
vec![
    ("%let", TokenType::KwmLet),
    (" ", TokenType::WS),
    ("a", TokenType::Identifier),
    (" ", TokenType::WS),
    // Recovered from missing assign hence empty string
    ("", TokenType::ASSIGN),
    ("&mv", TokenType::MacroVarExpr),
    ("=1", TokenType::MacroString),
    (";", TokenType::SEMI),
    ],
    vec![(ErrorType::MissingExpected("="), 7)]
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
        (ErrorType::MissingExpected("ERROR: Expecting a variable name after %LET."), 5),
        (ErrorType::MissingExpected("="), 5)
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
        (ErrorType::MissingExpected("ERROR: Expecting a variable name after %LET."), 4),
        (ErrorType::MissingExpected("="), 4)
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
        (ErrorType::MissingExpected("ERROR: Expecting a variable name after %LET."), 5),
        (ErrorType::MissingExpected("="), 5)
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
    ("a", TokenType::Identifier, TokenChannel::DEFAULT),
    // Recovered from missing assign hence empty string
    ("", TokenType::ASSIGN, TokenChannel::DEFAULT),
    ("%nrstr", TokenType::KwmNrStr, TokenChannel::HIDDEN),
    ("(", TokenType::LPAREN, TokenChannel::HIDDEN),
    ("a", TokenType::MacroString, TokenChannel::DEFAULT),
    (")", TokenType::RPAREN, TokenChannel::HIDDEN),
    ("=2", TokenType::MacroString, TokenChannel::DEFAULT),
    (";", TokenType::SEMI, TokenChannel::DEFAULT),
    ],
    vec![(ErrorType::MissingExpected("="), 6)]
)]
#[case::quote_call_in_name_2("%let a%quote(a)=2;",
vec![
    ("%let", TokenType::KwmLet),
    (" ", TokenType::WS),
    ("a", TokenType::Identifier),
    // Recovered from missing assign hence empty string
    ("", TokenType::ASSIGN),
    ("%quote", TokenType::KwmQuote),
    ("(", TokenType::LPAREN),
    ("a", TokenType::MacroString),
    (")", TokenType::RPAREN),
    ("=2", TokenType::MacroString),
    (";", TokenType::SEMI)
    ],
    vec![(ErrorType::MissingExpected("="), 6)]
)]
#[case::quote_call_in_name_3("%let a%str(%inner())=2;",
vec![
    ("%let", TokenType::KwmLet, TokenChannel::DEFAULT),
    (" ", TokenType::WS, TokenChannel::HIDDEN),
    ("a", TokenType::Identifier, TokenChannel::DEFAULT),
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
    vec![(ErrorType::MissingExpected("="), 6)]
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
        ("v", TokenType::Identifier, TokenChannel::DEFAULT),
        ("=", TokenType::ASSIGN, TokenChannel::DEFAULT),
        ("1", TokenType::MacroString, TokenChannel::DEFAULT),
        (";", TokenType::SEMI, TokenChannel::DEFAULT),
        (")", TokenType::RPAREN, TokenChannel::DEFAULT),
        (";", TokenType::SEMI, TokenChannel::DEFAULT),
        ],
    vec![
        (ErrorType::SASSessionUnrecoverableError("ERROR: Open code statement recursion detected."), 6),
        (ErrorType::MissingExpected(")"), 6)
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
        (ErrorType::MissingExpected("("), 8)
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
        (ErrorType::MissingExpected(")"), 11)
        ]
)]
fn test_macro_nrstr_call_error_recovery(
    #[case] contents: &str,
    #[case] expected_token: Vec<impl TokenTestCase>,
    #[case] expected_error: Vec<impl ErrorTestCase>,
) {
    assert_lexing(contents, expected_token, expected_error);
}
