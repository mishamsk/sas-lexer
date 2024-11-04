use super::{
    super::{lex_program, LexResult},
    util::{error_to_string, token_to_string},
};
use insta::assert_yaml_snapshot;
use rstest::rstest;
use std::{fs, path::PathBuf};

macro_rules! set_snapshot_suffix {
    ($($expr:expr),*) => {
        let mut settings = insta::Settings::clone_current();
        settings.set_snapshot_suffix(format!($($expr,)*));
        let _guard = settings.bind_to_scope();
    }
}

#[rstest]
fn test_snapshots(#[files("src/lexer/tests/samples/**/*.sas")] path: PathBuf) {
    // Compute the absolute path of the prefix

    let prefix = fs::canonicalize(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/src/lexer/tests/samples/"
    ))
    .unwrap();

    let snap_name = path.strip_prefix(&prefix).unwrap();
    let snap_name_str = snap_name
        .to_str()
        .unwrap()
        .replace(std::path::MAIN_SEPARATOR, "__");

    set_snapshot_suffix!("{}", snap_name_str);

    let contents = fs::read_to_string(&path).unwrap();
    let LexResult {
        buffer: tok_buffer,
        errors,
        ..
    } = lex_program(&contents).unwrap();
    let tokens: Vec<String> = tok_buffer
        .into_iter()
        .map(|tidx| token_to_string(tidx, &tok_buffer, &contents))
        .collect();

    assert_yaml_snapshot!("tokens", tokens);

    if errors.is_empty() {
        insta::allow_duplicates! {
            assert_yaml_snapshot!("errors_snapshot", @r###"
            errors_snapshot
            "###);
        }
    } else {
        let error_strings = errors
            .iter()
            .map(|error| error_to_string(error, &tok_buffer, &contents).to_string())
            .collect::<Vec<String>>();
        assert_yaml_snapshot!("errors_snapshot", error_strings);
    }
}

/// Tests that tokens cover all offsets in the file from 0 to the end
#[rstest]
fn test_full_coverage(#[files("src/lexer/tests/samples/**/*.sas")] path: PathBuf) {
    let contents = fs::read_to_string(&path).unwrap();

    let LexResult {
        buffer: tok_buffer, ..
    } = lex_program(&contents).unwrap();

    let mut end = 0;

    for token in &tok_buffer {
        // Check that the token starts where the previous token ended
        assert_eq!(
            tok_buffer
                .get_token_start(token)
                .expect("wrong token")
                .get(),
            end,
            "Token <{}> does not start where the previous token ended",
            token_to_string(token, &tok_buffer, &contents)
        );

        // Set the new end
        end = tok_buffer.get_token_end(token).expect("wrong token").get();

        // Check that the end is greater than the start
        assert!(
            end >= tok_buffer
                .get_token_start(token)
                .expect("wrong token")
                .get(),
            "Token <{}> has an end before the start",
            token_to_string(token, &tok_buffer, &contents)
        );
    }

    // Check that the last token ends at the end of the file
    assert_eq!(end, contents.len() as u32);
}
