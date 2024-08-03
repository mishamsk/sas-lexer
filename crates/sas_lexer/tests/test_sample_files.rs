#[macro_use]
mod util;

use insta::assert_yaml_snapshot;
use rstest::rstest;
use sas_lexer::{lex, print::token_to_string};
use std::{fs, path::PathBuf};

#[rstest]
fn test_snapshots(#[files("tests/samples/**/*.sas")] path: PathBuf) {
    // Compute the absolute path of the prefix

    use sas_lexer::print::error_to_string;
    let prefix = fs::canonicalize("tests/samples/").unwrap();

    let snap_name = path.strip_prefix(&prefix).unwrap();
    let snap_name_str = snap_name
        .to_str()
        .unwrap()
        .replace(std::path::MAIN_SEPARATOR, "__");

    set_snapshot_suffix!("{}", snap_name_str);

    let contents = fs::read_to_string(&path).unwrap();
    let (tok_buffer, errors) = lex(&contents).unwrap();
    let tokens: Vec<String> = tok_buffer
        .into_iter()
        .map(|tidx| token_to_string(tidx, &tok_buffer, &contents))
        .collect();

    assert_yaml_snapshot!("tokens", tokens);

    if errors.is_empty() {
        assert_yaml_snapshot!("errors_snapshot", @r###"
        ---
        errors_snapshot
        "###);
    } else {
        let error_strins = errors
            .iter()
            .map(|error| format!("{}", error_to_string(error, &tok_buffer, &contents)))
            .collect::<Vec<String>>();
        assert_yaml_snapshot!("errors_snapshot", error_strins);
    }
}

/// Tests that tokens cover all offsets in the file from 0 to the end
#[rstest]
fn test_full_coverage(#[files("tests/samples/**/*.sas")] path: PathBuf) {
    let contents = fs::read_to_string(&path).unwrap();

    let (tok_buffer, _) = lex(&contents).unwrap();

    let mut end = 0;

    for token in &tok_buffer {
        // Check that the token starts where the previous token ended
        assert_eq!(
            tok_buffer.get_token_start(token).get(),
            end,
            "Token <{}> does not start where the previous token ended",
            token_to_string(token, &tok_buffer, &contents)
        );

        // Set the new end
        end = tok_buffer.get_token_end(token).get();

        // Check that the end is greater than the start
        assert!(
            end >= tok_buffer.get_token_start(token).get(),
            "Token <{}> has an end before the start",
            token_to_string(token, &tok_buffer, &contents)
        );
    }

    // Check that the last token ends at the end of the file
    assert_eq!(end, contents.len() as u32);
}
