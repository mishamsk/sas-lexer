#[macro_use]
mod util;

use insta::assert_yaml_snapshot;
use rstest::rstest;
use sas_lexer::{lex, print::print_token};
use std::{fs, path::PathBuf};

#[rstest]
fn test_snapshots(#[files("tests/samples/**/*.sas")] path: PathBuf) {
    // Compute the absolute path of the prefix
    let prefix = fs::canonicalize("tests/samples/").unwrap();

    let snap_name = path.strip_prefix(&prefix).unwrap();
    let snap_name_str = snap_name
        .to_str()
        .unwrap()
        .replace(std::path::MAIN_SEPARATOR, "__");

    set_snapshot_suffix!("{}", snap_name_str);

    let contents = fs::read_to_string(&path).unwrap();
    let tok_buffer = lex(contents.as_str());
    let tokens: Vec<String> = tok_buffer
        .into_iter()
        .map(|tidx| print_token(tidx, &tok_buffer))
        .collect();

    assert_yaml_snapshot!(tokens);
}

/// Tests that tokens cover all offsets in the file from 0 to the end
#[rstest]
fn test_full_coverage(#[files("tests/samples/**/*.sas")] path: PathBuf) {
    let contents = fs::read_to_string(&path).unwrap();

    let tok_buffer = lex(contents.as_str());

    let mut end = 0;

    for token in tok_buffer.into_iter() {
        // Check that the token starts where the previous token ended
        assert_eq!(
            tok_buffer.get_token_start(token),
            end,
            "Token <{}> does not start where the previous token ended",
            print_token(token, &tok_buffer)
        );

        // Set the new end
        end = tok_buffer.get_token_end(token);

        // Check that the end is greater than the start
        assert!(
            end >= tok_buffer.get_token_start(token),
            "Token <{}> has an end before the start",
            print_token(token, &tok_buffer)
        );
    }

    // Check that the last token ends at the end of the file
    assert_eq!(end, contents.len() as u32);
}