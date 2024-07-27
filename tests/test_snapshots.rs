#[macro_use]
mod util;

use insta::assert_yaml_snapshot;
use rstest::rstest;
use sas_lexer::{lex, MaterializedToken};
use std::{fs, path::PathBuf};

#[rstest]
fn test_files(#[files("tests/samples/**/*.sas")] path: PathBuf) {
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
    let tokens: Vec<MaterializedToken> = tok_buffer
        .into_iter()
        .map(|tidx| tok_buffer.materialize_token(tidx))
        .collect();

    assert_yaml_snapshot!(tokens);
}
