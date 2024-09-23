use std::fs::metadata;

use std::{fs, path::PathBuf};

use indicatif::{ProgressBar, ProgressStyle};
use polars::prelude::*;
use sas_lexer::{Payload, ResolvedTokenInfo};
use walkdir::WalkDir;

use crate::{
    lex::safe_lex,
    print::{get_string_literal, get_token_raw_text},
};

#[allow(dead_code)]
fn print_schema(df_name: &str, df: &mut LazyFrame) {
    println!("{} DataFrame schema:", df_name);
    for col in df.collect_schema().unwrap().iter_fields() {
        println!("{:?}", col);
    }
}

fn create_token_df(
    file_id: u32,
    tokens: &Vec<ResolvedTokenInfo>,
    string_literals_buffer: &str,
    source: &str,
) -> PolarsResult<LazyFrame> {
    let mut token_index = Vec::with_capacity(tokens.len());
    let mut token_start = Vec::with_capacity(tokens.len());
    let mut token_stop = Vec::with_capacity(tokens.len());
    let mut token_raw_text = Vec::with_capacity(tokens.len());
    let mut token_type = Vec::with_capacity(tokens.len());
    let mut start_line = Vec::with_capacity(tokens.len());
    let mut end_line = Vec::with_capacity(tokens.len());
    let mut start_column = Vec::with_capacity(tokens.len());
    let mut end_column = Vec::with_capacity(tokens.len());
    let mut token_channel = Vec::with_capacity(tokens.len());
    let mut payload_int = Vec::with_capacity(tokens.len());
    let mut payload_float = Vec::with_capacity(tokens.len());
    let mut payload_str = Vec::with_capacity(tokens.len());

    for token in tokens {
        token_index.push(token.token_index);
        token_start.push(token.start);
        token_stop.push(token.stop);
        token_raw_text.push(get_token_raw_text(token, source));

        token_type.push(token.token_type as u32);
        start_line.push(token.line);
        end_line.push(token.end_line);
        start_column.push(token.column);
        end_column.push(token.end_column);
        token_channel.push(token.channel as u32);

        match token.payload {
            Payload::None => {
                payload_int.push(None);
                payload_float.push(None);
                payload_str.push(None);
            }
            Payload::Integer(val) => {
                payload_int.push(Some(val));
                payload_float.push(None);
                payload_str.push(None);
            }
            Payload::Float(val) => {
                payload_int.push(None);
                payload_float.push(Some(val));
                payload_str.push(None);
            }
            Payload::StringLiteral(start, stop) => {
                payload_int.push(None);
                payload_float.push(None);
                payload_str.push(Some(get_string_literal(
                    string_literals_buffer,
                    start,
                    stop,
                )));
            }
        };
    }

    let df = DataFrame::new(vec![
        Series::new("token_index".into(), token_index),
        Series::new("token_start".into(), token_start),
        Series::new("token_stop".into(), token_stop),
        Series::new("token_raw_text".into(), token_raw_text),
        Series::new("token_type".into(), token_type),
        Series::new("start_line".into(), start_line),
        Series::new("end_line".into(), end_line),
        Series::new("start_column".into(), start_column),
        Series::new("end_column".into(), end_column),
        Series::new("token_channel".into(), token_channel),
        Series::new("payload_int".into(), payload_int),
        Series::new("payload_float".into(), payload_float),
        Series::new("payload_str".into(), payload_str),
    ])?;

    Ok(df
        .clone()
        .lazy()
        .with_columns([lit(file_id).cast(DataType::UInt32).alias("file_id").into()]))
}

fn gen_stats_inner(output: &Option<PathBuf>, samples: &PathBuf) -> Result<(), PolarsError> {
    let all_files = WalkDir::new(samples)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.path().extension().and_then(|ext| ext.to_str()) == Some("sas"))
        .collect::<Vec<_>>();

    let total_files = all_files.len();

    let mut file_ids = Vec::with_capacity(total_files);
    let mut file_names = Vec::with_capacity(total_files);
    let mut file_bytes = Vec::with_capacity(total_files);
    let mut file_readable = Vec::with_capacity(total_files);
    let mut file_lexed = Vec::with_capacity(total_files);
    let mut file_str_buf_len = Vec::with_capacity(total_files);
    let mut token_dfs = Vec::with_capacity(total_files);

    let pb = ProgressBar::new(total_files as u64);
    pb.set_style(
        ProgressStyle::default_bar()
            .template("{msg} [{bar:40.cyan/blue}] {pos}/{len} ({eta})")
            .unwrap()
            .progress_chars("#>-"),
    );

    pb.set_message("Lexing files");

    for entry in all_files {
        pb.inc(1);

        let entry_path = entry.path();

        let file_id = file_ids.len() as u32;
        file_ids.push(file_id);
        file_names.push(entry_path.display().to_string());
        file_bytes.push(metadata(entry_path).map_or_else(|_| None, |m| Some(m.len())));

        let mut readable = true;
        let mut lexed = true;
        let mut string_buffer_length = None;

        if let Ok(contents) = fs::read_to_string(entry_path) {
            match safe_lex(&contents, false, false) {
                Some((tok_buffer, errors, _)) => {
                    let tokens = tok_buffer.into_resolved_token_vec();
                    let string_literals_buffer = tok_buffer.string_literals_buffer();
                    string_buffer_length = Some(string_literals_buffer.len() as u32);

                    token_dfs.push(create_token_df(
                        file_id,
                        &tokens,
                        string_literals_buffer,
                        &contents,
                    )?);
                }
                None => {
                    lexed = false;
                }
            }
        } else {
            readable = false;
            lexed = false;
        }

        file_readable.push(readable);
        file_lexed.push(lexed);
        file_str_buf_len.push(string_buffer_length);
    }

    let total_readable = file_readable.clone().iter().filter(|&&b| b).count();
    let total_lexed = file_lexed.clone().iter().filter(|&&b| b).count();

    pb.finish_with_message(format!(
        "Done. Readable: {total_readable}/{total_files}, Lexed: {total_lexed}/{total_files}",
    ));

    println!("Combining all DataFrame's...");

    let all_tokens_df = concat(token_dfs, UnionArgs::default())?;

    println!("Generating sources DataFrame...");

    let sources_df = df!(
        "file_id" => file_ids,
        "name" => file_names,
        "size" => file_bytes,
        "readable" => file_readable,
        "lexed" => file_lexed,
        "string_buffer_length" => file_str_buf_len,
    )?
    .clone()
    .lazy();

    if let Some(output_path) = output {
        //TODO
    }

    println!("Calculating token statistics...");

    let tok_aggregates = sources_df
        .clone()
        .join(
            all_tokens_df,
            [col("file_id")],
            [col("file_id")],
            // this will keep only the files that we were able to lex
            JoinArgs::new(JoinType::Inner),
        )
        // Remove empty files
        .filter(col("size").gt(0))
        .group_by([col("file_id")])
        .agg([
            len().cast(DataType::UInt32).alias("tokens_count"),
            col("end_line").max().alias("lines"),
            col("token_type").n_unique().alias("unique_token_types"),
        ]);

    let tok_ratio = col("size").cast(DataType::Float64) / col("tokens_count");
    let line_ratio = col("size").cast(DataType::Float64) / col("lines");
    let str_buf_ratio = when(col("string_buffer_length").gt(0))
        .then(col("size").cast(DataType::Float64) / col("string_buffer_length"))
        .otherwise(lit(NULL));

    let tok_report = sources_df
        .join(
            tok_aggregates,
            [col("file_id")],
            [col("file_id")],
            JoinArgs::new(JoinType::Inner),
        )
        .select([
            tok_ratio
                .clone()
                .min()
                .floor()
                .cast(DataType::UInt32)
                .alias("tok_ratio_min"),
            col("name")
                .sort_by(
                    [tok_ratio.clone().floor().cast(DataType::UInt32)],
                    SortMultipleOptions::default()
                        .with_nulls_last(true)
                        .with_order_descending(false),
                )
                .first()
                .alias("file_with_min_size_to_token_ratio"),
            tok_ratio
                .clone()
                .max()
                .ceil()
                .cast(DataType::UInt32)
                .alias("tok_ratio_max"),
            tok_ratio
                .clone()
                .median()
                .ceil()
                .cast(DataType::UInt32)
                .alias("tok_ratio_median"),
            line_ratio
                .clone()
                .min()
                .floor()
                .cast(DataType::UInt32)
                .alias("line_ratio_min"),
            col("name")
                .sort_by(
                    [line_ratio.clone().floor().cast(DataType::UInt32)],
                    SortMultipleOptions::default()
                        .with_nulls_last(true)
                        .with_order_descending(false),
                )
                .first()
                .alias("file_with_min_size_to_line_ratio"),
            line_ratio
                .clone()
                .max()
                .ceil()
                .cast(DataType::UInt32)
                .alias("line_ratio_max"),
            line_ratio
                .clone()
                .median()
                .ceil()
                .cast(DataType::UInt32)
                .alias("line_ratio_median"),
            str_buf_ratio
                .clone()
                .min()
                .floor()
                .cast(DataType::UInt32)
                .alias("str_buf_ratio_min"),
            col("name")
                .sort_by(
                    [str_buf_ratio.clone().floor().cast(DataType::UInt32)],
                    SortMultipleOptions::default()
                        .with_nulls_last(true)
                        .with_order_descending(false),
                )
                .first()
                .alias("file_with_min_size_to_str_buf_ratio"),
            str_buf_ratio
                .clone()
                .max()
                .ceil()
                .cast(DataType::UInt32)
                .alias("str_buf_ratio_max"),
            str_buf_ratio
                .clone()
                .median()
                .ceil()
                .cast(DataType::UInt32)
                .alias("str_buf_ratio_median"),
            col("unique_token_types")
                .cast(DataType::UInt64)
                .max()
                .alias("max_unique_token_types"),
            col("name")
                .sort_by(
                    [col("unique_token_types")],
                    SortMultipleOptions::default()
                        .with_nulls_last(true)
                        .with_order_descending(true),
                )
                .first()
                .alias("file_with_max_unique_token_types"),
        ])
        .collect()?;

    println!("Token statistics:");

    for col in tok_report.get_columns() {
        println!("{:?}: {:?}", col.name(), col.get(0)?);
    }

    Ok(())
}

pub(super) fn gen_stats(output: &Option<PathBuf>, samples: &PathBuf) {
    if let Err(err) = gen_stats_inner(output, samples) {
        eprintln!("Failed to generate stats: {err:?}");
        return;
    }
}
