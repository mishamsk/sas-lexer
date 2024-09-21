use polars::prelude::*;
use sas_lexer::{Payload, ResolvedTokenInfo};

use crate::print::{get_string_literal, get_token_raw_text};

fn create_token_df(
    tokens: &Vec<ResolvedTokenInfo>,
    string_literals_buffer: &str,
    source: &str,
) -> PolarsResult<DataFrame> {
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

    DataFrame::new(vec![
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
    ])
}
