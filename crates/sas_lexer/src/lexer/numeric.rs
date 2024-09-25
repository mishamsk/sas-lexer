use std::num::NonZeroUsize;

/// A collection of functions that converts
/// SAS numeric literals (hex & decimal) to their respective values
use lexical::{
    format_is_valid, parse_partial_with_options,
    Error::{EmptyExponent, Overflow, Underflow},
    NumberFormatBuilder, ParseFloatOptions, ParseIntegerOptions,
};

use super::{buffer::Payload, error::ErrorType, token_type::TokenType};

/// The default SAS numeric parser format, works for integers and floats
const SAS_DECIMAL: u128 = NumberFormatBuilder::new()
    // Disable leading sign, since we handle it separately
    .no_positive_mantissa_sign(true)
    // Disable all special numbers, such as Nan and Inf.
    .no_special(true)
    .build();

const SAS_HEX: u128 = NumberFormatBuilder::rebuild(NumberFormatBuilder::hexadecimal())
    // Disable all special numbers, such as Nan and Inf.
    .no_special(true)
    .build();

const SAS_PARSE_FLOAT_OPTIONS: ParseFloatOptions = ParseFloatOptions::new();
const SAS_PARSE_INT_OPTIONS: ParseIntegerOptions = ParseIntegerOptions::new();

pub(super) struct NumericParserResult {
    /// The parsed token
    pub(super) token: (TokenType, Payload),
    /// The length of the parsed token
    /// in bytes (not characters).
    pub(super) length: NonZeroUsize,
    /// An optional error (e.g. overflow)
    pub(super) error: Option<ErrorType>,
}

fn _try_parse_integer(byte_view: &[u8]) -> Option<NumericParserResult> {
    match parse_partial_with_options::<u64, _, SAS_DECIMAL>(byte_view, &SAS_PARSE_INT_OPTIONS) {
        Ok((value, len)) => {
            // If len is 0, it means that the number was not parsed
            NonZeroUsize::new(len).map(|length| NumericParserResult {
                token: (TokenType::IntegerLiteral, Payload::Integer(value)),
                length,
                error: None,
            })
        }
        // Anything else, including overflow means it can't be parsed as an u64 integer
        Err(_) => None,
    }
}

fn _try_parse_float(byte_view: &[u8]) -> Option<NumericParserResult> {
    match parse_partial_with_options::<f64, _, SAS_DECIMAL>(byte_view, &SAS_PARSE_FLOAT_OPTIONS) {
        Ok((value, len)) => {
            // For floats, it is important whether it
            // it was created from scientific notation or not,
            // due to ambiguity in formats parsing

            #[allow(clippy::indexing_slicing)]
            // SAFETY: parser returns the length of the parsed number, so the slice is valid
            let token_type = if byte_view[..len].contains(&b'e') || byte_view[..len].contains(&b'E')
            {
                TokenType::FloatExponentLiteral
            } else {
                TokenType::FloatLiteral
            };

            // If len is 0, it means that the number was not parsed
            NonZeroUsize::new(len).map(|length| NumericParserResult {
                token: (token_type, Payload::Float(value)),
                length,
                error: None,
            })
        }
        Err(Overflow(_) | Underflow(_)) => {
            // Should not happen in real life, but just in case.
            // We need to calculate the length of the number to "consume" it
            let mut seen_dot = false;

            let len = byte_view
                .iter()
                .map(|&c| {
                    if c == b'.' {
                        if seen_dot {
                            // Effectively break the loop
                            return b'_';
                        }
                        seen_dot = true;
                    }

                    c
                })
                .position(|c| !c.is_ascii_digit() && c != b'.')
                .unwrap_or(byte_view.len());

            // If len is 0, it means that the number was not parsed
            NonZeroUsize::new(len).map(|length| NumericParserResult {
                token: (TokenType::FloatLiteral, Payload::Float(0.0)),
                length,
                error: Some(ErrorType::InvalidNumericLiteral),
            })
        }
        Err(EmptyExponent(len)) => {
            // Realistically len can't be 0 here, but type-safety...
            NonZeroUsize::new(len).map(|length| NumericParserResult {
                token: (TokenType::FloatLiteral, Payload::Float(0.0)),
                length,
                error: Some(ErrorType::InvalidNumericLiteral),
            })
        }
        // Anything else means not a kind of error we recognize
        Err(_) => None,
    }
}

/// Tries to parses a possibly valid SAS number from the source str.
///
/// The function will try to parse the number as an u64 integer and an f64 float,
/// based on the `try_integer` and `try_float` flags.
///
/// Overflowing integers will be parsed as floats. Leading `+` sign is not
/// allowed explicitly by this function, and the leading `-` is not supposed
/// to be present in the source string.
pub(super) fn try_parse_decimal(
    source: &str,
    try_integer: bool,
    try_float: bool,
) -> Option<NumericParserResult> {
    debug_assert!(format_is_valid::<SAS_DECIMAL>());

    let byte_view = source.as_bytes();

    let int_result = if try_integer {
        _try_parse_integer(byte_view)
    } else {
        None
    };

    let float_result = if try_float {
        _try_parse_float(byte_view)
    } else {
        None
    };

    match (int_result, float_result) {
        (Some(int_result), Some(float_result)) => {
            // If both are present, we need to compare the lengths
            // to see which one is longer
            if int_result.length >= float_result.length {
                Some(int_result)
            } else {
                Some(float_result)
            }
        }
        (Some(int_result), None) => Some(int_result),
        (None, Some(float_result)) => Some(float_result),
        // None of the parsers succeeded
        (None, None) => None,
    }
}

/// Parses a possibly valid SAS HEX number from the source str (sans the trailing `x` or `X`).
///
/// By default tries to parse as u64. SAS only allows up-to 8 bytes (16 HEX digits)
/// for HEX literals, with the first one capped at 9F, so it is
/// slightly less then +/-(2^63 - 1) and theoretically can't overflow.
/// But who knows what SAS users can come up with;) =>
/// we revert to f64 if the number is too big, but will return an error
/// along with the token, since this would be a SAS error.
pub(super) fn try_parse_hex_integer(source: &str) -> Option<NumericParserResult> {
    debug_assert!(format_is_valid::<SAS_HEX>());

    let byte_view = source.as_bytes();

    match parse_partial_with_options::<u64, _, SAS_HEX>(byte_view, &SAS_PARSE_INT_OPTIONS) {
        Ok((value, len)) => {
            // If len is 0, it means that the number was not parsed
            NonZeroUsize::new(len).map(|length| NumericParserResult {
                token: (TokenType::IntegerLiteral, Payload::Integer(value)),
                length,
                error: None,
            })
        }
        Err(Overflow(_)) => {
            match parse_partial_with_options::<f64, _, SAS_HEX>(byte_view, &SAS_PARSE_FLOAT_OPTIONS)
            {
                Ok((value, len)) => {
                    // Len can't be possibly 0 here, as we already tried to parse as u64 and
                    // it failed with overflow (meaning the number is too big but still valid).
                    // But type-safety...
                    NonZeroUsize::new(len).map(|length| NumericParserResult {
                        token: (TokenType::FloatLiteral, Payload::Float(value)),
                        length,
                        error: Some(ErrorType::InvalidNumericLiteral),
                    })
                }
                _ => {
                    // This should not happen in real life (a HEX literal too big for f64?)
                    None
                }
            }
        }
        // Anything else means not a HEX number
        Err(_) => None,
    }
}
