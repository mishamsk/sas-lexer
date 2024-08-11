/// A collection of functions that converts
/// SAS HEX literals (numeric & string) to their respective values
use super::buffer::Payload;

use super::error::ErrorType;
use super::token_type::TokenType;

/// Parses a valid HEX number (sans the trailing `x` or `X`)
///
/// # Returns
///
/// A tuple with the parsed token and an optional error
#[allow(clippy::cast_precision_loss)]
pub(super) fn parse_numeric_hex_str(number: &str) -> ((TokenType, Payload), Option<ErrorType>) {
    // Try parse as i64. SAS only allows up-to 8 bytes (16 HEX digits)
    // for HEX literals, with the first one capped at 9F, so it is
    // slightly less then +/-(2^63 - 1), but can theoretically overflow
    match i64::from_str_radix(number, 16) {
        Ok(value) => ((TokenType::IntegerLiteral, Payload::Integer(value)), None),
        Err(e) => {
            match e.kind() {
                std::num::IntErrorKind::NegOverflow | std::num::IntErrorKind::PosOverflow => {
                    // Wow, very big number;) Ok

                    // First remove leading sign if any, remember the sign
                    let (truncated, negative) = if let Some(stripped) = number.strip_prefix('-') {
                        (stripped, true)
                    } else if let Some(stripped) = number.strip_prefix('+') {
                        (stripped, false)
                    } else {
                        (number, false)
                    };

                    // Check length, SAS itself wil not allow more than 16 HEX digits
                    let (emit_error, truncated) = if truncated.len() > 16 {
                        // Emit an error, but truncate the number to parse something

                        // Unwrap here is safe really, as we know the length is > 16
                        // but we still provide default value to avoid panics
                        (true, truncated.get(..16).unwrap_or(truncated))
                    } else {
                        (false, truncated)
                    };

                    // This must work! We truncated the number to 16 HEX digits
                    let int_vale = u64::from_str_radix(truncated, 16)
                        .expect("The truncated number should be valid HEX number");

                    // Convert to f64 and apply the sign
                    let fvalue = if negative {
                        -(int_vale as f64)
                    } else {
                        int_vale as f64
                    };

                    if emit_error {
                        return (
                            (TokenType::FloatLiteral, Payload::Float(fvalue)),
                            Some(ErrorType::InvalidNumericLiteral),
                        );
                    }

                    ((TokenType::FloatLiteral, Payload::Float(fvalue)), None)
                }
                _ => {
                    // This is an internal error, should not happen
                    (
                        (TokenType::IntegerLiteral, Payload::Integer(0)),
                        Some(ErrorType::InternalError("Failed to parse HEX literal")),
                    )
                }
            }
        }
    }
}
