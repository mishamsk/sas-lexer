use super::buffer::Payload;
/// A collection of functions that converts
/// SAS numeric literals (hex & decimal) to their respective values
use std::str::FromStr;

use super::error::ErrorType;
use super::token_type::TokenType;

/// Tries to parses a possibly valid SAS number.
///
/// # Returns
///
/// A tuple with the parsed token and an optional error
#[allow(clippy::cast_precision_loss)]
pub(super) fn parse_numeric(number: &str) -> ((TokenType, Payload), Option<ErrorType>) {
    let Ok(fvalue) = f64::from_str(number) else {
        return (
            (TokenType::IntegerLiteral, Payload::Integer(0)),
            Some(ErrorType::InvalidNumericLiteral),
        );
    };

    // See if it is an integer
    if fvalue.fract() == 0.0 && fvalue.abs() < u64::MAX as f64 {
        // For integers we need to emit different tokens, depending on
        // the presence of the dot as we use different token types.
        // The later is unfortunatelly necesasry due to SAS numeric formats
        // context sensitivity and no way of disambiguating between a number `1.`
        // and the same numeric format `1.`

        // Leading 0 - we can emit the integer token, can't be a format
        #[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
        let payload = Payload::Integer(fvalue as u64);

        // Unwrap here is safe, as we know the length is > 0
        // but we still provide default value to avoid panics
        match *number.as_bytes().first().unwrap_or(&b'_') {
            b'0' => ((TokenType::IntegerLiteral, payload), None),
            _ => {
                if number.contains('.') {
                    ((TokenType::IntegerDotLiteral, payload), None)
                } else {
                    ((TokenType::IntegerLiteral, payload), None)
                }
            }
        }
    } else {
        // And for floats, similarly it is important whether it
        // it was created from scientific notation or not, but
        // this function only handles the decimal literals, so
        // no need to check for that
        ((TokenType::FloatLiteral, Payload::Float(fvalue)), None)
    }
}

/// Parses a possibly valid SAS HEX number (sans the trailing `x` or `X`).
///
/// # Returns
///
/// A tuple with the parsed token and an optional error
#[allow(clippy::cast_precision_loss)]
pub(super) fn parse_numeric_hex_str(number: &str) -> ((TokenType, Payload), Option<ErrorType>) {
    // Try parse as u64. SAS only allows up-to 8 bytes (16 HEX digits)
    // for HEX literals, with the first one capped at 9F, so it is
    // slightly less then +/-(2^63 - 1) and theoretically can't overflow
    // but who knows what SAS users can come up with;)
    match u64::from_str_radix(number, 16) {
        Ok(value) => ((TokenType::IntegerLiteral, Payload::Integer(value)), None),
        Err(e) => {
            match e.kind() {
                std::num::IntErrorKind::NegOverflow | std::num::IntErrorKind::PosOverflow => {
                    // Wow, very big number;) Ok

                    // Check length, SAS itself wil not allow more than 16 HEX digits
                    let (emit_error, truncated) = if number.len() > 16 {
                        // Emit an error, but truncate the number to parse something

                        // Unwrap here is safe really, as we know the length is > 16
                        // but we still provide default value to avoid panics
                        (true, number.get(..16).unwrap_or(number))
                    } else {
                        (false, number)
                    };

                    // This must work! We truncated the number to 16 HEX digits
                    let int_vale = u64::from_str_radix(truncated, 16)
                        .expect("The truncated number should be valid HEX number");

                    // Convert to f64 and apply the sign
                    let fvalue = int_vale as f64;

                    if emit_error {
                        return (
                            (TokenType::FloatLiteral, Payload::Float(int_vale as f64)),
                            Some(ErrorType::InvalidNumericLiteral),
                        );
                    }

                    ((TokenType::FloatLiteral, Payload::Float(fvalue)), None)
                }
                _ => {
                    // This is an internal error if called from regular lexer
                    // or an invalid HEX literal when called from the macro eval lexer
                    (
                        (TokenType::IntegerLiteral, Payload::Integer(0)),
                        Some(ErrorType::InternalErrorFailedToParseHexLiteral),
                    )
                }
            }
        }
    }
}
