use super::error::ErrorKind;

/// Parses a SAS hex string literal into an actual string.
///
/// `pending_token_text` should be the entire text including quotes and trailing 'x'.
///
/// # Arguments
///
/// * `pending_token_text` - The text to parse
///
/// # Returns
///
/// A decoded string (always using Unicode, no locale).
pub(super) fn parse_sas_hex_string(pending_token_text: &str) -> Result<String, ErrorKind> {
    debug_assert!(
        pending_token_text.starts_with(|c| c == '\'' || c == '"')
            && (pending_token_text.ends_with("'x")
                || pending_token_text.ends_with("\"x")
                || pending_token_text.ends_with("'X")
                || pending_token_text.ends_with("\"X"))
    );

    let cleaned_text = pending_token_text
        .get(1..pending_token_text.len() - 2)
        .ok_or_else(|| ErrorKind::InvalidHexStringConstant)?
        .replace(",", ""); // SAS allows commas in hex strings

    let bytes_result: Result<Vec<u8>, ErrorKind> = (0..cleaned_text.len())
        .step_by(2)
        .map(move |i| {
            let hex_char = cleaned_text
                .get(i..i + 2)
                .ok_or_else(|| ErrorKind::InvalidHexStringConstant)?;
            u8::from_str_radix(hex_char, 16).map_err(|_| ErrorKind::InvalidHexStringConstant)
        })
        .collect();

    let bytes = bytes_result?;

    String::from_utf8(bytes).map_err(|_| ErrorKind::InvalidHexStringConstant)
}
