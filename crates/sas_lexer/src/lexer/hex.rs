fn parse_hex_literal<S: AsRef<str>>(hex: S) -> Result<u64, ParseIntError> {
    u64::from_str_radix(hex.as_ref(), 16)
}
