use super::sas_lang::is_valid_sas_name_start;

/// Predicate to check if an encountered ampersand is a start of macro variable
/// reference or expression.
///
/// Must be passed an iterator that starts with the ampersand.
///
/// Consumes the iterator! Pass a clone if you need to keep the original.
/// Returns a tuple of:
/// - `bool`: `true` if the ampersand is a start of macro variable reference/expr,
///     `false` otherwise.
/// - `u32`: number of ampersands encountered.
pub(crate) fn is_macro_amp<I: Iterator<Item = char>>(mut chars: I) -> (bool, u32) {
    // SAFETY: lexer guarantees that there are at max u32 chars in the input
    let mut amp_count: u32 = 0;

    loop {
        match chars.next() {
            Some('&') => {
                amp_count += 1;
                continue;
            }
            Some(c) if is_valid_sas_name_start(c) => return (true, amp_count),
            _ => return (false, amp_count),
        }
    }
}

/// Predicate to check if an encountered percent is a start of macro expression
/// or statement.
///
/// Must be passed an iterator that starts with the percent.
///
/// Consumes the iterator! Pass a clone if you need to keep the original.
pub(crate) fn is_macro_percent<I: Iterator<Item = char>>(mut chars: I) -> bool {
    return false;
    !todo!();
}
