use crate::TokenType;
use smol_str::SmolStr;
use std::result::Result;
use unicode_ident::is_xid_continue;

use super::{
    cursor::Cursor,
    sas_lang::is_valid_sas_name_start,
    token_type::{
        get_macro_quote_call_token_type_range, get_macro_stat_token_type_range, parse_macro_keyword,
    },
};

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
pub(super) fn is_macro_amp<I: Iterator<Item = char>>(mut chars: I) -> (bool, u32) {
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
/// Must be passed a char following the percent sign.
pub(super) fn is_macro_percent(follow_char: char, in_eval_context: bool) -> bool {
    match follow_char {
        // Macro comment
        '*' => true,
        // Expermientally shown to work! (ignores the %)
        // e.g. `%^ 0` returned 1 (true)
        | '~' | '^'
        // Expermientally shown to kinda work! makes the expression false
        // e.g. `0 %= 0` returned 0, and `%= eq %=` is false, but `%= or 1` is true
        | '=' if in_eval_context => true,
        c if is_valid_sas_name_start(c) => true,
        _ => false,
    }
}

#[inline]
pub(super) fn is_macro_stat(tok_type: TokenType) -> bool {
    get_macro_stat_token_type_range().contains(&tok_type.into())
}

#[inline]
pub(super) fn is_macro_quote_call(tok_type: TokenType) -> bool {
    get_macro_quote_call_token_type_range().contains(&tok_type.into())
}

/// Consumes the cursor starting at % and followed by a valid sas name start,
/// returning a token type (either one of the built-in call/stats) or a macro
/// identifier token.
///
/// We are not making distinction between a label and a custom call, i.e. doesn't do a lookahead
/// past the identifier to see if colon follows. So far it seems that SAS would
/// always fail if a label appears in a place where we assume only a macro call
/// can be, so this should be ok.
///
/// Consumes the input! So if a lookeahed is necessary - pass a clone of the main
/// cursor.
///
/// Returns a tuple of:
/// - `TokenType`: `TokenType`
/// - `u32`: number of characters to consume if it is a macro call.
///
/// Error in this function means a bug, but is returned for safety
pub(super) fn lex_macro_call_stat_or_label(
    cursor: &mut Cursor,
) -> Result<(TokenType, u32), &'static str> {
    debug_assert_eq!(cursor.peek(), Some('%'));
    debug_assert!(is_valid_sas_name_start(cursor.peek_next()));

    let start_rem_length = cursor.remaining_len();
    let start_char_offset = cursor.char_offset();
    let source_view = cursor.as_str();

    // Move past the % to the first character of the identifier
    cursor.advance();

    // Start tracking whether the identifier is ASCII
    // It is necessary, as we need to upper case the identifier if it is ASCII
    // for checking against statement names, and if it is not ASCII,
    // we know it is not a keyword and can skip the test right away
    let mut is_ascii = true;

    // Eat the identifier. We can safely use `is_xid_continue` becase the caller
    // already checked that the first character is a valid start of an identifier
    cursor.eat_while(|c| {
        if c.is_ascii() {
            matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
        } else if is_xid_continue(c) {
            is_ascii = false;
            true
        } else {
            false
        }
    });

    // If the identifier is not ASCII, we can safely return true
    // must be a macro call
    if !is_ascii {
        return Ok((
            TokenType::MacroIdentifier,
            cursor.char_offset() - start_char_offset,
        ));
    }

    // Ok, ascii - check if we match a statement

    // My guess is this should be quicker than capturing the value as we consume it
    // avoids the allocation and copying
    let ident_end_byte_offset = start_rem_length - cursor.remaining_len();

    let ident = source_view
        .get(1..ident_end_byte_offset as usize)
        .ok_or("Unexpected error getting ident slice in `lex_macro_call_stat_or_label`")?
        .chars()
        .map(|c| c.to_ascii_uppercase())
        .collect::<SmolStr>();

    if ident.is_empty() {
        // Something like %*
        return Err(
            "Unexpected error in `lex_macro_call_stat_or_label` - no identifier followed %",
        );
    }

    Ok((
        parse_macro_keyword(&ident).unwrap_or(TokenType::MacroIdentifier),
        cursor.char_offset() - start_char_offset,
    ))
}
