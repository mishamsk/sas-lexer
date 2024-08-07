use unicode_ident::is_xid_continue;

use crate::TokenType;

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
fn is_macro_stat(tok_type: TokenType) -> bool {
    get_macro_stat_token_type_range().contains(&tok_type.into())
}

#[inline]
fn is_macro_quote_call(tok_type: TokenType) -> bool {
    get_macro_quote_call_token_type_range().contains(&tok_type.into())
}

/// Performs lookahead to determine if the current position is a macro call.
///
/// Macro call is anything that is `%identifier` where `identifier` is a valid
/// SAS identifier and not one of the statement identifiers.
///
/// This function will not check whether it is a label, i.e. doesn't do a lookahead
/// past the identifier to see if colon follows. At least in %let cases,
/// it seems like SAS will not allow a label to follow a %let keyword.
///
/// Doesn't consume the input.
///
/// Returns a tuple of:
/// - `Option<TokenType>`: `TokenType` if the current position is a macro call, None otherwise.
/// - `u32`: number of characters to consume if it is a macro call.
pub(super) fn is_macro_call(cursor: &Cursor, allow_quote_call: bool) -> (Option<TokenType>, u32) {
    debug_assert_eq!(cursor.peek(), '%');

    // Since in most cases it will be a macro call, we clone the cursor
    // to do lookahead right away
    let mut la_view = cursor.clone();

    // Move past the % to the first character of the identifier
    la_view.advance();

    // Start tracking whether the identifier is ASCII
    // It is necessary, as we need to upper case the identifier if it is ASCII
    // for checking against statement anmes, and if it is not ASCII,
    // we know it is not a keyword and can skip the test right away
    let mut is_ascii = true;

    // Eat the identifier. We can safely use `is_xid_continue` becase the caller
    // already checked that the first character is a valid start of an identifier
    la_view.eat_while(|c| {
        if c.is_ascii() {
            matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
        } else {
            is_ascii = false;
            is_xid_continue(c)
        }
    });

    // If the identifier is not ASCII, we can safely return true
    // must be a macro call
    if !is_ascii {
        return (
            Some(TokenType::MacroIdentifier),
            la_view.char_offset() - cursor.char_offset(),
        );
    }

    // Ok, ascii - check if we match a statement

    // My guess is this should be quicker than capturing the value as we consume it
    // avoids the allocation and copying
    // The last -1 is to exclude the % when taking the slice below
    let ident_bytes = cursor.remaining_len() - la_view.remaining_len() - 1;
    let ident = cursor.as_str()[1..ident_bytes as usize].to_ascii_uppercase();

    if let Some(kw_tok_type) = parse_macro_keyword(&ident) {
        if is_macro_stat(kw_tok_type) {
            // It is a statement, not a macro call
            (None, 0)
        } else if is_macro_quote_call(kw_tok_type) && !allow_quote_call {
            // A quote call that is not allowed
            (None, 0)
        } else {
            // Looks like a built macro function call
            (
                Some(kw_tok_type),
                la_view.char_offset() - cursor.char_offset(),
            )
        }
    } else {
        match ident.as_str() {
            // Nrstr is not a keyword, but a special case of quoted literal
            "NRSTR" => {
                if !allow_quote_call {
                    return (None, 0);
                }

                (
                    Some(TokenType::NrStrLiteral),
                    la_view.char_offset() - cursor.char_offset(),
                )
            }
            _ => {
                // Not a statement, not a quote call, must be a macro call
                (
                    Some(TokenType::MacroIdentifier),
                    la_view.char_offset() - cursor.char_offset(),
                )
            }
        }
    }
}
