use crate::TokenType;
use smol_str::SmolStr;
use std::result::Result;
use unicode_ident::is_xid_continue;

use super::{
    cursor::Cursor,
    sas_lang::is_valid_sas_name_start,
    token_type::{
        parse_macro_keyword, TokenTypeMacroCallOrStat, MACRO_QUOTE_CALL_TOKEN_TYPE_RANGE,
        MACRO_STAT_TOKEN_TYPE_RANGE,
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

#[inline]
pub(super) fn is_macro_eval_quotable_op(c: char) -> bool {
    // Expermientally shown to work! (ignores the %)
    // e.g. `%^ 0` returned 1 (true)
    ['~', '^', '='].contains(&c)
}

/// Predicate to check if an encountered percent is a start of macro expression
/// or statement.
///
/// Must be passed a char following the percent sign.
pub(super) fn is_macro_percent(follow_char: char, in_eval_context: bool) -> bool {
    match follow_char {
        // Macro comment
        '*' => true,
        c if is_valid_sas_name_start(c) || (in_eval_context && is_macro_eval_quotable_op(c)) => {
            true
        }
        _ => false,
    }
}

#[inline]
pub(super) const fn is_macro_stat_tok_type(tok_type: TokenType) -> bool {
    let tt_i = tok_type as u16;
    MACRO_STAT_TOKEN_TYPE_RANGE.0 <= tt_i && MACRO_STAT_TOKEN_TYPE_RANGE.1 >= tt_i
}

#[inline]
pub(super) const fn is_macro_quote_call_tok_type(tok_type: TokenType) -> bool {
    let tt_i = tok_type as u16;
    MACRO_QUOTE_CALL_TOKEN_TYPE_RANGE.0 <= tt_i && MACRO_QUOTE_CALL_TOKEN_TYPE_RANGE.1 >= tt_i
}

#[inline]
pub(super) const fn is_macro_eval_logical_op(tok_type: TokenType) -> bool {
    matches!(
        tok_type,
        TokenType::LT
            | TokenType::KwLT
            | TokenType::LE
            | TokenType::KwLE
            | TokenType::ASSIGN
            | TokenType::KwEQ
            | TokenType::HASH
            | TokenType::KwIN
            | TokenType::NE
            | TokenType::KwNE
            | TokenType::GT
            | TokenType::KwGT
            | TokenType::GE
            | TokenType::KwGE
    )
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
) -> Result<(TokenTypeMacroCallOrStat, u32), &'static str> {
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
            TokenTypeMacroCallOrStat::MacroIdentifier,
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

    parse_macro_keyword(&ident)
        .map_or(Ok(TokenTypeMacroCallOrStat::MacroIdentifier), |t| {
            TokenTypeMacroCallOrStat::try_from(t)
        })
        .map(|t| (t, cursor.char_offset() - start_char_offset))
        .map_err(|()| "Unexpected error in `parse_macro_keyword` - not a keyword returned")
}

/// Predicate to check if the following chracters are one of macro logical
/// expression mnemonics (eq, ne, lt, le, gt, ge, and, or, not, in).
///
/// Must be passed an iterator that starts with the first character
/// of the possible mnemonic.
///
/// Consumes the iterator! Pass a clone if you need to keep the original.
/// Returns a tuple of:
/// - `Option<TokenType>`: `Some(TokenType)` if the mnemonic is a macro logical
///    expression mnemonic, `None` otherwise.
/// - `u32`: number of symbols in mnemonic besides the start char.
pub(super) fn is_macro_eval_mnemonic<I: Iterator<Item = char>>(
    mut chars: I,
) -> (Option<TokenType>, u32) {
    // We must check not just the keyword, but also that it is followed by a
    // non-identifier character
    let Some(start_char) = chars.next() else {
        return (None, 0);
    };

    debug_assert!(matches!(
        start_char,
        'e' | 'n' | 'l' | 'g' | 'a' | 'o' | 'i' | 'E' | 'N' | 'L' | 'G' | 'A' | 'O' | 'I'
    ));

    let Some(next_char) = chars.next() else {
        return (None, 0);
    };

    let second_next_char = chars.next().unwrap_or(' ');
    let second_next_non_id = !is_xid_continue(second_next_char);

    match (start_char, next_char, second_next_non_id) {
        // Simple cases
        ('e' | 'E', 'q' | 'Q', true) => (Some(TokenType::KwEQ), 1),
        ('i' | 'I', 'n' | 'N', true) => (Some(TokenType::KwIN), 1),
        ('o' | 'O', 'r' | 'R', true) => (Some(TokenType::KwOR), 1),
        // Now two symbol, but with options
        ('l' | 'L', 't' | 'T', true) => (Some(TokenType::KwLT), 1),
        ('l' | 'L', 'e' | 'E', true) => (Some(TokenType::KwLE), 1),
        ('g' | 'G', 't' | 'T', true) => (Some(TokenType::KwGT), 1),
        ('g' | 'G', 'e' | 'E', true) => (Some(TokenType::KwGE), 1),
        ('a' | 'A', 'n' | 'N', false) if ['d', 'D'].contains(&second_next_char) => {
            if is_xid_continue(chars.next().unwrap_or(' ')) {
                (None, 0)
            } else {
                (Some(TokenType::KwAND), 2)
            }
        }
        ('n' | 'N', 'e' | 'E', true) => (Some(TokenType::KwNE), 1),
        ('n' | 'N', 'o' | 'O', false) if ['t', 'T'].contains(&second_next_char) => {
            if is_xid_continue(chars.next().unwrap_or(' ')) {
                (None, 0)
            } else {
                (Some(TokenType::KwNOT), 2)
            }
        }
        _ => (None, 0),
    }
}

/// Predicate to do a lookahead and check if the following `%ccc` is strictly
/// a macro statement keyword.
///
/// Must be passed an iterator that starts with the first % character
///
/// Consumes the iterator! Pass a clone if you need to keep the original.
/// Returns a tuple of:
/// - `Option<TokenType>`: `Some(TokenType)` if the mnemonic is a macro logical
///    expression mnemonic, `None` otherwise.
/// - `u32`: number of symbols in mnemonic besides the start char.
pub(super) fn is_macro_stat<I: Iterator<Item = char> + Clone>(chars: I) -> bool {
    debug_assert!(chars.clone().next().map_or(false, |c| c == '%'));

    // Unfortunatelly this one needs a very inefficient lookahead
    // to check if we have any statement upfront.
    let ident = chars
        // Move past the % to the first character of the identifier
        .skip(1)
        .map(|c| {
            // For simplicity of code, given rare realistic usage of unicode,
            // we just replace unicode with empty char, which will effectivly
            // make the following logic correct, since the first non-ascii
            // char guarantees this is not a statement keyword
            if c.is_ascii() {
                c.to_ascii_uppercase()
            } else {
                ' '
            }
        })
        .take_while(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
        .collect::<SmolStr>();

    parse_macro_keyword(&ident).is_some_and(is_macro_stat_tok_type)
}
