use std::result::Result;
use unicode_ident::is_xid_continue;

use super::{
    cursor::Cursor,
    error::ErrorKind,
    sas_lang::{is_valid_sas_name_continue, is_valid_unicode_sas_name_start},
    token_type::{
        parse_macro_keyword, TokenType, TokenTypeMacroCallOrStat,
        MACRO_QUOTE_CALL_TOKEN_TYPE_RANGE, MACRO_STAT_TOKEN_TYPE_RANGE, MAX_MKEYWORDS_LEN,
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
///   `false` otherwise.
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
            Some(c) if is_valid_unicode_sas_name_start(c) => return (true, amp_count),
            _ => return (false, amp_count),
        }
    }
}

pub(super) fn get_macro_resolve_ops_from_amps(amp_count: u32) -> Vec<u8> {
    // The way SAS works, effectively (but how SAS engine process it in reality)
    // the N consecutive amps become K resolve operations. From right to left
    // each operation is some power of 2, starting from 0 (1 amp).
    // E.g `&&&var._` is `&&((&var.)_)` and not `&(&(&var.))_`

    // So we just need to collect bit positions of the amps to get the
    // list of resolve operations with their corresponding powers of 2.
    // These powers correspond to reverse precedence of the resolve operation.

    (0..32u8)
        .rev()
        .filter(|i| (amp_count & (1 << i)) != 0)
        .collect()
}

#[inline]
pub(super) fn is_macro_eval_quotable_op(c: char) -> bool {
    // Experimentally shown to work! (ignores the %)
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
        c if is_valid_unicode_sas_name_start(c)
            || (in_eval_context && is_macro_eval_quotable_op(c)) =>
        {
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

#[inline]
#[cfg(feature = "macro_sep")]
pub(super) const fn needs_macro_sep(
    prev_token_type: Option<TokenType>,
    tok_type: TokenType,
) -> bool {
    // Not following a proper statement delimiter
    // And precedes a standalone macro statement
    !matches!(
        prev_token_type,
        None | Some(
            TokenType::SEMI | TokenType::MacroLabel | TokenType::KwmThen | TokenType::KwmElse
        )
    ) && matches!(
        tok_type,
        TokenType::MacroLabel
            | TokenType::KwmAbort
            | TokenType::KwmCopy
            | TokenType::KwmDisplay
            | TokenType::KwmGlobal
            | TokenType::KwmGoto
            | TokenType::KwmInput
            | TokenType::KwmLocal
            | TokenType::KwmPut
            | TokenType::KwmReturn
            | TokenType::KwmSymdel
            | TokenType::KwmSyscall
            | TokenType::KwmSysexec
            | TokenType::KwmSyslput
            | TokenType::KwmSysmacdelete
            | TokenType::KwmSysmstoreclear
            | TokenType::KwmSysrput
            | TokenType::KwmWindow
            | TokenType::KwmMacro
            | TokenType::KwmMend
            | TokenType::KwmLet
            | TokenType::KwmIf
            | TokenType::KwmElse
            | TokenType::KwmDo
            | TokenType::KwmEnd
    )
}

/// Consumes the cursor starting at a valid sas name start after %,
/// returning a token type (either one of the built-in call/stats) or a macro
/// identifier token.
///
/// Cursor should be advanced past the % character.
///
/// We are not making distinction between a label and a custom call, i.e. doesn't do a lookahead
/// past the identifier to see if colon follows here. Instead the logic
/// is handled by the main lexer loop alongside disambiguation between
/// parenthesis-less and normal macro calls
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
) -> Result<(TokenTypeMacroCallOrStat, u32), ErrorKind> {
    debug_assert!(
        cursor.peek().is_some_and(is_valid_unicode_sas_name_start),
        "Unexpected first character in the cursor: {:?}",
        cursor.peek()
    );

    let start_rem_length = cursor.remaining_len();
    let start_char_offset = cursor.char_offset();
    let source_view = cursor.as_str();

    // Start tracking whether the identifier is ASCII
    // It is necessary, as we need to upper case the identifier if it is ASCII
    // for checking against statement names, and if it is not ASCII,
    // we know it is not a keyword and can skip the test right away.
    // And the only reason we even bother with unicode is because
    // apparently unicode macro labels actually work in SAS despite the docs...
    let mut is_ascii = true;

    // Eat the identifier. We can safely use `is_xid_continue` because the caller
    // already checked that the first character is a valid start of an identifier
    cursor.eat_while(|c| {
        if c.is_ascii() {
            is_valid_sas_name_continue(c)
        } else if is_xid_continue(c) {
            is_ascii = false;
            true
        } else {
            false
        }
    });

    let pending_ident_len = (start_rem_length - cursor.remaining_len()) as usize;
    let pending_ident = source_view
        .get(..pending_ident_len)
        .ok_or(ErrorKind::InternalErrorOutOfBounds)?;

    // If the identifier is not ASCII or longer then max length,
    // we can safely return true must be a macro call
    if !is_ascii || pending_ident_len > MAX_MKEYWORDS_LEN {
        return Ok((
            TokenTypeMacroCallOrStat::MacroIdentifier,
            cursor.char_offset() - start_char_offset,
        ));
    }

    // Ok, ascii and short enough - check if we match a macro keyword

    // This is much quicker than capturing the value as we consume the cursor.
    // Using fixed size buffer, similar to SmolStr crate and others
    let mut buf = [0u8; MAX_MKEYWORDS_LEN];

    #[allow(clippy::indexing_slicing)]
    for (i, c) in pending_ident.as_bytes().iter().enumerate() {
        buf[i] = c.to_ascii_uppercase();
    }

    #[allow(unsafe_code, clippy::indexing_slicing)]
    let ident = unsafe { ::core::str::from_utf8_unchecked(&buf[..pending_ident_len]) };

    parse_macro_keyword(ident)
        .map_or(Ok(TokenTypeMacroCallOrStat::MacroIdentifier), |t| {
            TokenTypeMacroCallOrStat::try_from(t)
        })
        .map(|t| (t, cursor.char_offset() - start_char_offset))
        .map_err(|()| ErrorKind::InternalErrorOutOfBounds)
}

/// Predicate to check if the following characters are one of macro logical
/// expression mnemonics (eq, ne, lt, le, gt, ge, and, or, not, in).
///
/// Must be passed an iterator that starts with the first character
/// of the possible mnemonic.
///
/// Consumes the iterator! Pass a clone if you need to keep the original.
/// Returns a tuple of:
/// - `Option<TokenType>`: `Some(TokenType)` if the mnemonic is a macro logical
///   expression mnemonic, `None` otherwise.
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
/// Must be passed a slice that starts with the first % character
pub(super) fn is_macro_stat(input: &str) -> bool {
    debug_assert!(input.as_bytes().iter().next().is_some_and(|&c| c == b'%'));

    // Unfortunately this one needs a very inefficient lookahead
    // to check if we have any statement upfront.
    let mut is_ascii = true;

    // Start past the % to the first character of the identifier
    let pending_ident_len = input[1..]
        .find(|c: char| {
            if c.is_ascii() {
                !is_valid_sas_name_continue(c)
            } else if is_xid_continue(c) {
                is_ascii = false;
                false
            } else {
                true
            }
        })
        .unwrap_or_else(|| input.len() - 1);
    let pending_ident = &input[1..=pending_ident_len];

    if !is_ascii || pending_ident_len > MAX_MKEYWORDS_LEN {
        return false;
    }

    // Ok, ascii and short enough - check if we match a macro keyword

    // This is much quicker than capturing the value as we consume the cursor.
    // Using fixed size buffer, similar to SmolStr crate and others
    let mut buf = [0u8; MAX_MKEYWORDS_LEN];

    #[allow(clippy::indexing_slicing)]
    for (i, c) in pending_ident.as_bytes().iter().enumerate() {
        buf[i] = c.to_ascii_uppercase();
    }

    #[allow(unsafe_code, clippy::indexing_slicing)]
    let ident = unsafe { ::core::str::from_utf8_unchecked(&buf[..pending_ident_len]) };

    parse_macro_keyword(ident).is_some_and(is_macro_stat_tok_type)
}
