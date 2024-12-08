use strum::EnumIs;
use unicode_ident::is_xid_start;

/// This is the unicode version of allowed SAS name start characters.
///
/// Despite docs, it seems like SAS only complains about unicode
/// in macro definitions and macro calls, but unicode macro labels
/// actually work. Unicode macro variables do not complain, but
/// just skipped in let statements.
///
/// But due to the fact that goto works with unicode we have to use
/// full unicode check more or less everywhere...
#[inline]
pub(super) fn is_valid_unicode_sas_name_start(c: char) -> bool {
    is_xid_start(c) || c == '_'
}

/// ASCII only valid SAS Name start character
#[inline]
pub(super) fn is_valid_sas_name_start(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '_')
}

/// ASCII only valid SAS Name continue character.
///
/// Tehcnically docs say that user names can't end in a number,
/// so this should have been `valid_mid` really, but lexing
/// errors around last chars of identifiers is not worth the effort.
#[inline]
pub(super) fn is_valid_sas_name_continue(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
}

#[derive(Debug, EnumIs)]
pub(super) enum StringLiteralQuote {
    Single,
    Double,
}
