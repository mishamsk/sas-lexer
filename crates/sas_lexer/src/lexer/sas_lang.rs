use unicode_ident::is_xid_start;

#[inline]
pub(super) fn is_valid_sas_name_start(c: char) -> bool {
    is_xid_start(c)
}
