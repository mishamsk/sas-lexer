pub(super) const fn is_valid_sas_name_start(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '_')
}
