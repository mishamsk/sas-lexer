from msgspec import Struct

from sas_lexer.error_kind import ErrorKind


class Error(Struct, array_like=True, gc=False, frozen=True):
    error_kind: ErrorKind
    at_byte_offset: int
    at_char_offset: int
    on_line: int
    at_column: int
    last_token_index: int | None
