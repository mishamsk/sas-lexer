from msgspec import Struct


class Error(Struct, array_like=True, gc=False, frozen=True):
    error_kind: int
    at_byte_offset: int
    at_char_offset: int
    on_line: int
    at_column: int
    last_token_index: int | None
