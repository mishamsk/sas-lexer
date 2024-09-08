from msgspec import Struct


class Token(Struct, array_like=True, gc=False, frozen=True):
    channel: int
    token_type: int
    token_index: int
    start: int
    stop: int
    line: int
    column: int
    end_line: int
    end_column: int
    payload: int | float | tuple[int, int] | None
