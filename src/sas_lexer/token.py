from msgspec import Struct

from sas_lexer.token_channel import TokenChannel
from sas_lexer.token_type import TokenType


class Token(Struct, array_like=True, gc=False, frozen=True):
    """Represents one lexed token.

    Attributes:
        channel (TokenChannel): Channel of the token.
        token_type (TokenType): Type of the token.
        token_index (int): Token index.
        start (int): Zero-based char index of the token start in the source string.
        stop (int): Zero-based char index of the token end in the source string.
            Will point to the character immediately after the token.
        line (int): Starting line of the token, 1-based.
        column (int): Zero-based column of the token start on the start line.
        end_line (int): Ending line of the token, 1-based.
        end_column (int): Zero-based column of the token end on the end line.
                          This is the column of the character immediately after the token.
        payload (int | float | tuple[int, int] | None): Extra data associated with the token.
            For integer and float literals, this will be the corresponding value.
            For MacroVarResolve it is the inverterted precedence for the operator (&),
            where number means the log2n of amp count.
            For [macro] string literals this may be not None and include the start and stop
            byte offsets into the string literal buffer. Taking a slice of the buffer with
            these offsets will give the encoded unquoted string value of the token.
    """

    channel: TokenChannel
    token_type: TokenType
    token_index: int
    start: int
    stop: int
    line: int
    column: int
    end_line: int
    end_column: int
    payload: int | float | tuple[int, int] | None
