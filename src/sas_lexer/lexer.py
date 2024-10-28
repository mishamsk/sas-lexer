from collections.abc import Sequence

from msgspec.msgpack import Decoder

from sas_lexer._sas_lexer_rust import _lex_program_from_str
from sas_lexer.error import Error
from sas_lexer.token import Token

LEXER_DECODER = Decoder(tuple[list[Token], list[Error], bytes])


def lex_program_from_str(source: str) -> tuple[Sequence[Token], Sequence[Error], bytes]:
    """Lexes a full SAS program from UTF-8 string and returns a tuple of
    tokens, errors and the string literal buffer for unquoted strings.

    Args:
        source (str): The SAS program to lex.

    Returns:
        tuple[Sequence[Token], Sequence[Error], str]: A tuple of tokens,
        errors and the string literal

    Raises:
        RuntimeError: If the lexer encounters an unrecoverable error.
    """
    return LEXER_DECODER.decode(_lex_program_from_str(source))
