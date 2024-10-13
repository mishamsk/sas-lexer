from collections.abc import Sequence

from msgspec.msgpack import Decoder

from sas_lexer._sas_lexer_rust import lex_program_from_str as _lex_program_from_str
from sas_lexer.token import Token

TOK_LIST_DECODER = Decoder(list[Token])


def lex_program_from_str(source: str) -> Sequence[Token]:
    return TOK_LIST_DECODER.decode(_lex_program_from_str(source))
