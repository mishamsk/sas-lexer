from collections.abc import Sequence

from msgspec.msgpack import Decoder

from sas_lexer._sas_lexer_rust import lex_str as _lex_str
from sas_lexer.token import Token

TOK_LIST_DECODER = Decoder(list[Token])


def lex_str(source: str) -> Sequence[Token]:
    return TOK_LIST_DECODER.decode(_lex_str(source))
