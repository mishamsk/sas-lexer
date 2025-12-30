from collections.abc import Sequence
from json import dumps
from pathlib import Path

import pytest
from anycase import to_pascal

from sas_lexer import lex_program_from_str
from sas_lexer.token import Token
from sas_lexer.token_type import TokenType

TESTS_BASE_PATH = Path(__file__).parent
TEST_SAMPLES = TESTS_BASE_PATH / "samples"
TEST_SNAPSHOTS = TESTS_BASE_PATH / "snapshots"


def _get_snapshot_path(sample_file: Path) -> Path:
    """Mimics the insta crate's snapshot path generation."""
    fname = sample_file.name

    return TEST_SNAPSHOTS / f"sas_lexer__lexer__tests__test_sample_files__tokens@{fname}.snap"


def _token_type_to_rust_str(token_type: TokenType) -> str:
    return to_pascal(
        token_type.name,
        {
            "eof": "EOF",
            "ws": "WS",
            "semi": "SEMI",
            "amp": "AMP",
            "percent": "PERCENT",
            "lparen": "LPAREN",
            "rparen": "RPAREN",
            "lcurly": "LCURLY",
            "rcurly": "RCURLY",
            "lbrack": "LBRACK",
            "rbrack": "RBRACK",
            "star": "STAR",
            "excl": "EXCL",
            "excl2": "EXCL2",
            "bpipe": "BPIPE",
            "bpipe2": "BPIPE2",
            "pipe2": "PIPE2",
            "star2": "STAR2",
            "not": "NOT",
            "fslash": "FSLASH",
            "plus": "PLUS",
            "minus": "MINUS",
            "gtlt": "GTLT",
            "ltgt": "LTGT",
            "lt": "LT",
            "le": "LE",
            "ne": "NE",
            "gt": "GT",
            "ge": "GE",
            "pipe": "PIPE",
            "dot": "DOT",
            "comma": "COMMA",
            "colon": "COLON",
            "assign": "ASSIGN",
            "dollar": "DOLLAR",
            "at": "AT",
            "hash": "HASH",
            "question": "QUESTION",
        },
    )


def _to_rust_yaml_repr(val: str) -> str:
    s = dumps(val)[1:-1]
    return f'\\"{s}\\"'


def _token_to_string(token: Token, source: str, str_lit_buf: bytes) -> str:
    """Generates the same string representation as our test printer in Rust."""
    payload_str = "<None>"

    if token.payload is not None:
        match token.payload:
            case int():
                payload_str = f"{token.payload!s}"
            case float() as val:
                epsilon = 1e-9  # A small epsilon value for floating-point comparison
                rounded_val = round(val * 1000.0) / 1000.0

                if abs(rounded_val - val) < epsilon:
                    payload_str = f"{val:.3f}"
                else:
                    payload_str = f"{val:.3e}"
            case (int() as st, int() as en):
                payload_str = _to_rust_yaml_repr(str_lit_buf[st:en].decode("utf-8"))

    token_raw_text = _to_rust_yaml_repr(
        source[token.start : token.stop] if token.stop > token.start else "<no text>"
    )

    return (
        f"[@{token.token_index!s},{token.start}:{token.stop}={token_raw_text},"
        f"<{_token_type_to_rust_str(token.token_type)}>,L{token.line}:C{token.column}-"
        f"L{token.end_line}:C{token.end_column},chl=<{token.channel.name[0]}>,pl={payload_str}]"
    )


def _get_expected_snap_content_from_tokens(
    tokens: Sequence[Token], source: str, str_lit_buf: bytes
):
    header = """\
---
source: crates/sas-lexer/src/lexer/tests/test_sample_files.rs
expression: tokens
---
- "\
"""
    body = '"\n- "'.join(_token_to_string(token, source, str_lit_buf) for token in tokens)
    return header + body + '"\n'


@pytest.mark.parametrize(
    "sample_file",
    TEST_SAMPLES.rglob("*.sas"),
    ids=lambda p: str(p.relative_to(TEST_SAMPLES)) if isinstance(p, Path) else "None",
)
def test_snapshots(sample_file: Path) -> None:
    """Tests that python lexer generates exactly the same output as rust."""
    source = sample_file.read_text()

    tokens, errors, str_lit_buf = lex_program_from_str(source)

    # As of today, we don't have any errors in the lexer tests
    assert not errors

    expected_snap_content = _get_expected_snap_content_from_tokens(tokens, source, str_lit_buf)
    snap_content = _get_snapshot_path(sample_file).read_text()

    assert expected_snap_content == snap_content
