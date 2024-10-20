import os
from pathlib import Path
from time import perf_counter_ns

from sas_lexer import lex_program_from_str

SAS_LEX_SAMPLES = os.environ["SAS_LEX_SAMPLES"]
PERF_LARGE_SAMPLE = os.environ["PERF_LARGE_SAMPLE"]


def test_wip():
    """Delete this test when done."""
    st = perf_counter_ns()
    tokens, errors, str_lit_buf = lex_program_from_str(Path(PERF_LARGE_SAMPLE).read_text())
    print(
        f"Got {len(tokens)} tokens, {len(errors)} errors, {len(str_lit_buf)} string bufffer len."
        f" Elapsed: {(perf_counter_ns() - st) / 1_000_000} ms"
    )

    st = perf_counter_ns()
    file_count = 0
    tokens_count = 0
    for sas_file in Path(SAS_LEX_SAMPLES).rglob("*.sas"):
        file_count += 1
        sas_file.read_text(errors="replace")

    print(f"Plain read from {file_count} files. Elapsed: {(perf_counter_ns() - st) / 1_000_000} ms")

    st = perf_counter_ns()
    file_count = 0
    tokens_count = 0
    errors_count = 0
    str_lit_buf_len = 0
    for sas_file in Path(SAS_LEX_SAMPLES).rglob("*.sas"):
        tokens, errors, str_lit_buf = lex_program_from_str(sas_file.read_text(errors="replace"))
        file_count += 1
        tokens_count += len(tokens)
        errors_count += len(errors)
        str_lit_buf_len += len(str_lit_buf)

    print(
        f"Got {tokens_count} tokens from {file_count} files.\n"
        f"A total of {errors_count} errors and {str_lit_buf_len} buffer lengths.\n"
        f"Elapsed: {(perf_counter_ns() - st) / 1_000_000} ms"
    )
