import os
from pathlib import Path
from time import perf_counter_ns

from sas_lexer import lex_str

PERF_ALL_SAMPLES = os.environ["PERF_ALL_SAMPLES"]
PERF_LARGE_SAMPLE = os.environ["PERF_LARGE_SAMPLE"]


def test_wip():
    """Delete this test when done."""
    st = perf_counter_ns()
    ret = lex_str(Path(PERF_LARGE_SAMPLE).read_text())
    print(f"Got {len(ret)} tokens. Elapsed: {(perf_counter_ns() - st) / 1_000_000} ms")

    st = perf_counter_ns()
    file_count = 0
    tokens_count = 0
    for sas_file in Path(PERF_ALL_SAMPLES).rglob("*.sas"):
        sas_file.read_text(errors="replace")

    print(f"Plain read from {file_count} files. Elapsed: {(perf_counter_ns() - st) / 1_000_000} ms")

    st = perf_counter_ns()
    file_count = 0
    tokens_count = 0
    for sas_file in Path(PERF_ALL_SAMPLES).rglob("*.sas"):
        ret = lex_str(sas_file.read_text(errors="replace"))
        file_count += 1
        tokens_count += len(ret)

    print(
        f"Got {tokens_count} tokens from {file_count} files. Elapsed: {(perf_counter_ns() - st) / 1_000_000} ms"
    )
