"""Compare gungraun (Callgrind) results of a base run with a head run and gate on regressions.

Reads the JSON-lines output (`--output-format=json`) of the base run and of the head run
(`--baseline=base`) for every feature config, prints a markdown table, appends it to
$GITHUB_STEP_SUMMARY when set, and exits 1 if any gated metric of any benchmark got worse
by `--threshold` percent or more.

The check fails closed: missing or duplicate benchmarks, configs or measurements, a
benchmark set that differs from the expected list, or a head run that didn't compare
against this base run all make it exit 2 with an INVALID verdict instead of a pass.

Usage:
    report.py --threshold 10 --expected benchmarks.txt --expect-configs default macro_sep \
        --config default base.jsonl head.jsonl --config macro_sep base.jsonl head.jsonl
"""

from __future__ import annotations

import argparse
import json
import math
import os
import sys
from dataclasses import dataclass
from pathlib import Path

GATED = ("Ir", "EstimatedCycles")
LABELS = {"Ir": "Instructions", "EstimatedCycles": "Est. cycles"}
TEXT_COLUMNS = 2  # Benchmark, Features; the rest are right-aligned numbers.
MAX_THRESHOLD = 1000.0

EXIT_OK = 0
EXIT_REGRESSION = 1
EXIT_INVALID = 2


class InvalidResultsError(Exception):
    """The results can't support a verdict (incomplete or inconsistent measurements)."""


@dataclass(frozen=True)
class Row:
    config: str
    name: str
    base: dict[str, int]
    head: dict[str, int]


def read_expected(path: Path) -> list[str]:
    names = []
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.split("#", 1)[0].strip()
        if line:
            names.append(line)
    if not names:
        msg = f"{path}: no expected benchmarks"
        raise InvalidResultsError(msg)
    if len(set(names)) != len(names):
        msg = f"{path}: duplicate benchmark names"
        raise InvalidResultsError(msg)
    return names


def load_run(path: Path, label: str) -> dict[str, dict]:
    """Benchmarks of one run keyed by `function/id`."""
    if not path.is_file():
        msg = f"{label}: results file {path} is missing"
        raise InvalidResultsError(msg)
    runs: dict[str, dict] = {}
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.strip()
        if not line.startswith("{"):
            continue
        bench = json.loads(line)
        name = f"{bench['function_name']}/{bench['id']}"
        if name in runs:
            msg = f"{label}: duplicate results for {name}"
            raise InvalidResultsError(msg)
        runs[name] = bench
    if not runs:
        msg = f"{label}: no benchmark results in {path}"
        raise InvalidResultsError(msg)
    return runs


def metric_values(bench: dict, metric: str, label: str) -> dict:
    profiles = [p for p in bench.get("profiles", []) if p.get("tool") == "Callgrind"]
    if len(profiles) != 1:
        msg = f"{label}: expected exactly one Callgrind profile"
        raise InvalidResultsError(msg)
    try:
        return profiles[0]["data"]["total"]["metrics"][metric]["values"]
    except KeyError:
        msg = f"{label}: metric {metric} missing"
        raise InvalidResultsError(msg) from None


def positive_count(value: object, label: str) -> int:
    if not isinstance(value, int) or isinstance(value, bool) or value <= 0:
        msg = f"{label}: missing or non-positive count ({value!r})"
        raise InvalidResultsError(msg)
    return value


def check_inventory(found: set[str], expected: list[str], label: str) -> None:
    missing = [n for n in expected if n not in found]
    extra = sorted(found - set(expected))
    if missing or extra:
        msg = f"{label}: benchmark set differs from the expected list (missing: {missing}, unexpected: {extra})"
        raise InvalidResultsError(msg)


def collect(
    configs: list[list[str]], expected: list[str], expected_configs: list[str]
) -> list[Row]:
    names = [c[0] for c in configs]
    if len(set(names)) != len(names):
        msg = f"duplicate feature configs: {names}"
        raise InvalidResultsError(msg)
    if sorted(names) != sorted(expected_configs):
        msg = f"feature configs {names} differ from the expected {expected_configs}"
        raise InvalidResultsError(msg)

    rows = []
    for config, base_path, head_path in configs:
        base_run = load_run(Path(base_path), f"[{config}] base")
        head_run = load_run(Path(head_path), f"[{config}] head")
        check_inventory(set(base_run), expected, f"[{config}] base")
        check_inventory(set(head_run), expected, f"[{config}] head")
        for name in expected:
            base, head = {}, {}
            for m in GATED:
                where = f"[{config}] {name} {m}"
                base_values = metric_values(base_run[name], m, f"{where} base")
                head_values = metric_values(head_run[name], m, f"{where} head")
                base[m] = positive_count(base_values.get("new"), f"{where} base")
                head[m] = positive_count(head_values.get("new"), f"{where} head")
                # The head run must have compared against exactly this base run.
                if head_values.get("old") != base[m]:
                    msg = f"{where}: head baseline {head_values.get('old')!r} != base result {base[m]}"
                    raise InvalidResultsError(msg)
            rows.append(Row(config, name, base, head))
    return rows


def pct(old: int, new: int) -> float:
    return (new - old) / old * 100.0


def render(
    rows: list[Row], threshold: float, title: str, notes: list[str]
) -> tuple[str, list[str]]:
    header = ["Benchmark", "Features"]
    for m in GATED:
        header += [f"{LABELS[m]} base", f"{LABELS[m]} head", "Δ"]
    header.append("Status")
    lines = [
        f"### {title}",
        "",
        f"Gate: any of {', '.join(LABELS[m] for m in GATED)} worse by ≥ {threshold:g}% fails. "
        "Counts are deterministic (Valgrind), so any non-zero Δ is a real change in executed code.",
        *(f"{n}  " for n in notes),
        "",
        "| " + " | ".join(header) + " |",
        "|" + "|".join("---" if i < TEXT_COLUMNS else "---:" for i in range(len(header))) + "|",
    ]
    failures = []
    for row in rows:
        cells = [f"`{row.name}`", row.config]
        status = "✅"
        for m in GATED:
            change = pct(row.base[m], row.head[m])
            cells += [f"{row.base[m]:,}", f"{row.head[m]:,}", f"{change:+.2f}%"]
            if change >= threshold:
                status = "❌ regression"
                failures.append(f"{row.name} [{row.config}] {m} {change:+.2f}%")
            elif change <= -threshold and status == "✅":
                status = "🚀 faster"
        cells.append(status)
        lines.append("| " + " | ".join(cells) + " |")
    lines.append("")
    if failures:
        lines.append(f"**FAILED: {len(failures)} metric(s) regressed by ≥ {threshold:g}%:**")
        lines += [f"- {f}" for f in failures]
    else:
        lines.append(f"**PASSED: no metric regressed by ≥ {threshold:g}%.**")
    return "\n".join(lines) + "\n", failures


def threshold_arg(value: str) -> float:
    try:
        threshold = float(value)
    except ValueError:
        threshold = math.nan
    if not math.isfinite(threshold) or not 0 < threshold <= MAX_THRESHOLD:
        msg = f"threshold must be a finite number in (0, {MAX_THRESHOLD:g}], got {value!r}"
        raise argparse.ArgumentTypeError(msg)
    return threshold


def emit(markdown: str, output: Path | None) -> None:
    print(markdown)
    if output:
        output.write_text(markdown, encoding="utf-8")
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with Path(summary).open("a", encoding="utf-8") as f:
            f.write(markdown)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        "--threshold", type=threshold_arg, required=True, help="fail at >= this %% regression"
    )
    parser.add_argument(
        "--expected", type=Path, required=True, help="file listing the gated `function/id` names"
    )
    parser.add_argument(
        "--expect-configs", nargs="+", required=True, help="feature configs that must be present"
    )
    parser.add_argument(
        "--config",
        nargs=3,
        action="append",
        required=True,
        metavar=("NAME", "BASE_JSONL", "HEAD_JSONL"),
    )
    parser.add_argument("--title", default="Performance check (Callgrind)")
    parser.add_argument("--note", action="append", default=[], help="extra line under the title")
    parser.add_argument("--output", type=Path, help="also write the markdown here")
    args = parser.parse_args(argv)

    try:
        rows = collect(args.config, read_expected(args.expected), args.expect_configs)
    except (InvalidResultsError, json.JSONDecodeError, KeyError, TypeError, AttributeError) as e:
        emit(
            f"### {args.title}\n\n**INVALID: the comparison is incomplete, no verdict.**\n\n- {e}\n",
            args.output,
        )
        return EXIT_INVALID

    markdown, failures = render(rows, args.threshold, args.title, args.note)
    emit(markdown, args.output)
    return EXIT_REGRESSION if failures else EXIT_OK


if __name__ == "__main__":
    sys.exit(main())
