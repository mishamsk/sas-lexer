"""Compare gungraun (Callgrind) results of a base run with a head run and gate on regressions.

Reads the JSON-lines output of `cargo bench -- --baseline=base --output-format=json`
(one file per feature config), prints a markdown table, appends it to
$GITHUB_STEP_SUMMARY when set, and exits 1 if any gated metric of any benchmark
got worse by `--threshold` percent or more.

Usage: report.py --threshold 10 --title "..." CONFIG=results.jsonl [CONFIG=results.jsonl ...]
"""

from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path

GATED = ("Ir", "EstimatedCycles")
TEXT_COLUMNS = 2  # Benchmark, Features; the rest are right-aligned numbers.
LABELS = {"Ir": "Instructions", "EstimatedCycles": "Est. cycles"}


def load(path: Path) -> list[dict]:
    rows = []
    for raw in path.read_text().splitlines():
        line = raw.strip()
        if line.startswith("{"):
            rows.append(json.loads(line))
    if not rows:
        sys.exit(f"error: no benchmark results in {path}")
    return rows


def metric(bench: dict, name: str) -> tuple[int | None, int | None]:
    profile = next(p for p in bench["profiles"] if p["tool"] == "Callgrind")
    values = profile["data"]["total"]["metrics"][name]["values"]
    return values.get("old"), values.get("new")


def pct(old: int | None, new: int | None) -> float | None:
    if old is None or new is None or old == 0:
        return None
    return (new - old) / old * 100.0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--threshold", type=float, default=10.0, help="fail at >= this %% regression"
    )
    parser.add_argument("--title", default="Performance check (Callgrind)")
    parser.add_argument("--output", type=Path, help="also write the markdown here")
    parser.add_argument("results", nargs="+", help="CONFIG=path/to/results.jsonl")
    args = parser.parse_args()

    header = ["Benchmark", "Features"]
    for m in GATED:
        header += [f"{LABELS[m]} base", f"{LABELS[m]} head", "Δ"]
    header.append("Status")
    lines = [
        f"### {args.title}",
        "",
        f"Gate: any of {', '.join(LABELS[m] for m in GATED)} worse by ≥ {args.threshold:g}% fails. "
        "Counts are deterministic (Valgrind), so any non-zero Δ is a real change in executed code.",
        "",
        "| " + " | ".join(header) + " |",
        "|" + "|".join("---" if i < TEXT_COLUMNS else "---:" for i in range(len(header))) + "|",
    ]

    failures = []
    for spec in args.results:
        config, _, path = spec.partition("=")
        for bench in load(Path(path)):
            name = f"{bench['function_name']}/{bench['id']}"
            row = [f"`{name}`", config]
            status = "ok"
            for m in GATED:
                old, new = metric(bench, m)
                change = pct(old, new)
                row += [
                    f"{old:,}" if old is not None else "n/a",
                    f"{new:,}" if new is not None else "n/a",
                    f"{change:+.2f}%" if change is not None else "new",
                ]
                if change is not None and change >= args.threshold:
                    status = "❌ regression"
                    failures.append(f"{name} [{config}] {m} {change:+.2f}%")
                elif change is not None and change <= -args.threshold and status == "ok":
                    status = "🚀 faster"
            row.append(status if status != "ok" else "✅")
            lines.append("| " + " | ".join(row) + " |")

    lines.append("")
    if failures:
        lines.append(f"**FAILED: {len(failures)} metric(s) regressed by ≥ {args.threshold:g}%:**")
        lines += [f"- {f}" for f in failures]
    else:
        lines.append(f"**PASSED: no metric regressed by ≥ {args.threshold:g}%.**")
    markdown = "\n".join(lines) + "\n"

    print(markdown)
    if args.output:
        args.output.write_text(markdown)
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with Path(summary).open("a", encoding="utf-8") as f:
            f.write(markdown)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
