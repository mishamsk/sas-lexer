"""Compare gungraun (Callgrind) results of a base run with a head run and gate on regressions.

Two commands (stdlib only):

    report.py check ...    (the default when the first argument isn't a command)
        Reads the JSON-lines output (`--output-format=json`) of the base run and of the head run
        (`--baseline=base`) for every feature config, writes a results JSON (`--results`), prints
        the report markdown, and exits 1 if any gated metric of any benchmark got worse by
        `--threshold` percent or more.

        The check fails closed: missing or duplicate benchmarks, configs or measurements, a
        benchmark set that differs from the expected list, or a head run that didn't compare
        against this base run all make it exit 2 with an INVALID verdict instead of a pass.

    report.py comment RESULTS [--bypassed-by LOGIN] [--output FILE] [--step-summary]
        Renders the results JSON as the PR comment / job summary markdown. `--bypassed-by`
        marks a regression as accepted by that (already verified) admin; it never turns an
        INVALID result into anything else.

Usage:
    report.py check --threshold 2 --expected benchmarks.txt --expect-configs default macro_sep \
        --config default base.jsonl head.jsonl --config macro_sep base.jsonl head.jsonl \
        --base-sha SHA --head DESC --tools TEXT --results results.json
    report.py comment results.json --output comment.md
"""

from __future__ import annotations

import argparse
import io
import json
import math
import os
import sys
from dataclasses import dataclass
from pathlib import Path

GATED = ("Ir", "EstimatedCycles")
LABELS = {"Ir": "Instructions", "EstimatedCycles": "Est. cycles"}
MAX_THRESHOLD = 1000.0
# Hidden marker that identifies the sticky PR comment.
MARKER = "<!-- sas-lexer-perf-report -->"
BYPASS_LABEL = "perf-regression-accepted"
RESULTS_VERSION = 1

EXIT_OK = 0
EXIT_REGRESSION = 1
EXIT_INVALID = 2

PASS, REGRESSION, INVALID = "pass", "regression", "invalid"


class InvalidResultsError(Exception):
    """The results can't support a verdict (incomplete or inconsistent measurements)."""


@dataclass(frozen=True)
class Row:
    config: str
    name: str
    # Every metric of the Callgrind total: name -> (base, head).
    metrics: dict[str, tuple[float, float]]


# ---------------------------------------------------------------------------
# Reading and validating gungraun output
# ---------------------------------------------------------------------------


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


def total_metrics(bench: dict, label: str) -> dict:
    profiles = [p for p in bench.get("profiles", []) if p.get("tool") == "Callgrind"]
    if len(profiles) != 1:
        msg = f"{label}: expected exactly one Callgrind profile"
        raise InvalidResultsError(msg)
    try:
        return profiles[0]["data"]["total"]["metrics"]
    except KeyError:
        msg = f"{label}: Callgrind totals missing"
        raise InvalidResultsError(msg) from None


def metric_values(metrics: dict, metric: str, label: str) -> dict:
    try:
        return metrics[metric]["values"]
    except KeyError:
        msg = f"{label}: metric {metric} missing"
        raise InvalidResultsError(msg) from None


def positive_count(value: object, label: str) -> int:
    if not isinstance(value, int) or isinstance(value, bool) or value <= 0:
        msg = f"{label}: missing or non-positive count ({value!r})"
        raise InvalidResultsError(msg)
    return value


def is_number(value: object) -> bool:
    return isinstance(value, (int, float)) and not isinstance(value, bool)


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
            base_all = total_metrics(base_run[name], f"[{config}] {name} base")
            head_all = total_metrics(head_run[name], f"[{config}] {name} head")
            metrics: dict[str, tuple[float, float]] = {}
            for m in GATED:
                where = f"[{config}] {name} {m}"
                base_values = metric_values(base_all, m, f"{where} base")
                head_values = metric_values(head_all, m, f"{where} head")
                base = positive_count(base_values.get("new"), f"{where} base")
                head = positive_count(head_values.get("new"), f"{where} head")
                # The head run must have compared against exactly this base run.
                if head_values.get("old") != base:
                    msg = f"{where}: head baseline {head_values.get('old')!r} != base result {base}"
                    raise InvalidResultsError(msg)
                metrics[m] = (base, head)
            # Everything else is informational: kept when both sides report a number.
            for m, entry in base_all.items():
                if m in metrics or not isinstance(entry, dict):
                    continue
                base = entry.get("values", {}).get("new")
                head = head_all.get(m, {}).get("values", {}).get("new")
                if is_number(base) and is_number(head):
                    metrics[m] = (base, head)
            rows.append(Row(config, name, metrics))
    return rows


# ---------------------------------------------------------------------------
# Verdict and results JSON
# ---------------------------------------------------------------------------


def pct(old: float, new: float) -> float:
    return (new - old) / old * 100.0


def failures(rows: list[Row], threshold: float) -> list[str]:
    out = []
    for row in rows:
        for m in GATED:
            base, head = row.metrics[m]
            change = pct(base, head)
            if change >= threshold:
                out.append(f"`{row.name}` [{row.config}] {LABELS[m]} {change:+.2f}%")
    return out


def results_doc(
    *,
    verdict: str,
    threshold: float,
    meta: dict[str, str],
    rows: list[Row] | None = None,
    error: str | None = None,
) -> dict:
    return {
        "version": RESULTS_VERSION,
        "verdict": verdict,
        "threshold": threshold,
        "gated": list(GATED),
        **meta,
        "error": error,
        "rows": [
            {
                "config": r.config,
                "name": r.name,
                "metrics": {m: list(v) for m, v in r.metrics.items()},
            }
            for r in rows or []
        ],
    }


def rows_from_doc(doc: dict) -> list[Row]:
    return [
        Row(r["config"], r["name"], {m: (v[0], v[1]) for m, v in r["metrics"].items()})
        for r in doc["rows"]
    ]


# ---------------------------------------------------------------------------
# Markdown
# ---------------------------------------------------------------------------


def fmt_pct(threshold: float) -> str:
    return f"{threshold:g}%"


def fmt_delta(base: float, head: float, threshold: float | None = None) -> str:
    if base == head:
        return "±0%"
    if base == 0:
        return f"0 → {head:,}"
    change = pct(base, head)
    text = f"{change:+.2f}%"
    if threshold is not None and change >= threshold:
        text = f"**{text}**"
    return text


def fmt_count(value: float) -> str:
    return f"{value:,}" if isinstance(value, int) else f"{value:,.4g}"


def row_status(row: Row, threshold: float) -> str:
    changes = [pct(*row.metrics[m]) for m in GATED]
    if max(changes) >= threshold:
        return "🔴"
    if min(changes) <= -threshold:
        return "🟢"
    return "⚪"


def plural(n: int, word: str) -> str:
    return f"{n} {word}{'' if n == 1 else 's'}"


def table(header: list[str], align: list[str], rows: list[list[str]]) -> list[str]:
    return [
        "| " + " | ".join(header) + " |",
        "|" + "|".join(align) + "|",
        *("| " + " | ".join(r) + " |" for r in rows),
    ]


def render(doc: dict, bypassed_by: str | None = None) -> str:
    """The sticky PR comment / job summary for one results JSON."""
    threshold = float(doc["threshold"])
    t = fmt_pct(threshold)
    base, head = doc.get("base_sha", "?"), doc.get("head", "?")
    lines = [MARKER]

    if doc["verdict"] == INVALID:
        lines += [
            "### ❌ Perf: INVALID, the comparison is incomplete (no verdict)",
            "",
            f"- {doc.get('error') or 'unknown error'}",
            "",
            f"<sub>Base `{base[:10]}` → head `{head}`. The gate fails closed; an admin bypass "
            "doesn't apply to invalid results.</sub>",
        ]
        return "\n".join(lines) + "\n"

    rows = rows_from_doc(doc)
    regressed = [r for r in rows if row_status(r, threshold) == "🔴"]
    if not regressed:
        lines.append(f"### ✅ Perf: no regressions over {t}")
    elif bypassed_by:
        lines.append(
            f"### ⚠️ Perf: {plural(len(regressed), 'regression')} over {t}, "
            f"accepted by @{bypassed_by} (admin)"
        )
    else:
        lines.append(f"### ❌ Perf: {plural(len(regressed), 'regression')} over {t}")
    lines.append("")
    if regressed:
        lines += [f"- {f}" for f in failures(rows, threshold)]
        lines.append("")
        if bypassed_by:
            lines.append(
                f"The gate passes because of the `{BYPASS_LABEL}` label. Remove the label to "
                "enforce it again."
            )
        else:
            lines.append(
                f"If this is intended, a repo admin can accept it with the `{BYPASS_LABEL}` label."
            )
        lines.append("")

    lines += table(
        ["", "Benchmark", "Features", *(LABELS[m] for m in GATED)],
        [":-:", "---", "---", *("---:" for _ in GATED)],
        [
            [
                row_status(r, threshold),
                f"`{r.name}`",
                r.config,
                *(fmt_delta(*r.metrics[m], threshold) for m in GATED),
            ]
            for r in rows
        ],
    )
    lines += [
        "",
        f"<sub>Base `{base[:10]}` → head `{head}`. 🔴 a gated metric is ≥ {t} worse, "
        f"🟢 ≥ {t} better, ⚪ within ±{t}.</sub>",
        "",
        "<details>",
        "<summary>Full Callgrind results</summary>",
        "",
        f"- Base: `{base}`",
        f"- Head: `{head}`",
        f"- Gate: {' or '.join(f'{LABELS[m]} (`{m}`)' for m in GATED)} worse by ≥ {t} fails.",
        f"- Tools: {doc.get('tools', '?')}",
        "- Counts are deterministic (Valgrind), so any non-zero Δ is a real change in executed code.",
        "",
    ]
    gated_header = ["Benchmark", "Features"]
    for m in GATED:
        gated_header += [f"{LABELS[m]} base", f"{LABELS[m]} head", "Δ"]
    lines += table(
        gated_header,
        ["---", "---", *("---:" for _ in range(3 * len(GATED)))],
        [
            [
                f"`{r.name}`",
                r.config,
                *(
                    cell
                    for m in GATED
                    for cell in (
                        fmt_count(r.metrics[m][0]),
                        fmt_count(r.metrics[m][1]),
                        fmt_delta(*r.metrics[m]),
                    )
                ),
            ]
            for r in rows
        ],
    )
    # Other metrics: integer event counts only (the derived rates are float percentages).
    others: list[str] = []
    for r in rows:
        for m, (b, h) in r.metrics.items():
            if m not in GATED and m not in others and isinstance(b, int) and isinstance(h, int):
                others.append(m)
    if others:
        lines += ["", "Other Callgrind metrics (Δ, not gated):", ""]
        lines += table(
            ["Benchmark", "Features", *others],
            ["---", "---", *("---:" for _ in others)],
            [
                [
                    f"`{r.name}`",
                    r.config,
                    *(fmt_delta(*r.metrics[m]) if m in r.metrics else "n/a" for m in others),
                ]
                for r in rows
            ],
        )
    lines += ["", "</details>"]
    return "\n".join(lines) + "\n"


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def threshold_arg(value: str) -> float:
    try:
        threshold = float(value)
    except ValueError:
        threshold = math.nan
    if not math.isfinite(threshold) or not 0 < threshold <= MAX_THRESHOLD:
        msg = f"threshold must be a finite number in (0, {MAX_THRESHOLD:g}], got {value!r}"
        raise argparse.ArgumentTypeError(msg)
    return threshold


def write_markdown(markdown: str, output: Path | None, *, step_summary: bool = False) -> None:
    print(markdown)
    if output:
        output.write_text(markdown, encoding="utf-8")
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if step_summary and summary:
        with Path(summary).open("a", encoding="utf-8") as f:
            f.write(markdown)


def check_main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(
        prog="report.py check",
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
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
    parser.add_argument("--base-sha", default="?", help="full base commit SHA")
    parser.add_argument("--head", default="?", help="head description, e.g. a short SHA")
    parser.add_argument("--tools", default="?", help="tool versions line for the details")
    parser.add_argument("--results", type=Path, help="write the results JSON here")
    parser.add_argument("--output", type=Path, help="also write the markdown here")
    args = parser.parse_args(argv)

    meta = {"base_sha": args.base_sha, "head": args.head, "tools": args.tools}
    try:
        rows = collect(args.config, read_expected(args.expected), args.expect_configs)
    except (InvalidResultsError, json.JSONDecodeError, KeyError, TypeError, AttributeError) as e:
        doc = results_doc(verdict=INVALID, threshold=args.threshold, meta=meta, error=str(e))
        code = EXIT_INVALID
    else:
        failed = failures(rows, args.threshold)
        doc = results_doc(
            verdict=REGRESSION if failed else PASS, threshold=args.threshold, meta=meta, rows=rows
        )
        code = EXIT_REGRESSION if failed else EXIT_OK
    if args.results:
        args.results.write_text(json.dumps(doc, indent=1) + "\n", encoding="utf-8")
    write_markdown(render(doc), args.output)
    return code


def comment_main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(
        prog="report.py comment", description="Render a results JSON as the PR comment markdown."
    )
    parser.add_argument("results", type=Path)
    parser.add_argument(
        "--bypassed-by", help="login of the admin who accepted the regression (verified by CI)"
    )
    parser.add_argument("--output", type=Path, help="also write the markdown here")
    parser.add_argument(
        "--step-summary", action="store_true", help="append to $GITHUB_STEP_SUMMARY when set"
    )
    args = parser.parse_args(argv)
    if not args.results.exists():
        # perf-check stopped before the comparison (e.g. a build failure): still report it.
        doc = results_doc(
            verdict=INVALID,
            threshold=MAX_THRESHOLD,
            meta={},
            error="no results: the perf check failed before the comparison (see the job log)",
        )
        write_markdown(render(doc), args.output, step_summary=args.step_summary)
        return EXIT_OK
    try:
        doc = json.loads(args.results.read_text(encoding="utf-8"))
        if doc.get("version") != RESULTS_VERSION:
            msg = f"unsupported results version {doc.get('version')!r}"
            raise InvalidResultsError(msg)
        markdown = render(doc, args.bypassed_by)
    except (OSError, InvalidResultsError, json.JSONDecodeError, KeyError, TypeError) as e:
        print(f"cannot render {args.results}: {e}", file=sys.stderr)
        return EXIT_INVALID
    write_markdown(markdown, args.output, step_summary=args.step_summary)
    return EXIT_OK


def main(argv: list[str] | None = None) -> int:
    argv = sys.argv[1:] if argv is None else argv
    if argv[:1] == ["comment"]:
        return comment_main(argv[1:])
    if argv[:1] == ["check"]:
        argv = argv[1:]
    return check_main(argv)


if __name__ == "__main__":
    # The markdown has emoji; don't crash on a console that can't encode them (e.g. cp1252).
    if isinstance(sys.stdout, io.TextIOWrapper):
        sys.stdout.reconfigure(errors="replace")
    sys.exit(main())
