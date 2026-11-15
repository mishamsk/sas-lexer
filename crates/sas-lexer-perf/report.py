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
        INVALID result into anything else. Missing or unreadable results render as INVALID.

    report.py gate RESULTS --rc RC --base-sha SHA --expected FILE --expect-configs NAME...
            [--bypassed-by LOGIN] [--status]
        The CI gate decision. The comparison counts as a regression (the only bypassable state)
        or a pass only if the `check` exit code RC and a schema-valid results JSON for exactly
        this base and inventory agree and the rows confirm the verdict; anything else is INVALID
        and fails, even with a bypass. `--status` only prints pass/regression/invalid.

Usage:
    report.py check --threshold 2 --expected benchmarks.txt --expect-configs default macro_sep \
        --config default base.jsonl head.jsonl --config macro_sep base.jsonl head.jsonl \
        --base-sha SHA --head DESC --tools TEXT --results results.json
    report.py comment results.json --output comment.md
"""

import argparse
import io
import json
import math
import os
import sys
import traceback
from dataclasses import dataclass
from pathlib import Path
from typing import Any

GATED = ("Ir", "EstimatedCycles")
LABELS = {"Ir": "Instructions", "EstimatedCycles": "Est. cycles"}
MAX_THRESHOLD = 1000.0
# Hidden marker that identifies the sticky PR comment.
MARKER = "<!-- sas-lexer-perf-report -->"
BYPASS_LABEL = "perf-regression-accepted"
# 2: adds the benchmark/config inventory (`benchmarks`, `configs`).
RESULTS_VERSION = 2

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


def results_doc(  # noqa: PLR0913 - keyword-only document builder
    *,
    verdict: str,
    threshold: float,
    meta: dict[str, str],
    rows: list[Row] | None = None,
    error: str | None = None,
    benchmarks: list[str] | None = None,
    configs: list[str] | None = None,
) -> dict:
    return {
        "version": RESULTS_VERSION,
        "verdict": verdict,
        "threshold": threshold,
        "gated": list(GATED),
        **meta,
        "error": error,
        # The inventory the rows must cover exactly (empty for INVALID results).
        "benchmarks": list(benchmarks or []),
        "configs": list(configs or []),
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
        "- Counts are deterministic for a given binary. Base and head binaries of identical source "
        "can still differ by up to ~0.3% (code layout, build paths), so smaller deltas are not "
        "meaningful on their own.",
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
        output.parent.mkdir(parents=True, exist_ok=True)
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
        expected = read_expected(args.expected)
        rows = collect(args.config, expected, args.expect_configs)
    except (InvalidResultsError, OSError, ValueError, KeyError, TypeError, AttributeError) as e:
        doc = results_doc(verdict=INVALID, threshold=args.threshold, meta=meta, error=str(e))
        code = EXIT_INVALID
    else:
        failed = failures(rows, args.threshold)
        doc = results_doc(
            verdict=REGRESSION if failed else PASS,
            threshold=args.threshold,
            meta=meta,
            rows=rows,
            benchmarks=expected,
            configs=args.expect_configs,
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
    try:
        doc = load_results(args.results)
        try:
            markdown = render(doc, args.bypassed_by)
        except (KeyError, TypeError, ValueError, IndexError, ZeroDivisionError) as e:
            msg = f"results {args.results} can't be rendered: {e!r}"
            raise InvalidResultsError(msg) from None
    except InvalidResultsError as e:
        # E.g. perf-check stopped before the comparison: still report it, as INVALID.
        doc = results_doc(verdict=INVALID, threshold=MAX_THRESHOLD, meta={}, error=str(e))
        markdown = render(doc)
    write_markdown(markdown, args.output, step_summary=args.step_summary)
    return EXIT_OK


def load_results(path: Path) -> dict:
    """A results JSON written by `check`; InvalidResultsError if it's missing or unusable."""
    if not path.is_file():
        msg = "no results: the perf check failed before the comparison (see the job log)"
        raise InvalidResultsError(msg)
    try:
        doc = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError) as e:
        msg = f"unreadable results {path}: {e}"
        raise InvalidResultsError(msg) from None
    validate_doc(doc)
    return doc


def _invalid(msg: str) -> InvalidResultsError:
    return InvalidResultsError(f"malformed results: {msg}")


def _is_finite(value: Any) -> bool:
    return is_number(value) and math.isfinite(value)


def _str_list(doc: dict, key: str) -> list[str]:
    value = doc.get(key)
    if not isinstance(value, list):
        raise _invalid(f"`{key}` must be a list")
    if not all(isinstance(v, str) and v for v in value):
        raise _invalid(f"`{key}` must be a list of non-empty strings")
    if len(set(value)) != len(value):
        raise _invalid(f"`{key}` has duplicates")
    return value


def validate_doc(doc: Any) -> None:  # noqa: PLR0912 - one flat list of schema rules
    """Check everything the gate and the renderer rely on; InvalidResultsError if not."""
    if not isinstance(doc, dict):
        raise _invalid("not a JSON object")
    if doc.get("version") != RESULTS_VERSION:
        raise _invalid(f"unsupported results version {doc.get('version')!r}")
    if doc.get("verdict") not in {PASS, REGRESSION, INVALID}:
        raise _invalid(f"unknown verdict {doc.get('verdict')!r}")
    threshold = doc.get("threshold")
    if not _is_finite(threshold) or not 0 < threshold <= MAX_THRESHOLD:
        raise _invalid(f"threshold must be finite and in (0, {MAX_THRESHOLD:g}], got {threshold!r}")
    if doc.get("gated") != list(GATED):
        raise _invalid(f"gated metrics {doc.get('gated')!r} != {list(GATED)}")
    for key in ("base_sha", "head", "tools"):
        if not isinstance(doc.get(key, ""), str):
            raise _invalid(f"`{key}` must be a string")
    if not isinstance(doc.get("rows"), list):
        raise _invalid("`rows` must be a list")
    benchmarks, configs = _str_list(doc, "benchmarks"), _str_list(doc, "configs")
    if doc["verdict"] == INVALID:
        return  # no measurements to validate; the renderer only shows the error.
    if not benchmarks or not configs:
        raise _invalid("a pass/regression result needs a non-empty benchmark and config inventory")
    seen: set[tuple[str, str]] = set()
    for row in doc["rows"]:
        if not isinstance(row, dict) or not isinstance(row.get("metrics"), dict):
            raise _invalid(f"row {row!r} must be an object with `metrics`")
        key = (row.get("config"), row.get("name"))
        if not all(isinstance(k, str) for k in key):
            raise _invalid(f"row {key!r}: `config` and `name` must be strings")
        if key in seen:
            raise _invalid(f"duplicate row {key}")
        seen.add(key)
        for metric, values in row["metrics"].items():
            if (
                not isinstance(values, list)
                or len(values) != 2  # noqa: PLR2004 - (base, head)
                or not all(_is_finite(v) for v in values)
            ):
                raise _invalid(
                    f"{key} {metric}: expected [base, head] finite numbers, got {values!r}"
                )
        for metric in GATED:
            values = row["metrics"].get(metric)
            if values is None or not all(
                isinstance(v, int) and not isinstance(v, bool) and v > 0 for v in values
            ):
                raise _invalid(
                    f"{key} {metric}: gated counts must be positive integers, got {values!r}"
                )
    expected = {(c, b) for c in configs for b in benchmarks}
    if seen != expected:
        missing = sorted(expected - seen)
        extra = sorted(seen - expected, key=str)
        raise _invalid(f"rows don't match the inventory (missing: {missing}, unexpected: {extra})")


def classify(doc: dict, rc: str, base_sha: str) -> tuple[str, str]:  # noqa: PLR0911
    """(status, reason): `pass` or `regression` only when every signal agrees, else `invalid`."""
    if doc.get("base_sha") != base_sha:
        return INVALID, f"results are for base {doc.get('base_sha')!r}, not {base_sha!r}"
    verdict = doc.get("verdict")
    if verdict == INVALID:
        return INVALID, f"the comparison is INVALID: {doc.get('error')}"
    expected_rc = {PASS: str(EXIT_OK), REGRESSION: str(EXIT_REGRESSION)}.get(verdict)
    if expected_rc is None:
        return INVALID, f"unknown verdict {verdict!r}"
    if rc != expected_rc:
        return INVALID, f"exit code {rc!r} doesn't match the {verdict} verdict"
    rows = rows_from_doc(doc)
    if not rows:
        return INVALID, "results have no benchmark rows"
    try:
        failed = failures(rows, float(doc["threshold"]))
    except (KeyError, TypeError, ValueError, ZeroDivisionError) as e:
        return INVALID, f"results rows are unusable: {e}"
    if bool(failed) != (verdict == REGRESSION):
        return INVALID, f"the rows don't support the {verdict} verdict"
    return str(verdict), str(verdict)


def gate_main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(prog="report.py gate", description="The CI gate decision.")
    parser.add_argument("results", type=Path)
    parser.add_argument("--rc", required=True, help="exit code of `report.py check` / perf-check")
    parser.add_argument("--base-sha", required=True, help="the base commit this run compared")
    parser.add_argument(
        "--expected", type=Path, required=True, help="file listing the gated `function/id` names"
    )
    parser.add_argument(
        "--expect-configs", nargs="+", required=True, help="feature configs that must be present"
    )
    parser.add_argument("--bypassed-by", help="admin who accepted a regression (verified by CI)")
    parser.add_argument("--status", action="store_true", help="only print the status")
    args = parser.parse_args(argv)
    try:
        doc = load_results(args.results)
        status, reason = classify(doc, args.rc, args.base_sha)
        if status != INVALID:
            inventory = (sorted(doc["benchmarks"]), sorted(doc["configs"]))
            wanted = (sorted(read_expected(args.expected)), sorted(args.expect_configs))
            if inventory != wanted:
                status, reason = INVALID, f"results inventory {inventory} != expected {wanted}"
    except (InvalidResultsError, OSError) as e:
        status, reason = INVALID, str(e)
    if args.status:
        print(status)
        return EXIT_OK
    if status == PASS:
        print("No regressions.")
        return EXIT_OK
    if status == REGRESSION and args.bypassed_by:
        print(
            f"::warning::Performance regression accepted by admin @{args.bypassed_by} "
            f"({BYPASS_LABEL} label)."
        )
        return EXIT_OK
    if status == REGRESSION:
        print(
            "::error::Performance regression; see the job summary. A repo admin can accept it "
            f"with the {BYPASS_LABEL} label."
        )
    else:
        print(f"::error::The perf check is INVALID (no bypass possible): {reason}")
    return EXIT_REGRESSION


def main(argv: list[str] | None = None) -> int:
    argv = sys.argv[1:] if argv is None else argv
    commands = {"comment": comment_main, "gate": gate_main, "check": check_main}
    command = commands.get(argv[0], None) if argv else None
    if command is None:
        return check_main(argv)
    return command(argv[1:])


def run() -> int:
    """Entry point: an unexpected crash exits 2 (INVALID), never 1 (regression)."""
    try:
        return main()
    except Exception:  # deliberately broad: fail closed with a distinct code
        traceback.print_exc()
        return EXIT_INVALID


if __name__ == "__main__":
    # The markdown has emoji; don't crash on a console that can't encode them (e.g. cp1252).
    if isinstance(sys.stdout, io.TextIOWrapper):
        sys.stdout.reconfigure(errors="replace")
    sys.exit(run())
