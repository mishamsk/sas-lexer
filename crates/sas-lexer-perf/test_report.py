"""Tests for report.py (stdlib only): `uv run --no-project python -m unittest` in this directory."""

import contextlib
import io
import json
import os
import tempfile
import unittest
from collections.abc import Callable
from pathlib import Path
from unittest import mock

import report

CONFIGS = ["default", "macro_sep"]
BENCHES = ["lex/real", "lex/macros"]
BASE_SHA = "0" * 40


def run(argv: list[str]) -> tuple[int, str]:
    out = io.StringIO()
    with (
        mock.patch.dict(os.environ, {"GITHUB_STEP_SUMMARY": ""}),
        contextlib.redirect_stdout(out),
        contextlib.redirect_stderr(io.StringIO()),
    ):
        code = report.main(argv)
    return code, out.getvalue()


def bench(name: str, ir: dict, cycles: dict, **extra: dict) -> dict:
    function_name, bench_id = name.split("/")
    metrics = {"Ir": {"values": ir}, "EstimatedCycles": {"values": cycles}}
    metrics.update({k: {"values": v} for k, v in extra.items()})
    return {
        "function_name": function_name,
        "id": bench_id,
        "profiles": [{"tool": "Callgrind", "data": {"total": {"metrics": metrics}}}],
    }


def head(name: str, ir: int = 1000, cycles: int = 2000) -> dict:
    return bench(name, {"new": ir, "old": 1000}, {"new": cycles, "old": 2000})


class CheckTest(unittest.TestCase):
    """`report.py check` on gungraun JSON lines: verdicts, and fail closed on bad input."""

    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = Path(self.tmp.name)
        self.reset()

    def reset(self) -> None:
        self.base = {c: [bench(n, {"new": 1000}, {"new": 2000}) for n in BENCHES] for c in CONFIGS}
        self.head = {c: [head(n) for n in BENCHES] for c in CONFIGS}

    def tearDown(self) -> None:
        self.tmp.cleanup()

    def check(self, threshold: str = "10", configs: list[str] = CONFIGS) -> tuple[int, str, dict]:
        expected = self.dir / "expected.txt"
        expected.write_text("# comment\n" + "\n".join(BENCHES) + "\n", encoding="utf-8")
        argv = ["check", "--threshold", threshold, "--expected", str(expected)]
        argv += ["--expect-configs", *CONFIGS, "--results", str(self.dir / "results.json")]
        argv += ["--base-sha", BASE_SHA]
        for c in configs:
            for side, runs in (("base", self.base), ("head", self.head)):
                path = self.dir / f"{c}.{side}.jsonl"
                path.write_text("".join(json.dumps(r) + "\n" for r in runs[c]), encoding="utf-8")
            argv += [
                "--config",
                c,
                str(self.dir / f"{c}.base.jsonl"),
                str(self.dir / f"{c}.head.jsonl"),
            ]
        code, out = run(argv)
        doc = json.loads((self.dir / "results.json").read_text(encoding="utf-8"))
        report.validate_doc(doc)  # what check writes always satisfies the loader
        return code, out, doc

    def test_verdicts(self) -> None:
        cases = [
            # (config, bench index, head kwargs, threshold, expected exit, text in the report)
            ("default", 0, {}, "10", report.EXIT_OK, "✅ Perf: no regressions over 10%"),
            ("default", 0, {"ir": 1099}, "10", report.EXIT_OK, "✅"),
            ("default", 0, {"ir": 1100}, "10", report.EXIT_REGRESSION, "Instructions +10.00%"),
            (
                "default",
                0,
                {"ir": 1020},
                "2",
                report.EXIT_REGRESSION,
                "❌ Perf: 1 regression over 2%",
            ),
            ("default", 0, {"ir": 1019}, "2", report.EXIT_OK, "✅"),
            ("macro_sep", 1, {"cycles": 2400}, "10", report.EXIT_REGRESSION, "Est. cycles +20.00%"),
            (
                "default",
                0,
                {"ir": 500, "cycles": 1000},
                "10",
                report.EXIT_OK,
                "| 🟢 | `lex/real` |",
            ),
            (
                "default",
                0,
                {"ir": 500, "cycles": 2500},
                "10",
                report.EXIT_REGRESSION,
                "| 🔴 | `lex/real` |",
            ),
        ]
        for config, index, kwargs, threshold, code, text in cases:
            with self.subTest(config=config, kwargs=kwargs, threshold=threshold):
                self.reset()
                self.head[config][index] = head(BENCHES[index], **kwargs)
                got, out, doc = self.check(threshold)
                self.assertEqual(got, code, out)
                self.assertIn(text, out)
                self.assertEqual(doc["verdict"], report.REGRESSION if code else report.PASS)

    def test_other_metrics_are_reported_not_gated(self) -> None:
        self.base["default"][0] = bench(
            "lex/real", {"new": 1000}, {"new": 2000}, Dr={"new": 100}, L1HitRate={"new": 99.5}
        )
        self.head["default"][0] = bench(
            "lex/real",
            {"new": 1000, "old": 1000},
            {"new": 2000, "old": 2000},
            Dr={"new": 900},
            L1HitRate={"new": 50.0},
        )
        code, out, _ = self.check()
        self.assertEqual(code, report.EXIT_OK, out)
        self.assertIn("+800.00%", out)  # Dr, in the details table
        self.assertNotIn("L1HitRate", out)  # float rates are not shown

    def test_incomplete_or_inconsistent_input_is_invalid(self) -> None:
        def first(runs: dict, row: dict) -> None:
            runs["default"][0] = row

        def drop_cycles(runs: dict) -> None:
            del runs["default"][0]["profiles"][0]["data"]["total"]["metrics"]["EstimatedCycles"]

        # Each case mutates (base runs, head runs).
        cases: dict[str, Callable[[dict, dict], object]] = {
            "missing base count": lambda b, _: first(b, bench("lex/real", {}, {"new": 2000})),
            "missing head baseline": lambda _, h: first(
                h, bench("lex/real", {"new": 1000}, {"new": 2000})
            ),
            "baseline mismatch": lambda b, _: first(
                b, bench("lex/real", {"new": 999}, {"new": 2000})
            ),
            "zero base": lambda b, _: first(b, bench("lex/real", {"new": 0}, {"new": 2000})),
            "missing metric": lambda _, h: drop_cycles(h),
            "missing benchmark": lambda b, h: (b["default"].pop(), h["default"].pop()),
            "unexpected benchmark": lambda b, h: (
                b["default"].append(bench("lex/new", {"new": 1}, {"new": 1})),
                h["default"].append(head("lex/new")),
            ),
            "duplicate benchmark": lambda _, h: h["default"].append(head("lex/real")),
            "empty run": lambda b, _: b["default"].clear(),
        }
        for case, mutate in cases.items():
            with self.subTest(case=case):
                self.reset()
                mutate(self.base, self.head)
                code, out, doc = self.check()
                self.assertEqual(code, report.EXIT_INVALID, out)
                self.assertIn("INVALID", out)
                self.assertEqual(doc["verdict"], report.INVALID)
        with self.subTest(case="missing config"):
            self.reset()
            self.assertEqual(self.check(configs=["default"])[0], report.EXIT_INVALID)

    def test_operational_error_exits_invalid(self) -> None:
        # E.g. a missing benchmarks.txt: exit 2 and an INVALID results JSON, never exit 1.
        argv = ["check", "--threshold", "2", "--expected", str(self.dir / "nope.txt")]
        argv += ["--expect-configs", "default", "--config", "default", "a.jsonl", "b.jsonl"]
        argv += ["--results", str(self.dir / "results.json")]
        self.assertEqual(run(argv)[0], report.EXIT_INVALID)
        verdict = json.loads((self.dir / "results.json").read_text(encoding="utf-8"))["verdict"]
        self.assertEqual(verdict, report.INVALID)

    def test_bad_thresholds_are_rejected(self) -> None:
        for bad in ["nan", "inf", "-5", "0", "abc", "2000"]:
            with self.subTest(threshold=bad), self.assertRaises(SystemExit):
                self.check(threshold=bad)

    def test_unexpected_crash_exits_invalid(self) -> None:
        with (
            mock.patch.object(report, "main", side_effect=RuntimeError("boom")),
            contextlib.redirect_stderr(io.StringIO()),
        ):
            self.assertEqual(report.run(), report.EXIT_INVALID)


def results(verdict: str = report.REGRESSION) -> dict:
    """A results JSON: lex/real regresses (Ir +5%) unless the verdict is a pass."""
    ir_head = 1050 if verdict == report.REGRESSION else 1000
    rows = [
        report.Row(
            c,
            n,
            {
                "Ir": (1000, ir_head if n == "lex/real" else 1000),
                "EstimatedCycles": (2000, 2000),
                "Dr": (10, 11),
            },
        )
        for c in CONFIGS
        for n in BENCHES
    ]
    meta = {"base_sha": BASE_SHA, "head": "fedcba9", "tools": "valgrind 3.26.0"}
    doc = report.results_doc(
        verdict=verdict, threshold=2.0, meta=meta, rows=rows, benchmarks=BENCHES, configs=CONFIGS
    )
    return json.loads(json.dumps(doc))  # as read back from disk


class RenderTest(unittest.TestCase):
    def test_pass(self) -> None:
        markdown = report.render(results(report.PASS))
        self.assertTrue(markdown.startswith(report.MARKER + "\n"))
        self.assertIn("### ✅ Perf: no regressions over 2%", markdown)
        self.assertIn("| ⚪ | `lex/real` | default |", markdown)
        self.assertIn("<details>\n<summary>Full Callgrind results</summary>", markdown)
        self.assertTrue(markdown.rstrip().endswith("</details>"))
        self.assertEqual(report.render(results(report.PASS), bypassed_by="mishamsk"), markdown)

    def test_regression_and_bypass(self) -> None:
        markdown = report.render(results())
        self.assertIn("### ❌ Perf: 2 regressions over 2%", markdown)
        self.assertIn("| 🔴 | `lex/real` | default | **+5.00%** |", markdown)
        self.assertIn("perf-regression-accepted", markdown)
        bypassed = report.render(results(), bypassed_by="mishamsk")
        self.assertIn("### ⚠️ Perf: 2 regressions over 2%, accepted by @mishamsk (admin)", bypassed)

    def test_invalid_ignores_the_bypass(self) -> None:
        doc = report.results_doc(verdict=report.INVALID, threshold=2.0, meta={}, error="boom")
        markdown = report.render(doc, bypassed_by="mishamsk")
        self.assertIn("### ❌ Perf: INVALID", markdown)
        self.assertIn("- boom", markdown)
        self.assertNotIn("accepted", markdown)

    def test_comment_command(self) -> None:
        def broken_metrics(path: Path) -> None:
            doc = results()
            del doc["rows"][0]["metrics"]["EstimatedCycles"]
            path.write_text(json.dumps(doc), encoding="utf-8")

        cases: dict[str, tuple[Callable[[Path], object] | None, str]] = {
            "valid": (lambda p: p.write_text(json.dumps(results()), encoding="utf-8"), "⚠️"),
            "missing": (None, "failed before the comparison"),
            "not json": (lambda p: p.write_text("{truncated", encoding="utf-8"), "INVALID"),
            "malformed metrics": (broken_metrics, "EstimatedCycles"),
        }
        for case, (write, text) in cases.items():
            with self.subTest(case=case), tempfile.TemporaryDirectory() as tmp:
                path = Path(tmp) / "results.json"
                if write:
                    write(path)
                output = Path(tmp) / "target" / "perf-check" / "comment.md"  # parent is created
                argv = ["comment", str(path), "--bypassed-by", "mishamsk", "--output", str(output)]
                self.assertEqual(run(argv)[0], report.EXIT_OK)
                markdown = output.read_text(encoding="utf-8")
                self.assertIn(text, markdown)
                self.assertEqual("accepted" in markdown, case == "valid")


class GateTest(unittest.TestCase):
    """The CI decision: only a validated regression for this base and inventory is bypassable."""

    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        self.path = Path(self.tmp.name) / "results.json"
        self.expected = Path(self.tmp.name) / "benchmarks.txt"
        self.expected.write_text("\n".join(BENCHES) + "\n", encoding="utf-8")

    def tearDown(self) -> None:
        self.tmp.cleanup()

    def gate(self, doc: dict | None, rc: str, *extra: str, base: str = BASE_SHA) -> tuple[int, str]:
        if doc is not None:
            self.path.write_text(json.dumps(doc), encoding="utf-8")
        argv = ["gate", str(self.path), "--rc", rc, "--base-sha", base, *extra]
        return run([*argv, "--expected", str(self.expected), "--expect-configs", *CONFIGS])

    def test_valid_results(self) -> None:
        self.assertEqual(self.gate(results(report.PASS), "0", "--status")[1].strip(), report.PASS)
        self.assertEqual(self.gate(results(report.PASS), "0")[0], report.EXIT_OK)
        self.assertEqual(self.gate(results(), "1", "--status")[1].strip(), report.REGRESSION)
        code, out = self.gate(results(), "1")
        self.assertEqual(code, 1)
        self.assertIn("::error::Performance regression", out)
        code, out = self.gate(results(), "1", "--bypassed-by", "mishamsk")
        self.assertEqual(code, report.EXIT_OK)
        self.assertIn("::warning::Performance regression accepted by admin @mishamsk", out)

    def test_anything_else_is_invalid_even_with_a_bypass(self) -> None:
        def changed(start: str = report.REGRESSION, **fields: object) -> dict:
            doc = results(start)
            doc.update(fields)
            return doc

        def edit_row(edit: Callable[[dict], object], verdict: str = report.REGRESSION) -> dict:
            doc = results(verdict)
            edit(doc["rows"][0])
            return doc

        regression = results()
        cases: dict[str, tuple[dict | None, str, str]] = {
            # name: (results JSON or None for missing, rc, base)
            "no results, exit 1": (None, "1", BASE_SHA),
            "INVALID verdict, exit 1": (changed(verdict=report.INVALID), "1", BASE_SHA),
            "INVALID verdict, exit 2": (changed(verdict=report.INVALID), "2", BASE_SHA),
            "another base": (regression, "1", "f" * 40),
            "exit 0 vs regression": (regression, "0", BASE_SHA),
            "exit 137": (regression, "137", BASE_SHA),
            "exit 1 vs pass": (results(report.PASS), "1", BASE_SHA),
            "rows don't regress": (changed(report.PASS, verdict=report.REGRESSION), "1", BASE_SHA),
            "no rows": (changed(rows=[]), "1", BASE_SHA),
            "partial inventory": (changed(rows=regression["rows"][:1]), "1", BASE_SHA),
            "inventory vs benchmarks.txt": (
                changed(
                    benchmarks=BENCHES[:1],
                    rows=[r for r in regression["rows"] if r["name"] == BENCHES[0]],
                ),
                "1",
                BASE_SHA,
            ),
            "NaN threshold": (changed(threshold=float("nan")), "1", BASE_SHA),
            "wrong gated list": (changed(gated=["Ir"]), "1", BASE_SHA),
            "negative counts": (
                edit_row(lambda r: r["metrics"].update(Ir=[-1000, -1050])),
                "1",
                BASE_SHA,
            ),
            "float counts": (
                edit_row(lambda r: r["metrics"].update(Ir=[1000.0, 1050.0])),
                "1",
                BASE_SHA,
            ),
            "missing gated metric": (
                edit_row(lambda r: r["metrics"].pop("EstimatedCycles")),
                "1",
                BASE_SHA,
            ),
            "NaN other metric": (
                edit_row(lambda r: r["metrics"].update(Dr=[float("nan"), 1])),
                "1",
                BASE_SHA,
            ),
            "non-string config": (edit_row(lambda r: r.update(config=["default"])), "1", BASE_SHA),
            "duplicate row": (
                changed(rows=[*regression["rows"], regression["rows"][0]]),
                "1",
                BASE_SHA,
            ),
        }
        for case, (doc, rc, base) in cases.items():
            with self.subTest(case=case):
                self.path.unlink(missing_ok=True)
                self.assertEqual(
                    self.gate(doc, rc, "--status", base=base)[1].strip(), report.INVALID
                )
                code, out = self.gate(doc, rc, "--bypassed-by", "mishamsk", base=base)
                self.assertEqual(code, 1, out)
                self.assertNotIn("accepted", out)


if __name__ == "__main__":
    unittest.main()
