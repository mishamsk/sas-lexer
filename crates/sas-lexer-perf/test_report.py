"""Tests for report.py (stdlib only): `uv run --no-project python -m unittest` in this directory.

The comment renderer is snapshot-tested against testdata/*.md. After an intended change to the
markdown, regenerate them with `UPDATE_SNAPSHOTS=1` and review the diff.
"""

from __future__ import annotations

import contextlib
import io
import json
import os
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import report

EXPECTED = ["lex/real", "lex/macros"]
CONFIGS = ["default", "macro_sep"]
BASE_IR = 1000
BASE_CYCLES = 2000
TESTDATA = Path(__file__).parent / "testdata"


def bench(name: str, ir: dict, cycles: dict, extra: dict | None = None) -> dict:
    function_name, bench_id = name.split("/")
    metrics = {"Ir": {"values": ir}, "EstimatedCycles": {"values": cycles}}
    metrics.update(extra or {})
    return {
        "function_name": function_name,
        "id": bench_id,
        "profiles": [{"tool": "Callgrind", "data": {"total": {"metrics": metrics}}}],
    }


def base_bench(name: str, ir: int = BASE_IR, cycles: int = BASE_CYCLES) -> dict:
    return bench(name, {"new": ir}, {"new": cycles})


def head_bench(name: str, ir: int = BASE_IR, cycles: int = BASE_CYCLES) -> dict:
    return bench(name, {"new": ir, "old": BASE_IR}, {"new": cycles, "old": BASE_CYCLES})


class ReportTest(unittest.TestCase):
    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = Path(self.tmp.name)
        (self.dir / "expected.txt").write_text("# comment\n" + "\n".join(EXPECTED) + "\n")
        self.base = {c: [base_bench(n) for n in EXPECTED] for c in CONFIGS}
        self.head = {c: [head_bench(n) for n in EXPECTED] for c in CONFIGS}

    def tearDown(self) -> None:
        self.tmp.cleanup()

    def write(self, name: str, rows: list[dict]) -> Path:
        path = self.dir / name
        path.write_text("".join(json.dumps(r) + "\n" for r in rows))
        return path

    def run_report(
        self, threshold: str = "10", configs: list[str] | None = None
    ) -> tuple[int, str]:
        argv = ["check", "--threshold", threshold, "--expected", str(self.dir / "expected.txt")]
        argv += ["--expect-configs", *CONFIGS, "--results", str(self.dir / "results.json")]
        argv += ["--base-sha", "b" * 40, "--head", "h" * 7, "--tools", "valgrind 3.26.0"]
        for c in configs if configs is not None else CONFIGS:
            base = self.write(f"{c}.base.jsonl", self.base.get(c, []))
            head = self.write(f"{c}.head.jsonl", self.head.get(c, []))
            argv += ["--config", c, str(base), str(head)]
        out = io.StringIO()
        with (
            mock.patch.dict(os.environ, {"GITHUB_STEP_SUMMARY": ""}),
            contextlib.redirect_stdout(out),
        ):
            code = report.main(argv)
        return code, out.getvalue()

    def results(self) -> dict:
        return json.loads((self.dir / "results.json").read_text())

    def assert_invalid(self, configs: list[str] | None = None) -> str:
        code, out = self.run_report(configs=configs)
        self.assertEqual(code, report.EXIT_INVALID, out)
        self.assertIn("INVALID", out)
        self.assertNotIn("✅", out)
        self.assertEqual(self.results()["verdict"], report.INVALID)
        return out

    # Verdicts.
    def test_identical_passes(self) -> None:
        code, out = self.run_report()
        self.assertEqual(code, report.EXIT_OK, out)
        self.assertIn("✅ Perf: no regressions over 10%", out)
        self.assertEqual(self.results()["verdict"], report.PASS)

    def test_just_below_threshold_passes(self) -> None:
        self.head["default"][0] = head_bench("lex/real", ir=1099)
        self.assertEqual(self.run_report()[0], report.EXIT_OK)

    def test_exactly_threshold_fails(self) -> None:
        self.head["default"][0] = head_bench("lex/real", ir=1100)
        code, out = self.run_report()
        self.assertEqual(code, report.EXIT_REGRESSION, out)
        self.assertIn("`lex/real` [default] Instructions +10.00%", out)
        self.assertIn("❌ Perf: 1 regression over 10%", out)
        self.assertEqual(self.results()["verdict"], report.REGRESSION)

    def test_tight_threshold(self) -> None:
        self.head["default"][0] = head_bench("lex/real", ir=1020)
        self.assertEqual(self.run_report(threshold="2")[0], report.EXIT_REGRESSION)
        self.head["default"][0] = head_bench("lex/real", ir=1019)
        self.assertEqual(self.run_report(threshold="2")[0], report.EXIT_OK)

    def test_cycles_alone_fail(self) -> None:
        self.head["macro_sep"][1] = head_bench("lex/macros", cycles=2400)
        code, out = self.run_report()
        self.assertEqual(code, report.EXIT_REGRESSION, out)
        self.assertIn("Est. cycles +20.00%", out)

    def test_improvement_passes(self) -> None:
        self.head["default"][0] = head_bench("lex/real", ir=500, cycles=1000)
        code, out = self.run_report()
        self.assertEqual(code, report.EXIT_OK, out)
        self.assertIn("| 🟢 | `lex/real` |", out)

    def test_regression_and_improvement_mixed_fails(self) -> None:
        self.head["default"][0] = head_bench("lex/real", ir=500, cycles=2500)
        code, out = self.run_report()
        self.assertEqual(code, report.EXIT_REGRESSION)
        self.assertIn("| 🔴 | `lex/real` |", out)

    def test_other_metrics_are_reported_not_gated(self) -> None:
        extra_base = {"Dr": {"values": {"new": 100}}, "L1HitRate": {"values": {"new": 99.5}}}
        extra_head = {"Dr": {"values": {"new": 900}}, "L1HitRate": {"values": {"new": 50.0}}}
        self.base["default"][0] = bench(
            "lex/real", {"new": BASE_IR}, {"new": BASE_CYCLES}, extra_base
        )
        self.head["default"][0] = bench(
            "lex/real",
            {"new": BASE_IR, "old": BASE_IR},
            {"new": BASE_CYCLES, "old": BASE_CYCLES},
            extra_head,
        )
        code, out = self.run_report()
        self.assertEqual(code, report.EXIT_OK, out)
        self.assertIn("+800.00%", out)  # Dr, in the details table
        self.assertNotIn("L1HitRate", out)  # float rates are not shown

    # Fail closed on incomplete or inconsistent measurements.
    def test_missing_base_value_is_invalid(self) -> None:
        self.base["default"][0] = bench("lex/real", {}, {"new": BASE_CYCLES})
        self.assert_invalid()

    def test_missing_baseline_in_head_is_invalid(self) -> None:
        self.head["default"][0] = bench("lex/real", {"new": BASE_IR}, {"new": BASE_CYCLES})
        self.assert_invalid()

    def test_head_baseline_mismatch_is_invalid(self) -> None:
        self.base["default"][0] = base_bench("lex/real", ir=999)
        self.assert_invalid()

    def test_zero_base_is_invalid(self) -> None:
        self.base["default"][0] = base_bench("lex/real", ir=0)
        self.assert_invalid()

    def test_missing_head_value_is_invalid(self) -> None:
        self.head["default"][0] = bench(
            "lex/real", {"old": BASE_IR}, {"new": BASE_CYCLES, "old": BASE_CYCLES}
        )
        self.assert_invalid()

    def test_missing_metric_is_invalid(self) -> None:
        row = head_bench("lex/real")
        del row["profiles"][0]["data"]["total"]["metrics"]["EstimatedCycles"]
        self.head["default"][0] = row
        self.assert_invalid()

    def test_missing_benchmark_is_invalid(self) -> None:
        self.head["default"].pop()
        self.base["default"].pop()
        out = self.assert_invalid()
        self.assertIn("lex/macros", out)

    def test_unexpected_benchmark_is_invalid(self) -> None:
        self.base["default"].append(base_bench("lex/new"))
        self.head["default"].append(head_bench("lex/new"))
        self.assert_invalid()

    def test_duplicate_benchmark_is_invalid(self) -> None:
        self.head["default"].append(head_bench("lex/real"))
        self.assert_invalid()

    def test_missing_config_is_invalid(self) -> None:
        self.assert_invalid(configs=["default"])

    def test_empty_results_are_invalid(self) -> None:
        self.base["default"] = []
        self.assert_invalid()

    def test_bad_thresholds_are_rejected(self) -> None:
        for bad in ["nan", "inf", "-5", "0", "abc", "2000"]:
            with (
                self.subTest(threshold=bad),
                self.assertRaises(SystemExit),
                contextlib.redirect_stderr(io.StringIO()),
            ):
                self.run_report(threshold=bad)


# name -> ((base Ir, head Ir), (base cycles, head cycles)) for the regression fixture.
FIXTURE = {
    "lex/real": ((50_000_000, 51_234_567), (70_000_000, 70_000_000)),  # Ir +2.47%
    "lex/macros": ((20_000_000, 20_000_000), (30_000_000, 31_000_000)),  # cycles +3.33%
    "lex/strings": ((10_000_000, 9_000_000), (12_500_000, 12_000_000)),  # faster
    "lex/comments": ((8_000_000, 8_080_000), (9_000_000, 9_000_000)),  # +1%, within 2%
}


def fixture_doc(verdict: str) -> dict:
    """A results JSON shaped like a real run (two configs, extra Callgrind metrics)."""
    benches = dict(FIXTURE)
    if verdict == report.PASS:
        del benches["lex/real"], benches["lex/macros"]
    rows = [
        report.Row(
            config,
            name,
            {
                "Ir": ir,
                "EstimatedCycles": cycles,
                "Dr": (12_000_000, 12_000_000),
                "Dw": (6_000_000, 6_100_000),
                "I1mr": (1_200, 1_250),
                "DLmw": (0, 3),
                "L1HitRate": (99.1, 99.0),
            },
        )
        for config in CONFIGS
        for name, (ir, cycles) in benches.items()
    ]
    return report.results_doc(
        verdict=verdict,
        threshold=2.0,
        meta={
            "base_sha": "0123456789abcdef0123456789abcdef01234567",
            "head": "fedcba9",
            "tools": "valgrind 3.26.0, gungraun 0.20.0, base rustc 1.92.0, head rustc 1.92.0.",
        },
        rows=rows,
    )


class CommentSnapshotTest(unittest.TestCase):
    def assert_snapshot(self, name: str, markdown: str) -> None:
        path = TESTDATA / f"{name}.md"
        if os.environ.get("UPDATE_SNAPSHOTS") == "1":
            path.write_text(markdown, encoding="utf-8")
        self.assertEqual(markdown, path.read_text(encoding="utf-8"), f"snapshot {path} differs")
        self.assertTrue(markdown.startswith(report.MARKER + "\n"))

    def test_pass(self) -> None:
        self.assert_snapshot("comment-pass", report.render(fixture_doc(report.PASS)))

    def test_regression(self) -> None:
        self.assert_snapshot("comment-regression", report.render(fixture_doc(report.REGRESSION)))

    def test_bypassed(self) -> None:
        doc = fixture_doc(report.REGRESSION)
        self.assert_snapshot("comment-bypassed", report.render(doc, bypassed_by="mishamsk"))

    def test_bypass_does_not_change_a_pass(self) -> None:
        doc = fixture_doc(report.PASS)
        self.assertEqual(report.render(doc, bypassed_by="mishamsk"), report.render(doc))

    def test_invalid(self) -> None:
        doc = report.results_doc(
            verdict=report.INVALID,
            threshold=2.0,
            meta={"base_sha": "0123456789abcdef", "head": "fedcba9", "tools": "?"},
            error="[default] head: benchmark set differs from the expected list",
        )
        markdown = report.render(doc, bypassed_by="mishamsk")
        self.assert_snapshot("comment-invalid", markdown)
        self.assertNotIn("accepted", markdown)

    def test_comment_command_round_trips(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            results = Path(tmp) / "results.json"
            output = Path(tmp) / "comment.md"
            summary = Path(tmp) / "summary.md"
            doc = fixture_doc(report.REGRESSION)
            results.write_text(json.dumps(doc))
            with (
                mock.patch.dict(os.environ, {"GITHUB_STEP_SUMMARY": str(summary)}),
                contextlib.redirect_stdout(io.StringIO()),
            ):
                argv = ["comment", str(results), "--bypassed-by", "mishamsk"]
                code = report.main([*argv, "--output", str(output), "--step-summary"])
            self.assertEqual(code, report.EXIT_OK)
            expected = report.render(doc, bypassed_by="mishamsk")
            self.assertEqual(output.read_text(), expected)
            self.assertEqual(summary.read_text(), expected)

    def test_comment_without_results_reports_invalid(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            output = Path(tmp) / "comment.md"
            with contextlib.redirect_stdout(io.StringIO()):
                argv = ["comment", str(Path(tmp) / "missing.json"), "--bypassed-by", "mishamsk"]
                code = report.main([*argv, "--output", str(output)])
            self.assertEqual(code, report.EXIT_OK)
            markdown = output.read_text()
            self.assertIn("INVALID", markdown)
            self.assertIn("failed before the comparison", markdown)
            self.assertNotIn("accepted", markdown)

    def test_comment_command_rejects_bad_results(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            results = Path(tmp) / "results.json"
            results.write_text('{"version": 99}')
            with contextlib.redirect_stderr(io.StringIO()):
                self.assertEqual(report.main(["comment", str(results)]), report.EXIT_INVALID)


if __name__ == "__main__":
    unittest.main()
