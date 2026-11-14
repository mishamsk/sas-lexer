"""Tests for report.py (stdlib only): `uv run --no-project python -m unittest` in this directory."""

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


def bench(name: str, ir: dict, cycles: dict) -> dict:
    function_name, bench_id = name.split("/")
    return {
        "function_name": function_name,
        "id": bench_id,
        "profiles": [
            {
                "tool": "Callgrind",
                "data": {
                    "total": {
                        "metrics": {"Ir": {"values": ir}, "EstimatedCycles": {"values": cycles}}
                    }
                },
            }
        ],
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
        argv = ["--threshold", threshold, "--expected", str(self.dir / "expected.txt")]
        argv += ["--expect-configs", *CONFIGS]
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

    def assert_invalid(self, configs: list[str] | None = None) -> str:
        code, out = self.run_report(configs=configs)
        self.assertEqual(code, report.EXIT_INVALID, out)
        self.assertIn("INVALID", out)
        self.assertNotIn("PASSED", out)
        return out

    # Verdicts.
    def test_identical_passes(self) -> None:
        code, out = self.run_report()
        self.assertEqual(code, report.EXIT_OK, out)
        self.assertIn("PASSED", out)

    def test_just_below_threshold_passes(self) -> None:
        self.head["default"][0] = head_bench("lex/real", ir=1099)
        self.assertEqual(self.run_report()[0], report.EXIT_OK)

    def test_exactly_threshold_fails(self) -> None:
        self.head["default"][0] = head_bench("lex/real", ir=1100)
        code, out = self.run_report()
        self.assertEqual(code, report.EXIT_REGRESSION, out)
        self.assertIn("lex/real [default] Ir +10.00%", out)

    def test_cycles_alone_fail(self) -> None:
        self.head["macro_sep"][1] = head_bench("lex/macros", cycles=2400)
        code, out = self.run_report()
        self.assertEqual(code, report.EXIT_REGRESSION, out)
        self.assertIn("EstimatedCycles +20.00%", out)

    def test_improvement_passes(self) -> None:
        self.head["default"][0] = head_bench("lex/real", ir=500, cycles=1000)
        code, out = self.run_report()
        self.assertEqual(code, report.EXIT_OK, out)
        self.assertIn("faster", out)

    def test_regression_and_improvement_mixed_fails(self) -> None:
        self.head["default"][0] = head_bench("lex/real", ir=500, cycles=2500)
        self.assertEqual(self.run_report()[0], report.EXIT_REGRESSION)

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


if __name__ == "__main__":
    unittest.main()
