"""Happy-path tests for report.py: `uv run --no-project python -m unittest` in this directory."""

import contextlib
import io
import json
import os
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import report

CONFIGS = ["default", "macro_sep"]
BENCHES = ["lex/real", "lex/macros"]
BASE_SHA = "0" * 40


def run(argv: list[str]) -> tuple[int, str]:
    out = io.StringIO()
    with mock.patch.dict(os.environ, {"GITHUB_STEP_SUMMARY": ""}), contextlib.redirect_stdout(out):
        code = report.main(argv)
    return code, out.getvalue()


def results(ir_head: int) -> dict:
    """A results JSON in which `lex/real` goes from 1000 to `ir_head` instructions."""
    rows = [
        report.Row(
            c,
            n,
            {"Ir": (1000, ir_head if n == "lex/real" else 1000), "EstimatedCycles": (2000, 2000)},
        )
        for c in CONFIGS
        for n in BENCHES
    ]
    failed = report.failures(rows, 2.0)
    return report.results_doc(
        verdict=report.REGRESSION if failed else report.PASS,
        threshold=2.0,
        meta={"base_sha": BASE_SHA, "head": "fedcba9", "tools": "valgrind 3.26.0"},
        rows=rows,
        benchmarks=BENCHES,
        configs=CONFIGS,
    )


class ReportTest(unittest.TestCase):
    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = Path(self.tmp.name)

    def tearDown(self) -> None:
        self.tmp.cleanup()

    def test_check(self) -> None:
        def line(name: str, ir: dict) -> str:
            function_name, bench_id = name.split("/")
            metrics = {
                "Ir": {"values": ir},
                "EstimatedCycles": {"values": {"new": 2000, "old": 2000}},
            }
            profile = {"tool": "Callgrind", "data": {"total": {"metrics": metrics}}}
            return json.dumps(
                {"function_name": function_name, "id": bench_id, "profiles": [profile]}
            )

        expected = self.dir / "benchmarks.txt"
        expected.write_text("\n".join(BENCHES), encoding="utf-8")
        for ir_head, code in [(1000, report.EXIT_OK), (1100, report.EXIT_REGRESSION)]:
            argv = ["check", "--threshold", "2", "--expected", str(expected)]
            argv += ["--expect-configs", *CONFIGS, "--results", str(self.dir / "results.json")]
            for c in CONFIGS:
                base, head = self.dir / f"{c}.base.jsonl", self.dir / f"{c}.head.jsonl"
                base.write_text(
                    "\n".join(line(n, {"new": 1000}) for n in BENCHES), encoding="utf-8"
                )
                head_ir = {
                    n: {"new": ir_head if n == "lex/real" else 1000, "old": 1000} for n in BENCHES
                }
                head.write_text("\n".join(line(n, head_ir[n]) for n in BENCHES), encoding="utf-8")
                argv += ["--config", c, str(base), str(head)]
            self.assertEqual(run(argv)[0], code)
            doc = json.loads((self.dir / "results.json").read_text(encoding="utf-8"))
            report.validate_doc(doc)

    def test_render(self) -> None:
        passed = report.render(results(1000))
        self.assertTrue(passed.startswith(report.MARKER + "\n"))
        self.assertIn("### ✅ Perf: no regressions over 2%", passed)
        self.assertIn("<details>\n<summary>Full Callgrind results</summary>", passed)
        regressed = report.render(results(1050))
        self.assertIn("### ❌ Perf: 2 regressions over 2%", regressed)
        self.assertIn("| 🔴 | `lex/real` | default | **+5.00%** |", regressed)
        accepted = report.render(results(1050), bypassed_by="mishamsk")
        self.assertIn("### ⚠️ Perf: 2 regressions over 2%, accepted by @mishamsk (admin)", accepted)

    def test_gate(self) -> None:
        path, expected = self.dir / "results.json", self.dir / "benchmarks.txt"
        expected.write_text("\n".join(BENCHES), encoding="utf-8")

        def gate(ir_head: int, rc: str, *extra: str) -> int:
            path.write_text(json.dumps(results(ir_head)), encoding="utf-8")
            argv = ["gate", str(path), "--rc", rc, "--base-sha", BASE_SHA, *extra]
            return run([*argv, "--expected", str(expected), "--expect-configs", *CONFIGS])[0]

        self.assertEqual(gate(1000, "0"), 0)
        self.assertEqual(gate(1050, "1"), 1)
        self.assertEqual(gate(1050, "1", "--bypassed-by", "mishamsk"), 0)
        self.assertEqual(gate(1050, "0", "--bypassed-by", "mishamsk"), 1)  # rc/verdict mismatch


if __name__ == "__main__":
    unittest.main()
