"""Tests for sticky_comment.py with a fake GitHub API (stdlib only)."""

from __future__ import annotations

import contextlib
import io
import tempfile
import unittest
from pathlib import Path
from typing import Any

import sticky_comment
from report import MARKER

REPO = "o/r"
PR = 19
HEAD_A = "a" * 40
HEAD_B = "b" * 40
BODY = MARKER + "\n### ✅ Perf: no regressions over 2%\n"


class FakeApi:
    """Records calls; serves the PR head and paginated issue comments."""

    def __init__(self, head: str, pages: list[list[dict]]) -> None:
        self.head = head
        self.pages = pages
        self.calls: list[tuple[str, str, dict | None]] = []

    def __call__(self, method: str, path: str, body: dict | None) -> tuple[Any, str | None]:
        self.calls.append((method, path, body))
        if method == "GET" and path == f"repos/{REPO}/pulls/{PR}":
            return {"head": {"sha": self.head}}, None
        if method == "GET" and "/comments" in path:
            index = int(path.split("?page=")[1]) if path.startswith("https://api.example") else 0
            following = f"https://api.example/comments?page={index + 1}"
            return self.pages[index], following if index + 1 < len(self.pages) else None
        if method == "POST":
            return {"id": 999}, None
        return {}, None

    def writes(self) -> list[tuple[str, str, dict | None]]:
        return [c for c in self.calls if c[0] in {"POST", "PATCH"}]


def bot(comment_id: int, body: str) -> dict:
    return {"id": comment_id, "user": {"login": sticky_comment.BOT_LOGIN}, "body": body}


class UpsertTest(unittest.TestCase):
    def test_creates_when_missing(self) -> None:
        api = FakeApi(HEAD_A, [[bot(1, "unrelated")]])
        self.assertEqual(sticky_comment.upsert(api, REPO, PR, HEAD_A, BODY), "created comment 999")
        self.assertEqual(
            api.writes(), [("POST", f"repos/{REPO}/issues/{PR}/comments", {"body": BODY})]
        )

    def test_updates_the_bot_comment_on_a_later_page(self) -> None:
        user_copy = {"id": 5, "user": {"login": "someone"}, "body": MARKER + "\nfake"}
        api = FakeApi(HEAD_A, [[user_copy], [bot(7, MARKER + "\nold")]])
        self.assertEqual(sticky_comment.upsert(api, REPO, PR, HEAD_A, BODY), "updated comment 7")
        self.assertEqual(
            api.writes(), [("PATCH", f"repos/{REPO}/issues/comments/7", {"body": BODY})]
        )

    # Review r5 finding 3: a re-run for older commit A after B was pushed must not overwrite B's report.
    def test_stale_run_does_not_touch_the_comment(self) -> None:
        api = FakeApi(HEAD_B, [[bot(7, MARKER + "\n### ❌ Perf: report for B")]])
        result = sticky_comment.upsert(api, REPO, PR, HEAD_A, BODY)
        self.assertTrue(result.startswith("skipped"), result)
        self.assertEqual(api.writes(), [])

    def test_rejects_a_body_without_the_marker(self) -> None:
        api = FakeApi(HEAD_A, [[]])
        with self.assertRaises(ValueError):
            sticky_comment.upsert(api, REPO, PR, HEAD_A, "### no marker\n")
        self.assertEqual(api.writes(), [])

    def test_main_reports_api_errors_as_exit_1(self) -> None:
        def broken(_method: str, _path: str, _body: dict | None) -> tuple[Any, str | None]:
            raise OSError("HTTP 403")

        with tempfile.TemporaryDirectory() as tmp:
            comment = Path(tmp) / "comment.md"
            comment.write_text(BODY, encoding="utf-8")
            argv = [str(comment), "--repo", REPO, "--pr", str(PR), "--head-sha", HEAD_A]
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                self.assertEqual(sticky_comment.main(argv, api=broken), 1)
            self.assertIn("HTTP 403", out.getvalue())


if __name__ == "__main__":
    unittest.main()
