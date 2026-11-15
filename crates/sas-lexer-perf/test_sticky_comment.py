"""Happy-path test for sticky_comment.py with a tiny fake GitHub API."""

import unittest
from typing import Any

import sticky_comment
from report import MARKER

BODY = MARKER + "\n### ✅ Perf: no regressions over 2%\n"


class UpsertTest(unittest.TestCase):
    def test_creates_then_updates(self) -> None:
        comments: list[dict] = []
        writes: list[str] = []

        def api(method: str, path: str, body: dict | None) -> tuple[Any, str | None]:
            if path.endswith("/pulls/19"):
                return {"head": {"sha": "abc"}}, None
            if method == "GET":
                return comments, None
            writes.append(f"{method} {path}")
            if method == "POST":
                comments.append(
                    {"id": 7, "user": {"login": sticky_comment.BOT_LOGIN}, **(body or {})}
                )
            return {"id": 7}, None

        sticky_comment.upsert(api, "o/r", 19, "abc", BODY)
        sticky_comment.upsert(api, "o/r", 19, "abc", BODY)
        self.assertEqual(
            writes, ["POST repos/o/r/issues/19/comments", "PATCH repos/o/r/issues/comments/7"]
        )


if __name__ == "__main__":
    unittest.main()
