"""Create or update the sticky perf report comment on a PR (stdlib only).

    sticky_comment.py COMMENT_MD --repo OWNER/NAME --pr N --head-sha SHA

Used by the `comment` job of .github/workflows/perf.yml with GITHUB_TOKEN (pull-requests: write).
The comment is the one authored by `github-actions[bot]` whose body starts with the hidden
marker. A run whose PR head SHA is no longer the PR's current head (e.g. a re-run of an older
commit) doesn't touch the comment, so it can't replace a newer report with a stale one.

Exit codes: 0 posted, updated or skipped as stale; 1 on API or input errors.
"""

import argparse
import json
import os
import re
import sys
import urllib.request
from collections.abc import Callable
from pathlib import Path
from typing import Any

from report import MARKER

BOT_LOGIN = "github-actions[bot]"
API_ROOT = os.environ.get("GITHUB_API_URL", "https://api.github.com")

# (method, path or absolute URL, JSON body or None) -> (decoded JSON, next-page URL or None)
Api = Callable[[str, str, dict | None], tuple[Any, str | None]]


def github_api(token: str) -> Api:
    def call(method: str, path: str, body: dict | None) -> tuple[Any, str | None]:
        url = path if path.startswith("https://") else f"{API_ROOT}/{path}"
        request = urllib.request.Request(
            url,
            method=method,
            data=json.dumps(body).encode() if body is not None else None,
            headers={
                "Accept": "application/vnd.github+json",
                "Authorization": f"Bearer {token}",
                "X-GitHub-Api-Version": "2022-11-28",
                "User-Agent": "sas-lexer-perf-comment",
            },
        )
        with urllib.request.urlopen(request, timeout=30) as response:
            data = json.loads(response.read() or b"null")
            match = re.search(r'<([^>]+)>;\s*rel="next"', response.headers.get("Link", ""))
        return data, match.group(1) if match else None

    return call


def paginate(api: Api, path: str) -> list[dict]:
    items: list[dict] = []
    url: str | None = path
    while url:
        page, url = api("GET", url, None)
        if not isinstance(page, list):
            msg = f"expected a list from {path}"
            raise TypeError(msg)
        items += page
    return items


def upsert(api: Api, repo: str, pr: int, head_sha: str, body: str) -> str:
    """Post or update the sticky comment; returns what happened."""
    if not body.startswith(MARKER + "\n"):
        msg = "the comment body doesn't start with the perf report marker"
        raise ValueError(msg)
    current, _ = api("GET", f"repos/{repo}/pulls/{pr}", None)
    current_head = current["head"]["sha"] if isinstance(current, dict) else None
    if current_head != head_sha:
        return (
            f"skipped: this run is for {head_sha[:10]}, the PR head is now {str(current_head)[:10]}"
        )
    comments = paginate(api, f"repos/{repo}/issues/{pr}/comments?per_page=100")
    existing = next(
        (
            c
            for c in comments
            if c.get("user", {}).get("login") == BOT_LOGIN
            and str(c.get("body", "")).startswith(MARKER)
        ),
        None,
    )
    if existing:
        api("PATCH", f"repos/{repo}/issues/comments/{existing['id']}", {"body": body})
        return f"updated comment {existing['id']}"
    created, _ = api("POST", f"repos/{repo}/issues/{pr}/comments", {"body": body})
    return f"created comment {created['id'] if isinstance(created, dict) else '?'}"


def main(argv: list[str] | None = None, api: Api | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Create or update the sticky perf report comment on a PR."
    )
    parser.add_argument("comment", type=Path)
    parser.add_argument("--repo", required=True)
    parser.add_argument("--pr", type=int, required=True)
    parser.add_argument("--head-sha", required=True, help="PR head SHA of the run's event")
    args = parser.parse_args(argv)
    try:
        if api is None:
            api = github_api(os.environ["GITHUB_TOKEN"])
        body = args.comment.read_text(encoding="utf-8")
        print(upsert(api, args.repo, args.pr, args.head_sha, body))
    except Exception as e:
        print(f"::warning::Could not post the perf report comment: {e}")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
