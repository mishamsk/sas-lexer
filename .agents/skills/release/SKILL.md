---
name: release
description: Prepare and publish a sas-lexer release (Rust crates or Python package) with changelog review and smoke checks
argument-hint: "<rust|py> [version|level]"
disable-model-invocation: true
---

## Release Command

Execute the release flow for one sas-lexer train, e.g. `/release rust`, `/release py beta` or `/release rust 1.0.0` in Claude Code (`$release ...` in Codex).

| Train | Package (`-p`) | Changelog | Tag | CI workflow |
|---|---|---|---|---|
| `rust` | `sas-lexer` (version from `[workspace.package]`) | `CHANGELOG-RUST.md` | `rust-vX` | `release-rust.yml` |
| `py` | `sas-lexer-py` (PyPI name `sas-lexer`) | `CHANGELOG-PY.md` | `py-vX` | `release-python.yml` |

cargo-release (pinned in `mise.toml`, run as `mise x -- cargo release ...`) bumps versions, rewrites the changelog headers and links, commits, signs and tags, and pushes. It never publishes: CI does that from the tag via trusted publishing, then publishes the GitHub release with the changelog section as notes.

## Context Collection

- Current branch: `git branch --show-current`
- Working tree status: `git status --porcelain`
- Sync with remote: `git fetch origin && git status -sb`
- Last train tag: `git describe --tags --abbrev=0 --match '<rust|py>-v*'`
- Current version: `grep -m1 '^version = ' Cargo.toml` (rust) or `grep -m1 '^version = ' pyproject.toml` (py)
- CI on HEAD: `gh run list --branch main --workflow ci.yml -L 1`

## Pre-flight Checks

1. Working tree is clean.
2. On `main`, level with `origin/main`.
3. The last train tag exists and CI on HEAD is green.

If any check fails, stop and report the issue.

## Execution Steps

### Step 1: Changelog Review (Use Subagent)

Launch a subagent to review the changes between the last train tag and HEAD:
- List the commits since the tag that touch the train: `rust` = `crates/sas-lexer`, `crates/sas-lexer-macro`, `Cargo.toml`; `py` = `crates/sas-lexer-py`, `src/`, `pyproject.toml`, `Cargo.toml` and `uv.lock` dependency changes.
- Map commits to PRs (PR numbers in commit messages, `gh pr list --state merged --base main`).
- Compare against the train changelog's `[Unreleased]` section and identify missing user-facing items.

Update the `[Unreleased]` section. Match the verbosity and style of existing entries, including their emoji prefixes, and link PRs as `([#N](https://github.com/mishamsk/sas-lexer/pull/N))`. Replace the `-` placeholder line of a subsection you fill; leave the placeholder in empty subsections (cargo-release drops those).

### Step 2: Version and User Verification (Gate 1)

If the invocation gives a version or level (`major`, `minor`, `patch`, `release`, `alpha`, `beta`, `rc`), use it. Otherwise infer it:
- Current version is a pre-release (e.g. `1.0.0-beta.4`): the next pre-release of that series (`beta` → `1.0.0-beta.5`).
- Otherwise: anything under Removed or marked breaking (💥) → `major`; entries under Added → `minor`; only Fixed → `patch`.

Preview the result with `mise x -- cargo release version <level> -p <package>` (a dry run; it reports invalid levels too).

`rust` only: if `git diff --quiet <last tag> HEAD -- crates/sas-lexer-macro` reports changes, `sas-lexer-macro` (versioned separately, `0.x`) needs a bump too: `patch` for fixes, `minor` for anything else.

Show the changelog diff, the new version(s) and the inferred level. For `py`, say that the release commit goes to `main` before the TestPyPI check (the tag waits for Gate 2). Ask the user to confirm.

### Step 3: Bump, Commit and Tag

```bash
# rust, only if the macro changed:
mise x -- cargo release version <patch|minor> -p sas-lexer-macro --execute --no-confirm
git commit -am "docs: update <CHANGELOG-RUST.md|CHANGELOG-PY.md> for release"
mise x -- cargo release <level|version> -p <package> --execute --no-confirm --no-push
```

The last command creates the signed release commit and the signed tag locally.

### Step 4: Smoke Checks

`rust`:
```bash
cargo publish --dry-run --locked -p sas-lexer            # add `-p sas-lexer-macro` first if it was bumped
just fmt-check clippy test
```

`py`: TestPyPI builds from GitHub, so push the release commit (not the tag) first, then build and upload that exact commit:
```bash
git push origin main
sha=$(git rev-parse HEAD); start=$(date -u +%Y-%m-%dT%H:%M:%SZ)
gh workflow run publish-test-pypi.yml --ref main
gh run list --workflow publish-test-pypi.yml --event workflow_dispatch --branch main --commit "$sha" \
  --created ">=$start" --json databaseId,url -q '.[] | "\(.databaseId) \(.url)"'
```
Repeat the `gh run list` command every few seconds until it prints the run, then `gh run watch <id> --exit-status`. Continue only if the run succeeds. Then install X from TestPyPI and smoke it:
```bash
uv run --isolated --no-project --refresh-package sas-lexer --index https://test.pypi.org/simple/ \
  --with "sas-lexer==X" -- python -c "import sas_lexer; tokens, errors, _ = sas_lexer.lex_program_from_str('data a; x = 1; run;'); assert sas_lexer.__version__ == 'X' and tokens and not errors; print(len(tokens), 'tokens')"
```

If the upload fails only because TestPyPI already has version X (a retry of the same release), skip the upload, smoke-test the existing X, and say so at Gate 2.

### Step 5: Final Confirmation (Gate 2)

Report the smoke results. If all passed, ask the user: "Ready to trigger the production release?"

### Step 6: Push

If the user confirms:

```bash
mise x -- cargo release push -p <package> --execute --no-confirm
```

This pushes `main` and the tag. The tag starts the train's CI workflow: tests, publishing, and the GitHub release. Find that run with `gh run list --workflow <release-rust.yml|release-python.yml> --branch <tag> --json databaseId,url`, then wait on it with `gh run watch <id> --exit-status`. After it succeeds, report the release link (`gh release view <tag> --json url -q .url`). If the rendered notes look wrong, fix only the notes with `gh release edit <tag> --notes-file <file>`; never touch the release assets.

If the user declines, stop. Nothing has been published; report the local commit and tag (and, for `py`, that the release commit is on `main`).

## Important Notes

- Follow `AGENTS.md`.
- Never use `--no-verify` when committing, and never publish from a local machine.
