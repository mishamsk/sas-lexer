---
name: release
description: Prepare a sas-lexer Rust or Python release PR, validate the merged commit, and hand the signed tag to the maintainer; CI publishes from the tag
argument-hint: "<rust|py> <X.Y.Z | X.Y.Z-(alpha|beta|rc).N>"
disable-model-invocation: true
---

# Release

Release one train of sas-lexer: `/release rust 1.0.0-beta.5` or `/release py 1.0.0-beta.3`.

**Both arguments are required.** Never infer the train or the version, and never pick the train from the most recent tag. If either is missing or invalid, stop and ask.

| Train | Publishes | Version source | Changelog | Tag | Workflow |
|---|---|---|---|---|---|
| `rust` | `sas-lexer-macro` (if bumped), `sas-lexer` to crates.io | `[workspace.package] version` in `Cargo.toml` | `CHANGELOG-RUST.md` | `rust-vX.Y.Z` | `release-rust.yml` |
| `py` | `sas-lexer` to PyPI | `crates/sas-lexer-py/Cargo.toml`, mirrored in `pyproject.toml` and `src/sas_lexer/__init__.py` | `CHANGELOG-PY.md` | `py-vX.Y.Z` | `release-python.yml` |

Versions are written in Cargo spelling everywhere (`1.0.0-beta.3`). Wheels and PyPI use the PEP 440 form (`1.0.0b3`; `-alpha.N` → `aN`, `-rc.N` → `rcN`).

## Roles

- **Agent:** prepares the release PR, then validates the merged commit. It never merges, tags, publishes or dispatches the production workflows.
- **Maintainer:** reviews and squash-merges the PR, then runs `just release-tag <train> <sha>`. That signs the tag and pushes it. CI then tests, builds and publishes with no further approval.

## 1. Pre-flight

1. `git status --porcelain` is empty. Run `git fetch origin main --tags`.
2. Find the last tag of the train: `git describe --tags --abbrev=0 --match 'rust-v*' origin/main` (or `'py-v*'`). The new version must be newer than the current one. `just release-check <train>` prints the current version.
3. **py, dependency ordering:** the binding builds against the *published* `sas-lexer` crate, not the workspace source.
   - If the release must contain lexer changes made since the last Rust release, release the Rust train first.
   - Then bump both `sas-lexer` entries (`[dependencies]` and `[build-dependencies]`) in `crates/sas-lexer-py/Cargo.toml`, together with the regenerated `token_type.py`, `token_channel.py` and `error_kind.py`. Do this in the release PR or in a PR merged before it.
   - Otherwise, stop and ask.
4. **rust, macro crate:**
   - If `crates/sas-lexer-macro` changed since the last `rust-v*` tag, the release PR must bump its `version` and the `sas-lexer-macro` requirement in `[workspace.dependencies]`.
   - CI then publishes the macro before the core. `just release-rust-dry-run` fails if the macro changed without a bump.

## 2. Changelog review (subagent)

Launch a subagent to compare the changes since the train's last tag with the changelog's Unreleased section.

- Scope the commits by path:
  - rust: `crates/` except `crates/sas-lexer-py`;
  - py: `crates/sas-lexer-py`, `src/`, `tests/`, `pyproject.toml`.
- Map each commit to its PR. Find the numbers in the commit messages, or use `gh pr list --state merged --base main`.
- List the user-facing items that are missing from the changelog.

Propose entries in the existing style, with PR links `([#N](https://github.com/mishamsk/sas-lexer/pull/N))`. Show them to the maintainer before continuing.

## 3. Release PR

```bash
git switch --no-track -c release/<train>-<version> origin/main
# add any missing changelog entries to the Unreleased section first
just release-prep <train> <version>
git diff
```

`release-prep` does the following, and fails without changing any file if the version or the changelog is not valid:

- sets the version in every source of the train;
- drops the empty subsections from Unreleased and turns it into `## [<version>] - <today>`;
- adds a fresh Unreleased template;
- updates the compare links;
- updates `Cargo.lock`, and `uv.lock` for py.

Then:

1. Check the diff. Run `just fmt`, `just lint` and `just release-check <train>`.
2. **rust:** run `just release-rust-dry-run`. It packages and verifies the macro and the core as crates.io would, without uploading.
3. Commit with the message `chore(release): <rust|py> <version>`, push only this branch, and open a PR against `main` with a short summary of the changelog entry.
4. Share the PR URL. The maintainer reviews and squash-merges it. Never merge it yourself.

## 4. Validate the merged commit

1. `git switch main && git pull --ff-only`. The release SHA is the squash commit of the release PR. If anything merged after it, ask before continuing.
2. CI is green on that commit: `gh run list --commit <sha> --event push` shows successful `ci.yml` and `perf.yml` runs. `just release-tag` checks this again.
3. **py, TestPyPI candidate** (every dispatch builds a unique `<PEP 440>.dev<run-id>` version, so it can be repeated):

```bash
gh workflow run publish-test-pypi.yml --ref main
gh run list --workflow publish-test-pypi.yml --event workflow_dispatch --limit 3 --json databaseId,headSha,createdAt,status
```

Take the run created by this dispatch and require its `headSha` to equal the release SHA. Then run `gh run watch <run-id> --exit-status`. Install the exact uploaded wheel, checked against the run's own artifact, with dependencies from PyPI. On Apple silicon, with `X.Y.ZbN` being the PEP 440 release version:

```bash
bash -euo pipefail -s -- <run-id> X.Y.ZbN 3.13 <<'SMOKE'
run_id="$1"; pv="$2.dev$1"; py="$3"; dir="$(mktemp -d)"
gh run download "$run_id" -n "wheels-macos-aarch64-$py" -D "$dir/run"
whl="$(basename "$dir"/run/*.whl)"
case "$whl" in "sas_lexer-$pv-"*) ;; *) echo "$whl is not version $pv" >&2; exit 1 ;; esac
curl -fsS "https://test.pypi.org/pypi/sas-lexer/$pv/json" -o "$dir/release.json"
meta="$(uv run --no-project python -I -c 'import json, sys; f = next(u for u in json.load(open(sys.argv[1]))["urls"] if u["filename"] == sys.argv[2]); print(f["url"], f["digests"]["sha256"])' "$dir/release.json" "$whl")"
read -r url sha256 <<<"$meta"
curl -fsSL -o "$dir/$whl" "$url"
# The TestPyPI file, its listed digest and the run's own artifact must be identical.
for f in "$dir/$whl" "$dir/run/$whl"; do
    actual="$(shasum -a 256 "$f" | cut -d' ' -f1)"
    [ "$actual" = "$sha256" ] || { echo "sha256 mismatch: $f is $actual, TestPyPI lists $sha256" >&2; exit 1; }
done
mise x "python@$py" -- uv run --no-project --isolated --with "$dir/$whl" python -I -c '
import sys, importlib.metadata as md, sas_lexer
from sas_lexer import lex_program_from_str
assert md.version("sas-lexer") == sys.argv[1] == sas_lexer.__version__, (md.version("sas-lexer"), sas_lexer.__version__)
tokens, errors, _ = lex_program_from_str("data a; x = 1; run;")
assert tokens and not errors, errors
print("ok", sys.argv[1])
' "$pv"
echo "verified $whl ($sha256)"
SMOKE
```

Repeat for the oldest and the newest supported Python (see `requires-python` and the wheel matrix). If any hash or assertion fails, stop. Never substitute another run's upload or a production build.

## 5. Hand off the tag

Report:

- the release SHA, the version and the CI links;
- for py, the TestPyPI run and the smoke results.

Then ask the maintainer to run:

```bash
just release-tag <train> <sha>
```

The recipe checks that the working tree is clean, that `HEAD` is `<sha>` on `origin/main`, that the CI and perf runs are green, the versions, the dated changelog entry, and that the tag does not exist yet. It then creates the signed tag `<rust|py>-v<version>` and pushes only that tag.

Never run it yourself, and never push a release tag to test anything.

## 6. After the tag

1. Find the run with `gh run list --workflow release-<rust|python>.yml --event push --limit 3`, then `gh run watch <run-id> --exit-status`.
2. **rust:**
   - The workflow validates, runs CI, publishes `sas-lexer-macro` (only when bumped), then `sas-lexer`, and drafts the GitHub release.
   - Re-running failed jobs is safe. A version that is already on crates.io is accepted only if it matches this commit's package.
   - Python releases that need this lexer then bump the binding's dependencies (pre-flight 3).
3. **py:**
   - The workflow validates, runs CI, builds and tests the wheels, checks that every artifact is this version, attests them, uploads to PyPI, and drafts the GitHub release.
   - Re-running failed jobs is safe: files that PyPI already has are skipped, and different files are rejected.
4. Report the run and the published version. The maintainer reviews and publishes the draft GitHub release.

## Notes

- Never dispatch TestPyPI, tag or publish to validate documentation or tooling changes. `just release-check` and `just release-rust-dry-run` are the safe previews.
- Train versions change only through `just release-prep`; the macro version is bumped by hand (pre-flight 4).
