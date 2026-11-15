# Personal recipes, if present (missing file is fine).
import? '~/.justfile'

set shell := ["bash", "-euo", "pipefail", "-c"]
# Bare `just` lists the recipes.
set default-list
# Loads .env (if present) for SAS_LEX_SAMPLES, PERF_LARGE_SAMPLE, etc.
set dotenv-load

# Non-Python CLI tools (prek, typos, cargo-hack, cargo-udeps, hyperfine) and uv are pinned in
# mise.toml and run via `mise x --`. Python tools (ruff, ty, pytest) come from the pyproject dev
# group / uv.lock and run via `uv run`.

# Dated nightly used for udeps and the nightly test job. Keep in sync with CI.
nightly := "nightly-2025-12-30"

# ---------------------------------------------------------------------------
# Formatting, linting, checks (same commands as CI)
# ---------------------------------------------------------------------------

fmt:
    cargo fmt --all

fmt-check:
    cargo fmt --all --check

# Clippy over the whole workspace, warnings are errors (CI "cargo clippy").
clippy:
    cargo clippy --workspace --all-features --locked -- -D warnings

# Run all linters: pre-commit hooks (via prek) and typos.
lint:
    mise x -- prek run --all-files
    mise x -- typos

# Run lint auto-fixes (clippy --fix).
lint-fix:
    cargo clippy --fix --allow-dirty --allow-staged --workspace --all-features --locked -- -D warnings

# Python lint and format check (CI "ruff"). Only the dev group is synced, so no extension build.
ruff:
    uv run --only-group dev ruff check src
    uv run --only-group dev ruff format --check src

# Unused dependencies, on the pinned nightly (CI "cargo udeps").
udeps:
    mise x -- cargo +{{ nightly }} udeps --workspace --all-features --release

# Check declared per-package MSRVs over the feature powerset (CI "Check MSRV").
msrv:
    mise x -- cargo hack check --feature-powerset --locked --rust-version --ignore-private --workspace --all-targets --skip opti_stats

# ---------------------------------------------------------------------------
# Build (extra args are forwarded, e.g. `just build -v`)
# ---------------------------------------------------------------------------

# Debug build of the default workspace members.
[positional-arguments]
build *args:
    cargo build "$@"

# Build the docs.
[positional-arguments]
doc *args:
    cargo doc "$@"

[positional-arguments]
build-rel *args:
    cargo build --release --workspace "$@"

[positional-arguments]
build-stats-polars *args:
    cargo build -r --features polars --package sas-lexer-cli --target-dir target/with_polars "$@"

# Rebuild and install the python extension into the project venv (release profile, see [tool.maturin]).
[positional-arguments]
build-dev *args:
    uv sync --locked --reinstall-package sas-lexer "$@"

# ---------------------------------------------------------------------------
# Tests (extra args are forwarded to cargo test / pytest)
# ---------------------------------------------------------------------------

# Rust tests for the core crate; target is a cargo target selector, e.g. all-targets or lib.
[positional-arguments]
test-rust target="all-targets" *args:
    cargo test -p sas-lexer --doc "${@:2}"
    cargo test -p sas-lexer --{{ target }} "${@:2}"
    cargo test -p sas-lexer --{{ target }} --features macro_sep --features serde "${@:2}"

# Python tests in an isolated, non-editable environment (rebuilds the extension).
[positional-arguments]
test-python *args:
    uv run --isolated --no-editable pytest "$@"

# Python tests as run in CI: sync the project non-editable, then pytest.
[positional-arguments]
test-python-ci *args:
    uv sync --no-editable --locked
    uv run pytest "$@"

# All tests: Rust doc/unit/integration, then Python.
test: test-rust test-python

# Run everything CI runs, locally, with the same recipes as the CI jobs.
ci: fmt-check clippy udeps msrv ruff build doc test-rust test-python-ci

# ---------------------------------------------------------------------------
# Python environment
# ---------------------------------------------------------------------------

# Sync dependencies & install project.
py-sync:
    uv sync --all-extras --locked --compile-bytecode --python python

# Create the venv, sync, create .env from .env.sample and install git hooks. Refuses if .venv exists.
[unix]
py-init:
    #!/usr/bin/env bash
    set -euo pipefail
    uv --version >/dev/null || { echo "uv is not installed"; exit 1; }
    if test -d .venv; then
        echo "A virtual environment already exists. Remove it first or use py-init-force." >&2
        exit 1
    fi
    uv venv --python python
    test -f .env || cp .env.sample .env
    just py-sync
    mise x -- prek install
    echo Done

# DESTRUCTIVE: remove caches and .venv, then recreate the environment (asks for confirmation).
[confirm("This deletes .venv and all caches and recreates the environment. Continue?")]
[unix]
py-init-force: clean-all-caches clean-test
    rm -fr .venv
    just py-init

# ---------------------------------------------------------------------------
# Cleanup (local, explicit; nothing here runs implicitly)
# ---------------------------------------------------------------------------

# Remove Python file artifacts.
[unix]
clean-pyc:
    find . -name '*.pyc' -not -path '*/.venv/*' -not -path '*/.tox/*' -exec rm -f {} +
    find . -name '*.pyo' -not -path '*/.venv/*' -not -path '*/.tox/*' -exec rm -f {} +
    find . -name '*~' -not -path '*/.venv/*' -not -path '*/.tox/*' -exec rm -f {} +
    find . -name '__pycache__' -not -path '*/.venv/*' -not -path '*/.tox/*' -exec rm -fr {} +

# Remove test and coverage artifacts.
[unix]
clean-test:
    rm -f .coverage .coverage.* coverage.xml report.xml
    rm -fr htmlcov/ .pytest_cache testtemp/

# Remove lint (ruff) caches.
[unix]
clean-lint-caches:
    rm -fr .ruff_cache

# Remove all caches.
[unix]
clean-all-caches: clean-pyc clean-lint-caches
    rm -fr .pytest_cache

# DESTRUCTIVE: clean everything incl. cargo target. Like a fresh clone except for .env (asks for confirmation).
[confirm("This removes all caches and runs cargo clean. Continue?")]
[unix]
clean: clean-all-caches clean-test
    cargo clean

# ---------------------------------------------------------------------------
# Performance (need SAS_LEX_SAMPLES / PERF_LARGE_SAMPLE, see .env.sample)
# ---------------------------------------------------------------------------

# Profile the lexer benchmark.
perf-profile:
    cargo bench --bench lexer -- --profile-time 5
    @echo "Path to the profile: target/criterion/lex/lex/profile/flamegraph.svg"

# Benchmark the current release build.
[unix]
perf-cur: build-rel
    mise x -- hyperfine -N -w 10 "target/release/sas-lexer lex ${SAS_LEX_SAMPLES:?set SAS_LEX_SAMPLES in .env}"
    mise x -- hyperfine -N -w 5 "target/release/sas-lexer lex ${PERF_LARGE_SAMPLE:?set PERF_LARGE_SAMPLE in .env}"

# Compare the current release build with the given commit, built in an isolated git worktree.
[unix]
perf-compare sha: build-rel
    #!/usr/bin/env bash
    set -euo pipefail
    : "${SAS_LEX_SAMPLES:?set SAS_LEX_SAMPLES in .env}" "${PERF_LARGE_SAMPLE:?set PERF_LARGE_SAMPLE in .env}"
    work="$(mktemp -d)"
    cleanup() { git worktree remove --force "$work/base" >/dev/null 2>&1 || true; rm -rf "$work"; }
    trap cleanup EXIT
    git worktree add --detach "$work/base" "{{ sha }}"
    # The baseline gets its own target dir (reused between runs); the current tree is untouched.
    cargo build --release --package sas-lexer-cli --manifest-path "$work/base/Cargo.toml" --target-dir "{{ justfile_directory() }}/target/perf-compare"
    base="{{ justfile_directory() }}/target/perf-compare/release/sas-lexer"
    mise x -- hyperfine -w 10 "$base lex $SAS_LEX_SAMPLES" "target/release/sas-lexer lex $SAS_LEX_SAMPLES"
    mise x -- hyperfine -w 5 "$base lex $PERF_LARGE_SAMPLE" "target/release/sas-lexer lex $PERF_LARGE_SAMPLE"
