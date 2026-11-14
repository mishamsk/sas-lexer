set shell := ["bash", "-euo", "pipefail", "-c"]
set windows-shell := ["pwsh", "-NoLogo", "-Command"]
# Loads .env (if present) for SAS_LEX_SAMPLES, PERF_LARGE_SAMPLE, etc.
set dotenv-load

# Dated nightly used for udeps and the nightly test job. Keep in sync with CI.
nightly := "nightly-2025-12-30"

# Show help (same as just running just)
help:
    @just --list

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
    uv run prek run --all-files
    typos

# Run lint auto-fixes (clippy --fix).
lint-fix:
    cargo clippy --fix --allow-dirty --allow-staged --workspace --all-features --locked -- -D warnings

# Python lint and format check (CI "ruff"). Ruff version matches uv.lock and the pre-commit hook.
ruff:
    uvx ruff@0.14.10 check src
    uvx ruff@0.14.10 format --check src

# Unused dependencies, on the pinned nightly (CI "cargo udeps").
udeps:
    cargo +{{ nightly }} udeps --workspace --all-features --release

# Check declared per-package MSRVs over the feature powerset (CI "Check MSRV").
msrv:
    cargo hack check --feature-powerset --locked --rust-version --ignore-private --workspace --all-targets --skip opti_stats

# ---------------------------------------------------------------------------
# Build
# ---------------------------------------------------------------------------

# Debug build of the default workspace members.
build:
    cargo build --verbose

# Build the docs.
doc:
    cargo doc --verbose

build-rel:
    cargo build --release --workspace

build-stats-polars:
    cargo build -r --features polars --package sas-lexer-cli --target-dir target/with_polars

# Build and install the python extension into the current venv.
build-dev:
    uvx maturin develop --uv -r

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

# Rust tests for the core crate; target is a cargo target selector, e.g. all-targets or lib.
test-rust target="all-targets":
    cargo test -p sas-lexer --doc
    cargo test -p sas-lexer --{{ target }}
    cargo test -p sas-lexer --{{ target }} --features macro_sep --features serde

# Python tests in an isolated, non-editable environment (rebuilds the extension).
test-python:
    uv run --isolated --no-editable pytest

# Python tests as run in CI: sync the project non-editable, then pytest.
test-python-ci:
    uv sync --no-editable --locked
    uv run pytest

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
    uv run prek install
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
# Performance (need SAS_LEX_SAMPLES / PERF_LARGE_SAMPLE, see .env.sample; hyperfine)
# ---------------------------------------------------------------------------

# Profile the lexer benchmark.
perf-profile:
    cargo bench --bench lexer -- --profile-time 5
    @echo "Path to the profile: target/criterion/lex/lex/profile/flamegraph.svg"

# Benchmark the current release build.
[unix]
perf-cur: build-rel
    hyperfine -N -w 10 "target/release/sas-lexer lex ${SAS_LEX_SAMPLES:?set SAS_LEX_SAMPLES in .env}"
    hyperfine -N -w 5 "target/release/sas-lexer lex ${PERF_LARGE_SAMPLE:?set PERF_LARGE_SAMPLE in .env}"

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
    hyperfine -w 10 "$base lex $SAS_LEX_SAMPLES" "target/release/sas-lexer lex $SAS_LEX_SAMPLES"
    hyperfine -w 5 "$base lex $PERF_LARGE_SAMPLE" "target/release/sas-lexer lex $PERF_LARGE_SAMPLE"

# ---------------------------------------------------------------------------
# Release (cargo-release). Plain recipes are DRY RUNS; the -execute ones really release.
# ---------------------------------------------------------------------------

# Dry run: release the main sas-lexer crate. level: major|minor|patch|release|rc|beta|alpha or an explicit semver
[unix]
release-rust $level:
    @just _release sas-lexer "$level" dry

# EXECUTES: release the main sas-lexer crate (commit, tag, publish).
[confirm("Really release sas-lexer (not a dry run)?")]
[unix]
release-rust-execute $level:
    @just _release sas-lexer "$level" execute

# Dry run: release the python package. level: major|minor|patch|release|rc|beta|alpha or an explicit semver
[unix]
release-py $level:
    @just _release sas-lexer-py "$level" dry

# EXECUTES: release the python package (commit, tag).
[confirm("Really release sas-lexer-py (not a dry run)?")]
[unix]
release-py-execute $level:
    @just _release sas-lexer-py "$level" execute

# Shared cargo-release wrapper. Only an allow-listed level or a plain semver is accepted,
# so flags such as --execute cannot be smuggled in; only mode=execute adds -x.
[private]
[unix]
_release package $level mode:
    #!/usr/bin/env bash
    set -euo pipefail
    case "$level" in
        major|minor|patch|release|rc|beta|alpha) ;;
        *)
            if ! [[ "$level" =~ ^[0-9]+\.[0-9]+\.[0-9]+(-[0-9A-Za-z.-]+)?(\+[0-9A-Za-z.-]+)?$ ]]; then
                echo "invalid release level: $level (expected major|minor|patch|release|rc|beta|alpha or a semver)" >&2
                exit 1
            fi
            ;;
    esac
    if [ "{{ mode }}" = execute ]; then
        exec cargo release -p "{{ package }}" -x "$level"
    fi
    exec cargo release -p "{{ package }}" "$level"
