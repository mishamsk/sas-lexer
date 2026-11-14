set shell := ["bash", "-euo", "pipefail", "-c"]
set windows-shell := ["pwsh", "-NoLogo", "-Command"]
# Loads .env (if present) for SAS_LEX_SAMPLES, PERF_LARGE_SAMPLE, etc.
set dotenv-load

# Dated nightly used for udeps and the nightly test job. Single source of truth: CI reads it from here.
nightly := "nightly-2026-11-04"

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

# Python lint and format check (CI "ruff"). Runs the locked Ruff from the dev group (matches the
# pre-commit hook rev) without building the extension.
ruff:
    uv run --locked --only-group dev ruff check src
    uv run --locked --only-group dev ruff format --check src

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

# Run everything CI runs, locally, with the same recipes as the CI jobs (except the
# Linux-only `perf-check` gate; run `just perf-check` or `just perf-docker perf-check` for it).
ci: fmt-check clippy udeps msrv ruff build doc test-rust test-python-ci perf-test

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
# Performance regression guard (Callgrind instruction counts; Linux + valgrind +
# gungraun-runner; on macOS use the -docker variants). CI runs `just perf-check`.
# ---------------------------------------------------------------------------

# Fail the check when a gated metric (instructions, estimated cycles) is this much (%) worse.
perf_threshold := env("PERF_THRESHOLD", "10")
# Valgrind major.minor that CI (ubuntu-26.04) and the perf Docker image ship; perf-check refuses others.
perf_valgrind := "3.26"
perf_image := "sas-lexer-perf:local"

# Run the Callgrind benchmarks for the working tree, both feature configs. Extra args go to gungraun.
[positional-arguments]
[unix]
perf-bench *args:
    cargo bench -p sas-lexer-perf --bench lexer -- --home="{{ justfile_directory() }}/target/gungraun/default" "$@"
    cargo bench -p sas-lexer-perf --bench lexer --features macro_sep -- --home="{{ justfile_directory() }}/target/gungraun/macro_sep" "$@"

# Validate the perf harness: benchmark inputs (both feature configs) and the report/gate logic.
perf-test:
    cargo test -p sas-lexer-perf --lib --locked
    cargo test -p sas-lexer-perf --lib --locked --features macro_sep
    uv run --no-project --directory crates/sas-lexer-perf python -m unittest -v test_report

# Regression gate: the lexer at `base` vs the working tree, same harness and inputs. Fails on >= perf_threshold% regressions.
[unix]
perf-check $base="origin/main":
    #!/usr/bin/env bash
    set -euo pipefail
    root="{{ justfile_directory() }}"
    perf="$root/crates/sas-lexer-perf"
    out="$root/target/perf-check"
    configs=(default macro_sep)
    # Nothing from the environment (or .env) may filter or reconfigure the gated runs.
    while IFS= read -r var; do unset "$var"; done < <(compgen -e | grep '^GUNGRAUN_' || true)
    # Only the base build below may select the base API adapter (cfg(perf_base)).
    unset SAS_LEXER_PERF_BASE
    # Tool versions: the runner must match the gungraun crate, valgrind the pinned major.minor.
    gungraun="$(sed -n 's/^gungraun = "=\(.*\)"$/\1/p' "$perf/Cargo.toml")"
    runner="$(gungraun-runner --version | awk '{print $2}')"
    valgrind="$(valgrind --version | sed 's/^valgrind-//')"
    if [ "$runner" != "$gungraun" ]; then
        echo "gungraun-runner $runner does not match the gungraun crate $gungraun" >&2; exit 2
    fi
    case "$valgrind" in
        "{{ perf_valgrind }}" | "{{ perf_valgrind }}".*) ;;
        *) echo "valgrind $valgrind, expected {{ perf_valgrind }}.x (update perf_valgrind with the CI image)" >&2; exit 2 ;;
    esac
    base_sha="$(git rev-parse --verify --end-of-options "${base}^{commit}")"
    head_desc="$(git rev-parse --short HEAD)$(git diff --quiet HEAD -- || echo '+dirty')"
    rm -rf "$out"
    mkdir -p "$out"
    # The base lives in an isolated, throwaway worktree; the current checkout is never touched.
    src="$out/base-src"
    cleanup() { git worktree remove --force "$src" >/dev/null 2>&1 || true; }
    trap cleanup EXIT
    git worktree add --detach "$src" "$base_sha"
    # Measure the base library with HEAD's harness and inputs, so both sides run identical
    # benchmarks (this also works when the base has no perf crate yet). The base keeps its own
    # toolchain, profiles and Cargo.lock; its build is unlocked, so the harness's own deps may
    # be added to (or bump build-time deps in) the base lock. See `sas_lexer_perf::api` for
    # PRs that change the lexer API.
    rm -rf "$src/crates/sas-lexer-perf"
    cp -R "$perf" "$src/crates/sas-lexer-perf"
    report_args=()
    for cfg in "${configs[@]}"; do
        features=()
        if [ "$cfg" = macro_sep ]; then features=(--features macro_sep); fi
        home="$out/gungraun-$cfg"
        echo "::group::perf-check: base ${base_sha:0:10} [$cfg]"
        # Same target dir as head: shared deps are built once, and both bench binaries get
        # equally long paths (the path length shifts the initial stack and can change counts).
        # SAS_LEXER_PERF_BASE=1 selects the base side of `sas_lexer_perf::api` (cfg(perf_base)).
        (cd "$src" && SAS_LEXER_PERF_BASE=1 cargo bench -p sas-lexer-perf --bench lexer "${features[@]}" \
            --target-dir "$root/target" -- --home="$home" --save-baseline=base --output-format=json) \
            > "$out/$cfg.base.jsonl"
        echo "::endgroup::"
        echo "::group::perf-check: head $head_desc [$cfg]"
        cargo bench -p sas-lexer-perf --bench lexer --locked "${features[@]}" -- \
            --home="$home" --baseline=base --output-format=json > "$out/$cfg.head.jsonl"
        echo "::endgroup::"
        report_args+=(--config "$cfg" "$out/$cfg.base.jsonl" "$out/$cfg.head.jsonl")
    done
    uv run --no-project "$perf/report.py" \
        --threshold "{{ perf_threshold }}" --output "$out/summary.md" \
        --expected "$perf/benchmarks.txt" --expect-configs "${configs[@]}" \
        --title "Performance check: base \`${base_sha:0:10}\` vs head \`$head_desc\`" \
        --note "Tools: valgrind $valgrind, gungraun $gungraun, base $(cd "$src" && rustc --version), head $(rustc --version)." \
        "${report_args[@]}"

# Build the Linux image used by the -docker perf recipes (pinned Rust, valgrind, gungraun-runner).
[unix]
perf-docker-image:
    docker build -t {{ perf_image }} \
        --build-arg RUST_VERSION="$(sed -n 's/^channel = "\(.*\)"$/\1/p' rust-toolchain.toml)" \
        --build-arg UV_VERSION="$(sed -n 's/^required-version = "==\(.*\)"$/\1/p' pyproject.toml)" \
        --build-arg GUNGRAUN_VERSION="$(sed -n 's/^gungraun = "=\(.*\)"$/\1/p' crates/sas-lexer-perf/Cargo.toml)" \
        crates/sas-lexer-perf

# Run any just recipe in the Linux perf container, e.g. `just perf-docker perf-check origin/main`.
[positional-arguments]
[unix]
perf-docker *recipe: perf-docker-image
    #!/usr/bin/env bash
    set -euo pipefail
    root="{{ justfile_directory() }}"
    git_dir="$(cd "$(git rev-parse --git-common-dir)" && pwd)"
    # Same absolute paths inside the container so git worktrees resolve. Linux build output goes
    # to a named volume over target/ so it never mixes with host builds. seccomp=unconfined lets
    # gungraun disable ASLR (setarch), which the default Docker profile forbids.
    docker run --rm --security-opt seccomp=unconfined \
        -v "$root:$root" -v "$git_dir:$git_dir" \
        -v sas-lexer-perf-target:"$root/target" \
        -v sas-lexer-perf-cargo:/usr/local/cargo/registry \
        -e PERF_THRESHOLD="{{ perf_threshold }}" \
        -w "$root" {{ perf_image }} just "$@"

# Profile the criterion wall-clock benchmark (flamegraph).
perf-profile:
    cargo bench -p sas-lexer --bench lexer -- --profile-time 5
    @echo "Path to the profile: target/criterion/lex/lex/profile/flamegraph.svg"

# Wall-clock: benchmark the release CLI on private corpora (SAS_LEX_SAMPLES / PERF_LARGE_SAMPLE in .env; hyperfine).
[unix]
perf-cur: build-rel
    hyperfine -N -w 10 "target/release/sas-lexer lex ${SAS_LEX_SAMPLES:?set SAS_LEX_SAMPLES in .env}"
    hyperfine -N -w 5 "target/release/sas-lexer lex ${PERF_LARGE_SAMPLE:?set PERF_LARGE_SAMPLE in .env}"

# Wall-clock: compare the release CLI with the given commit (built in an isolated git worktree) on private corpora.
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
