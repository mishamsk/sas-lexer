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
# Performance regression guard (Callgrind instruction counts; Linux + valgrind +
# gungraun-runner; on macOS use the -docker variants). CI runs `just perf-check`.
# ---------------------------------------------------------------------------

# Fail the check when a gated metric (instructions, estimated cycles) is this much (%) worse.
perf_threshold := env("PERF_THRESHOLD", "10")
perf_image := "sas-lexer-perf:local"

# Run the Callgrind benchmarks for the working tree, both feature configs. Extra args go to gungraun.
[positional-arguments]
[unix]
perf-bench *args:
    cargo bench -p sas-lexer-perf --bench lexer -- --home="{{ justfile_directory() }}/target/gungraun/default" "$@"
    cargo bench -p sas-lexer-perf --bench lexer --features macro_sep -- --home="{{ justfile_directory() }}/target/gungraun/macro_sep" "$@"

# Regression gate: the lexer at `base` vs the working tree, same harness and inputs. Fails on >= perf_threshold% regressions.
[unix]
perf-check $base="origin/main":
    #!/usr/bin/env bash
    set -euo pipefail
    root="{{ justfile_directory() }}"
    out="$root/target/perf-check"
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
    # Cargo.lock and profiles, so dependency or profile changes in the PR are measured too.
    rm -rf "$src/crates/sas-lexer-perf"
    cp -R "$root/crates/sas-lexer-perf" "$src/crates/sas-lexer-perf"
    results=()
    for cfg in default macro_sep; do
        features=()
        if [ "$cfg" = macro_sep ]; then features=(--features macro_sep); fi
        home="$out/gungraun-$cfg"
        echo "::group::perf-check: base ${base_sha:0:10} [$cfg]"
        (cd "$src" && cargo bench -p sas-lexer-perf --bench lexer "${features[@]}" \
            --target-dir "$root/target/perf-base" -- --home="$home" --save-baseline=base)
        echo "::endgroup::"
        echo "::group::perf-check: head $head_desc [$cfg]"
        cargo bench -p sas-lexer-perf --bench lexer --locked "${features[@]}" -- \
            --home="$home" --baseline=base --output-format=json > "$out/$cfg.jsonl"
        echo "::endgroup::"
        results+=("$cfg=$out/$cfg.jsonl")
    done
    uv run --no-project "$root/crates/sas-lexer-perf/report.py" \
        --threshold "{{ perf_threshold }}" --output "$out/summary.md" \
        --title "Performance check: base \`${base_sha:0:10}\` vs head \`$head_desc\`" \
        "${results[@]}"

# Build the Linux image used by the -docker perf recipes (pinned Rust, valgrind, gungraun-runner).
[unix]
perf-docker-image:
    docker build -t {{ perf_image }} \
        --build-arg RUST_VERSION="$(sed -n 's/^channel = "\(.*\)"$/\1/p' rust-toolchain.toml)" \
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
    mise x -- hyperfine -N -w 10 "target/release/sas-lexer lex ${SAS_LEX_SAMPLES:?set SAS_LEX_SAMPLES in .env}"
    mise x -- hyperfine -N -w 5 "target/release/sas-lexer lex ${PERF_LARGE_SAMPLE:?set PERF_LARGE_SAMPLE in .env}"

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
    mise x -- hyperfine -w 10 "$base lex $SAS_LEX_SAMPLES" "target/release/sas-lexer lex $SAS_LEX_SAMPLES"
    mise x -- hyperfine -w 5 "$base lex $PERF_LARGE_SAMPLE" "target/release/sas-lexer lex $PERF_LARGE_SAMPLE"
