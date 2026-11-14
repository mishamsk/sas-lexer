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

# Dated nightly for udeps, pinned in rust-toolchain-nightly.toml (CI reads the same file).
nightly := `sed -n 's/^channel = "\(.*\)"$/\1/p' rust-toolchain-nightly.toml`

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
    uv run --only-group dev ruff check src crates/sas-lexer-perf
    uv run --only-group dev ruff format --check src crates/sas-lexer-perf

# Unused dependencies, on the pinned nightly (CI "cargo udeps").
udeps:
    mise x -- cargo +{{ nightly }} udeps --workspace --all-features --release

# Check declared per-package MSRVs (CI "Check MSRV"): the public crates over the feature
# powerset, then the private Python binding crate, whose floor PyO3 sets (needs a Python 3.11+).
msrv:
    mise x -- cargo hack check --feature-powerset --locked --rust-version --ignore-private --workspace --all-targets --skip opti_stats
    mise x -- cargo hack check --locked --rust-version --package sas-lexer-py --all-targets

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

# Run everything CI runs, locally, with the same recipes (perf gate: `just perf-docker perf-check`).
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
# Callgrind counts are deterministic (same-source base/head offsets stay under 0.3%), so the
# gate is tight; an admin can accept an intended regression on a PR (see the README).
perf_threshold := env("PERF_THRESHOLD", "2")
perf_image := "sas-lexer-perf:local"
# Feature configs measured by perf-check and required by perf-gate.
perf_configs := "default macro_sep"

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
    uv run --no-project --directory crates/sas-lexer-perf python -m unittest -v test_report test_sticky_comment

# Regression gate: the lexer at `base` vs the working tree, same harness and inputs. Fails on >= perf_threshold% regressions.
[unix]
perf-check $base="origin/main":
    #!/usr/bin/env bash
    set -euo pipefail
    root="{{ justfile_directory() }}"
    perf="$root/crates/sas-lexer-perf"
    out="$root/target/perf-check"
    configs=({{ perf_configs }})
    # Nothing from the environment (or .env) may filter or reconfigure the gated runs.
    while IFS= read -r var; do unset "$var"; done < <(compgen -e | grep '^GUNGRAUN_' || true)
    # Only the base build below may select the base API adapter (cfg(perf_base)).
    unset SAS_LEXER_PERF_BASE
    # Tool versions (pinned in the perf crate's Cargo.toml): the runner must match the gungraun
    # crate, valgrind the pinned major.minor.
    gungraun="$(sed -n 's/^gungraun = "=\(.*\)"$/\1/p' "$perf/Cargo.toml")"
    want_valgrind="$(sed -n 's/^valgrind = "\(.*\)"$/\1/p' "$perf/Cargo.toml")"
    runner="$(gungraun-runner --version | awk '{print $2}')"
    valgrind="$(valgrind --version | sed 's/^valgrind-//')"
    if [ -z "$gungraun" ] || [ -z "$want_valgrind" ]; then
        echo "cannot read the gungraun or valgrind pin from $perf/Cargo.toml" >&2; exit 2
    fi
    if [ "$runner" != "$gungraun" ]; then
        echo "gungraun-runner $runner does not match the gungraun crate $gungraun" >&2; exit 2
    fi
    case "$valgrind" in
        "$want_valgrind" | "$want_valgrind".*) ;;
        *) echo "valgrind $valgrind, expected $want_valgrind.x (update [package.metadata.perf] with the CI image)" >&2; exit 2 ;;
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
    # Writes results.json (for `just perf-comment`) and prints the report; exit 1 = regression,
    # 2 = INVALID.
    uv run --no-project "$perf/report.py" check \
        --threshold "{{ perf_threshold }}" --results "$out/results.json" --output "$out/comment.md" \
        --expected "$perf/benchmarks.txt" --expect-configs "${configs[@]}" \
        --base-sha "$base_sha" --head "$head_desc" \
        --tools "valgrind $valgrind, gungraun $gungraun, base $(cd "$src" && rustc --version), head $(rustc --version)." \
        "${report_args[@]}"

# Render the perf PR comment from the last `perf-check` (target/perf-check/results.json) to
# target/perf-check/comment.md. Extra args go to `report.py comment`, e.g. `--bypassed-by LOGIN`.
[positional-arguments]
perf-comment *args:
    uv run --no-project crates/sas-lexer-perf/report.py comment target/perf-check/results.json --output target/perf-check/comment.md "$@"

# Arguments: `--rc RC --base-sha SHA [--bypassed-by LOGIN] [--status]`.
# CI gate for the last perf-check: only a validated regression for that base is bypassable.
[positional-arguments]
perf-gate *args:
    uv run --no-project crates/sas-lexer-perf/report.py gate target/perf-check/results.json \
        --expected crates/sas-lexer-perf/benchmarks.txt --expect-configs {{ perf_configs }} "$@"

# Build the Linux image used by the -docker perf recipes (Rust, gungraun-runner and uv at the repo pins).
[unix]
perf-docker-image:
    docker build -t {{ perf_image }} \
        --build-arg RUST_VERSION="$(sed -n 's/^channel = "\(.*\)"$/\1/p' rust-toolchain.toml)" \
        --build-arg GUNGRAUN_VERSION="$(sed -n 's/^gungraun = "=\(.*\)"$/\1/p' crates/sas-lexer-perf/Cargo.toml)" \
        --build-arg UV_VERSION="$(sed -n 's/^uv = "\(.*\)"$/\1/p' mise.toml)" \
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
