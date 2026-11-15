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

# Check declared per-package MSRVs (CI "Check MSRV"): the public crates over the feature
# powerset, then the private Python binding crate, whose floor PyO3 sets (needs a Python 3.11+).
msrv:
    cargo hack check --feature-powerset --locked --rust-version --ignore-private --workspace --all-targets --skip opti_stats
    cargo hack check --locked --rust-version --package sas-lexer-py --all-targets

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

# Tests of the release version and changelog tooling (.github/scripts/release.py, stdlib only).
release-scripts-test:
    uv run --no-project --directory .github/scripts python -m unittest test_release

# All tests: Rust doc/unit/integration, then Python.
test: test-rust test-python

# Run everything CI runs, locally, with the same recipes as the CI jobs (except the
# Linux-only `perf-check` gate; run `just perf-check` or `just perf-docker perf-check` for it).
ci: fmt-check clippy udeps msrv ruff build doc test-rust test-python-ci perf-test release-scripts-test

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
# Release: two trains. rust = crates sas-lexer and sas-lexer-macro (tags rust-v*), py = the PyPI
# package (tags py-v*). A release PR sets the version and dates the changelog; a signed tag on the
# merged commit makes CI publish (release-rust.yml, release-python.yml). Full flow:
# .claude/skills/release/SKILL.md. train: rust|py; version: X.Y.Z or X.Y.Z-(alpha|beta|rc).N
# ---------------------------------------------------------------------------

# Set the train's version, turn the changelog's Unreleased section into a dated entry, update lockfiles.
[unix]
release-prep $train $version:
    uv run --no-project python -I .github/scripts/release.py prep "$train" "$version"
    cargo update --workspace --quiet
    if [ "$train" = py ]; then uv lock --quiet; fi

# Check the train's version sources agree; with a tag, also the tag and the dated changelog entry.
[unix]
release-check $train $tag="":
    @uv run --no-project python -I .github/scripts/release.py check "$train" ${tag:+--tag "$tag"}

# Package and verify the crates as crates.io would (both in one run, so a new macro version is used), without uploading.
[unix]
release-rust-dry-run:
    just _release-macro-bumped >/dev/null
    cargo publish --dry-run --locked -p sas-lexer-macro -p sas-lexer

# Safe to re-run: a version already on crates.io must match this commit.
# CI only (release-rust.yml, needs CARGO_REGISTRY_TOKEN): publish sas-lexer-macro if this release bumps it, then sas-lexer.
[unix]
release-publish-crates:
    if [ "$(just _release-macro-bumped)" = yes ]; then just _release-publish-crate sas-lexer-macro; fi
    just _release-publish-crate sas-lexer

# Maintainer only: sign and push the release tag on the merged release commit; CI then publishes with no further approval.
[confirm("Create and push the signed release tag? CI then publishes it without further approval.")]
[unix]
release-tag $train $sha:
    #!/usr/bin/env bash
    set -euo pipefail
    die() { echo "error: $*" >&2; exit 1; }
    [[ "$sha" =~ ^[0-9a-f]{40}$ ]] || die "pass the full 40-character SHA of the merged release commit"
    [ -z "$(git status --porcelain)" ] || die "the working tree is not clean"
    git fetch origin main --tags
    [ "$(git rev-parse HEAD)" = "$sha" ] || die "HEAD is not $sha: check out the merged release commit"
    git merge-base --is-ancestor "$sha" origin/main || die "$sha is not on origin/main"
    for wf in ci.yml perf.yml; do
        ok="$(gh run list --workflow "$wf" --commit "$sha" --event push --status success --json databaseId --jq length)"
        [ "$ok" -gt 0 ] || die "no successful $wf run on main for $sha"
    done
    version="$(just release-check "$train" | sed -n 's/^version=//p')"
    tag="$([ "$train" = rust ] && echo rust || echo py)-v$version"
    just release-check "$train" "$tag" >/dev/null
    ! git show-ref --quiet --verify "refs/tags/$tag" || die "tag $tag already exists locally"
    ! git ls-remote --exit-code --tags origin "refs/tags/$tag" >/dev/null || die "tag $tag already exists on origin"
    git tag -s -m "release $tag" "$tag" "$sha"
    git cat-file tag "$tag" | grep -q -- '-----BEGIN .* SIGNATURE-----' || die "tag $tag is not signed"
    git push origin "refs/tags/$tag:refs/tags/$tag"
    echo "Pushed $tag ($sha). Watch: gh run list --event push --limit 3"

# Print "yes" if this release bumps sas-lexer-macro (its version differs from the previous rust-v*
# tag), else "no". Fails if the macro changed since that tag without a version bump.
[private]
[unix]
_release-macro-bumped:
    #!/usr/bin/env bash
    set -euo pipefail
    last="$(git describe --tags --abbrev=0 --match 'rust-v*' HEAD^)"
    version="$(just _release-version sas-lexer-macro)"
    # The macro manifest has a literal version, so the first top-level `version =` line is it.
    prev="$(git show "$last:crates/sas-lexer-macro/Cargo.toml" | sed -n 's/^version = "\(.*\)"$/\1/p' | head -n 1)"
    [ -n "$prev" ] || { echo "error: no sas-lexer-macro version found at $last" >&2; exit 1; }
    if [ "$version" != "$prev" ]; then
        echo yes
    elif git diff --quiet "$last" HEAD -- crates/sas-lexer-macro; then
        echo no
    else
        echo "error: crates/sas-lexer-macro changed since $last but is still $version; bump its version and the [workspace.dependencies] requirement" >&2
        exit 1
    fi

# Publish crate@<manifest version>. If crates.io already has that version (a re-run), require its
# checksum to equal this commit's `cargo package` output (reproducible for the same commit and
# toolchain), so an upload from another commit is never taken for this release.
[private]
[unix]
_release-publish-crate crate:
    #!/usr/bin/env bash
    set -euo pipefail
    die() { echo "error: $*" >&2; exit 1; }
    crate="{{ crate }}"
    version="$(just _release-version "$crate")"
    index="$(curl -fsS "https://index.crates.io/${crate:0:2}/${crate:2:2}/$crate")" || die "cannot read the crates.io index for $crate"
    entry="$(grep -F "\"vers\":\"$version\"" <<<"$index" || true)"
    if [ -z "$entry" ]; then
        cargo publish --locked -p "$crate"
        exit 0
    fi
    published="$(grep -oE '"cksum":"[0-9a-f]{64}"' <<<"$entry" | cut -d'"' -f4)"
    [ -n "$published" ] || die "no checksum for $crate $version in the crates.io index"
    cargo package --locked --no-verify --quiet -p "$crate"
    target="$(cargo metadata --no-deps --format-version 1 | grep -oE '"target_directory":"[^"]+"' | cut -d'"' -f4)"
    local_sum="$(shasum -a 256 "$target/package/$crate-$version.crate" | cut -d' ' -f1)"
    [ "$local_sum" = "$published" ] || die "$crate $version is on crates.io (sha256 $published) but this commit packages to $local_sum"
    echo "$crate $version is already on crates.io and matches this commit"

# Print the manifest version of a workspace crate.
[private]
[unix]
_release-version package:
    #!/usr/bin/env bash
    set -euo pipefail
    # From the manifests (not Cargo.lock); --no-deps lists only workspace members.
    meta="$(cargo metadata --no-deps --format-version 1)"
    version="$(grep -oE "\"name\":\"{{ package }}\",\"version\":\"[^\"]+\"" <<<"$meta" | cut -d\" -f8 || true)"
    [ -n "$version" ] || { echo "error: no version found for {{ package }}" >&2; exit 1; }
    echo "$version"
