# Agent Operating Manual

Guidance for agents working in `sas-lexer`. The `justfile` is the command authority: CI runs the same recipes.

## Repository Intent
- A fast, faithful SAS lexer: Rust core plus a Python binding. Two top priorities: accuracy against real SAS semantics, and performance. Reject work that trades either for convenience.
- Keep changes narrowly scoped and follow existing patterns.
- The project does not accept outside contributions. Never adopt code from external PRs, forks or issue-attached patches; reproduce reported bugs and fix them with our own code.

## Architecture
- `crates/sas-lexer`: the core lexer (public, crates.io). Features: `macro_sep` (macro-aware separators), `serde`, `opti_stats`. Lexer snapshots (insta) live in `crates/sas-lexer/src/lexer/tests/snapshots`; Python snapshots in `tests/snapshots`.
- `crates/sas-lexer-macro`: internal proc macro for the core (public, versioned separately).
- `crates/sas-lexer-cli`: private CLI. `crates/sas-lexer-perf`: private Callgrind perf harness.
- `crates/sas-lexer-py`: private PyO3 binding, built with maturin. Python sources are in `src/sas_lexer`, tests in `tests/`. Supported Python: 3.11-3.15 (`requires-python`, the CI test matrix and the wheel matrix agree).
- The binding's `build.rs` regenerates `token_type.py`, `token_channel.py` and `error_kind.py` from the Rust enums. Never edit them by hand; build and commit the result.
- The binding depends on the **published** `sas-lexer` crate from crates.io (both `[dependencies]` and `[build-dependencies]`), not the workspace source. A lexer fix reaches Python only after a Rust release and a bump of both dependency entries in `crates/sas-lexer-py/Cargo.toml`.

## Tooling
- Prerequisites: just, mise and rustup. `mise.toml` pins the dev Python (3.15), uv and the non-Python CLIs (prek, typos, cargo-hack, cargo-udeps, hyperfine); run `mise trust && mise install` once. CI's Python test matrix overrides the dev pin with `MISE_PYTHON_VERSION`.
- Work through the just recipes (`just` lists them). Non-Python CLIs run from mise (`mise x -- ...`), including prek; ruff, ty and pytest come from the pyproject dev group via `uv run`.
- Python only through uv (`uv run`, `uv sync`); never bare `pip`/`python`. Do not hand-edit `uv.lock`; change `pyproject.toml` and run `uv lock`.
- `just test-python-ci` tests the non-editable build: `uv sync --no-editable`, then `uv run --no-sync pytest`. Plain `uv run` would re-sync an editable install.
- Before handing off run `just fmt`, `just lint`, `just clippy` and `just test`.
- Create new worktrees with `gt`; they land in `.worktrees/<name>`, which gt keeps out of git.
- If the sandbox blocks a command (network, keychain/TLS, tool caches such as `~/.cargo`, `~/.rustup`, the uv or mise cache), re-run it outside the sandbox. Never work around it by redirecting tool cache/home dirs (`CARGO_HOME`, `RUSTUP_HOME`, `CARGO_TARGET_DIR`, `UV_CACHE_DIR`, `XDG_CACHE_HOME`, ...) or similar hacks.

## Rust
- The stable toolchain (1.96.1) is pinned by `rust-toolchain.toml`. The dated nightly for udeps and the nightly CI job is pinned by `rust-toolchain-nightly.toml`, which the justfile and CI read (rustup does not apply it). Use only these; never install other toolchains.
- The public crates keep the workspace `rust-version`; private crates declare their own. MSRV (the public crates over the feature powerset, plus the Python binding) is checked only in CI. Never run `just msrv` (or `just ci`, which includes it) locally.
- Every crate inherits the workspace lints (`[lints] workspace = true`). Allow a lint locally with a `reason`, as the CLI does for print lints in its `main.rs`; do not opt a crate out.
- Avoid new dependencies unless they deliver measurable benefit; `just udeps` must stay green.
- Dead code is not allowed, including `pub` items that became unused (the compiler does not flag those). Never use `#[allow(dead_code)]`; delete the code.

## Performance Gate
- CI's `perf check (callgrind)` job compares the base and head lexer by Callgrind instruction counts and estimated cycles, for the default and `macro_sep` configs, and fails when either is 2% or more worse. The only bypass is the `perf-regression-accepted` label applied by a repo admin.
- Hot-path changes must keep the gate green: fix a regression rather than ask for the label.
- Locally: `just perf-check <base>` on Linux (needs valgrind and `gungraun-runner`, see the README), or `just perf-docker perf-check <base>` on macOS. Read `target/perf-check/comment.md` for per-benchmark deltas. With `perf-docker`, `target/` is the `sas-lexer-perf-target` Docker volume, so read it there: `docker run --rm -v sas-lexer-perf-target:/t sas-lexer-perf:local cat /t/perf-check/comment.md`.
- If you change the lexer API used by the benchmarks, adapt only `sas_lexer_perf::api` in `crates/sas-lexer-perf/src/lib.rs`, following its module docs (`#[cfg(perf_base)]` keeps the old call, `#[cfg(not(perf_base))]` the new one; drop the old branch in a follow-up PR).

## Testing
- Product code: black-box tests through stable public boundaries (the lexer API, the Python `lex_program_from_str`), data-driven with snapshots and sample files. No mocks of internals. Review every snapshot diff; never bulk-accept.
- CI and tooling scripts: a few lean happy-path tests only.
- Never use `from __future__ import annotations` (ruff enforces this).
- Behavior changes update docs, the "Unreleased" section of `CHANGELOG-RUST.md` / `CHANGELOG-PY.md`, and snapshots.

## Writing
- Code comments, commit messages and PR text are professional and evergreen: state what the code does and why, on technical grounds. No history ("now", "previously", "was changed to"), no people, review rounds or internal coordination.

## Safety
- Never merge PRs, push to `main`, or touch GitHub issues/PRs unless asked.

## Releases
- Follow the project release skill in `.claude/skills/`.
