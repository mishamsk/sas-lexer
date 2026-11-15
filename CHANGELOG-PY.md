# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- next-header -->

## [Unreleased] - ReleaseDate

### Added
- 🛠️ Support for Python 3.13, 3.14 and 3.15. Wheels are built for CPython 3.11–3.15 on all supported platforms.

### Changed
- 🎉 First stable release: the public API follows Semantic Versioning, so breaking changes only ship in a new major version.
- 🛠️ Python bindings upgraded to PyO3 0.30 and maturin 1.16.
- 🛠️ The minimum msgspec version depends on the Python version (0.18.0 on 3.11, 0.18.3 on 3.12, 0.19.0 on 3.13, 0.20.0 on 3.14, 0.22.0 on 3.15): the first release with wheels for that Python.

### Deprecated
-

### Removed
- 💥 Python 3.10 support (end of life since 2026-10-01). The minimum supported version is Python 3.11. Breaking change: Python 3.10 environments do not install this or later releases.

### Fixed
- 🐛 Fixed a `*` comment statement directly after a `datalines`/`cards`/`lines` block (including the `4` variants) being lexed as code instead of `PredictedCommentStat`, where an apostrophe in the comment started a string literal that swallowed the rest of the program. The bindings build on the `sas-lexer` 1.0.0 crate, which contains the fix. ([#17](https://github.com/mishamsk/sas-lexer/pull/17))
- 🛠️ The source distribution includes the Python binding crate's `Cargo.toml` and the `Cargo.lock`, so it builds and installs from source.

### Security
-

## [1.0.0-beta.2] - 2025-01-05

### Removed
- 🛠️ stop building py package for win32, as msgspec doesn't support it

### Fixed
- 💥 Reworked macro var expression lexing, to properly account for trailing terminating dots. This is a breaking change, as the token types emitted by the lexer have changed. Now instead of one token `MacroVarExpr` for the entire expression, a sequence of `MacroVarResolve`, `MacroString` and optional `MacroVarTerm` are emitted.

## [1.0.0-beta.1] - 2024-11-08

### Added
- Python initial pre-release.

<!-- next-url -->
[Unreleased]: https://github.com/mishamsk/sas-lexer/compare/py-v1.0.0-beta.2...HEAD
[1.0.0-beta.2]: https://github.com/mishamsk/sas-lexer/compare/py-v1.0.0-beta.1...py-v1.0.0-beta.2
[1.0.0-beta.1]: https://github.com/mishamsk/sas-lexer/releases/tag/py-v1.0.0-beta.1
