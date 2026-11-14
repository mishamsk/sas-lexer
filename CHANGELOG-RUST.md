# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- next-header -->

## [Unreleased] - ReleaseDate

### Added
-

### Changed
-

### Deprecated
-

### Removed
-

### Fixed
- 🐛 A `*` comment statement directly after a `datalines`/`cards`/`lines` block (including the `4` variants) is now lexed as `PredictedCommentStat`. Previously it was lexed as code, and an apostrophe in it could start a string literal that swallowed the rest of the program.

### Security
-

## [1.0.0-beta.4] - 2025-01-05

### Added
- ✨ Added two public iterator API's on `TokenizedBuffer`:
  - `iter_tokens` - returns an iterator over token indexes
  - `iter_tokens_infos` - returns an iterator over token infos
    - It uses the new public type: `TokenInfoIter`
- ✨ New buffer API to get fully resolved token text: `get_token_resolved_text` instead of manually checking for string payload

### Changed
- ✨ Made `TokenInfo` public with read-only getters

### Removed
- 🔥 remove IntoIterator impl for TokenizedBuffer

### Fixed
- 💥 Reworked macro var expression lexing, to properly account for trailing terminating dots. This is a breaking change, as the token types emitted by the lexer have changed. Now instead of one token `MacroVarExpr` for the entire expression, a sequence of `MacroVarResolve`, `MacroString` and optional `MacroVarTerm` are emitted.

## [1.0.0-beta.3] - 2024-11-08

### Changed
- Distinguish built-in macro calls and autocall macros that mask comma in arguments from those that don't.

## [1.0.0-beta.2] - 2024-11-07

### Changed
- 📌 Reduce MSRV to 1.74 to allow musl maturin builds

## [1.0.0-beta.1] - 2024-11-05

### Added
- Rust initial pre-release.

<!-- next-url -->
[Unreleased]: https://github.com/mishamsk/sas-lexer/compare/rust-v1.0.0-beta.4...HEAD
[1.0.0-beta.4]: https://github.com/mishamsk/sas-lexer/compare/rust-v1.0.0-beta.3...rust-v1.0.0-beta.4
[1.0.0-beta.3]: https://github.com/mishamsk/sas-lexer/compare/rust-v1.0.0-beta.2...rust-v1.0.0-beta.3
[1.0.0-beta.2]: https://github.com/mishamsk/sas-lexer/compare/rust-v1.0.0-beta.1...rust-v1.0.0-beta.2
[1.0.0-beta.1]: https://github.com/mishamsk/sas-lexer/releases/tag/rust-v1.0.0-beta.1
