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
- 💥 Reworked macro var expression lexing, to properly account for trailing terminating dots. This is a breaking change, as the token types emitted by the lexer have changed. Now instead of one token `MacroVarExpr` for the entire expression, a sequence of `MacroVarResolve`, `MacroString` and optional `MacroVarTerm` are emitted.

### Security
-

## [1.0.0-beta.1] - 2024-11-08

### Added
- Python initial pre-release.

<!-- next-url -->
[Unreleased]: https://github.com/mishamsk/sas-lexer/compare/py-v1.0.0-beta.1...HEAD
[1.0.0-beta.1]: https://github.com/mishamsk/sas-lexer/releases/tag/py-v1.0.0-beta.1
