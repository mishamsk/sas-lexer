# SAS Lexer

[![crates](https://img.shields.io/crates/v/sas-lexer.svg)](https://crates.io/crates/sas-lexer/)
[![pypi](https://img.shields.io/pypi/v/sas-lexer.svg)](https://pypi.org/project/sas-lexer/)
[![license](https://img.shields.io/pypi/l/sas-lexer.svg)](https://github.com/mishamsk/sas-lexer)
[![python](https://img.shields.io/pypi/pyversions/sas-lexer.svg)](https://pypi.org/project/sas-lexer/)

Ultra fast "correct" static context-aware parsing SAS code lexer.

Let me break it down for you:

* **How fast exactly?:** On my MacBook M1 Pro 2021, I get a single-threaded throughput of ~180MB/s. That's about 10 million lines of real-world SAS code in 1-2 seconds! This is despite full Unicode support, context-awareness, and all the quirks of the SAS language.
* **What's the fuss with correctness & context-awareness?:** SAS isn't just context-sensitive; it's environment-sensitive. To get 100% correct lexing for all possible programs, you actually need to execute the code in a SAS session. Yes, you read that right—it's environment-sensitive lexing. No joke. See below for more details.
* **What do you mean by "parsing" lexer?:** The term might be my invention, but due to the unique nature of the SAS language, the lexer has to handle tasks that typically fall under parsing.

## Table of Contents

- [SAS Lexer](#sas-lexer)
    - [Table of Contents](#table-of-contents)
    - [Lexer Features](#lexer-features)
    - [Heuristics, limitations and known deviations from the SAS engine](#heuristics-limitations-and-known-deviations-from-the-sas-engine)
        - [Keyword Token Types](#keyword-token-types)
    - [Getting Started](#getting-started)
        - [Installation](#installation)
        - [Usage (Rust)](#usage-rust)
            - [Crate Features](#crate-features)
        - [Usage (Python)](#usage-python)
    - [Let's talk about SAS](#lets-talk-about-sas)
        - [Integer literals with inline comments](#integer-literals-with-inline-comments)
        - [Invisible null strings in macro expressions](#invisible-null-strings-in-macro-expressions)
        - [Statements inside string expressions and macro call arguments](#statements-inside-string-expressions-and-macro-call-arguments)
        - [Total ambiguity of numeric formats](#total-ambiguity-of-numeric-formats)
        - [Environment-dependent lexing: parentheses after a macro name](#environment-dependent-lexing-parentheses-after-a-macro-name)
        - [Macro call arguments that start with an equals sign](#macro-call-arguments-that-start-with-an-equals-sign)
        - [Context-aware comma masking and inconsistent sister functions](#context-aware-comma-masking-and-inconsistent-sister-functions)
        - [Function-aware lexing of %sysfunc and %syscall](#function-aware-lexing-of-sysfunc-and-syscall)
        - [String literals that are not string literals](#string-literals-that-are-not-string-literals)
        - [Star comments that sometimes disable macro processing](#star-comments-that-sometimes-disable-macro-processing)
    - [Motivation](#motivation)
    - [License](#license)
    - [Contributing](#contributing)
        - [Development](#development)
    - [Acknowledgments](#acknowledgments)

## Lexer Features

- **Correctness**: handles all known SAS language quirks and rarest of edge cases to get as accurate as possible without executing the code. This includes a small amount of heuristics, which should work for 99.99999% of the cases.
- **Unicode Support**: full support for Unicode characters in SAS code.
- **Parses Literals**: supports and parses all numeric and string literals in SAS, including scientific notation, hex and decimal notation. And yes, it supports the [Character Constants in Hexadecimal Notation](https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/lepg/p08kj69i94digfn1sgdhjop08tgh.htm#n01npkx8ef6fmqn196dgyurknxto), thanks for asking!
- **Ridiculously Fast**: leverages cutting-edge techniques for performance. Even python version still clocks in at 1-2 million lines per second on a single thread.
- **Error detection & recovery**: a number of coding errors are detected, reported and sometimes recovered from. See [error.rs](crates/sas-lexer/src/lexer/error.rs) for the full list.
- **Test coverage**: with 2000+ meticulously manually crafted test cases, the lexer has a very high level of confidence in correctness.

Available in two flavors:

- **Rust Crate**: A high-performance Rust crate for efficient SAS language lexing.
- **Python Bindings**: Easy-to-use Python package with bindings for seamless integration with Python projects.

## Heuristics, limitations and known deviations from the SAS engine

The key limitation is that the lexer is static, meaning it does not execute the code. One can produce SAS code that is impossible to statically tokenize the same way SAS scanner would. Hence the need for some heuristics. However, you're unlikely to run into these limitations in practice.

* Lexer supports files up-to 4GB in size. For those of you with 5GB SAS programs, well, I am sorry...
* String expressions and literals in macro text expressions are lexed as in open code, although SAS lexes them as just text, verbatim and later interprets at call site. E.g. `%let v='01jan87'd;` will lex `'01jan87'd` as a `DateLiteral` token instead of `MacroString`.
* Parenthesis following a macro identifier are always assumed to be a part of the macro call as lexer is not environment-aware. See [below](#lets-talk-about-sas) for more details.
* Trailing whitespace is insignificant in macro strings, but is not stripped by the lexer in all contexts. For example, `%mcall(  arg value  )` will have a `MacroString` token with the text `arg value  `.
* Numeric formats are not lexed as a separate token, as they are indistinguishable from numeric literals and/or column references and require context to interpret.
* SAS session skips the entire macro definition (including the body) on pretty much any error. For example, `%macro $bad` will cause whatever follows up-to `%mend` to be skipped. The lexer does not do this, and will try to recover and continue lexing.
* Lexer recovery sometimes goes beyond what SAS engine does. For instance, both SAS and this lexer will recover missing `=` in `%let a 1;` but SAS will not recover missing `)` in `%macro a(a=1;` , while this lexer will.

### Keyword Token Types

SAS has thousands of keywords, and none of them are reserved. All fans of columns named `when`, rejoice, you can finally execute sql that looks like this `select case when when = 42 then then else else end from table`!

Thus the selection of keywords that are lexed as a dedicated token type vs. as an identifier is somewhat arbitrary and based on personal experience of writing parsers for SAS code.

## Getting Started

### Installation

You can add the Rust crate as a dependency via Cargo:

```bash
cargo add sas-lexer
```

For Python, install the package using pip:

```bash
pip install sas-lexer
```

The Python package supports CPython 3.11–3.15. Wheels are published for macOS (x86_64 and arm64), Windows (x86_64) and Linux x86_64 (manylinux and musllinux). On other platforms pip builds the package from the source distribution, which requires a Rust toolchain (via [rustup](https://rustup.rs)).

### Usage (Rust)

```rust
use sas_lexer::{lex_program, LexResult, TokenIdx};

fn main() {
    let source = "data mydata; set mydataset; run;";

    let LexResult { buffer, .. } = lex_program(&source).unwrap();

    let tokens: Vec<TokenIdx> = buffer.iter_tokens().collect();

    for token in tokens {
        println!("{:?}", buffer.get_token_raw_text(token, &source));
    }
}
```

#### Crate Features

* `macro_sep`: Enables a special virtual `MacroSep` token that is emitted between open code and macro statements when there is no "natural" separator, or when semicolon is missing between two macro statements (a coding error). This may be used by a downstream parser as a reliable terminating token for dynamic open code and thus avoid doing lookaheads. Dynamic, means that the statement has a macro statements in it, like `data %if cond %then %do; t1 %end; %else %do; t2 %end;;`
* `serde`: Enables serialization and deserialization of the `ResolvedTokenInfo` struct using the `serde` library. For an example of usage, see the Python bindings crate `sas-lexer-py`.
* `opti_stats`: Enables some additional statistics during lexing, used for performance tuning. Not intended for general use.

### Usage (Python)

```python
from sas_lexer import lex_program_from_str

tokens, errors, str_lit_buf = lex_program_from_str("data mydata; set mydataset; run;")

for token in tokens:
    print(token)
```

## Let's talk about SAS

Whether it is because the Dragon Book had not been published when the language was conceived, or due to the deep and unwavering love of its users, the SAS language allows for almost anything, except perhaps brewing your coffee in the morning. Although, I wouldn't be surprised if that turned out to be another undocumented feature.

If you think I am exaggerating, read on.

### Integer literals with inline comments

SAS cuts comments out of the character stream before it reads tokens, so `1/*hi*/2` is the integer 12, in a DATA step and in macro arithmetic alike. A lexer that produces contiguous tokens can't glue the halves back together: it emits `1`, a comment and `2`, and the parser gets to play Humpty Dumpty.

```sas
/*  v the integer 12 in a DATA step... */
x = 1/*hi*/2;
%put %eval(1/*hi*/2 + 1);
/*         ^^^^^^^^ ...and in macro arithmetic, so this prints 13 */
```

### Invisible null strings in macro expressions

In a macro comparison either operand may be missing, and a missing operand is an empty ("null") string. The only exception is `in`, which SAS rejects with an error when an operand is null. Comparisons chain left to right, so a run of bare operators is a perfectly valid condition made of invisible operands. The lexer emits a zero-width `MacroStringEmpty` token for each of them, so the parser doesn't have to guess where they are.

The fun part is what such conditions evaluate to. `%if ne ne` reads as `("" ne "") ne ""`: the inner comparison is false, giving `0`, and `0 ne ""` is true. Swap the operators and `("" eq "") eq ""` turns into `1 eq ""`, which is false. And `ne ne eq` adds one more step: `1 eq ""` is false again.

```sas
/* v  v  v three invisible empty strings: ("" ne "") ne "" is true */
%if ne ne %then %put yes;
%if eq eq %then %put yes;
/* ^  ^  ^ the same three: ("" eq "") eq "" is false */
```

### Statements inside string expressions and macro call arguments

Macro statements can sit inside a double-quoted string, and inside macro call arguments too. SAS executes such a statement the moment it reads it, removes it from the string, and keeps whatever is left as the string's value. I've seen this bite a real project: a program wrote macro code to a file and then included it, but every `%let` it "wrote" had already run, and the file got what was left of each string. The lexer handles this by switching to statement mode halfway through the string.

The only exception is a string inside another macro statement, like `%put "%let v=1;";`. There SAS reports an open code recursion error and skips the statement. It still disappears from the string, though, so the lexer lexes it and reports the same error.

```sas
data _null_;
  file "setup.sas";
  put "%let env=prod;";
/*     ^^^^^^^^^^^^^^ runs as SAS reads this line and vanishes: setup.sas gets an empty line */
run;
```

### Total ambiguity of numeric formats

A numeric format looks exactly like a number, or like a name followed by a number, and only the surrounding statement tells them apart. So the lexer doesn't try: formats come out as ordinary tokens. Character formats are the exception, because the leading `$` gives them away.

```sas
/*  v          v the same FloatLiteral: a number here, a format there */
x = 8.2; put x 8.2;

/*    v Identifier `dollar10` + FloatLiteral `.2` */
put y dollar10.2 z $char10.;
/*                 ^^^^^^^^ `$` formats get their own CharFormat token */
```

### Environment-dependent lexing: parentheses after a macro name

Whether `(...)` after a macro name holds its arguments depends on how the macro was defined, which a static lexer can't see. If the macro has no parameters, SAS calls it and leaves the parentheses to whatever follows. Calling a macro without parentheses is a leftover from an early version of the macro language, and SAS's unbending commitment to backward compatibility means it supports both forms by tracking at runtime whether each macro was defined with parameters ;-) The lexer can't do that, so it always treats the parentheses as arguments, even across whitespace, comments and newlines.

```sas
%macro ds; sashelp.class %mend;
data b; set %ds (keep=age); run;
/*              ^^^^^^^^^^ SAS: a data set option; lexer: arguments of the %ds call */
```

### Macro call arguments that start with an equals sign

`name=value` is a named argument, unless there is no name before the `=`. Then the `=` is just the first character of a positional value. The name part, in turn, may be built from macro variables at runtime.

```sas
/* v positional value "=x" */
%m(=x, a==b)
/*     ^^^^ name `a`, value "=b" */

/* v a named argument whose name comes from &n */
%m(&n=v)
```

### Context-aware comma masking and inconsistent sister functions

Parentheses inside an argument usually hide (mask) a comma, and some built-ins mask every comma because they take exactly one argument. Which ones? Not the ones you'd guess: `%upcase` masks, its sister `%lowcase` doesn't. And `%sysevalf` ignores parentheses altogether, so the first comma always ends its first argument.

```sas
/*           v one argument "a,b" */
%put %upcase(a,b) %lowcase(a,b);
/*                          ^ two arguments, which is a SAS error */

/*         v parentheses mask the comma... */
%put %scan((a,b),1) %sysevalf((1,);
/*                              ^^ ...except in %sysevalf: this comma ends the argument and `)` closes the call */
```

### Function-aware lexing of %sysfunc and %syscall

`%eval` knows only integers, so `1.5` is plain text there. `%sysfunc` and `%syscall` evaluate the arguments of the called function like `%sysevalf`, but only when that function expects a number. The lexer doesn't keep a catalog of every SAS function signature, so it always lexes the arguments as expressions.

```sas
/*         v text              v a FloatLiteral, evaluated as in %sysevalf */
%put %eval(1.5+1) %sysfunc(sum(1.5,1), 8.2);
/*                                     ^^^ and an optional format at the end */
```

### String literals that are not string literals

A macro comment `%* ...;` ends at the first semicolon, unless that semicolon is inside quotes. Nothing in a comment is a string literal, of course, but the quotes still count. A stray apostrophe in a comment is all it takes.

```sas
/*   v a quote opens here... */
%* it's a trap; %put never runs; ';
/*                               ^ ...and the comment ends only after it closes */
```

### Star comments that sometimes disable macro processing

In open code a `* ...;` comment really is a comment: macro calls inside it don't run, so they can't mask a semicolon. Inside a macro definition it is just text to the macro processor, so any macro statement in it executes. The lexer predicts a comment in open code, and inside macro definitions only when no `%name` macro call or statement comes before the semicolon (a `&var` reference doesn't count).

```sas
* %mcall(doesn't mask ; semi);
/*                    ^ open code: the comment ends here and %mcall never runs */

/*        v a comment */
%macro m; *plain; *%let a=2; %mend;
/*                 ^^^^^^^^^ but inside a macro definition this %let executes */
```

## Motivation

Why build a modern lexer specifically for the SAS language? Mostly for fun! SAS is possibly the most complicated programming language for static parsing in the world. I have worked with it for many years as part of my day job, which eventually included a transpiler from SAS to PySpark. I wanted to see how fast a complex context-aware lexer can theoretically be, and SAS seemed like a perfect candidate for this experiment.

## License

This project is licensed under the [AGPL-3.0](LICENSE). If you are interested in using the lexer for commercial purposes, please reach out to me for further discussion.

## Contributing

We welcome contributions in the form of issues, feature requests, and feedback! However, due to licensing complexities, we are not currently accepting pull requests. Please feel free to open an issue for any proposals or suggestions.

### Development

Prerequisites (base tooling, install them yourself): [just](https://github.com/casey/just), [mise](https://mise.jdx.dev) and [rustup](https://rustup.rs).

- The Rust toolchain is pinned in `rust-toolchain.toml`.
- Python tools (ruff, ty, pytest) are in the pyproject `dev` group (`uv.lock`) and run via `uv run`.
- `mise.toml` pins the development Python, uv and the non-Python CLI tools (prek, typos, cargo-hack, cargo-udeps, hyperfine). Run `mise trust && mise install` once.
- `mise.toml` also exports `UV_PYTHON` for the mise-selected Python, so uv and PyO3 builds (which find `python` on `PATH`) use the same supported interpreter. CI's Python test matrix overrides the dev pin with `MISE_PYTHON_VERSION`.
- Run `just` to list recipes, `just py-init` to set up the environment and `just ci` to run the CI checks locally. Extra flags are forwarded to build and test recipes, e.g. `just build -v` or `just test-python -k comment`.

#### Performance regression guard

Every PR runs `.github/workflows/perf.yml`. It builds the lexer at the PR base and at the PR head in the same job and runs the [gungraun](https://github.com/gungraun/gungraun) (Valgrind/Callgrind) benchmarks from `crates/sas-lexer-perf` against both. The benchmarks lex a real-world corpus plus deterministic synthetic inputs (macro, strings, comments, datalines, numerics), with default features and with `macro_sep`. Instruction counts and estimated cycles are deterministic, so runner noise doesn't affect them. The check fails if any benchmark gets ≥ 2% worse in either metric (same-source base/head runs differ by under 0.3%). It also fails, as INVALID, if any expected measurement is missing. The report goes to the job summary and to one sticky PR comment that is updated in place: a verdict line, a per-benchmark table (🔴 regression, 🟢 improvement, ⚪ within ±2%), and the full Callgrind numbers, SHAs and tool versions in a collapsed section. The gated benchmark list is `crates/sas-lexer-perf/benchmarks.txt`.

- `just perf-check <base-ref>` runs the same check locally: it compares `<base-ref>` with the working tree. The base is built in a throwaway git worktree with this branch's benchmark harness. It needs Linux, the Valgrind major.minor pinned in `[package.metadata.perf]` of `crates/sas-lexer-perf/Cargo.toml` (the apt version on the `ubuntu-26.04` CI runner) and `gungraun-runner` (same version as the `gungraun` dev-dependency). uv comes from `mise.toml`, like for the other recipes. `PERF_THRESHOLD=5 just perf-check ...` sets a different threshold.
- On macOS, `just perf-docker perf-check <base-ref>` runs it in a Linux container (needs Docker).
- `just perf-comment` renders the PR comment from the last `perf-check` into `target/perf-check/comment.md`, to preview it locally.
- **Accepting an intended regression (repo admins only):** add the `perf-regression-accepted` label to the PR. Label changes re-run the check. The bypass only applies when the comparison completed and found a regression: `just perf-gate` checks that perf-check's exit code and this run's `results.json` for exactly this base agree. INVALID results, missing results or any other failure always fail. CI then verifies through the API that the label's latest application was by a user with admin permission. If so, the check passes with a warning and the comment shows ⚠️ and who accepted it. A label added by a non-admin is ignored. The label stays valid for later pushes to the PR; remove it to enforce the gate again. A push to main inherits it only when the push is exactly that PR's merge commit, compared with its first parent. Wider pushes and manual runs get no bypass.
- The sticky comment is only updated by a run for the PR's current head commit, so re-running an older commit can't replace a newer report.
- `just perf-test` checks the benchmark inputs, the gate logic and the comment poster. `just perf-bench` runs the Callgrind benchmarks alone. `just perf-profile`, `just perf-cur` and `just perf-compare` are wall-clock tools for local tuning.
- PRs that change the lexer API used by the benchmarks must adapt `sas_lexer_perf::api`, which has to compile against both the base and the head. The old call goes under `#[cfg(perf_base)]` and the new one under `#[cfg(not(perf_base))]`. Only `perf-check` sets that cfg, for the base build (`SAS_LEXER_PERF_BASE=1`). It isn't a Cargo feature, so the `--all-features` workspace checks (clippy, udeps) still compile the head side. Drop the old branch once the change is on main. The module docs have the details.

## Acknowledgments

- The lexer is inspired by the the [Carbon language parser](https://github.com/carbon-language/carbon-lang), particularly as described in the talk "Modernizing Compiler Design for Carbon Toolchain" by Chandler Carruth at CppNow 2023. You can find the talk [here](https://www.youtube.com/watch?v=ZI198eFghJk).
- Cargo benchmark and an end-2-end test use SAS code from the [SAS Enlighten Apply GitHub repository](https://github.com/sassoftware/enlighten-apply), which is licensed under Apache-2.0. The code is included in `crates/sas-lexer-perf/corpus` without modifications.
- The Python package utilizes the amazing [msgspec library](https://github.com/jcrist/msgspec) for (de)serialization, which is licensed under BSD-3-Clause.
