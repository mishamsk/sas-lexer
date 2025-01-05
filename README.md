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
    - [Motivation](#motivation)
    - [License](#license)
    - [Contributing](#contributing)
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

tokens, errors, str_lit_buf = lex_program_from_str(
    "data mydata; set mydataset; run;"
)

for token in tokens:
    print(token)
```

## Let's talk about SAS

Whether it is because the Dragon Book had not been published when the language was conceived, or due to the deep and unwavering love of its users, the SAS language allows for almost anything, except perhaps brewing your coffee in the morning. Although, I wouldn't be surprised if that turned out to be another undocumented feature.

If you think I am exaggerating, read on.

THIS SECTION IS WIP. PLANNED CONTENT:

* Integer literals with inline comments
* Fun with macro mnemonics and "null" strings in expressions
* Statements inside macro/function call arguments, string expressions and comments
* Total ambiguity of numeric formats
* Environment-dependent lexing: parenthesis following macro identifier
* Macro call arguments starting with `=`
* Context-aware masking of ',' in macro call arguments and discrepancies between sister functions
* `%sysfunc/%syscall` function aware lexing
* String literals that "hide" semicolon from macro but are not string literals
* Star comments that sometimes disable macro processing and sometimes not

## Motivation

Why build a modern lexer specifically for the SAS language? Mostly for fun! SAS is possibly the most complicated programming language for static parsing in the world. I have worked with it for many years as part of my day job, which eventually included a transpiler from SAS to PySpark. I wanted to see how fast a complex context-aware lexer can theoretically be, and SAS seemed like a perfect candidate for this experiment.

## License

This project is licensed under the [AGPL-3.0](LICENSE). If you are interested in using the lexer for commercial purposes, please reach out to me for further discussion.

## Contributing

We welcome contributions in the form of issues, feature requests, and feedback! However, due to licensing complexities, we are not currently accepting pull requests. Please feel free to open an issue for any proposals or suggestions.

## Acknowledgments

- The lexer is inspired by the the [Carbon language parser](https://github.com/carbon-language/carbon-lang), particularly as described in the talk "Modernizing Compiler Design for Carbon Toolchain" by Chandler Carruth at CppNow 2023. You can find the talk [here](https://www.youtube.com/watch?v=ZI198eFghJk).
- Cargo benchmark and an end-2-end test use SAS code from the [SAS Enlighten Apply GitHub repository](https://github.com/sassoftware/enlighten-apply), which is licensed under Apache-2.0. The code is included in the `tests` directory without modifications.
- The Python package utilizes the amazing [msgspec library](https://github.com/jcrist/msgspec) for (de)serialization, which is licensed under BSD-3-Clause.
