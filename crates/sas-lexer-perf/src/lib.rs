//! Inputs for the sas-lexer performance regression benchmarks.
//!
//! Everything here is deterministic: the real-world corpus is a checked-in zip and
//! the synthetic inputs come from a fixed-seed generator. The base and head builds
//! compared by `just perf-check` always use this crate from HEAD, so both sides lex
//! byte-identical inputs.
//!
//! Each synthetic input stresses one area of the lexer, so a regression in that area
//! is not diluted by the rest of the corpus.
//!
//! The benchmarks call the lexer only through [`api`], so a PR that changes the
//! lexer API has a single place to adapt (see the module docs).

use std::fmt::Write as _;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

use zip::ZipArchive;

/// The lexer under test. The `macro_sep` feature of this crate enables the same lexer feature.
pub use sas_lexer;

/// The only calls into the lexer that the benchmarks make (the timed workload).
///
/// `just perf-check` builds this crate from HEAD against both the base and the head
/// lexer, so this module must compile against both. The base build always enables the
/// `perf-base` feature. When a PR changes the lexer API used here:
///
/// 1. keep the old call under `#[cfg(feature = "perf-base")]` and add the new call under
///    `#[cfg(not(feature = "perf-base"))]`, doing the same work on the same input;
/// 2. drop the `perf-base` branch in a follow-up PR once the change is on main.
///
/// A brand-new entry point with no base equivalent isn't comparable: benchmark it in a
/// follow-up PR once it exists on main, rather than skipping the gate.
pub mod api {
    use crate::sas_lexer::lex_program;

    /// Lexes `source`; returns the token plus error count.
    ///
    /// # Panics
    /// Panics if the source can't be lexed at all (larger than 4 GB).
    #[must_use]
    pub fn lex(source: &str) -> usize {
        let result = lex_program(&source).expect("lexing failed");
        result.buffer.token_count() as usize + result.errors.len()
    }

    /// Lexes `source` and resolves every token, as the Python binding does.
    ///
    /// # Panics
    /// Panics if the source can't be lexed at all (larger than 4 GB).
    #[must_use]
    pub fn lex_resolve(source: &str) -> usize {
        let result = lex_program(&source).expect("lexing failed");
        result.buffer.into_resolved_token_vec().len()
    }
}

/// Approximate size of each synthetic input.
const SYNTHETIC_TARGET_BYTES: usize = 128 * 1024;

/// Real SAS code from the SAS Enlighten Apply repository (Apache-2.0).
const ENLIGHTEN_ZIP: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/corpus/enlighten-apply-code-only.zip"
);

/// Concatenates all valid UTF-8 `.sas` files from a zip archive, in archive order.
///
/// # Errors
/// Returns an error if the archive can't be opened or read.
pub fn read_sas_files_from_zip<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let mut archive = ZipArchive::new(File::open(path)?)?;
    let mut combined = String::new();

    for i in 0..archive.len() {
        let mut file = archive.by_index(i)?;
        if Path::new(file.name())
            .extension()
            .is_some_and(|ext| ext.eq_ignore_ascii_case("sas"))
        {
            let mut contents = Vec::new();
            file.read_to_end(&mut contents)?;
            if let Ok(contents) = String::from_utf8(contents) {
                combined.push_str(&contents);
                // Keep file boundaries: some files lack a trailing newline.
                if !combined.ends_with('\n') {
                    combined.push('\n');
                }
            }
        }
    }

    Ok(combined)
}

/// The real-world corpus (~250 KB of SAS code).
///
/// # Panics
/// Panics if the checked-in corpus is missing or unreadable.
#[must_use]
pub fn real_corpus() -> String {
    read_sas_files_from_zip(ENLIGHTEN_ZIP).expect("failed to read the enlighten corpus zip")
}

/// Small deterministic PRNG (xorshift64*), so inputs never depend on external crates.
struct Rng(u64);

impl Rng {
    fn new(seed: u64) -> Self {
        Self(seed | 1)
    }

    fn next(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        self.0 = x;
        x.wrapping_mul(0x2545_F491_4F6C_DD1D)
    }

    /// Uniform-ish value in `0..n`.
    fn below(&mut self, n: usize) -> usize {
        // n is tiny, modulo bias is irrelevant here.
        usize::try_from(self.next() % (n as u64)).unwrap_or_default()
    }

    fn pick<'a>(&mut self, items: &[&'a str]) -> &'a str {
        items
            .get(self.below(items.len()))
            .copied()
            .unwrap_or_default()
    }

    fn ident(&mut self) -> String {
        const STEMS: &[&str] = &[
            "amount", "cust_id", "region", "score", "dt", "flag", "total", "rate", "name", "grp",
            "x", "y", "_tmp", "balance", "segment",
        ];
        let stem = self.pick(STEMS);
        match self.below(3) {
            0 => stem.to_string(),
            _ => format!("{stem}{}", self.below(100)),
        }
    }
}

fn generate(seed: u64, mut chunk: impl FnMut(&mut Rng, &mut String)) -> String {
    let mut rng = Rng::new(seed);
    let mut out = String::with_capacity(SYNTHETIC_TARGET_BYTES + 4096);
    while out.len() < SYNTHETIC_TARGET_BYTES {
        chunk(&mut rng, &mut out);
    }
    out
}

/// Macro-heavy code: definitions, `%let`, `%if/%do`, `%sysfunc`, macro calls,
/// `&var.`/`&&var&i` references and quoting functions.
#[must_use]
pub fn synthetic_macro() -> String {
    generate(0x00C0_FFEE, |r, s| {
        let m = r.ident();
        let v = r.ident();
        let w = r.ident();
        let n = r.below(50) + 1;
        let _ = write!(
            s,
            "%macro m_{m}(ds=, var={v}, n={n}, opt=%str(a, b));\n\
             %local i j_{w};\n\
             %let j_{w} = %sysfunc(countw(&var., %str( )));\n\
             %do i = 1 %to &n.;\n\
             \x20 %let {w}&i = %eval(&i * {n} + %length(&ds));\n\
             \x20 %if &&{w}&i > {n} and %upcase(&opt) ne %str(B) %then %do;\n\
             \x20   %put NOTE: &=i &&{w}&i %nrstr(%not_a_call) &sysdate9.;\n\
             \x20 %end;\n\
             \x20 %else %if %sysevalf(&i / 2 = %sysfunc(floor(&i / 2))) %then %let {v}_&i = even;\n\
             %end;\n\
             data &ds._out; set &ds.(keep=&var. {v}_%sysfunc(min(&n, 3))); run;\n\
             %mend m_{m};\n\
             %m_{m}(ds=work.{w}, var=&{v}, n=%eval({n} + 1))\n\
             %let {v} = %scan(&{w}, {k}, %str(,));\n",
            k = r.below(5) + 1,
        );
    })
}

/// String literals of all kinds: single/double quoted, doubled quotes, macro
/// references and calls inside double quotes, name/date/hex literals.
#[must_use]
pub fn synthetic_strings() -> String {
    const WORDS: &[&str] = &[
        "alpha",
        "beta beta",
        "gamma, delta",
        "O''Neil",
        "say \"\"hi\"\"",
        "x = 1;",
        "/* no */",
        "  padded  ",
        "UPPER",
        "tab\there",
    ];
    generate(0x5EED_0002, |r, s| {
        let a = r.ident();
        let b = r.ident();
        let w1 = r.pick(WORDS).replace('"', "");
        let w2 = r.pick(WORDS).replace('\'', "");
        let _ = write!(
            s,
            "data _null_;\n\
             \x20 length {a} $200;\n\
             \x20 {a} = '{w1}' || \"{w2}\";\n\
             \x20 {b} = \"Report for &{a}. as of %sysfunc(today(), date9.) - &&{b}_{i}\";\n\
             \x20 if {a} = 'It''s here' then {b} = \"He said \"\"&{a}\"\"\";\n\
             \x20 dt = '{day:02}JAN20{yy:02}'d; tm = '{h:02}:30:00't; dtm = '01FEB2021:10:00:00'dt;\n\
             \x20 hex = '{h:02}0A'x; 'my var {i}'n = cats('{w1}', \"{w2}\", '&not_macro');\n\
             \x20 put \"{w2}\" {a}= / 'done';\n\
             run;\n\
             title \"Title %upcase(&{b}) - {w2}\";\n",
            i = r.below(20),
            day = r.below(28) + 1,
            yy = r.below(30),
            h = r.below(24),
        );
    })
}

/// Comments: block comments, statement comments and macro comments mixed with code.
#[must_use]
pub fn synthetic_comments() -> String {
    const TEXT: &[&str] = &[
        "TODO: revisit this logic before release",
        "Author: someone; Date: 2020-01-01",
        "this step merges the monthly extracts",
        "do not change; downstream depends on the order",
        "quote ' inside a comment",
        "macro &ref and %call(1) inside a comment",
    ];
    generate(0x5EED_0003, |r, s| {
        let t1 = r.pick(TEXT);
        let t2 = r.pick(TEXT);
        let t3 = r.pick(TEXT);
        let a = r.ident();
        let _ = write!(
            s,
            "/*******************************************************\n\
             \x20* {t1}\n\
             \x20* {t2}\n\
             \x20*******************************************************/\n\
             * {t3};\n\
             data {a}; /* inline {t1} */ set src; /* {t2} */ run;\n\
             %* macro comment: {t3};\n\
             *----- {t2} -----;\n\
             proc sort data={a}; by {a} /* key */; run; * trailing {t1};\n",
        );
    })
}

/// Data steps with `datalines`/`cards` and `datalines4`/`cards4` blocks.
#[must_use]
pub fn synthetic_datalines() -> String {
    generate(0x5EED_0004, |r, s| {
        let a = r.ident();
        let rows = r.below(30) + 10;
        let four = r.below(2) == 0;
        let _ = writeln!(
            s,
            "data {a};\n  infile datalines dsd truncover;\n  input id name :$20. amount date :date9.;\n  {}",
            if four { "datalines4;" } else { "datalines;" }
        );
        for i in 0..rows {
            let _ = writeln!(
                s,
                "{i},{name}_{n},{amt}.{c:02},{d:02}JAN20{y:02}",
                name = r.ident(),
                n = r.below(1000),
                amt = r.below(100_000),
                c = r.below(100),
                d = r.below(28) + 1,
                y = r.below(30),
            );
        }
        s.push_str(if four { ";;;;\nrun;\n" } else { ";\nrun;\n" });
    })
}

/// Numeric literals in expressions: integers, decimals, exponents, hex and missing values.
#[must_use]
pub fn synthetic_numerics() -> String {
    generate(0x5EED_0005, |r, s| {
        let a = r.ident();
        let b = r.ident();
        let _ = write!(
            s,
            "data _null_;\n\
             \x20 {a} = {i1} + {d1}.{d2} * {m}e{e} - .{d3} / {big};\n\
             \x20 {b} = 0{h:X}x + {i2}E-{e} + 1.{d1}e+{e} - {i1}.;\n\
             \x20 if {a} > {d2}.5 and {b} ne . then {a} = ._ ; else {b} = .A;\n\
             \x20 array arr{{{n}}} _temporary_ ({i1} {i2} {d1}.{d3} {m}e{e} -{i2});\n\
             \x20 do k = 1 to {n} by {step}; arr{{k}} = arr{{k}} * {d1}.{d3}; end;\n\
             run;\n",
            i1 = r.below(1_000_000),
            i2 = r.below(10_000),
            d1 = r.below(1000),
            d2 = r.below(100),
            d3 = r.below(100_000),
            m = r.below(9) + 1,
            e = r.below(30),
            big = 1_000_000_000_u64 + r.next() % 1_000_000_000,
            h = r.below(0xFFFF) + 0xA000,
            n = r.below(20) + 5,
            step = r.below(3) + 1,
        );
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use sas_lexer::lex_program;

    // `lex/*` benchmark inputs that must stay large and error-free.

    #[test]
    fn synthetic_inputs_are_valid_sas() {
        for (name, source) in [
            ("macro", synthetic_macro()),
            ("strings", synthetic_strings()),
            ("comments", synthetic_comments()),
            ("datalines", synthetic_datalines()),
            ("numerics", synthetic_numerics()),
        ] {
            assert!(source.len() >= SYNTHETIC_TARGET_BYTES, "{name} too small");
            let result = lex_program(&source).expect("lexing failed");
            assert!(result.errors.is_empty(), "{name} produced lexer errors");
        }
    }

    #[test]
    fn inputs_are_deterministic() {
        assert_eq!(synthetic_macro(), synthetic_macro());
        assert_eq!(synthetic_numerics(), synthetic_numerics());
    }

    #[test]
    fn real_corpus_loads() {
        assert!(real_corpus().len() > 200 * 1024);
    }
}
