<!-- sas-lexer-perf-report -->
### ❌ Perf: 4 regressions over 2%

- `lex/real` [default] Instructions +2.47%
- `lex/macros` [default] Est. cycles +3.33%
- `lex/real` [macro_sep] Instructions +2.47%
- `lex/macros` [macro_sep] Est. cycles +3.33%

If this is intended, a repo admin can accept it with the `perf-regression-accepted` label.

|  | Benchmark | Features | Instructions | Est. cycles |
|:-:|---|---|---:|---:|
| 🔴 | `lex/real` | default | **+2.47%** | ±0% |
| 🔴 | `lex/macros` | default | ±0% | **+3.33%** |
| 🟢 | `lex/strings` | default | -10.00% | -4.00% |
| ⚪ | `lex/comments` | default | +1.00% | ±0% |
| 🔴 | `lex/real` | macro_sep | **+2.47%** | ±0% |
| 🔴 | `lex/macros` | macro_sep | ±0% | **+3.33%** |
| 🟢 | `lex/strings` | macro_sep | -10.00% | -4.00% |
| ⚪ | `lex/comments` | macro_sep | +1.00% | ±0% |

<sub>Base `0123456789` → head `fedcba9`. 🔴 a gated metric is ≥ 2% worse, 🟢 ≥ 2% better, ⚪ within ±2%.</sub>

<details>
<summary>Full Callgrind results</summary>

- Base: `0123456789abcdef0123456789abcdef01234567`
- Head: `fedcba9`
- Gate: Instructions (`Ir`) or Est. cycles (`EstimatedCycles`) worse by ≥ 2% fails.
- Tools: valgrind 3.26.0, gungraun 0.20.0, base rustc 1.92.0, head rustc 1.92.0.
- Counts are deterministic for a given binary. Base and head binaries of identical source can still differ by up to ~0.3% (code layout, build paths), so smaller deltas are not meaningful on their own.

| Benchmark | Features | Instructions base | Instructions head | Δ | Est. cycles base | Est. cycles head | Δ |
|---|---|---:|---:|---:|---:|---:|---:|
| `lex/real` | default | 50,000,000 | 51,234,567 | +2.47% | 70,000,000 | 70,000,000 | ±0% |
| `lex/macros` | default | 20,000,000 | 20,000,000 | ±0% | 30,000,000 | 31,000,000 | +3.33% |
| `lex/strings` | default | 10,000,000 | 9,000,000 | -10.00% | 12,500,000 | 12,000,000 | -4.00% |
| `lex/comments` | default | 8,000,000 | 8,080,000 | +1.00% | 9,000,000 | 9,000,000 | ±0% |
| `lex/real` | macro_sep | 50,000,000 | 51,234,567 | +2.47% | 70,000,000 | 70,000,000 | ±0% |
| `lex/macros` | macro_sep | 20,000,000 | 20,000,000 | ±0% | 30,000,000 | 31,000,000 | +3.33% |
| `lex/strings` | macro_sep | 10,000,000 | 9,000,000 | -10.00% | 12,500,000 | 12,000,000 | -4.00% |
| `lex/comments` | macro_sep | 8,000,000 | 8,080,000 | +1.00% | 9,000,000 | 9,000,000 | ±0% |

Other Callgrind metrics (Δ, not gated):

| Benchmark | Features | Dr | Dw | I1mr | DLmw |
|---|---|---:|---:|---:|---:|
| `lex/real` | default | ±0% | +1.67% | +4.17% | 0 → 3 |
| `lex/macros` | default | ±0% | +1.67% | +4.17% | 0 → 3 |
| `lex/strings` | default | ±0% | +1.67% | +4.17% | 0 → 3 |
| `lex/comments` | default | ±0% | +1.67% | +4.17% | 0 → 3 |
| `lex/real` | macro_sep | ±0% | +1.67% | +4.17% | 0 → 3 |
| `lex/macros` | macro_sep | ±0% | +1.67% | +4.17% | 0 → 3 |
| `lex/strings` | macro_sep | ±0% | +1.67% | +4.17% | 0 → 3 |
| `lex/comments` | macro_sep | ±0% | +1.67% | +4.17% | 0 → 3 |

</details>
