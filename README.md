# replace-benchmark

Benchmarks for
[__replace-megaparsec__](https://github.com/jamesdbrock/replace-megaparsec)
and
[__replace-attoparsec__](https://github.com/jamesdbrock/replace-attoparsec).

# Usage

To run the benchmarks, clone __replace-megaparsec__ and __replace-attoparsec__
into peer directories and then run the `Makefile`.

```sh
git clone https://github.com/jamesdbrock/replace-benchmark.git
git clone https://github.com/jamesdbrock/replace-megaparsec.git
git clone https://github.com/jamesdbrock/replace-attoparsec.git
cd replace-benchmark
make
```

# Methodology

We want to benchmark to find all of the one-character patterns `x` in a
text stream and replace them with the constant string `oo`, which
could be expressed in regex as `s/x/oo/g`. The replacement
string is longer than the pattern strings so that the optimization of
mutating the input buffer is not available.

We have two benchmark input cases, which we call “dense” and “sparse”.

The “dense” case is one megabyte of alternating spaces and 'x's
like `… x x x x x x x x …`.

The “sparse” case is one megabyte of spaces with a single 'x' in the middle
like `…        x        …`.

Each benchmark program takes the input from `stdin`, performs the replacement,
and writes the result to `stdout`. The time elapsed is measured by `perf stat`.

# Results

In milliseconds, smaller is better.

| Program                          | Input  | dense msec | sparse msec |
| :---                             | :---   |     ---: |     ---: |
| sed¹                             | dense  |  75.33   |  3.63    |
| Python re.sub²                   | dense  |  55.22   |  22.96   |
| Python re.sub function³          | dense  |  90.63   |  23.14   |
| Perl s///⁴                       | dense  |  30.68   |  2.91    |
| replace-megaparsec String        | dense  |  467.46  |  381.37  |
| replace-megaparsec ByteString    | dense  |  646.57  |  445.92  |
| replace-attoparsec ByteString    | dense  |  533.05  |  405.42  |
| replace-megaparsec Text          | dense  |  616.94  |  350.64  |
| replace-attoparsec Text          | dense  |  457.78  |  350.63  |
| [regex-applicative][ra] String   | dense  |  1091.00 |  731.24  |
| [stringsearch][ss] ByteString    | dense  |  116.95  |  2.07    |
| [pcre-heavy][ph] Text            | dense  |  ∞       |  16.44   |

¹ sed (GNU sed) 4.5

² Python 3.7.4

³ Python 3.7.4

⁴ This is perl 5, version 28, subversion 2 (v5.28.2) built for x86_64-linux-thread-multi

[ra]: http://hackage.haskell.org/package/regex-applicative
[ss]: http://hackage.haskell.org/package/stringsearch
[ph]: http://hackage.haskell.org/package/pcre-heavy
