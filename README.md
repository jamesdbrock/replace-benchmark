# replace-benchmark

Benchmarks for
[__replace-megaparsec__](https://github.com/jamesdbrock/replace-megaparsec)
and
[__replace-attoparsec__](https://github.com/jamesdbrock/replace-attoparsec).

# Usage

To run the benchmarks, clone __replace-megaparsec__ and __replace-attoparsec__
into peer directories and then run the `Makefile`. Requires `sed` and `perl`
and `python` on the `PATH`.

```sh
git clone https://github.com/jamesdbrock/replace-benchmark.git
git clone https://github.com/jamesdbrock/replace-megaparsec.git
git clone https://github.com/jamesdbrock/replace-attoparsec.git
cd replace-benchmark
make
```

# Method

We want to benchmark to find all of the one-character patterns `x` in a
text stream and replace them with the constant string `oo` (`s/x/oo/g`).
The replacement string is longer than the pattern string so that the
optimization of mutating the input buffer is not available.

We have two benchmark input cases, which we call “dense” and “sparse”.

The “dense” case is one megabyte of alternating spaces and `x`s
like

```
x x x x x x x x x x x x x x x x x x x x x x x x x x x x
```

The “sparse” case is one megabyte of spaces with a single `x` in the middle
like

```
                         x
```

Each benchmark program reads the input from `stdin`, replaces `x` with `oo`,
and writes the result to `stdout`. The time elapsed is measured by `perf stat`.

# Results

In milliseconds, smaller is better.

## Function replacement

Here is a comparison of replacement methods which can use an arbitrary function
to calculate the replacement string.

| Program                                           | dense    | sparse   |
| :---                                              |     ---: |     ---: |
| Python `re.sub`¹                                  |  90.63   |  23.14   |
| Perl `s///`²                                      |  117.30  |  4.98    |
| [`Replace.Megaparsec.streamEdit`][m] `String`     |  467.46  |  381.37  |
| [`Replace.Megaparsec.streamEdit`][m] `ByteString` |  646.57  |  445.92  |
| [`Replace.Megaparsec.streamEdit`][m] `Text`       |  616.94  |  350.64  |
| [`Replace.Attoparsec.ByteString.streamEdit`][ab]  |  533.05  |  405.42  |
| [`Replace.Attoparsec.Text.streamEdit`][at]        |  457.78  |  350.63  |
| [`Text.Regex.Applicative.replace`][ra] `String`   |  1091.00 |  731.24  |
| [`Text.Regex.PCRE.Heavy.gsub`][ph] `Text`         |  ⊥³      |  16.44   |

## Constant replacement

For reference, here is a comparison of replacement methods which can only
replace with a constant string or a templated string.

| Program                                 | dense    | sparse   |
| :---                                    |     ---: |     ---: |
| Python `re.sub`¹                        |  55.22   |  22.96   |
| Perl `s///`²                            |  30.68   |  2.91    |
| sed⁴                                    |  75.33   |  3.63    |
| [`Data.ByteString.Search.replace`][ss]  |  116.95  |  2.07    |

¹ Python 3.7.4

² This is perl 5, version 28, subversion 2 (v5.28.2) built for x86_64-linux-thread-multi

³ Does not finish.

⁴ sed (GNU sed) 4.5


[m]: https://hackage.haskell.org/package/replace-megaparsec/docs/Replace-Megaparsec.html#v:streamEdit
[ab]: https://hackage.haskell.org/package/replace-attoparsec/docs/Replace-Attoparsec-ByteString.html#v:streamEdit
[at]: https://hackage.haskell.org/package/replace-attoparsec/docs/Replace-Attoparsec-Text.html#v:streamEdit
[ra]: http://hackage.haskell.org/package/regex-applicative/docs/Text-Regex-Applicative.html#v:replace
[ss]: http://hackage.haskell.org/package/stringsearch/docs/Data-ByteString-Search.html
[ph]: http://hackage.haskell.org/package/pcre-heavy/docs/Text-Regex-PCRE-Heavy.html
