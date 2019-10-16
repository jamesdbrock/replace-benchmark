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

The benchmark task is to find all of the one-character patterns `x` in a
text stream and replace them by a function which returns the constant
string `oo`. So, like the regex `s/x/oo/g`.

We have two benchmark input cases, which we call __dense__ and __sparse__.

The __dense__ case is one megabyte of alternating spaces and `x`s
like

```
x x x x x x x x x x x x x x x x x x x x x x x x x x x x
```

The __sparse__ case is one megabyte of spaces with a single `x` in the middle
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

| Program                                           | dense     | sparse   |
| :---                                              |      ---: |     ---: |
| Python `re.sub`¹                                  | 89.23ms   | 23.98ms  |
| Perl `s///ge`²                                    | 180.65ms  | 5.60ms   |
| [`Replace.Megaparsec.streamEdit`][m] `String`     | 454.95ms  | 375.04ms |
| [`Replace.Megaparsec.streamEdit`][m] `ByteString` | 611.98ms  | 433.26ms |
| [`Replace.Megaparsec.streamEdit`][m] `Text`       | 592.66ms  | 353.32ms |
| [`Replace.Attoparsec.ByteString.streamEdit`][ab]  | 537.57ms  | 407.33ms |
| [`Replace.Attoparsec.Text.streamEdit`][at]        | 549.62ms  | 280.96ms |
| [`Text.Regex.Applicative.replace`][ra] `String`   | 1083.98ms | 646.40ms |
| [`Text.Regex.PCRE.Heavy.gsub`][ph] `Text`         | ⊥³        | 14.76ms  |

## Constant replacement

For reference, here is a comparison of replacement methods which can only
replace with a constant string or a templated string.

| Program                                 | dense    | sparse  |
| :---                                    |     ---: |    ---: |
| Python `re.sub`¹                        | 53.49ms  | 24.39ms |
| Perl `s///g`²                           | 33.06ms  | 4.00ms  |
| sed⁴                                    | 68.20ms  | 3.53ms  |
| [`Data.ByteString.Search.replace`][ss]  | 118.21ms | 2.10ms  |

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
