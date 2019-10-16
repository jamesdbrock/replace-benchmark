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

| Program                                           | dense      | sparse     |
| :---                                              |       ---: |       ---: |
| Python `re.sub`¹                                  |  90.63ms   |  23.14ms   |
| Perl `s///ge`²                                    |  117.30ms  |  4.98ms    |
| [`Replace.Megaparsec.streamEdit`][m] `String`     |  467.46ms  |  381.37ms  |
| [`Replace.Megaparsec.streamEdit`][m] `ByteString` |  646.57ms  |  445.92ms  |
| [`Replace.Megaparsec.streamEdit`][m] `Text`       |  616.94ms  |  350.64ms  |
| [`Replace.Attoparsec.ByteString.streamEdit`][ab]  |  533.05ms  |  405.42ms  |
| [`Replace.Attoparsec.Text.streamEdit`][at]        |  457.78ms  |  350.63ms  |
| [`Text.Regex.Applicative.replace`][ra] `String`   |  1091.00ms |  731.24ms  |
| [`Text.Regex.PCRE.Heavy.gsub`][ph] `Text`         |  ⊥³        |  16.44ms   |

## Constant replacement

For reference, here is a comparison of replacement methods which can only
replace with a constant string or a templated string.

| Program                                 | dense     | sparse  |
| :---                                    |      ---: |    ---: |
| Python `re.sub`¹                        | 54.22ms   | 22.96ms |
| Perl `s///g`²                           | 30.68ms   | 2.91ms  |
| sed⁴                                    | 75.33ms   | 3.63ms  |
| [`Data.ByteString.Search.replace`][ss]  | 116.95ms  | 2.07ms  |

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
