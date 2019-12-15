# replace-benchmark

Benchmarks for
[__replace-megaparsec__](https://github.com/jamesdbrock/replace-megaparsec)
and
[__replace-attoparsec__](https://github.com/jamesdbrock/replace-attoparsec).

# Usage

To run the benchmarks, clone __replace-megaparsec__ and __replace-attoparsec__
into peer directories (see `cabal.project`) and then run the `Makefile`.
Requires `sed` and `perl` and `python` on the `PATH`.

```sh
git clone https://github.com/jamesdbrock/replace-benchmark.git
git clone https://github.com/jamesdbrock/replace-megaparsec.git
git clone https://github.com/jamesdbrock/replace-attoparsec.git
cd replace-benchmark
cabal v2-update
make
```

# Method

These benchmarks are intended to measure the wall-clock speed
of *everything except the actual pattern-matching*. Speed of the
pattern-matching is the responsibility of the
[__megaparsec__](http://hackage.haskell.org/package/megaparsec) and
[__attoparsec__](http://hackage.haskell.org/package/attoparsec)
libraries.

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
and writes the result to `stdout`. The time elapsed is measured by `perf stat`,
and the best observed time is recorded.

# Results

In milliseconds. Smaller is better.

## Function replacement

Here is a comparison of replacement methods which can use an arbitrary function
to calculate the replacement string.

| Program                                           | dense     | sparse   |
| :---                                              |      ---: |     ---: |
| [Python 3.7.4 `re.sub`][sub] *repl* function      | 89.23ms   | 23.98ms  |
| [Perl `s///ge`][s]                                | 180.65ms  | 5.02ms   |
| [`Replace.Megaparsec.streamEdit`][m] `String`     | 441.94ms  | 375.04ms |
| [`Replace.Megaparsec.streamEdit`][m] `ByteString` | 529.99ms  | 73.76ms  |
| [`Replace.Megaparsec.streamEdit`][m] `Text`       | 547.47ms  | 139.21ms |
| [`Replace.Attoparsec.ByteString.streamEdit`][ab]  | 394.12ms  | 41.13ms  |
| [`Replace.Attoparsec.Text.streamEdit`][at]        | 515.26ms  | 46.10ms  |
| [`Text.Regex.Applicative.replace`][ra] `String`   | 1083.98ms | 646.40ms |
| [`Text.Regex.PCRE.Heavy.gsub`][ph] `Text`         | > 10min   | 14.29ms  |
| [`Control.Lens.Regex.ByteString.match`][lb]       | > 10min   | 4.27ms   |
| [`Control.Lens.Regex.Text.match`][lt]             | > 10min   | 14.74ms  |


## Constant replacement

For reference, here is a comparison of replacement methods which can only
replace with a constant string or a templated string.

| Program                                    | dense    | sparse  |
| :---                                       |     ---: |    ---: |
| [Python 3.7.4 `re.sub`][sub] *repl* string | 53.49ms  | 24.39ms |
| [Perl 5 `s///g`][s]                        | 33.06ms  | 4.00ms  |
| GNU sed 4.5                                | 68.20ms  | 3.48ms  |
| [`Data.ByteString.Search.replace`][ss]     | 118.21ms | 2.04ms  |

[sub]: https://docs.python.org/3/library/re.html#re.sub
[s]: https://perldoc.perl.org/functions/s.html
[m]: https://hackage.haskell.org/package/replace-megaparsec/docs/Replace-Megaparsec.html#v:streamEdit
[ab]: https://hackage.haskell.org/package/replace-attoparsec/docs/Replace-Attoparsec-ByteString.html#v:streamEdit
[at]: https://hackage.haskell.org/package/replace-attoparsec/docs/Replace-Attoparsec-Text.html#v:streamEdit
[ra]: http://hackage.haskell.org/package/regex-applicative/docs/Text-Regex-Applicative.html#v:replace
[ss]: http://hackage.haskell.org/package/stringsearch/docs/Data-ByteString-Search.html#v:replace
[ph]: http://hackage.haskell.org/package/pcre-heavy/docs/Text-Regex-PCRE-Heavy.html#v:gsub
[lb]: https://hackage.haskell.org/package/lens-regex-pcre/docs/Control-Lens-Regex-ByteString.html#v:match
[lt]: https://hackage.haskell.org/package/lens-regex-pcre/docs/Control-Lens-Regex-Text.html#v:match
