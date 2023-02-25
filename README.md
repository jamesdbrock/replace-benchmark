# replace-benchmark

Benchmarks for
[__replace-megaparsec__](https://github.com/jamesdbrock/replace-megaparsec)
and
[__replace-attoparsec__](https://github.com/jamesdbrock/replace-attoparsec).

# Usage

To run the benchmarks,

```
nix run github:jamesdbrock/replace-benchmark
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

The __dense__ case is ten megabytes of alternating spaces and `x`s
like

```
x x x x x x x x x x x x x x x x x x x x x x x x x x x x
```

The __sparse__ case is ten megabytes of spaces with a single `x` in the middle
like

```
                         x
```

Each benchmark program reads the input from `stdin`, replaces `x` with `oo`,
and writes the result to `stdout`. The time elapsed is measured
in milliseconds by `perf stat`,
and the best observed time is recorded.

# Results

In milliseconds. Smaller is better.

## Function replacement

Here is a comparison of replacement methods which can use an arbitrary function
to calculate the replacement string.

| Program                                           | dense *ms*  | sparse *ms* |
| :---                                              |      ---: |     ---:  |
| Python 3.10.9 [`re.sub`](https://docs.python.org/3/library/re.html#re.sub) *repl* function | 558.01 | 36.37 |
| Perl  v5.36.0 [`s///ge`](https://perldoc.perl.org/functions/s.html) function | 1179.40 | 13.20 |
| [`Replace.Megaparsec.streamEdit`](https://hackage.haskell.org/package/replace-megaparsec/docs/Replace-Megaparsec.html#v:streamEdit) `String` | 3301.88 | 3036.15 |
| [`Replace.Megaparsec.streamEdit`](https://hackage.haskell.org/package/replace-megaparsec/docs/Replace-Megaparsec.html#v:streamEdit) `ByteString` | 4020.10 | 720.18 |
| [`Replace.Megaparsec.streamEdit`](https://hackage.haskell.org/package/replace-megaparsec/docs/Replace-Megaparsec.html#v:streamEdit) `Text` | 4157.66 | 907.27 |
| [`Replace.Attoparsec.ByteString.streamEdit`](https://hackage.haskell.org/package/replace-attoparsec/docs/Replace-Attoparsec-ByteString.html#v:streamEdit) | 3234.59 | 180.40 |
| [`Replace.Attoparsec.Text.streamEdit`](https://hackage.haskell.org/package/replace-attoparsec/docs/Replace-Attoparsec-Text.html#v:streamEdit) | 3436.51 | 301.51 |
| [`Text.Regex.Applicative.replace`](http://hackage.haskell.org/package/regex-applicative/docs/Text-Regex-Applicative.html#v:replace) `String` | 13855.78 | 4382.71 |
| [`Text.Regex.PCRE.Heavy.gsub`](http://hackage.haskell.org/package/pcre-heavy/docs/Text-Regex-PCRE-Heavy.html#v:gsub) `Text` | ∞ | 111.89 |
| [`Control.Lens.Regex.ByteString.match`](https://hackage.haskell.org/package/lens-regex-pcre/docs/Control-Lens-Regex-ByteString.html#v:match) | ∞ | 117.37 |


## Constant replacement

For reference, here is a comparison of replacement methods which can only
replace with a constant string or a templated string.

| Program                                    | dense  *ms* | sparse *ms* |
| :---                                       |     ---: |    ---: |
| GNU sed 4.9 | 426.97 | 20.85 |
| Python 3.10.9 [`re.sub`](https://docs.python.org/3/library/re.html#re.sub) *repl* string | 258.23 | 36.12 |
| Perl  v5.36.0 [`s///g`](https://perldoc.perl.org/functions/s.html) | 217.25 | 10.96 |
| [`Data.ByteString.Search.replace`](http://hackage.haskell.org/package/stringsearch/docs/Data-ByteString-Search.html#v:replace) | 811.07 | 10.94
