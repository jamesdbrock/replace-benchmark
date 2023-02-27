# replace-benchmark

Benchmarks for
[__replace-megaparsec__](https://github.com/jamesdbrock/replace-megaparsec)
and
[__replace-attoparsec__](https://github.com/jamesdbrock/replace-attoparsec).

# Usage

The `cabal.project` file is configured to look for the __replace-megaparsec__
and __replace-attoparsec__ packages in peer directories `../`. This is
for convenience when tuning those packages.

To run the benchmarks,

```
nix run
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

| Python 3.10.9 [`re.sub`](https://docs.python.org/3/library/re.html#re.sub) *repl* function | 561.89 | 35.74 |
| Perl  v5.36.0 [`s///ge`](https://perldoc.perl.org/functions/s.html) function | 1228.62 | 12.85 |
| [`Replace.Megaparsec.streamEdit`](https://hackage.haskell.org/package/replace-megaparsec/docs/Replace-Megaparsec.html#v:streamEdit) `String` | 3267.20 | 2883.93 |
| [`Replace.Megaparsec.streamEdit`](https://hackage.haskell.org/package/replace-megaparsec/docs/Replace-Megaparsec.html#v:streamEdit) `ByteString` | 4064.84 | 756.71 |
| [`Replace.Megaparsec.streamEdit`](https://hackage.haskell.org/package/replace-megaparsec/docs/Replace-Megaparsec.html#v:streamEdit) `Text` | 4119.75 | 906.57 |
| [`Replace.Attoparsec.ByteString.streamEdit`](https://hackage.haskell.org/package/replace-attoparsec/docs/Replace-Attoparsec-ByteString.html#v:streamEdit) | 3226.64 | 178.86 |
| [`Replace.Attoparsec.Text.streamEdit`](https://hackage.haskell.org/package/replace-attoparsec/docs/Replace-Attoparsec-Text.html#v:streamEdit) | 3405.49 | 312.29 |
| [`Replace.Attoparsec.Text.Lazy.streamEdit`](https://hackage.haskell.org/package/replace-attoparsec/docs/Replace-Attoparsec-Text-Lazy.html#v:streamEdit) | 3107.39 | 242.20 |
| [`Text.Regex.Applicative.replace`](http://hackage.haskell.org/package/regex-applicative/docs/Text-Regex-Applicative.html#v:replace) `String` | 13820.86 | 4364.59 |
| [`Text.Regex.PCRE.Heavy.gsub`](http://hackage.haskell.org/package/pcre-heavy/docs/Text-Regex-PCRE-Heavy.html#v:gsub) `Text` | ∞ | 113.66 |
| [`Control.Lens.Regex.ByteString.match`](https://hackage.haskell.org/package/lens-regex-pcre/docs/Control-Lens-Regex-ByteString.html#v:match) | ∞ | 115.43 |
| [`Control.Lens.Regex.Text.match`](https://hackage.haskell.org/package/lens-regex-pcre/docs/Control-Lens-Regex-Text.html#v:match) | ∞ | 35.65 |

## Constant replacement

For reference, here is a comparison of replacement methods which can only
replace with a constant string or a templated string.

| Program                                    | dense  *ms* | sparse *ms* |
| :---                                       |     ---: |    ---: |
| GNU sed 4.9 | 424.46 | 20.57 |
| Python 3.10.9 [`re.sub`](https://docs.python.org/3/library/re.html#re.sub) *repl* string | 261.83 | 35.42 |
| Perl  v5.36.0 [`s///g`](https://perldoc.perl.org/functions/s.html) | 217.59 | 11.07 |
| [`Data.ByteString.Search.replace`](http://hackage.haskell.org/package/stringsearch/docs/Data-ByteString-Search.html#v:replace) | 788.17 | 11.00

