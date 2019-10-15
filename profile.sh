cabal v2-build --enable-profiling -- attoparsec-text
cabal v2-exec --enable-profiling -- attoparsec-text +RTS -p < input/bench-test-dense.txt 1>/dev/null
cabal v2-build --enable-profiling -- attoparsec-bytestring
cabal v2-exec --enable-profiling -- attoparsec-bytestring +RTS -p < input/bench-test-dense.txt 1>/dev/null
cabal v2-build --enable-profiling -- megaparsec-text
cabal v2-exec --enable-profiling -- megaparsec-text +RTS -p < input/bench-test-dense.txt 1>/dev/null
cabal v2-build --enable-profiling -- megaparsec-bytestring
cabal v2-exec --enable-profiling -- megaparsec-bytestring +RTS -p < input/bench-test-dense.txt 1>/dev/null
cabal v2-build --enable-profiling -- megaparsec-string
cabal v2-exec --enable-profiling -- megaparsec-string +RTS -p < input/bench-test-dense.txt 1>/dev/null
