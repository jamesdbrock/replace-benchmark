#! /usr/bin/env bash

# Make a 1MB test file.
if [ ! -f bench-test.txt ]; then
    for ((i=1;i<=100000;i++))
    do
        echo "       foo" >> bench-test.txt
    done
fi

cabal v2-build megaparsec-string
cabal v2-build megaparsec-bytestring
cabal v2-build megaparsec-text

# diff <(sed 's/foo/bar/g' < bench-test.txt) <(cabal v2-run bench-string < bench-test.txt)
# diff <(sed 's/foo/bar/g' < bench-test.txt) <(cabal v2-run bench-bytestring < bench-test.txt)

# OPTIONS="--metrics task-clock:u"
OPTIONS="--metric-only"

perf stat ${OPTIONS} sed 's/foo/bar/g' < bench-test.txt 1> /dev/null
perf stat ${OPTIONS} cabal v2-run megaparsec-string < bench-test.txt 1> /dev/null
perf stat ${OPTIONS} cabal v2-run megaparsec-bytestring < bench-test.txt 1> /dev/null
perf stat ${OPTIONS} cabal v2-run megaparsec-text < bench-test.txt 1> /dev/null

