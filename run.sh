#! /usr/bin/env bash

# Make a 1MB test file.
if [ ! -f bench-test.txt ]; then
    for ((i=1;i<=100000;i++))
    do
        echo "       fnord" >> bench-test.txt
    done
fi

mkdir -p bin

cabal v2-install megaparsec-string megaparsec-bytestring megaparsec-text attoparsec-bytestring attoparsec-text --symlink-bindir=./bin

# diff <(sed 's/foo/bar/g' < bench-test.txt) <(cabal v2-run bench-string < bench-test.txt)
# diff <(sed 's/foo/bar/g' < bench-test.txt) <(cabal v2-run bench-bytestring < bench-test.txt)

OPTIONS="--metric-only"

perf stat ${OPTIONS} sed 's/fnord/bar/g' < bench-test.txt 1> /dev/null
perf stat ${OPTIONS} bin/megaparsec-string < bench-test.txt 1> /dev/null
perf stat ${OPTIONS} bin/megaparsec-bytestring < bench-test.txt 1> /dev/null
perf stat ${OPTIONS} bin/attoparsec-bytestring < bench-test.txt 1> /dev/null
perf stat ${OPTIONS} bin/megaparsec-text < bench-test.txt 1> /dev/null
perf stat ${OPTIONS} bin/attoparsec-text < bench-test.txt 1> /dev/null
perf stat ${OPTIONS} python3 replace-python.py < bench-test.txt 1> /dev/null
