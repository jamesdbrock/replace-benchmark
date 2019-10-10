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

OPTIONS="--field-separator , --event=task-clock"

echo -n "sed                   "
perf stat ${OPTIONS} sed 's/fnord/bar/g' < bench-test.txt 1> /dev/null
echo -n "megaparsec-string     "
perf stat ${OPTIONS} bin/megaparsec-string < bench-test.txt 1> /dev/null
echo -n "megaparsec-bytestring "
perf stat ${OPTIONS} bin/megaparsec-bytestring < bench-test.txt 1> /dev/null
echo -n "attoparsec-bytestring "
perf stat ${OPTIONS} bin/attoparsec-bytestring < bench-test.txt 1> /dev/null
echo -n "megaparsec-text       "
perf stat ${OPTIONS} bin/megaparsec-text < bench-test.txt 1> /dev/null
echo -n "attoparsec-text       "
perf stat ${OPTIONS} bin/attoparsec-text < bench-test.txt 1> /dev/null
echo -n "python3               "
perf stat ${OPTIONS} python3 replace-python.py < bench-test.txt 1> /dev/null
