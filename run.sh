#! /usr/bin/env bash

mkdir -p bin

cabal v2-install megaparsec-string megaparsec-bytestring megaparsec-text attoparsec-bytestring attoparsec-text --symlink-bindir=./bin

# Make a 1MB dense test file.
if [ ! -f bench-test-dense.txt ]; then
    for ((i=1;i<=500000;i++))
    do
        echo -n "x " >> bench-test-dense.txt
    done
fi

# Make a 1MB sparse test file.
if [ ! -f bench-test-sparse.txt ]; then
    for ((i=1;i<=500000;i++))
    do
        echo -n " " >> bench-test-sparse.txt
    done
    echo -n "x" >> bench-test-sparse.txt
    for ((i=1;i<=499999;i++))
    do
        echo -n " " >> bench-test-sparse.txt
    done
fi

OPTIONS="--field-separator , --event=task-clock"

echo -n "sed                   dense "
perf stat ${OPTIONS} sed 's/x/oo/g' < bench-test-dense.txt 1> /dev/null
echo -n "python3               dense "
perf stat ${OPTIONS} python3 replace-python.py < bench-test-dense.txt 1> /dev/null
echo -n "megaparsec-string     dense "
perf stat ${OPTIONS} bin/megaparsec-string < bench-test-dense.txt 1> /dev/null
echo -n "megaparsec-bytestring dense "
perf stat ${OPTIONS} bin/megaparsec-bytestring < bench-test-dense.txt 1> /dev/null
echo -n "attoparsec-bytestring dense "
perf stat ${OPTIONS} bin/attoparsec-bytestring < bench-test-dense.txt 1> /dev/null
echo -n "megaparsec-text       dense "
perf stat ${OPTIONS} bin/megaparsec-text < bench-test-dense.txt 1> /dev/null
echo -n "attoparsec-text       dense "
perf stat ${OPTIONS} bin/attoparsec-text < bench-test-dense.txt 1> /dev/null

echo -n "sed                   sparse "
perf stat ${OPTIONS} sed 's/x/oo/g' < bench-test-sparse.txt 1> /dev/null
echo -n "python3               sparse "
perf stat ${OPTIONS} python3 replace-python.py < bench-test-sparse.txt 1> /dev/null
echo -n "megaparsec-string     sparse "
perf stat ${OPTIONS} bin/megaparsec-string < bench-test-sparse.txt 1> /dev/null
echo -n "megaparsec-bytestring sparse "
perf stat ${OPTIONS} bin/megaparsec-bytestring < bench-test-sparse.txt 1> /dev/null
echo -n "attoparsec-bytestring sparse "
perf stat ${OPTIONS} bin/attoparsec-bytestring < bench-test-sparse.txt 1> /dev/null
echo -n "megaparsec-text       sparse "
perf stat ${OPTIONS} bin/megaparsec-text < bench-test-sparse.txt 1> /dev/null
echo -n "attoparsec-text       sparse "
perf stat ${OPTIONS} bin/attoparsec-text < bench-test-sparse.txt 1> /dev/null
