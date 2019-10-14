# To run the benchmarks, just `make`.

OPTIONS=--field-separator , --event=task-clock

.PHONY: run
run: bin/megaparsec-string bin/megaparsec-bytestring bin/megaparsec-text bin/attoparsec-bytestring bin/attoparsec-text bin/regex-applicative-string bin/stringsearch-bytestring bin/pcre-heavy-text input/bench-test-dense.txt input/bench-test-sparse.txt
	@date
	@grep 'PRETTY_NAME' < /etc/os-release
	@grep 'model name' < /proc/cpuinfo | head -1
	@echo -n "sed                      dense  "
	@perf stat ${OPTIONS} sed 's/x/oo/g' < input/bench-test-dense.txt 2>&1 1>/dev/null
	@echo -n "python3                  dense  "
	@perf stat ${OPTIONS} python3 replace-python.py < input/bench-test-dense.txt 2>&1 1>/dev/null
	@echo -n "python3 function         dense  "
	@perf stat ${OPTIONS} python3 replace-python-fn.py < input/bench-test-dense.txt 2>&1 1>/dev/null
	@echo -n "megaparsec-string        dense  "
	@perf stat ${OPTIONS} bin/megaparsec-string < input/bench-test-dense.txt 2>&1 1>/dev/null
	@echo -n "megaparsec-bytestring    dense  "
	@perf stat ${OPTIONS} bin/megaparsec-bytestring < input/bench-test-dense.txt 2>&1 1>/dev/null
	@echo -n "attoparsec-bytestring    dense  "
	@perf stat ${OPTIONS} bin/attoparsec-bytestring < input/bench-test-dense.txt 2>&1 1>/dev/null
	@echo -n "megaparsec-text          dense  "
	@perf stat ${OPTIONS} bin/megaparsec-text < input/bench-test-dense.txt 2>&1 1>/dev/null
	@echo -n "attoparsec-text          dense  "
	@perf stat ${OPTIONS} bin/attoparsec-text < input/bench-test-dense.txt 2>&1 1>/dev/null
	@echo -n "regex-applicative-string dense  "
	@perf stat ${OPTIONS} bin/regex-applicative-string < input/bench-test-dense.txt 2>&1 1>/dev/null
	@echo -n "stringsearch-bytestring  dense  "
	@perf stat ${OPTIONS} bin/stringsearch-bytestring < input/bench-test-dense.txt 2>&1 1>/dev/null
	@echo -n "pcre-heavy-text          dense  "
	@echo "∞" #	@perf stat ${OPTIONS} bin/pcre-heavy-text < input/bench-test-dense.txt 2>&1 1>/dev/null
	@echo -n "perl5                    dense  "
	@perf stat ${OPTIONS} perl -0777 -pe 's/x/oo/g' input/bench-test-dense.txt 2>&1 1>/dev/null
	@echo -n "sed                      sparse "
	@perf stat ${OPTIONS} sed 's/x/oo/g' < input/bench-test-sparse.txt 2>&1 1>/dev/null
	@echo -n "python3                  sparse "
	@perf stat ${OPTIONS} python3 replace-python.py < input/bench-test-sparse.txt 2>&1 1>/dev/null
	@echo -n "python3 function         sparse "
	@perf stat ${OPTIONS} python3 replace-python-fn.py < input/bench-test-sparse.txt 2>&1 1>/dev/null
	@echo -n "megaparsec-string        sparse "
	@perf stat ${OPTIONS} bin/megaparsec-string < input/bench-test-sparse.txt 2>&1 1>/dev/null
	@echo -n "megaparsec-bytestring    sparse "
	@perf stat ${OPTIONS} bin/megaparsec-bytestring < input/bench-test-sparse.txt 2>&1 1>/dev/null
	@echo -n "attoparsec-bytestring    sparse "
	@perf stat ${OPTIONS} bin/attoparsec-bytestring < input/bench-test-sparse.txt 2>&1 1>/dev/null
	@echo -n "megaparsec-text          sparse "
	@perf stat ${OPTIONS} bin/megaparsec-text < input/bench-test-sparse.txt 2>&1 1>/dev/null
	@echo -n "attoparsec-text          sparse "
	@perf stat ${OPTIONS} bin/attoparsec-text < input/bench-test-sparse.txt 2>&1 1>/dev/null
	@echo -n "regex-applicative-string sparse "
	@perf stat ${OPTIONS} bin/regex-applicative-string < input/bench-test-sparse.txt 2>&1 1>/dev/null
	@echo -n "stringsearch-bytestring  sparse "
	@perf stat ${OPTIONS} bin/stringsearch-bytestring < input/bench-test-sparse.txt 2>&1 1>/dev/null
	@echo -n "pcre-heavy-text          sparse "
	@perf stat ${OPTIONS} bin/pcre-heavy-text < input/bench-test-sparse.txt 2>&1 1>/dev/null
	@echo -n "perl5                    sparse "
	@perf stat ${OPTIONS} perl -0777 -pe 's/x/oo/g' input/bench-test-sparse.txt 2>&1 1>/dev/null

bin/%:
	mkdir -p bin
	cabal v2-install --overwrite-policy=always --symlink-bindir=./bin $(@F)

# Make a 1MB dense test file.
input/bench-test-dense.txt:
	mkdir -p input
	for ((i=1;i<=500000;i++)); \
	do \
		echo -n "x " >> input/bench-test-dense.txt; \
	done;

# Make a 1MB sparse test file.
input/bench-test-sparse.txt:
	mkdir -p input
	for ((i=1;i<=500000;i++)); \
	do \
		echo -n " " >> input/bench-test-sparse.txt; \
	done; \
	echo -n "x" >> input/bench-test-sparse.txt; \
	for ((i=1;i<=499999;i++)); \
	do \
		echo -n " " >> input/bench-test-sparse.txt; \
	done

