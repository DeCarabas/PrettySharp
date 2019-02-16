SOURCES=*.c
HEADERS=*.h *.inc

PYTHON?=python3

.PHONY: test clean

prettysharp: $(SOURCES) $(HEADERS)
	$(CC) -std=c99 -o prettysharp -Werror $(SOURCES)

test: prettysharp
	$(PYTHON) ./test.py

clean:
	rm prettysharp

prettysharp-afl: $(SOURCES) $(HEADERS)
	afl-clang -o prettysharp-afl -Werror -O3 $(SOURCES)
