SOURCES=*.c
HEADERS=*.h *.inc

prettysharp: $(SOURCES) $(HEADERS)
	clang -o prettysharp -Werror $(SOURCES)

clean:
	rm prettysharp
