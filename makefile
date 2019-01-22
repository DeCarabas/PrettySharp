SOURCES=main.c csharp.c core.c lexer.c token.c

prettysharp: $(SOURCES)
	clang -o prettysharp -Werror $(SOURCES)
