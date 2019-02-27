SOURCES=*.c
HEADERS=*.h *.inc

PYTHON?=python3

.PHONY: test clean install

# Warnings taken from http://dotyl.ink/l/i6unii6vgm except
# - No -switch-enum because the token type is just too big.
WARNINGS=\
	-Wall -Wextra -Wcast-qual -Wcast-align -Wstrict-aliasing -Wpointer-arith \
	-Winit-self -Wshadow -Wredundant-decls -Wfloat-equal -Wundef -Wformat=2 \
	-Wvla -Wstrict-prototypes -Wmissing-prototypes


prettysharp: $(SOURCES) $(HEADERS)
	$(CC) -std=c99 -o prettysharp -Werror $(WARNINGS) $(SOURCES)

test: prettysharp
	$(PYTHON) ./test.py

clean:
	rm prettysharp

install: prettysharp
	install ./prettysharp /usr/local/bin

prettysharp-asan: $(SOURCES) $(HEADERS)
	clang  -o prettysharp-asan -O1 -g -fsanitize=address -fno-omit-frame-pointer -Werror $(SOURCES)

prettysharp-afl: $(SOURCES) $(HEADERS)
	afl-clang -o prettysharp-afl -Werror -O3 $(SOURCES)

# This stuff down here is only for windows.
prettysharp.wixobj: prettysharp.wxs
	"C:\Program Files (x86)\WiX Toolset v3.11\bin\candle.exe" prettysharp.wxs

prettysharp.msi: prettysharp.wixobj
	"C:\Program Files (x86)\WiX Toolset v3.11\bin\light.exe" -ext WixUIExtension prettysharp.wixobj
