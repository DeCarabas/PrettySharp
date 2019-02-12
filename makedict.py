#!python3
"""Make a dictionary file for afl.
"""

print(
    """
#
# AFL Dictionary for C#
# ---------------------
#
# All the tokens from the prettysharp lexer.
#
""".strip()
)

ignore = {
    "ERROR",
    "EOF",
    "IDENTIFIER",
    "NONE",
    "INTERPOLATED_STRING",
    "INTERPOLATION_END",
    "INTERPOLATION_FORMAT",
}

with open("token.inc", "r") as token_file:
    lines = token_file.read().splitlines()
    for line in lines:
        if line.startswith("TKN("):
            parts = line[len("TKN(") : -1].split(",")
            token_id = parts[0].strip()
            text = parts[1].strip()

            if "TRIVIA" in token_id:
                continue
            if token_id in ignore:
                continue

            print("{}={}".format(token_id, text))
