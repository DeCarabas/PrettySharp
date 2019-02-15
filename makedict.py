#!python3

# prettysharp
# Copyright (C) 2019 John Doty
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

"""Make a dictionary file for afl based on the tokens in tokens.inc."""

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
