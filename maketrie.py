#!env python3

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

"""A dumb little script to automate the KW matching trie for the lexer."""

from collections import defaultdict

kw_map = {}
with open("token.inc", "r") as token_file:
    lines = token_file.read().splitlines()
    for line in lines:
        if line.startswith("TKN("):
            parts = line[len("TKN(") : -1].split(",")
            token_id = parts[0].strip()
            text = parts[1].strip()
            if token_id.startswith("KW_"):
                kw_map[text[1:-1]] = "TOKEN_" + token_id


def indent(lines, count=2):
    return [(" " * count) + line for line in lines]


class TrieNode(object):
    def __init__(self):
        self.children = None
        self.key = None
        self.value = None

    def insert(self, key, value):
        if self.key is None and self.children is None:
            self.key = key
            self.value = value
        else:
            self.ensureInterior()
            self.insertChild(key, value)

    def ensureInterior(self):
        if self.children is None:
            self.children = defaultdict(TrieNode)
            if len(self.key) > 0:
                self.insertChild(self.key, self.value)
                self.key = None
                self.value = None

    def insertChild(self, key, value):
        if len(key) == 0:
            assert self.key is None
            self.key = key
            self.value = value
        else:
            self.children[key[0]].insert(key[1:], value)

    def dump(self):
        lines = []
        if self.key is not None:
            lines.append("'{}' == {}".format(self.key, self.value))
        if self.children is not None:
            keys = list(sorted(self.children.keys()))
            for key in keys:
                value = self.children[key]
                lines.append("{} ->".format(key))
                lines.extend(indent(value.dump()))
        return lines

    def dump_c(self, prefix_length):
        assert self.children is not None or self.key is not None

        lines = []
        if self.children is not None:
            lines.append("if (len > {}) {{".format(prefix_length))
            lines.append("  switch(start[{}]) {{".format(prefix_length))
            keys = list(sorted(self.children.keys()))
            for key in keys:
                value = self.children[key]
                lines.append("  case '{}':".format(key))
                child_lines = value.dump_c(prefix_length + 1)
                lines.extend(indent(child_lines, 4))
                if not child_lines[-1].startswith("return"):
                    lines.append("    break;")
            lines.append("  }")
            if self.key is None:
                lines.append("}")
            else:
                lines.append("} else {")

        if self.key is not None:
            return_lines = []
            if len(self.key) == 0:
                return_lines.append("if (len == {}) {{".format(prefix_length))
                return_lines.append("  return {};".format(self.value))
                return_lines.append("}")
            else:
                return_lines.append(
                    'return check_kw(start, len, {}, {}, "{}", {});'.format(
                        prefix_length, len(self.key), self.key, self.value
                    )
                )

            if self.children is None:
                lines.extend(return_lines)
            else:
                lines.extend(indent(return_lines))
                lines.append("}")

        return lines


root = TrieNode()
for text, token in kw_map.items():
    root.insert(text, token)

lines = []
lines.append("enum TokenType keyword_type(const char *start, size_t len) {")
lines.extend(["  " + line for line in root.dump_c(0)])
lines.append("  return TOKEN_IDENTIFIER;")
lines.append("}")

print("\n".join(lines))
