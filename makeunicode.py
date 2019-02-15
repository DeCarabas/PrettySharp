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

"""A helper script to create the unicode classification functions found in
unicode.c based on the official public unicode data.
"""
import urllib.request

UNICODE_URL = "https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt"

letter_character = {"Lu", "Ll", "Lt", "Lm", "Lo", "Nl"}
combining_character = {"Mn", "Mc"}
connecting_character = {"Pc"}
decimal_digit_character = {"Nd"}
formatting_character = {"Cf"}

id_start_categories = letter_character
id_start_ranges = []

id_part_categories = (
    letter_character
    | decimal_digit_character
    | connecting_character
    | combining_character
    | formatting_character
)
id_part_ranges = []


def add_range(id_range, code):
    if len(id_range) == 0:
        id_range.append((code, code))
    else:
        last_range = id_range[-1]
        if code == last_range[1] + 1:
            id_range[-1] = (last_range[0], code)
        else:
            id_range.append((code, code))


text = urllib.request.urlopen(UNICODE_URL).read().decode("utf-8")
for line in text.splitlines():
    parts = line.split(";")

    code = int(parts[0], 16)
    name = parts[1]
    category = parts[2]
    if category in id_start_categories:
        add_range(id_start_ranges, code)
    if category in id_part_categories:
        add_range(id_part_ranges, code)

print("bool is_identifier_start_rune(uint32_t rune) {")
print("  // Any code point in {}, or '_'".format(", ".join(id_start_categories)))
print(
    "  return rune == '_' ||\n    {};".format(
        " ||\n    ".join(
            ["(rune >= {} && rune <= {})".format(r[0], r[1]) for r in id_start_ranges]
        )
    )
)
print("}")
print()

print("bool is_identifier_part_rune(uint32_t rune) {")
print("  // Any code point in {}".format(", ".join(id_part_categories)))
print(
    "  return {};".format(
        " ||\n    ".join(
            ["(rune >= {} && rune <= {})".format(r[0], r[1]) for r in id_part_ranges]
        )
    )
)
print("}")


# for r in id_start_ranges:
#     print("[{}, {}]".format(hex(r[0]), hex(r[1])))
