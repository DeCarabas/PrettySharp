#!python3
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
