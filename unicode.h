#ifndef UNICODE_H
#define UNICODE_H

#include "common.h"

bool decode_utf8(const char **position, uint32_t *result);
bool is_identifier_start_rune(uint32_t rune);
bool is_identifier_part_rune(uint32_t rune);

#endif
