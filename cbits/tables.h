#ifndef BASE64_TABLES_H
#define BASE64_TABLES_H

#include <stdint.h>

extern const uint32_t base64_table_dec_32bit_d0[];
extern const uint32_t base64_table_dec_32bit_d1[];
extern const uint32_t base64_table_dec_32bit_d2[];
extern const uint32_t base64_table_dec_32bit_d3[];

extern const uint16_t base64_table_enc_12bit[];

#endif	// BASE64_TABLES_H