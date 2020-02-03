#ifndef BASE64_TABLES_H
#define BASE64_TABLES_H

#include <stdint.h>

extern const uint32_t base64_table_dec_32bit_std_d0[];
extern const uint32_t base64_table_dec_32bit_std_d1[];
extern const uint32_t base64_table_dec_32bit_std_d2[];
extern const uint32_t base64_table_dec_32bit_std_d3[];

extern const uint32_t base64_table_dec_32bit_url_d0[];
extern const uint32_t base64_table_dec_32bit_url_d1[];
extern const uint32_t base64_table_dec_32bit_url_d2[];
extern const uint32_t base64_table_dec_32bit_url_d3[];

extern const uint16_t base64_table_enc_12bit_std[];
extern const uint16_t base64_table_enc_12bit_url[];

#endif	// BASE64_TABLES_H
