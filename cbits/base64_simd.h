/* Copyright (c) 2026, Emily Pillmore. BSD-3-Clause; see LICENSE. */

#ifndef HS_BASE64_SIMD_H
#define HS_BASE64_SIMD_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Process a prefix only; the Haskell implementation handles the remaining
 * bytes, padding, and error reporting. Source and destination must not overlap.
 * Source loads remain inside len; destination stores cover only the produced
 * prefix. Alphabet/table pointers must identify the complete tables below.
 *
 * Encode returns a multiple of 3 and writes exactly (result / 3) * 4 bytes.
 * Alphabet points to the existing 64-byte standard or URL alphabet.
 *
 * Decode returns a multiple of 4, leaves at least the final 4 source bytes,
 * and writes exactly (result / 4) * 3 bytes. A vector block containing an
 * invalid byte or padding is left completely untouched. Table points to the
 * existing 256-byte decoding table: table['-'] == 62 selects URL decoding.
 *
 * Unsupported targets return zero without reading or writing any buffers.
 */
size_t hs_base64_simd_encode(const uint8_t *src, size_t len, uint8_t *dst,
                            const uint8_t *alphabet);
size_t hs_base64_simd_decode(const uint8_t *src, size_t len, uint8_t *dst,
                            const uint8_t *table);

/*
 * 0: scalar fallback; 1: x86-64 SSSE3; 2: ARM64 NEON; 3: x86-64 AVX2.
 * x86 features are detected at runtime; AVX2 is preferred when supported.
 */
int hs_base64_simd_backend(void);

#ifdef __cplusplus
}
#endif

#endif /* HS_BASE64_SIMD_H */
