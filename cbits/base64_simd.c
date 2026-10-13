/* Copyright (c) 2026, Emily Pillmore. BSD-3-Clause; see LICENSE. */

#include <string.h>

#include "base64_simd.h"

/*
 * These are original prefix kernels. No external SIMD library is required.
 * Target-specific instructions are confined to their implementation below;
 * x86 builds need no global ISA flags, and dispatch has no mutable state.
 * HS_BASE64_SIMD_FORCE_SCALAR exercises the unsupported-target fallback.
 * HS_BASE64_SIMD_FORCE_SSSE3 caps x86 dispatch for testing older processors.
 */
#if !defined(HS_BASE64_SIMD_FORCE_SCALAR)
#if defined(__aarch64__) && defined(__ARM_NEON) && \
    (defined(__GNUC__) || defined(__clang__))
#define HS_BASE64_NEON 1
#include <arm_neon.h>
#elif defined(__x86_64__) && (defined(__GNUC__) || defined(__clang__))
#define HS_BASE64_SSSE3 1
#include <immintrin.h>
#define HS_BASE64_TARGET_SSSE3 __attribute__((target("ssse3")))
#if !defined(HS_BASE64_SIMD_FORCE_SSSE3)
#define HS_BASE64_AVX2 1
#define HS_BASE64_TARGET_AVX2 __attribute__((target("avx2")))
#endif
#endif
#endif

#if defined(HS_BASE64_NEON) || defined(HS_BASE64_SSSE3)
/*
 * Nibble-table classification, following the lookup approach used by Klomp
 * and simdutf. These masks are derived directly from our two alphabets:
 * bit h in valid_lo[l] says that byte (h << 4) | l is a valid character.
 * valid_hi[h] selects that bit for h = 2..7, and is zero otherwise.
 * Their intersection is nonzero exactly for an alphabet character, including
 * neither padding nor bytes >= 128. Rows are standard Base64, then Base64url.
 */
static const uint8_t hs_base64_valid_lo[2][16] = {
    {0xa8, 0xf8, 0xf8, 0xf8, 0xf8, 0xf8, 0xf8, 0xf8,
     0xf8, 0xf8, 0xf0, 0x54, 0x50, 0x50, 0x50, 0x54},
    {0xa8, 0xf8, 0xf8, 0xf8, 0xf8, 0xf8, 0xf8, 0xf8,
     0xf8, 0xf8, 0xf0, 0x50, 0x50, 0x54, 0x50, 0x70}
};
static const uint8_t hs_base64_valid_hi[16] = {
    0, 0, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0, 0, 0, 0, 0, 0, 0, 0
};

/*
 * Add these offsets modulo 256 to ASCII to obtain its sextet. The only
 * exception in each alphabet is its last character: '/' needs -3 after the
 * '+' offset; '_' needs +33 after the uppercase offset. Invalid lanes are
 * checked separately, so their translated values are never stored.
 */
static const uint8_t hs_base64_offsets[2][16] = {
    {0, 0, 62 - '+', 52 - '0', 256 - 'A', 256 - 'A',
     256 + 26 - 'a', 256 + 26 - 'a', 0, 0, 0, 0, 0, 0, 0, 0},
    {0, 0, 62 - '-', 52 - '0', 256 - 'A', 256 - 'A',
     256 + 26 - 'a', 256 + 26 - 'a', 0, 0, 0, 0, 0, 0, 0, 0}
};
#endif

#if defined(HS_BASE64_NEON)

static size_t hs_base64_neon_encode(const uint8_t *src, size_t len, uint8_t *dst,
                                   const uint8_t *alphabet)
{
    size_t consumed = 0;
    uint8x16x4_t lookup;
    const uint8x16_t mask = vdupq_n_u8(63);

    if (len < 48) {
        return 0;
    }
    lookup.val[0] = vld1q_u8(alphabet);
    lookup.val[1] = vld1q_u8(alphabet + 16);
    lookup.val[2] = vld1q_u8(alphabet + 32);
    lookup.val[3] = vld1q_u8(alphabet + 48);

    while (len - consumed >= 48) {
        /* Deinterleave sixteen triples without reading beyond their 48 bytes a la Klomp */
        const uint8x16x3_t input = vld3q_u8(src + consumed);
        uint8x16x4_t output;
        const uint8x16_t a = vshrq_n_u8(input.val[0], 2);
        const uint8x16_t b = vandq_u8(mask, vorrq_u8(vshlq_n_u8(input.val[0], 4),
                                                  vshrq_n_u8(input.val[1], 4)));
        const uint8x16_t c = vandq_u8(mask, vorrq_u8(vshlq_n_u8(input.val[1], 2),
                                                  vshrq_n_u8(input.val[2], 6)));
        const uint8x16_t d = vandq_u8(mask, input.val[2]);
        output.val[0] = vqtbl4q_u8(lookup, a);
        output.val[1] = vqtbl4q_u8(lookup, b);
        output.val[2] = vqtbl4q_u8(lookup, c);
        output.val[3] = vqtbl4q_u8(lookup, d);
        vst4q_u8(dst, output);
        consumed += 48;
        dst += 64;
    }
    return consumed;
}

static inline uint8x16_t hs_base64_neon_sextets(uint8x16_t input,
    uint8x16_t valid_lo, uint8x16_t valid_hi, uint8x16_t offsets,
    uint8x16_t last_char, uint8x16_t last_adjustment, uint8x16_t *valid)
{
    const uint8x16_t lo = vandq_u8(input, vdupq_n_u8(15));
    const uint8x16_t hi = vshrq_n_u8(input, 4);
    const uint8x16_t adjustment = vandq_u8(vceqq_u8(input, last_char), last_adjustment);

    *valid = vandq_u8(vqtbl1q_u8(valid_lo, lo), vqtbl1q_u8(valid_hi, hi));
    return vaddq_u8(input, vaddq_u8(vqtbl1q_u8(offsets, hi), adjustment));
}

static size_t hs_base64_neon_decode(const uint8_t *src, size_t len, uint8_t *dst,
                                   const uint8_t *table)
{
    size_t consumed = 0;

    if (len < 68) {
        return 0;
    }
    const int is_url = table['-'] == 62;
    const uint8x16_t valid_lo = vld1q_u8(hs_base64_valid_lo[is_url]);
    const uint8x16_t valid_hi = vld1q_u8(hs_base64_valid_hi);
    const uint8x16_t offsets = vld1q_u8(hs_base64_offsets[is_url]);
    const uint8x16_t last_char = vdupq_n_u8(is_url ? '_' : '/');
    const uint8x16_t last_adjustment = vdupq_n_u8((uint8_t)(is_url ? 33 : -3));

    /* Always reserve the last quartet for Haskell's padding/canonical checks not ala Klomp */
    while (len - consumed >= 68) {
        const uint8x16x4_t input = vld4q_u8(src + consumed);
        uint8x16_t valid_a, valid_b, valid_c, valid_d;
        const uint8x16_t a = hs_base64_neon_sextets(input.val[0], valid_lo, valid_hi,
            offsets, last_char, last_adjustment, &valid_a);
        const uint8x16_t b = hs_base64_neon_sextets(input.val[1], valid_lo, valid_hi,
            offsets, last_char, last_adjustment, &valid_b);
        const uint8x16_t c = hs_base64_neon_sextets(input.val[2], valid_lo, valid_hi,
            offsets, last_char, last_adjustment, &valid_c);
        const uint8x16_t d = hs_base64_neon_sextets(input.val[3], valid_lo, valid_hi,
            offsets, last_char, last_adjustment, &valid_d);
        const uint8x16_t valid = vminq_u8(vminq_u8(valid_a, valid_b),
                                        vminq_u8(valid_c, valid_d));
        uint8x16x3_t output;

        if (vminvq_u8(valid) == 0) {
            break;
        }
        output.val[0] = vorrq_u8(vshlq_n_u8(a, 2), vshrq_n_u8(b, 4));
        output.val[1] = vorrq_u8(vshlq_n_u8(b, 4), vshrq_n_u8(c, 2));
        output.val[2] = vorrq_u8(vshlq_n_u8(c, 6), d);
        vst3q_u8(dst, output);
        consumed += 64;
        dst += 48;
    }
    return consumed;
}

#elif defined(HS_BASE64_SSSE3)

HS_BASE64_TARGET_SSSE3
static size_t hs_base64_ssse3_encode(const uint8_t *src, size_t len, uint8_t *dst,
                                    const uint8_t *alphabet)
{
    size_t consumed = 0;
    __m128i lookup[4];
    const __m128i gather = _mm_setr_epi8(2, 1, 0, -1, 5, 4, 3, -1,
                                       8, 7, 6, -1, 11, 10, 9, -1);

    if (len < 12) {
        return 0;
    }
    for (size_t i = 0; i < 4; i++) {
        lookup[i] = _mm_loadu_si128((const __m128i *)(alphabet + i * 16));
    }

    while (len - consumed >= 12) {
        __m128i input = _mm_setzero_si128();
        __m128i indices;
        __m128i high;
        __m128i output = _mm_setzero_si128();

        /* memcpy expresses a 12-byte load, including at the buffer boundary. */
        memcpy(&input, src + consumed, 12);
        input = _mm_shuffle_epi8(input, gather);
        /* Each 32-bit lane is now a big-endian 24-bit input triple. */
        indices = _mm_and_si128(_mm_srli_epi32(input, 18), _mm_set1_epi32(0x0000003f));
        indices = _mm_or_si128(indices,
            _mm_and_si128(_mm_srli_epi32(input, 4), _mm_set1_epi32(0x00003f00)));
        indices = _mm_or_si128(indices,
            _mm_and_si128(_mm_slli_epi32(input, 10), _mm_set1_epi32(0x003f0000)));
        indices = _mm_or_si128(indices,
            _mm_and_si128(_mm_slli_epi32(input, 24), _mm_set1_epi32(0x3f000000)));
        high = _mm_and_si128(indices, _mm_set1_epi8(0x30));
        for (size_t i = 0; i < 4; i++) {
            const __m128i matches = _mm_cmpeq_epi8(high, _mm_set1_epi8((char)(i * 16)));
            output = _mm_or_si128(output,
                _mm_and_si128(matches, _mm_shuffle_epi8(lookup[i], indices)));
        }
        _mm_storeu_si128((__m128i *)dst, output);
        consumed += 12;
        dst += 16;
    }
    return consumed;
}

HS_BASE64_TARGET_SSSE3
static size_t hs_base64_ssse3_decode(const uint8_t *src, size_t len, uint8_t *dst,
                                    const uint8_t *table)
{
    size_t consumed = 0;
    const __m128i scatter = _mm_setr_epi8(2, 1, 0, 6, 5, 4, 10, 9,
                                        8, 14, 13, 12, -1, -1, -1, -1);

    if (len < 20) {
        return 0;
    }
    const int is_url = table['-'] == 62;
    const __m128i valid_lo = _mm_loadu_si128((const __m128i *)hs_base64_valid_lo[is_url]);
    const __m128i valid_hi = _mm_loadu_si128((const __m128i *)hs_base64_valid_hi);
    const __m128i offsets = _mm_loadu_si128((const __m128i *)hs_base64_offsets[is_url]);
    const __m128i last_char = _mm_set1_epi8(is_url ? '_' : '/');
    const __m128i last_adjustment = _mm_set1_epi8(is_url ? 33 : -3);
    const __m128i nibble_mask = _mm_set1_epi8(15);

    while (len - consumed >= 20) {
        const __m128i input = _mm_loadu_si128((const __m128i *)(src + consumed));
        const __m128i lo = _mm_and_si128(input, nibble_mask);
        const __m128i hi = _mm_and_si128(_mm_srli_epi16(input, 4), nibble_mask);
        const __m128i valid = _mm_and_si128(_mm_shuffle_epi8(valid_lo, lo),
                                           _mm_shuffle_epi8(valid_hi, hi));
        const __m128i adjustment = _mm_and_si128(_mm_cmpeq_epi8(input, last_char),
                                                last_adjustment);
        __m128i value;
        __m128i output;

        if (_mm_movemask_epi8(_mm_cmpeq_epi8(valid, _mm_setzero_si128())) != 0) {
            break;
        }
        value = _mm_add_epi8(input,
            _mm_add_epi8(_mm_shuffle_epi8(offsets, hi), adjustment));

        /* Adjacent sextets become 12-bit pairs, then 24-bit triples. */
        output = _mm_maddubs_epi16(value, _mm_set1_epi32(0x01400140));
        output = _mm_madd_epi16(output, _mm_set1_epi32(0x00011000));
        output = _mm_shuffle_epi8(output, scatter);
        /* A 16-byte store would overwrite Haskell's uncommitted suffix. */
        memcpy(dst, &output, 12);
        consumed += 16;
        dst += 12;
    }
    return consumed;
}

#if defined(HS_BASE64_AVX2)

HS_BASE64_TARGET_AVX2
static inline __m128i hs_base64_avx2_load12(const uint8_t *src)
{
    int64_t first;
    int32_t last;
    memcpy(&first, src, sizeof(first));
    memcpy(&last, src + 8, sizeof(last));
    return _mm_insert_epi32(_mm_cvtsi64_si128(first), last, 2);
}

HS_BASE64_TARGET_AVX2
static inline void hs_base64_avx2_store12(uint8_t *dst, __m128i value)
{
    const int64_t first = _mm_cvtsi128_si64(value);
    const int32_t last = _mm_extract_epi32(value, 2);
    memcpy(dst, &first, sizeof(first));
    memcpy(dst + 8, &last, sizeof(last));
}

HS_BASE64_TARGET_AVX2
static size_t hs_base64_avx2_encode(const uint8_t *src, size_t len, uint8_t *dst,
                                   const uint8_t *alphabet)
{
    size_t consumed = 0;
    __m256i lookup[4];
    const __m256i gather = _mm256_broadcastsi128_si256(
        _mm_setr_epi8(2, 1, 0, -1, 5, 4, 3, -1, 8, 7, 6, -1, 11, 10, 9, -1));

    if (len < 24) {
        return 0;
    }
    for (size_t i = 0; i < 4; i++) {
        lookup[i] = _mm256_broadcastsi128_si256(
            _mm_loadu_si128((const __m128i *)(alphabet + i * 16)));
    }

    while (len - consumed >= 24) {
        const __m128i first = hs_base64_avx2_load12(src + consumed);
        const __m128i second = hs_base64_avx2_load12(src + consumed + 12);
        __m256i input;
        __m256i indices;
        __m256i high;
        __m256i output = _mm256_setzero_si256();

        /* Each 128-bit lane receives exactly four triples; neither load is wide. */
        input = _mm256_inserti128_si256(_mm256_castsi128_si256(first), second, 1);
        input = _mm256_shuffle_epi8(input, gather);
        indices = _mm256_and_si256(_mm256_srli_epi32(input, 18),
                                   _mm256_set1_epi32(0x0000003f));
        indices = _mm256_or_si256(indices,
            _mm256_and_si256(_mm256_srli_epi32(input, 4), _mm256_set1_epi32(0x00003f00)));
        indices = _mm256_or_si256(indices,
            _mm256_and_si256(_mm256_slli_epi32(input, 10), _mm256_set1_epi32(0x003f0000)));
        indices = _mm256_or_si256(indices,
            _mm256_and_si256(_mm256_slli_epi32(input, 24), _mm256_set1_epi32(0x3f000000)));
        high = _mm256_and_si256(indices, _mm256_set1_epi8(0x30));
        for (size_t i = 0; i < 4; i++) {
            const __m256i matches = _mm256_cmpeq_epi8(high, _mm256_set1_epi8((char)(i * 16)));
            output = _mm256_or_si256(output,
                _mm256_and_si256(matches, _mm256_shuffle_epi8(lookup[i], indices)));
        }
        _mm256_storeu_si256((__m256i *)dst, output);
        consumed += 24;
        dst += 32;
    }
    return consumed;
}

HS_BASE64_TARGET_AVX2
static size_t hs_base64_avx2_decode(const uint8_t *src, size_t len, uint8_t *dst,
                                   const uint8_t *table)
{
    size_t consumed = 0;
    const __m256i scatter = _mm256_broadcastsi128_si256(
        _mm_setr_epi8(2, 1, 0, 6, 5, 4, 10, 9, 8, 14, 13, 12, -1, -1, -1, -1));

    if (len < 36) {
        return 0;
    }
    const int is_url = table['-'] == 62;
    const __m256i valid_lo = _mm256_broadcastsi128_si256(
        _mm_loadu_si128((const __m128i *)hs_base64_valid_lo[is_url]));
    const __m256i valid_hi = _mm256_broadcastsi128_si256(
        _mm_loadu_si128((const __m128i *)hs_base64_valid_hi));
    const __m256i offsets = _mm256_broadcastsi128_si256(
        _mm_loadu_si128((const __m128i *)hs_base64_offsets[is_url]));
    const __m256i last_char = _mm256_set1_epi8(is_url ? '_' : '/');
    const __m256i last_adjustment = _mm256_set1_epi8(is_url ? 33 : -3);
    const __m256i nibble_mask = _mm256_set1_epi8(15);

    while (len - consumed >= 36) {
        const __m256i input = _mm256_loadu_si256((const __m256i *)(src + consumed));
        const __m256i lo = _mm256_and_si256(input, nibble_mask);
        const __m256i hi = _mm256_and_si256(_mm256_srli_epi16(input, 4), nibble_mask);
        const __m256i valid = _mm256_and_si256(_mm256_shuffle_epi8(valid_lo, lo),
                                              _mm256_shuffle_epi8(valid_hi, hi));
        const __m256i adjustment = _mm256_and_si256(_mm256_cmpeq_epi8(input, last_char),
                                                   last_adjustment);
        __m256i value;
        __m256i output;
        __m128i first;
        __m128i second;

        /* Validate both lanes before committing any bytes from this block. */
        if (_mm256_movemask_epi8(_mm256_cmpeq_epi8(valid, _mm256_setzero_si256())) != 0) {
            break;
        }
        value = _mm256_add_epi8(input,
            _mm256_add_epi8(_mm256_shuffle_epi8(offsets, hi), adjustment));
        output = _mm256_maddubs_epi16(value, _mm256_set1_epi32(0x01400140));
        output = _mm256_madd_epi16(output, _mm256_set1_epi32(0x00011000));
        output = _mm256_shuffle_epi8(output, scatter);
        first = _mm256_castsi256_si128(output);
        second = _mm256_extracti128_si256(output, 1);
        /* Store exactly 24 bytes, with no gaps or overlapping speculative bytes. */
        hs_base64_avx2_store12(dst, first);
        hs_base64_avx2_store12(dst + 12, second);
        consumed += 32;
        dst += 24;
    }
    return consumed;
}

#endif /* HS_BASE64_AVX2 */

#endif

int hs_base64_simd_backend(void)
{
#if defined(HS_BASE64_NEON)
    return 2;
#elif defined(HS_BASE64_SSSE3)
#if defined(HS_BASE64_AVX2)
    /* The compiler's CPU query also checks OS support for the AVX register state. */
    if (__builtin_cpu_supports("avx2")) {
        return 3;
    }
#endif
    return __builtin_cpu_supports("ssse3") ? 1 : 0;
#else
    return 0;
#endif
}

size_t hs_base64_simd_encode(const uint8_t *src, size_t len, uint8_t *dst,
                            const uint8_t *alphabet)
{
#if defined(HS_BASE64_NEON)
    return hs_base64_neon_encode(src, len, dst, alphabet);
#elif defined(HS_BASE64_SSSE3)
    const int backend = hs_base64_simd_backend();
#if defined(HS_BASE64_AVX2)
    if (backend == 3) {
        return hs_base64_avx2_encode(src, len, dst, alphabet);
    }
#endif
    if (backend == 1) {
        return hs_base64_ssse3_encode(src, len, dst, alphabet);
    }
#endif
    (void)src;
    (void)len;
    (void)dst;
    (void)alphabet;
    return 0;
}

size_t hs_base64_simd_decode(const uint8_t *src, size_t len, uint8_t *dst,
                            const uint8_t *table)
{
#if defined(HS_BASE64_NEON)
    return hs_base64_neon_decode(src, len, dst, table);
#elif defined(HS_BASE64_SSSE3)
    const int backend = hs_base64_simd_backend();
#if defined(HS_BASE64_AVX2)
    if (backend == 3) {
        return hs_base64_avx2_decode(src, len, dst, table);
    }
#endif
    if (backend == 1) {
        return hs_base64_ssse3_decode(src, len, dst, table);
    }
#endif
    (void)src;
    (void)len;
    (void)dst;
    (void)table;
    return 0;
}
