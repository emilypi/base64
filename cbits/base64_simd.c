/* Copyright (c) 2026, Emily Pillmore. BSD-3-Clause; see LICENSE. */

#include <string.h>

#include "base64_simd.h"

/*
 * These are original prefix kernels. No external SIMD library is required.
 * Target-specific instructions are confined to their implementation below;
 * x86 builds need no global ISA flags. OS register-state support is cached
 * during library initialization; dispatch does not mutate that cache.
 * HS_BASE64_SIMD_FORCE_SCALAR exercises the unsupported-target fallback.
 * HS_BASE64_SIMD_FORCE_SSSE3 caps x86 dispatch for testing older processors.
 * FORCE_AVX2 and FORCE_AVX512BW similarly cap dispatch; they never enable
 * instructions that the CPU or operating system cannot execute.
 */
#if !defined(HS_BASE64_SIMD_FORCE_SCALAR)
#if defined(__aarch64__) && defined(__ARM_NEON) && \
    (defined(__GNUC__) || defined(__clang__))
#define HS_BASE64_NEON 1
#include <arm_neon.h>
#elif defined(__x86_64__) && (defined(__GNUC__) || defined(__clang__))
#define HS_BASE64_SSSE3 1
#include <cpuid.h>
#include <immintrin.h>
#if defined(__APPLE__)
#include <sys/types.h>
#include <sys/sysctl.h>
#endif
#define HS_BASE64_TARGET_SSSE3 __attribute__((target("ssse3")))
#if !defined(HS_BASE64_SIMD_FORCE_SSSE3)
#define HS_BASE64_AVX2 1
#define HS_BASE64_TARGET_AVX2 __attribute__((target("avx2")))
#if !defined(HS_BASE64_SIMD_FORCE_AVX2) && \
    ((defined(__clang__) && __clang_major__ >= 5) || \
     (!defined(__clang__) && __GNUC__ >= 6))
#define HS_BASE64_AVX512BW 1
#define HS_BASE64_TARGET_AVX512BW __attribute__((target("avx512f,avx512bw")))
#if !defined(HS_BASE64_SIMD_FORCE_AVX512BW) && \
    ((defined(__clang__) && __clang_major__ >= 7) || \
     (!defined(__clang__) && __GNUC__ >= 8))
#define HS_BASE64_AVX512VBMI 1
#define HS_BASE64_TARGET_AVX512VBMI __attribute__((target("avx512f,avx512bw,avx512vbmi")))
#endif
#endif
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

#if defined(HS_BASE64_AVX2)

/* Older compiler runtimes report hardware features without checking OS state.
 * Cache that check once, keeping CPUID and XGETBV out of the hot path. */
static unsigned hs_base64_x86_state;

static void __attribute__((constructor)) hs_base64_init_x86_state(void)
{
    unsigned eax, ebx, ecx, edx;
    unsigned xcr0_lo, xcr0_hi;
    if (!__get_cpuid(1, &eax, &ebx, &ecx, &edx) ||
        (ecx & ((1u << 27) | (1u << 28))) != ((1u << 27) | (1u << 28))) {
        return;
    }
    __asm__ volatile("xgetbv" : "=a"(xcr0_lo), "=d"(xcr0_hi) : "c"(0));
    (void)xcr0_hi;
    hs_base64_x86_state = xcr0_lo;
#if defined(__APPLE__)
    /* Darwin enables AVX-512 context lazily. Ask the kernel whether promotion
     * is supported, including when AVX-512 was disabled at boot. */
    if ((xcr0_lo & 0x6u) == 0x6u) {
        int enabled = 0;
        size_t size = sizeof(enabled);
        if (sysctlbyname("hw.optional.avx512f", &enabled, &size, NULL, 0) == 0 && enabled) {
            hs_base64_x86_state |= 0xe0u;
        }
    }
#endif
}
#endif

#if defined(HS_BASE64_AVX512BW)

HS_BASE64_TARGET_AVX512BW
static size_t hs_base64_avx512bw_encode(const uint8_t *src, size_t len,
                                      uint8_t *dst, const uint8_t *alphabet)
{
    size_t consumed = 0;
    __m512i lookup[4];
    const __m512i lanes = _mm512_setr_epi32(0, 1, 2, 0, 3, 4, 5, 0,
                                           6, 7, 8, 0, 9, 10, 11, 0);
    const __m512i gather = _mm512_broadcast_i32x4(
        _mm_setr_epi8(2, 1, 0, -1, 5, 4, 3, -1, 8, 7, 6, -1, 11, 10, 9, -1));
    if (len < 48) {
        return 0;
    }
    for (size_t i = 0; i < 4; i++) {
        lookup[i] = _mm512_broadcast_i32x4(
            _mm_loadu_si128((const __m128i *)(alphabet + i * 16)));
    }
    while (len - consumed >= 48) {
        /* Read exactly 48 bytes, then place 12 bytes in each 128-bit lane. */
        __m512i input = _mm512_maskz_loadu_epi8(UINT64_C(0xffffffffffff), src + consumed);
        __m512i indices;
        __m512i output = _mm512_setzero_si512();
        input = _mm512_shuffle_epi8(_mm512_permutexvar_epi32(lanes, input), gather);
        indices = _mm512_and_si512(_mm512_srli_epi32(input, 18),
                                   _mm512_set1_epi32(0x0000003f));
        indices = _mm512_or_si512(indices, _mm512_and_si512(
            _mm512_srli_epi32(input, 4), _mm512_set1_epi32(0x00003f00)));
        indices = _mm512_or_si512(indices, _mm512_and_si512(
            _mm512_slli_epi32(input, 10), _mm512_set1_epi32(0x003f0000)));
        indices = _mm512_or_si512(indices, _mm512_and_si512(
            _mm512_slli_epi32(input, 24), _mm512_set1_epi32(0x3f000000)));
        const __m512i high = _mm512_and_si512(indices, _mm512_set1_epi8(0x30));
        for (size_t i = 0; i < 4; i++) {
            const __mmask64 matches = _mm512_cmpeq_epi8_mask(high,
                _mm512_set1_epi8((char)(i * 16)));
            output = _mm512_mask_mov_epi8(output, matches,
                _mm512_shuffle_epi8(lookup[i], indices));
        }
        _mm512_storeu_si512((void *)dst, output);
        consumed += 48;
        dst += 64;
    }
    return consumed;
}

HS_BASE64_TARGET_AVX512BW
static size_t hs_base64_avx512bw_decode(const uint8_t *src, size_t len,
                                      uint8_t *dst, const uint8_t *table)
{
    size_t consumed = 0;
    if (len < 68) {
        return 0;
    }
    const int is_url = table['-'] == 62;
    const __m512i valid_lo = _mm512_broadcast_i32x4(
        _mm_loadu_si128((const __m128i *)hs_base64_valid_lo[is_url]));
    const __m512i valid_hi = _mm512_broadcast_i32x4(
        _mm_loadu_si128((const __m128i *)hs_base64_valid_hi));
    const __m512i offsets = _mm512_broadcast_i32x4(
        _mm_loadu_si128((const __m128i *)hs_base64_offsets[is_url]));
    const __m512i scatter = _mm512_broadcast_i32x4(
        _mm_setr_epi8(2, 1, 0, 6, 5, 4, 10, 9, 8, 14, 13, 12, -1, -1, -1, -1));
    const __m512i compact = _mm512_setr_epi32(0, 1, 2, 4, 5, 6, 8, 9,
                                             10, 12, 13, 14, 0, 0, 0, 0);
    const __m512i nibble_mask = _mm512_set1_epi8(15);
    const __m512i last_char = _mm512_set1_epi8(is_url ? '_' : '/');
    const __m512i last_adjustment = _mm512_set1_epi8(is_url ? 33 : -3);
    while (len - consumed >= 68) {
        const __m512i input = _mm512_loadu_si512((const void *)(src + consumed));
        const __m512i lo = _mm512_and_si512(input, nibble_mask);
        const __m512i hi = _mm512_and_si512(_mm512_srli_epi16(input, 4), nibble_mask);
        const __m512i valid = _mm512_and_si512(_mm512_shuffle_epi8(valid_lo, lo),
                                               _mm512_shuffle_epi8(valid_hi, hi));
        if (_mm512_cmpeq_epi8_mask(valid, _mm512_setzero_si512()) != 0) {
            break;
        }
        const __m512i adjustment = _mm512_maskz_mov_epi8(
            _mm512_cmpeq_epi8_mask(input, last_char), last_adjustment);
        const __m512i value = _mm512_add_epi8(input,
            _mm512_add_epi8(_mm512_shuffle_epi8(offsets, hi), adjustment));
        __m512i output = _mm512_maddubs_epi16(value, _mm512_set1_epi32(0x01400140));
        output = _mm512_madd_epi16(output, _mm512_set1_epi32(0x00011000));
        output = _mm512_permutexvar_epi32(compact, _mm512_shuffle_epi8(output, scatter));
        /* The final quartet and all uncommitted destination bytes are untouched. */
        _mm512_mask_storeu_epi8(dst, UINT64_C(0xffffffffffff), output);
        consumed += 64;
        dst += 48;
    }
    return consumed;
}

#if defined(HS_BASE64_AVX512VBMI)

HS_BASE64_TARGET_AVX512VBMI
static size_t hs_base64_avx512vbmi_encode(const uint8_t *src, size_t len,
                                        uint8_t *dst, const uint8_t *alphabet)
{
    size_t consumed = 0;
    if (len < 48) {
        return 0;
    }
    /* Each dword contains a reversed triple. Multishift selects bits
     * 18,12,6,0 in each dword; VPERMB uses only the low six bits of each
     * result to index the full alphabet. This is the Muła/Lemire VBMI
     * approach, with controls derived from the existing big-endian triples. */
    const __m512i gather = _mm512_setr_epi32(
        0x00000102, 0x00030405, 0x00060708, 0x00090a0b,
        0x000c0d0e, 0x000f1011, 0x00121314, 0x00151617,
        0x0018191a, 0x001b1c1d, 0x001e1f20, 0x00212223,
        0x00242526, 0x00272829, 0x002a2b2c, 0x002d2e2f);
    const __m512i shifts = _mm512_set1_epi64(INT64_C(0x20262c3200060c12));
    const __m512i lookup = _mm512_loadu_si512((const void *)alphabet);
    while (len - consumed >= 48) {
        const __m512i input = _mm512_maskz_loadu_epi8(
            UINT64_C(0xffffffffffff), src + consumed);
        const __m512i triples = _mm512_permutexvar_epi8(gather, input);
        const __m512i indices = _mm512_multishift_epi64_epi8(shifts, triples);
        _mm512_storeu_si512((void *)dst, _mm512_permutexvar_epi8(indices, lookup));
        consumed += 48;
        dst += 64;
    }
    return consumed;
}

HS_BASE64_TARGET_AVX512VBMI
static size_t hs_base64_avx512vbmi_decode(const uint8_t *src, size_t len,
                                        uint8_t *dst, const uint8_t *table)
{
    size_t consumed = 0;
    if (len < 68) {
        return 0;
    }
    const __m512i lower = _mm512_loadu_si512((const void *)table);
    const __m512i upper = _mm512_loadu_si512((const void *)(table + 64));
    /* Three significant bytes per dword, reversed into original byte order. */
    static const uint8_t compact_bytes[64] = {
         2,  1,  0,  6,  5,  4, 10,  9,  8, 14, 13, 12, 18, 17, 16, 22,
        21, 20, 26, 25, 24, 30, 29, 28, 34, 33, 32, 38, 37, 36, 42, 41,
        40, 46, 45, 44, 50, 49, 48, 54, 53, 52, 58, 57, 56, 62, 61, 60,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    };
    const __m512i compact = _mm512_loadu_si512((const void *)compact_bytes);
    while (len - consumed >= 68) {
        const __m512i input = _mm512_loadu_si512((const void *)(src + consumed));
        const __m512i value = _mm512_permutex2var_epi8(lower, input, upper);
        /* VPERMI2B ignores bit 7: reject high-bit aliases separately. All
         * table values >= 64, including either padding sentinel, are invalid. */
        if ((_mm512_movepi8_mask(input) |
             _mm512_cmpge_epu8_mask(value, _mm512_set1_epi8(64))) != 0) {
            break;
        }
        __m512i output = _mm512_maddubs_epi16(value, _mm512_set1_epi32(0x01400140));
        output = _mm512_madd_epi16(output, _mm512_set1_epi32(0x00011000));
        output = _mm512_permutexvar_epi8(compact, output);
        _mm512_mask_storeu_epi8(dst, UINT64_C(0xffffffffffff), output);
        consumed += 64;
        dst += 48;
    }
    return consumed;
}
#endif /* HS_BASE64_AVX512VBMI */
#endif /* HS_BASE64_AVX512BW */

#endif

int hs_base64_simd_backend(void)
{
#if defined(HS_BASE64_NEON)
    return 2;
#elif defined(HS_BASE64_SSSE3)
#if defined(HS_BASE64_AVX512BW)
    if (__builtin_cpu_supports("avx512f") &&
        __builtin_cpu_supports("avx512bw") &&
        (hs_base64_x86_state & 0xe6u) == 0xe6u) {
#if defined(HS_BASE64_AVX512VBMI)
        if (__builtin_cpu_supports("avx512vbmi")) {
            return 5;
        }
#endif
        return 4;
    }
#endif
#if defined(HS_BASE64_AVX2)
    if (__builtin_cpu_supports("avx2") &&
        (hs_base64_x86_state & 0x6u) == 0x6u) {
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
#if defined(HS_BASE64_AVX512VBMI)
    if (backend == 5) {
        return hs_base64_avx512vbmi_encode(src, len, dst, alphabet);
    }
#endif
#if defined(HS_BASE64_AVX512BW)
    if (backend == 4) {
        return hs_base64_avx512bw_encode(src, len, dst, alphabet);
    }
#endif
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
#if defined(HS_BASE64_AVX512VBMI)
    if (backend == 5) {
        return hs_base64_avx512vbmi_decode(src, len, dst, table);
    }
#endif
#if defined(HS_BASE64_AVX512BW)
    if (backend == 4) {
        return hs_base64_avx512bw_decode(src, len, dst, table);
    }
#endif
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
