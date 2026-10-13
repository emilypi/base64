/* Copyright (c) 2026, Emily Pillmore. BSD-3-Clause; see LICENSE. */

/*
 * Standalone differential/bounds checks; link with cbits/base64_simd.c.
 * Recommended: -std=c99 -O2 -Wall -Wextra -Werror
 *              -fsanitize=address,undefined -fno-omit-frame-pointer
 * Repeat with -DHS_BASE64_SIMD_FORCE_SCALAR to test fallback compilation.
 */
#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE 1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(__unix__) || defined(__APPLE__)
#include <sys/mman.h>
#include <unistd.h>
#define HS_BASE64_GUARD_PAGES 1
#endif

#include "base64_simd.h"

#define CHECK(condition) do { \
    if (!(condition)) { \
        fprintf(stderr, "%s:%d: check failed: %s\n", __FILE__, __LINE__, #condition); \
        exit(EXIT_FAILURE); \
    } \
} while (0)

static const uint8_t alphabets[2][65] = {
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
};

static size_t encode_block;
static size_t decode_block;
static size_t checks;

static void make_table(const uint8_t *alphabet, uint8_t *table)
{
    memset(table, 255, 256);
    for (size_t i = 0; i < 64; i++) {
        table[alphabet[i]] = (uint8_t)i;
    }
    table['='] = 64;
}

static size_t encode_prefix(size_t len)
{
    return encode_block == 0 ? 0 : len / encode_block * encode_block;
}

static size_t decode_prefix(const uint8_t *src, size_t len, const uint8_t *table)
{
    size_t consumed = 0;

    if (decode_block == 0 || len < 4) {
        return 0;
    }
    while (len - consumed >= decode_block + 4) {
        for (size_t i = 0; i < decode_block; i++) {
            if (table[src[consumed + i]] >= 64) {
                return consumed;
            }
        }
        consumed += decode_block;
    }
    return consumed;
}

static void scalar_encode(const uint8_t *src, size_t len, uint8_t *dst,
                          const uint8_t *alphabet)
{
    for (size_t i = 0; i < len; i += 3) {
        const unsigned triple = ((unsigned)src[i] << 16) |
                                ((unsigned)src[i + 1] << 8) | src[i + 2];
        *dst++ = alphabet[(triple >> 18) & 63];
        *dst++ = alphabet[(triple >> 12) & 63];
        *dst++ = alphabet[(triple >> 6) & 63];
        *dst++ = alphabet[triple & 63];
    }
}

static void scalar_decode(const uint8_t *src, size_t len, uint8_t *dst,
                          const uint8_t *table)
{
    for (size_t i = 0; i < len; i += 4) {
        const unsigned quartet = ((unsigned)table[src[i]] << 18) |
                                 ((unsigned)table[src[i + 1]] << 12) |
                                 ((unsigned)table[src[i + 2]] << 6) |
                                 table[src[i + 3]];
        *dst++ = (uint8_t)(quartet >> 16);
        *dst++ = (uint8_t)(quartet >> 8);
        *dst++ = (uint8_t)quartet;
    }
}

static void check_canary(const uint8_t *data, size_t len)
{
    for (size_t i = 0; i < len; i++) {
        CHECK(data[i] == 0xa5);
    }
}

static void check_encode(const uint8_t *src, size_t len, size_t offset,
                         const uint8_t *alphabet)
{
    const size_t expected = encode_prefix(len);
    const size_t output_len = expected / 3 * 4;
    uint8_t *output = malloc(offset + output_len + 16);
    uint8_t *reference = malloc(output_len + 1);
    size_t consumed;

    CHECK(output != NULL && reference != NULL);
    memset(output, 0xa5, offset + output_len + 16);
    scalar_encode(src, expected, reference, alphabet);
    consumed = hs_base64_simd_encode(src, len, output + offset, alphabet);
    CHECK(consumed == expected);
    CHECK(consumed <= len && consumed % 3 == 0);
    CHECK(memcmp(output + offset, reference, output_len) == 0);
    check_canary(output, offset);
    check_canary(output + offset + output_len, 16);
    free(reference);
    free(output);
    checks++;
}

static void check_decode(const uint8_t *src, size_t len, size_t offset,
                         const uint8_t *table)
{
    const size_t expected = decode_prefix(src, len, table);
    const size_t output_len = expected / 4 * 3;
    uint8_t *output = malloc(offset + output_len + 16);
    uint8_t *reference = malloc(output_len + 1);
    size_t consumed;

    CHECK(output != NULL && reference != NULL);
    memset(output, 0xa5, offset + output_len + 16);
    scalar_decode(src, expected, reference, table);
    consumed = hs_base64_simd_decode(src, len, output + offset, table);
    CHECK(consumed == expected);
    CHECK(consumed % 4 == 0 && consumed <= len);
    CHECK(len < 4 ? consumed == 0 : len - consumed >= 4);
    CHECK(memcmp(output + offset, reference, output_len) == 0);
    check_canary(output, offset);
    check_canary(output + offset + output_len, 16);
    free(reference);
    free(output);
    checks++;
}

static void test_lengths_and_alignment(void)
{
    uint8_t table[256];
    uint8_t source[1056];
    uint8_t encoded[1408];

    for (size_t alphabet = 0; alphabet < 2; alphabet++) {
        make_table(alphabets[alphabet], table);
        for (size_t offset = 0; offset < 32; offset++) {
            for (size_t i = 0; i < sizeof(source); i++) {
                source[i] = (uint8_t)(i * 73 + offset * 19);
            }
            scalar_encode(source, sizeof(source), encoded, alphabets[alphabet]);
            for (size_t len = 0; len <= 1024; len++) {
                check_encode(source + offset, len, 31 - offset, alphabets[alphabet]);
                check_decode(encoded + offset, len, 31 - offset, table);
            }
        }
    }
}

static void test_all_byte_values(void)
{
    uint8_t source[257];
    uint8_t encoded[344];
    uint8_t table[256];

    for (size_t alphabet = 0; alphabet < 2; alphabet++) {
        make_table(alphabets[alphabet], table);
        for (unsigned value = 0; value <= 255; value++) {
            memset(source, (int)value, sizeof(source));
            check_encode(source, sizeof(source), 1, alphabets[alphabet]);
            scalar_encode(source, 255, encoded, alphabets[alphabet]);
            memcpy(encoded + 340, "AAAA", 4);
            check_decode(encoded, sizeof(encoded), 1, table);
        }
    }
}

static void test_invalid_and_padding_stops(void)
{
    uint8_t source[580];
    uint8_t table[256];

    for (size_t alphabet = 0; alphabet < 2; alphabet++) {
        make_table(alphabets[alphabet], table);
        for (size_t position = 0; position < sizeof(source); position++) {
            for (unsigned value = 0; value <= 255; value++) {
                memset(source, 'A', sizeof(source));
                source[position] = (uint8_t)value;
                check_decode(source, sizeof(source), position % 32, table);
            }
        }
        /* In particular, every kind of final padding stays with the caller. */
        memset(source, '/', sizeof(source));
        for (size_t padding = 0; padding <= 4; padding++) {
            memset(source, alphabets[alphabet][63], sizeof(source));
            memset(source + sizeof(source) - padding, '=', padding);
            check_decode(source, sizeof(source), 0, table);
        }
    }
}

/* Fixed-seed xorshift exercises byte combinations outside repeated/cyclic data. */
static uint32_t next_random(uint32_t *state)
{
    uint32_t value = *state;
    value ^= value << 13;
    value ^= value >> 17;
    value ^= value << 5;
    *state = value;
    return value;
}

static void test_random_mixed_inputs(void)
{
    uint8_t source[6144];
    uint8_t encoded[8192];
    uint8_t table[256];
    uint32_t state = UINT32_C(0x6d253fb9);

    for (size_t alphabet = 0; alphabet < 2; alphabet++) {
        make_table(alphabets[alphabet], table);
        for (size_t sample = 0; sample < 256; sample++) {
            const size_t len = next_random(&state) % (sizeof(source) + 1);
            const size_t encoded_len = len / 3 * 4;
            const size_t offset = next_random(&state) % 32;

            for (size_t i = 0; i < len; i++) {
                source[i] = (uint8_t)next_random(&state);
            }
            check_encode(source, len, offset, alphabets[alphabet]);
            scalar_encode(source, len / 3 * 3, encoded, alphabets[alphabet]);
            check_decode(encoded, encoded_len, offset, table);
            if (encoded_len > 0) {
                const size_t position = next_random(&state) % encoded_len;
                encoded[position] = (uint8_t)next_random(&state);
                check_decode(encoded, encoded_len, offset, table);
            }
        }
    }
}

static void test_tight_allocations(void)
{
    uint8_t table[256];

    for (size_t alphabet = 0; alphabet < 2; alphabet++) {
        make_table(alphabets[alphabet], table);
        for (size_t len = 1; len <= 1024; len++) {
            uint8_t *source = malloc(len);
            const size_t encode_len = encode_prefix(len) / 3 * 4;
            uint8_t *encoded = malloc(encode_len == 0 ? 1 : encode_len);
            size_t decode_len;
            uint8_t *decoded;

            CHECK(source != NULL && encoded != NULL);
            memset(source, 0xfb, len);
            CHECK(hs_base64_simd_encode(source, len, encoded, alphabets[alphabet]) ==
                  encode_prefix(len));
            memset(source, alphabets[alphabet][63], len);
            decode_len = decode_prefix(source, len, table) / 4 * 3;
            decoded = malloc(decode_len == 0 ? 1 : decode_len);
            CHECK(decoded != NULL);
            CHECK(hs_base64_simd_decode(source, len, decoded, table) ==
                  decode_prefix(source, len, table));
            free(decoded);
            free(encoded);
            free(source);
            checks += 2;
        }
    }
}

#if defined(HS_BASE64_GUARD_PAGES)
/* Guard pages independently catch over-wide intrinsic loads/stores. */
static uint8_t *guarded_buffer(size_t page_size)
{
    uint8_t *mapping = mmap(NULL, page_size * 3, PROT_NONE,
                            MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    CHECK(mapping != MAP_FAILED);
    CHECK(mprotect(mapping + page_size, page_size, PROT_READ | PROT_WRITE) == 0);
    return mapping;
}

static void test_guard_pages(void)
{
    const long raw_page_size = sysconf(_SC_PAGESIZE);
    size_t page_size;
    uint8_t *source_mapping;
    uint8_t *output_mapping;
    uint8_t table[256];

    CHECK(raw_page_size > 0);
    page_size = (size_t)raw_page_size;
    source_mapping = guarded_buffer(page_size);
    output_mapping = guarded_buffer(page_size);

    for (size_t alphabet = 0; alphabet < 2; alphabet++) {
        make_table(alphabets[alphabet], table);
        for (size_t len = 0; len <= 1024; len++) {
            for (size_t at_end = 0; at_end < 2; at_end++) {
                uint8_t *source = source_mapping + page_size +
                                  (at_end ? page_size - len : 0);
                const size_t encode_len = encode_prefix(len) / 3 * 4;
                uint8_t *output = output_mapping + page_size +
                                  (at_end ? page_size - encode_len : 0);
                size_t expected;
                size_t decode_len;

                memset(source, 0xfb, len);
                CHECK(hs_base64_simd_encode(source, len, output, alphabets[alphabet]) ==
                      encode_prefix(len));
                memset(source, alphabets[alphabet][63], len);
                expected = decode_prefix(source, len, table);
                decode_len = expected / 4 * 3;
                output = output_mapping + page_size + (at_end ? page_size - decode_len : 0);
                CHECK(hs_base64_simd_decode(source, len, output, table) == expected);
                checks += 2;
            }
        }
    }
    CHECK(munmap(source_mapping, page_size * 3) == 0);
    CHECK(munmap(output_mapping, page_size * 3) == 0);
}
#endif

int main(int argc, char **argv)
{
    const int backend = hs_base64_simd_backend();

    if (argc != 1) {
        if (argc != 3 || strcmp(argv[1], "--require-backend") != 0 ||
            strlen(argv[2]) != 1 || argv[2][0] < '0' || argv[2][0] > '3') {
            fprintf(stderr, "usage: %s [--require-backend 0|1|2|3]\n", argv[0]);
            return EXIT_FAILURE;
        }
        if (backend != argv[2][0] - '0') {
            fprintf(stderr, "required SIMD backend %c, detected %d\n", argv[2][0], backend);
            return EXIT_FAILURE;
        }
    }
    CHECK(backend >= 0 && backend <= 3);
#if defined(HS_BASE64_SIMD_FORCE_SCALAR)
    CHECK(backend == 0);
#elif defined(HS_BASE64_SIMD_FORCE_SSSE3) && defined(__x86_64__)
    CHECK(backend == 0 || backend == 1);
#elif defined(__aarch64__) && defined(__ARM_NEON) && \
      (defined(__GNUC__) || defined(__clang__)) && !defined(HS_BASE64_SIMD_EMULATED_X86)
    CHECK(backend == 2);
#endif
    encode_block = backend == 3 ? 24 : backend == 2 ? 48 : backend == 1 ? 12 : 0;
    decode_block = backend == 3 ? 32 : backend == 2 ? 64 : backend == 1 ? 16 : 0;
    printf("SIMD backend: %d (%s)\n", backend,
           backend == 3 ? "AVX2" : backend == 2 ? "NEON" :
           backend == 1 ? "SSSE3" : "scalar fallback");
    test_lengths_and_alignment();
    test_all_byte_values();
    test_invalid_and_padding_stops();
    test_random_mixed_inputs();
    test_tight_allocations();
#if defined(HS_BASE64_GUARD_PAGES)
    test_guard_pages();
#endif
    printf("Passed %zu differential/bounds checks.\n", checks);
    return EXIT_SUCCESS;
}
