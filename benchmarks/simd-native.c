/* Copyright (c) 2026, Emily Pillmore. BSD-3-Clause; see LICENSE. */
/* Standalone native-prefix timing; no full codec is used in timed regions.
 * Build: cc -O2 -std=c99 -Icbits benchmarks/simd-native.c cbits/base64_simd.c -o simd-native-bench
 * Every case is validated against independent scalar bytes before timing.
 */
#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include "base64_simd.h"

typedef size_t (*prefix_fn)(const uint8_t *, size_t, uint8_t *, const uint8_t *);
struct variant { const char *name; prefix_fn encode; prefix_fn decode; };
static const struct variant variants[] = {
    {"native", hs_base64_simd_encode, hs_base64_simd_decode}
};
#define COUNT (sizeof(variants) / sizeof(variants[0]))
static const uint8_t alphabets[2][65] = {
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
};
static volatile size_t sink;

static void require(int condition, const char *message)
{
    if (!condition) { fprintf(stderr, "%s\n", message); exit(1); }
}
static double now(void)
{
    struct timespec t;
    require(clock_gettime(CLOCK_MONOTONIC, &t) == 0, "clock_gettime failed");
    return (double)t.tv_sec + (double)t.tv_nsec * 1e-9;
}
static void fixture(uint8_t *p, size_t len, unsigned kind)
{
    uint64_t state = UINT64_C(0x243f6a8885a308d3);
    for (size_t i = 0; i < len; i++) {
        state ^= state << 13; state ^= state >> 7; state ^= state << 17;
        p[i] = kind == 0 ? (uint8_t)state : kind == 1 ? (uint8_t)i : 90;
    }
}
static void reference_encode(const uint8_t *src, size_t len, uint8_t *dst,
                             const uint8_t *alphabet)
{
    for (size_t i = 0; i + 2 < len; i += 3) {
        const uint32_t n = (uint32_t)src[i] << 16 | (uint32_t)src[i + 1] << 8 | src[i + 2];
        *dst++ = alphabet[n >> 18]; *dst++ = alphabet[(n >> 12) & 63];
        *dst++ = alphabet[(n >> 6) & 63]; *dst++ = alphabet[n & 63];
    }
}
static double timed(prefix_fn fn, const uint8_t *src, size_t len, uint8_t *dst,
                    const uint8_t *table, size_t iterations)
{
    size_t total = 0;
    const double start = now();
    for (size_t i = 0; i < iterations; i++) total += fn(src, len, dst, table);
    const double elapsed = now() - start;
    sink += total + dst[0];
    return elapsed;
}
static size_t calibrate(prefix_fn fn, const uint8_t *src, size_t len, uint8_t *dst,
                        const uint8_t *table, double target)
{
    size_t iterations = 1;
    double elapsed;
    do {
        elapsed = timed(fn, src, len, dst, table, iterations);
        if (elapsed < 0.003) iterations *= 2;
    } while (elapsed < 0.003);
    return (size_t)((double)iterations * target / elapsed) + 1;
}
int main(int argc, char **argv)
{
    double seconds = 0.02;
    unsigned rounds = 3, timed_alphabet = 0;
    const char *only_pattern = NULL;
    int required_backend = -1;
    const size_t sizes[] = {47, 48, 49, 63, 64, 65, 95, 96, 97, 127, 128, 129, 4096, 1048576};
    const char *patterns[] = {"random", "cycle256", "repeated"};
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--seconds") == 0 && i + 1 < argc) seconds = strtod(argv[++i], NULL);
        else if (strcmp(argv[i], "--rounds") == 0 && i + 1 < argc) rounds = (unsigned)strtoul(argv[++i], NULL, 10);
        else if (strcmp(argv[i], "--pattern") == 0 && i + 1 < argc) only_pattern = argv[++i];
        else if (strcmp(argv[i], "--url") == 0) timed_alphabet = 1;
        else if (strcmp(argv[i], "--require-backend") == 0 && i + 1 < argc) {
            const char *value = argv[++i];
            require(strlen(value) == 1 && value[0] >= '0' && value[0] <= '5', "invalid backend");
            required_backend = value[0] - '0';
        }
        else { fprintf(stderr, "usage: %s [--seconds 0.02] [--rounds 3] [--url] [--pattern random|cycle256|repeated] [--require-backend 0|1|2|3|4|5]\n", argv[0]); return 1; }
    }
    require(seconds >= 0.001 && seconds <= 10 && rounds > 0 && rounds <= 50, "invalid timing options");
    if (only_pattern) {
        int found = 0;
        for (size_t i = 0; i < 3; i++) found |= strcmp(only_pattern, patterns[i]) == 0;
        require(found, "unknown --pattern (use random, cycle256, or repeated)");
    }
    const int backend = hs_base64_simd_backend();
    fprintf(stderr, "native backend=%d (0 scalar, 1 SSSE3, 2 NEON, 3 AVX2, 4 AVX512BW, 5 AVX512VBMI)\n", backend);
    require(required_backend < 0 || backend == required_backend, "required backend is unavailable");
    require(backend != 0, "no native SIMD backend is available to time");

    puts("variant,operation,alphabet,data,raw_bytes,input_bytes,consumed,offset,round,iterations,ns_per_call,input_gib_per_second");
    for (unsigned kind = 0; kind < 3; kind++) {
        if (only_pattern && strcmp(only_pattern, patterns[kind]) != 0) continue;
        for (size_t n = 0; n < sizeof(sizes) / sizeof(sizes[0]); n++) {
            const size_t len = sizes[n], encoded_len = len / 3 * 4 + 4;
            for (size_t offset = 0; offset <= 1; offset++) {
                uint8_t *raw_base = malloc(len + 2), *encoded_base = malloc(encoded_len + 2);
                uint8_t *output_base = malloc(encoded_len + 34), *reference = malloc(encoded_len);
                require(raw_base && encoded_base && output_base && reference, "allocation failed");
                uint8_t *raw = raw_base + offset, *encoded = encoded_base + offset;
                uint8_t *output = output_base + offset;
                fixture(raw, len, kind);
                /* Validate both alphabets even if timing only one. */
                for (unsigned alphabet = 0; alphabet < 2; alphabet++) {
                    uint8_t table[256];
                    memset(table, 255, sizeof(table));
                    for (size_t j = 0; j < 64; j++) table[alphabets[alphabet][j]] = (uint8_t)j;
                    table['='] = 64;
                    reference_encode(raw, len, encoded, alphabets[alphabet]);
                    memcpy(reference, encoded, encoded_len - 4);
                    memcpy(encoded + encoded_len - 4, "AAAA", 4);
                    for (unsigned operation = 0; operation < 2; operation++) {
                        const uint8_t *src = operation ? encoded : raw;
                        const uint8_t *lookup = operation ? table : alphabets[alphabet];
                        const size_t input_len = operation ? encoded_len : len;
                        const uint8_t *expected = operation ? raw : reference;
                        size_t consumed[COUNT] = {0}, iterations[COUNT] = {0};
                        for (size_t v = 0; v < COUNT; v++) {
                            prefix_fn fn = operation ? variants[v].decode : variants[v].encode;
                            memset(output_base, 0xa5, encoded_len + 34);
                            consumed[v] = fn(src, input_len, output, lookup);
                            const size_t written = operation ? consumed[v] / 4 * 3 : consumed[v] / 3 * 4;
                            require(consumed[v] <= input_len && consumed[v] % (operation ? 4 : 3) == 0,
                                    "invalid prefix return");
                            require(!operation || input_len - consumed[v] >= 4, "final quartet consumed");
                            require(memcmp(output, expected, written) == 0, "incorrect prefix bytes");
                            for (size_t j = 0; j < offset; j++) require(output_base[j] == 0xa5, "leading canary changed");
                            for (size_t j = written; j < encoded_len + 32; j++) require(output[j] == 0xa5, "trailing canary changed");
                            if (alphabet == timed_alphabet && consumed[v] != 0)
                                iterations[v] = calibrate(fn, src, input_len, output, lookup, seconds);
                        }
                        if (alphabet != timed_alphabet) continue;
                        for (unsigned round = 0; round < rounds; round++) {
                            /* Rotate implementation order to limit drift bias. */
                            for (size_t order = 0; order < COUNT; order++) {
                                const size_t v = (order + round) % COUNT;
                                if (consumed[v] == 0) continue;
                                prefix_fn fn = operation ? variants[v].decode : variants[v].encode;
                                const double elapsed = timed(fn, src, input_len, output, lookup, iterations[v]);
                                const double ns = elapsed * 1e9 / iterations[v];
                                printf("%s,%s,%s,%s,%zu,%zu,%zu,%zu,%u,%zu,%.6f,%.6f\n",
                                    variants[v].name, operation ? "decode" : "encode", alphabet ? "url" : "standard",
                                    patterns[kind], len, input_len, consumed[v], offset, round, iterations[v], ns,
                                    consumed[v] / ns * 1e9 / 1073741824.0);
                            }
                        }
                    }
                }
                free(reference); free(output_base); free(encoded_base); free(raw_base);
            }
        }
    }
    fprintf(stderr, "validated both alphabets; checksum=%zu\n", sink);
    return 0;
}
