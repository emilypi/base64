# Base64

![Build Status](https://github.com/emilypi/base64/workflows/ci/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/base64.svg)](https://hackage.haskell.org/package/base64)

Base64 encoding and decodings.

For the companion optics and pattern synonyms, see [base64-lens](https://hackage.haskell.org/package/base64-lens).

### Summary

The following types are supported for both std, padded url-safe, and unpadded url-safe alphabets:

- `Data.ByteString`
- `Data.ByteString.Lazy`
- `Data.ByteString.Short`
- `Data.Text`
- `Data.Text.Lazy`
- `Data.Text.Short`

Additionally this library has

- Better performance than `base64-bytestring` for encode and decode.
- Optics for handling more complex structures with Base64 representations via the `base64-lens` package
- Checks for both validity and correctness of Base64 and Base64url encodings
- Rejects non-canonical encodings that do not roundtrip in other base64 libraries like `ZE==`.

The library supports GHC 9.6 through 9.14. Its dependencies are `base`,
`bytestring`, `deepseq`, `text`, and `text-short`.

### Optional SIMD

Enable the manual `simd` Cabal flag to use bundled SIMD kernels for standard
and URL-safe encoding and strict decoding, including the typed decoders:

```sh
cabal build -fsimd
cabal test -fsimd --test-show-details=direct
```

The flag is **off by default**. ARM64 uses NEON; x86-64 selects AVX2 when
the CPU and operating system support it, then SSSE3 on older processors.
Other targets and short inputs use the existing
Haskell loops. No external C library is required. Remainders, padding checks,
canonical-encoding checks, and error messages still use the Haskell code.
Lenient decoding remains scalar.

Test both configurations and compare optimized benchmark builds separately:

```sh
cabal test -f-simd --builddir=dist-scalar --test-show-details=direct
cabal test -fsimd --builddir=dist-simd --test-show-details=direct
cabal bench -f-simd --builddir=dist-scalar -O2 --benchmark-options='--csv scalar.csv'
cabal bench -fsimd --builddir=dist-simd -O2 --benchmark-options='--csv simd.csv'
```

Benchmark names identify the selected flag configuration. The suite covers
standard Base64 and padded/unpadded Base64url, encoding and typed/untyped
decoding, short and large buffers, and sliced inputs. The test suite exercises
block boundaries, scalar tails, slices, invalid input, and wrapper types in
both configurations; CI also runs native memory checks on x86-64 and ARM64,
explicitly requiring AVX2 on Linux x86-64 and checking the SSSE3 and scalar
fallbacks separately.

For native kernel measurements, a standalone benchmark validates every case
against scalar output and checks both alphabets. It times pseudorandom,
cycling, and repeated inputs at two alignments.
It reports the detected backend and writes CSV results:

```sh
cc -O2 -std=c99 -Icbits cbits/base64_simd.c benchmarks/simd-native.c -o base64-native-bench
./base64-native-bench --seconds 0.05 --rounds 5 > native.csv
```

Add `--url` to time the URL alphabet, or `--pattern random` to select one
input distribution. On x86-64, compile a second executable with
`-DHS_BASE64_SIMD_FORCE_SSSE3` to compare the older backend on the same CPU.
These measurements cover native prefixes only; use the Cabal benchmarks above
to include Haskell allocation and scalar tails.
