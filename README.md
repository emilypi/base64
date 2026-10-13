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
- Parses standard Base64 into a validated `Base64` value, and checks Base64url encodings
- Rejects non-canonical encodings that do not roundtrip in other base64 libraries like `ZE==`.

The library supports GHC 9.6 through 9.14. Its dependencies are `base`,
`bytestring`, `deepseq`, `text`, and `text-short`.

Upgrading to 2.0? See the [migration guide](MIGRATION-2.0.md) for the replacement
of `isBase64` and `isValidBase64`, and the [release notes](CHANGELOG.md#20) for
the optional SIMD implementation.

### Parsing existing Base64

Use `parseBase64` to validate externally supplied standard Base64 and retain the
encoded value in a typed wrapper:

```haskell
>>> import qualified Data.ByteString.Base64 as B64
>>> :set -XOverloadedStrings
>>> B64.parseBase64 "U3Vu"
Right "U3Vu"
>>> fmap B64.decodeBase64 (B64.parseBase64 "U3Vu")
Right "Sun"
>>> B64.parseBase64 "U3V="
Left "non-canonical encoding detected at offset: 2"
```

Each standard-alphabet module exports `parseBase64 :: a -> Either Text (Base64
'StdPadded a)`, specialized to its string type. Parsing checks the alphabet,
length, padding, and canonical padding bits, then returns the original encoded
input. The Text variants validate Base64 independently of the decoded payload's
UTF-8 validity; use `decodeBase64UntypedWith` to check that conversion.

`parseBase64` replaces `isBase64` and `isValidBase64`. For a Boolean check, use
`isRight . parseBase64` from `Data.Either`. Unlike the old `isValidBase64`, this
rejects missing padding and non-canonical encodings. The URL-specific validators
`isBase64Url` and `isValidBase64Url` retain their existing behavior.

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
