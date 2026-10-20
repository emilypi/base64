# Migrating to base64 2.0

Version 2.0 replaces the standard-alphabet predicates `isBase64` and
`isValidBase64` with `parseBase64`. The new function checks the encoded input
and returns a typed `Base64` value, or an error explaining why validation
failed. The URL predicates `isBase64Url` and `isValidBase64Url` are likewise
replaced by `parseBase64Padded` and `parseBase64Unpadded`. All four predicates
are removed without a deprecation period.

The release also adds optional SIMD support. It is off by default, so existing
builds continue to use the scalar implementation.

## Parse externally supplied Base64

Each of these modules exports `parseBase64`, specialized to its string type:

- `Data.ByteString.Base64`
- `Data.ByteString.Lazy.Base64`
- `Data.ByteString.Short.Base64`
- `Data.Text.Encoding.Base64`
- `Data.Text.Lazy.Encoding.Base64`
- `Data.Text.Short.Encoding.Base64`

For example, the strict ByteString signature is:

```haskell
parseBase64 :: ByteString -> Either Text (Base64 'StdPadded ByteString)
```

Use the returned wrapper directly with `decodeBase64` instead of checking a
Boolean and then calling `assertBase64`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Base64 as B64

-- Right "U3Vu": the result contains the original encoded bytes.
encoded = B64.parseBase64 "U3Vu"

-- Right "Sun": decoding consumes the validated wrapper.
decoded = fmap B64.decodeBase64 encoded

-- Left "non-canonical encoding detected at offset: 2"
rejected = B64.parseBase64 "U3V="
```

Parsing preserves the input, including lazy chunk boundaries, and validates
the whole value before returning `Right`. It checks the alphabet, length,
padding, and unused padding bits. Error messages match `decodeBase64Untyped`.

## Replace Boolean checks

Import `isRight` from `Data.Either` and replace a call to `B64.isBase64 input`
with `isRight (B64.parseBase64 input)`. For a composed predicate, use
`isRight . B64.parseBase64`.

The same replacement is appropriate for `isValidBase64` when you want to check
for a decodable, canonical value. It is stricter than that function's former
alphabet-and-shape check:

| Input | Former `isBase64` | Former `isValidBase64` | `isRight . parseBase64` |
| --- | --- | --- | --- |
| `"U3Vu"` | `True` | `True` | `True` |
| `"U3V"` | `False` | `True` | `False` |
| `"U3V="` | `False` | `True` | `False` |
| `"%"` | `False` | `False` | `False` |
| `""` | `True` | `True` | `True` |

If your application intentionally accepts incomplete or non-canonical input,
keep that policy explicit; `parseBase64` does not reproduce the permissive
behavior of `isValidBase64`.

## URL-safe encodings

Each corresponding `.URL` module exports both parsers, specialized to its
string type:

```haskell
parseBase64Padded   :: a -> Either Text (Base64 'UrlPadded a)
parseBase64Unpadded :: a -> Either Text (Base64 'UrlUnpadded a)
```

This covers strict, lazy, and short ByteString and Text. The padded parser
requires complete padding when needed; the unpadded parser rejects padding.
Empty input and complete four-character quanta with no padding are valid in
both modes. Both parsers preserve the original encoded input, including lazy
chunk boundaries, and validate the entire value before returning `Right`.
They check the URL-safe alphabet and canonical padding bits, returning the
same errors as the corresponding strict ByteString untyped decoder.

```haskell
import qualified Data.ByteString.Base64.URL as B64U

-- Right "PDw_Pj4=", with alphabet UrlPadded.
padded = B64U.parseBase64Padded "PDw_Pj4="

-- Right "PDw_Pj4", with alphabet UrlUnpadded.
unpadded = B64U.parseBase64Unpadded "PDw_Pj4"

-- Both return Right "<<?>>".
decodedPadded = fmap B64U.decodeBase64Padded padded
decodedUnpadded = fmap B64U.decodeBase64Unpadded unpadded
```

For a Boolean check with a specific padding policy, use
`isRight . B64U.parseBase64Padded` or `isRight . B64U.parseBase64Unpadded`.
To accept either policy, replace `isBase64Url input` with:

```haskell
isRight (B64U.parseBase64Padded input)
  || isRight (B64U.parseBase64Unpadded input)
```

To retain a typed result when either policy is accepted, handle each
successful parser result with `decodeBase64`, which accepts both URL types:

```haskell
decodeEitherPadding input =
  case B64U.parseBase64Padded input of
    Right encoded -> Right (B64U.decodeBase64 encoded)
    Left _ -> fmap B64U.decodeBase64 (B64U.parseBase64Unpadded input)
```

The parsers replace the shape-only `isValidBase64Url` check with canonical
validation. For example, `ZE==` and `ZE` now fail, whereas `Zg==` succeeds
only with the padded parser and `Zg` only with the unpadded parser.

## Text payloads

The Text parsers validate the Base64 encoding, not the UTF-8 validity of the
decoded payload. For example, `parseBase64 "/w=="` succeeds even though its
decoded byte is not valid UTF-8. Use a ByteString decoder for arbitrary binary
payloads, or `decodeBase64UntypedWith` with a checked text conversion such as
`decodeUtf8'` when you need to report conversion errors.

The same rule applies to the URL Text parsers: `parseBase64Padded "_w=="`
and `parseBase64Unpadded "_w"` succeed without decoding the payload as text.
The encoding and decoding APIs are unchanged.

## Enable SIMD optionally

Enable the manual Cabal flag for bundled ARM64 NEON or runtime-selected
x86-64 AVX2/SSSE3 kernels:

```sh
cabal build -fsimd
cabal test -fsimd --test-show-details=direct
```

For a project dependency, enable the flag in `cabal.project`:

```cabal
package base64
  flags: +simd
```

Unsupported targets and short inputs fall back to the scalar implementation.
No external C library is required. Padding checks, canonicity checks, and
decoding errors retain their existing behavior; lenient decoding remains
scalar. See the [README](README.md#optional-simd) for benchmark commands.

## Update package bounds

After migrating callers, update dependency bounds to admit 2.0, for example
`base64 >= 2.0 && < 2.1` or `base64 ^>= 2.0`. GHC 9.6 through 9.14 remains
the supported compiler range. Applications migrating from a pre-1.0 release
should also follow [MIGRATION-1.0.md](MIGRATION-1.0.md).
