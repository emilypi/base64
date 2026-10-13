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
