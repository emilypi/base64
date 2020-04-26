# Base64

[![Build Status](https://travis-ci.com/emilypi/base64.svg?branch=master)](https://travis-ci.com/emilypi/base64)
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

There are no dependencies aside from those bundled with GHC.

