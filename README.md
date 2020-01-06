# Base64

[![Build Status](https://travis-ci.com/emilypi/base64.svg?branch=master)](https://travis-ci.com/emilypi/base64)
[![Hackage](https://img.shields.io/hackage/v/base64.svg)](https://hackage.haskell.org/package/base64)

Padded and unpadded base64 and base64url encodings for `Text` and `ByteString` values, along with their optics.

For the companion `Prism`s and pattern synonyms, see [base64-lens](https://hackage.haskell.org/package/base64-lens).


### Summary

What does this library provide? Here is the summary:

- Better performance over existing Base64 libraries (2x and 3x for most use-cases - see [PERFORMANCE.md](benchmarks/PERFORMANCE.md))
- Support for unpadded Base64 and Base64-url
- Support for `Text` encodings and decodings
- Optics for handling more complex structures with Base64 representations via the `base64-lens` package

There are no dependencies aside from `base`:

![base64 dependencies](https://i.imgur.com/qynI5HM.png)

### Motivation

Haskell has two main libraries for Base64: `memory`, and `base64-bytestring`.

Of these, `memory` is geared towards integration with other memory primitives in the library, without much of an eye towards performance, while `base64-bytestring` is built to exclusively address `ByteString` encoding and decoding, and is very performant. Many great strides have been made in the realm of Base64 performance and vectorization just in the past 5 years, which this library attempts to capture. Additionally, we attempt to fix perceived shortcomings with both APIs in the support of unpadded Base64 and Base64-url support (which `memory` provides, but not `base64-bytestring`), as well as supporting `Text` values (neither libraries provide).
