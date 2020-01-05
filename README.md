# Base64

[![Build Status](https://travis-ci.com/emilypi/base64.svg?branch=master)](https://travis-ci.com/emilypi/base64)

Padded and unpadded base64 and base64url encodings for `Text` and `ByteString` values, along with their optics.


### Why?

Haskell has two main libraries for Base64: `memory`, and `base64-bytestring`.

Of these, `memory` is geared towards integration with other memory primitives in the library, without much of an eye towards performance, while `base64-bytestring` is built to exclusively address `ByteString` encoding and decoding, and is very performant. Many great strides have been made in the realm of Base64 performance and vectorization just in the past 5 years, which this library attempts to capture. Additionally, we attempt to fix percieved shortcomings with both APIs in the support of unpadded Base64 and Base64-url support (which `memory` provides, but not `base64-bytestring`), as well as supporting `Text` values (neither libraries provide), and also supplying some nice compositional optics for composing structures with Base64-encodable/decodable focii (neither libraries provide).

### Optics

Structs may want to support encoding and decoding their substructures, which is supported with the following prismatic typeclass:

```haskell
class AsBase64 s where
    type Base64 s
    -- | A prism into a base64-encoded focus of
    -- some type
    --
    _Base64 :: Prism' s (Base64 s)

    -- | A prism into the base64url-encoded focus of
    -- some type
    --
    _Base64Url :: Prism' s (Base64 s)
```

The data of a `Prism` naturally conforms to this "encoding/decoding" dichotomy, where the `Review`, or "builder" half of the `Prism` of type `b -> t` is an encoding, and the "Matcher" half of the prism, of type `s -> Either t a`, represents a decoding of a similar structure. Monomorphizing for `t ~ s` and `a ~ b`, a simple `Prism` is formed:

```haskell
>>> _Base64 @Text # "<<???>>"
"PDw/Pz8+Pg=="

>>> "PDw/Pz8+Pg==" ^? _Base64 @Text
Just "<<???>>"
```

The two most obvious types for which we have an instance are those that are supported natively by the library: `ByteString` and `Text`. Trivially, their instances consist of the functions provided here in this library.

### Summary

What does this library provide? Here is the summary:

- Better performance over existing Base64 libraries (2x and 3x for most use-cases - see [PERFORMANCE.md](benchmarks/PERFORMANCE.md))
- Support for unpadded Base64 and Base64-url
- Support for `Text` encodings and decodings
- Classy optics for handling more complex structures with Base64 representations
