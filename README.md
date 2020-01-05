# Base64

[![Build Status](https://travis-ci.com/emilypi/base64.svg?branch=master)](https://travis-ci.com/emilypi/base64)
[![Hackage](https://img.shields.io/hackage/v/base64.svg)](https://hackage.haskell.org/package/base64)

Padded and unpadded base64 and base64url encodings for `Text` and `ByteString` values, along with their optics.


### Why?

Haskell has two main libraries for Base64: `memory`, and `base64-bytestring`.

Of these, `memory` is geared towards integration with other memory primitives in the library, without much of an eye towards performance, while `base64-bytestring` is built to exclusively address `ByteString` encoding and decoding, and is very performant. Many great strides have been made in the realm of Base64 performance and vectorization just in the past 5 years, which this library attempts to capture. Additionally, we attempt to fix percieved shortcomings with both APIs in the support of unpadded Base64 and Base64-url support (which `memory` provides, but not `base64-bytestring`), as well as supporting `Text` values (neither libraries provide), and also supplying some nice compositional optics for composing structures with Base64-encodable/decodable focii (neither libraries provide).

### Optics

`Prism`s for encoding and decoding `Text` and `ByteString` values are given as part of the library:


```haskell
_Base64 :: Prism' ByteString ByteString
_Base64Url :: Prism' ByteString ByteString
_Base64Unpadded :: Prism' ByteString ByteString
_Base64UrlUnpadded :: Prism' ByteString ByteString

-- and

_Base64Text :: Prism' Text Text
_Base64UrlText :: Prism' Text Text
_Base64UnpaddedText :: Prism' Text Text
_Base64UrlUnpaddedText :: Prism' Text Text

```

If a particular structure has a `Lens` into some `Text` or `ByteString` value they might want to encode (or decode), then composing such a `Lens` with these `Prisms` yields an affine `Traversal`, resulting in a structure which has the focus of its `Lens` encoded as or decoded from Base64(-url). All one needs to do is compose their optics:

```haskell

myB64Struct :: Traversal' s Text
myB64Struct = myLens . _Base64

```

The data of a `Prism` naturally conforms to this "encoding/decoding" dichotomy, where the `Review`, or "builder" half of the `Prism` of type `b -> t` is an encoding, and the "Matcher" half of the prism, of type `s -> Either t a`, represents a decoding of a similar structure. Hence, `Prism` is the most appropriate structure.

### Summary

What does this library provide? Here is the summary:

- Better performance over existing Base64 libraries (2x and 3x for most use-cases - see [PERFORMANCE.md](benchmarks/PERFORMANCE.md))
- Support for unpadded Base64 and Base64-url
- Support for `Text` encodings and decodings
- Classy optics for handling more complex structures with Base64 representations
