# Migrating to base64 2.1

Version 2.1 replaces `isBase64Url` and `isValidBase64Url` with
`parseBase64Padded` and `parseBase64Unpadded`. The parsers validate the encoded
input and return a typed `Base64` value, or an error explaining why validation
failed. The old URL predicates are removed without a deprecation period.

The standard-alphabet `parseBase64` functions introduced in 2.0 are unchanged.
If upgrading from 1.x, also follow [MIGRATION-2.0.md](MIGRATION-2.0.md).
Encoding, decoding, compiler support, and the optional SIMD flag are unchanged.

## Affected modules

Both parsers are available for all six string representations:

- `Data.ByteString.Base64.URL`
- `Data.ByteString.Lazy.Base64.URL`
- `Data.ByteString.Short.Base64.URL`
- `Data.Text.Encoding.Base64.URL`
- `Data.Text.Lazy.Encoding.Base64.URL`
- `Data.Text.Short.Encoding.Base64.URL`

## Parse URL-safe input

Each module specializes these signatures to its string type:

```haskell
parseBase64Padded   :: a -> Either Text (Base64 'UrlPadded a)
parseBase64Unpadded :: a -> Either Text (Base64 'UrlUnpadded a)
```

The padded parser requires complete padding when needed. The unpadded parser
rejects padding. Empty input and complete four-character quanta without
padding are valid in both modes.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Base64.URL as B64U

-- Right "PDw_Pj4=", with alphabet UrlPadded.
padded = B64U.parseBase64Padded "PDw_Pj4="

-- Right "PDw_Pj4", with alphabet UrlUnpadded.
unpadded = B64U.parseBase64Unpadded "PDw_Pj4"

-- Both return Right "<<?>>".
decodedPadded = fmap B64U.decodeBase64Padded padded
decodedUnpadded = fmap B64U.decodeBase64Unpadded unpadded
```

Both parsers check the URL-safe alphabet, length, padding, and unused padding
bits. They validate the entire input before returning `Right` and preserve
the original encoded value, including lazy chunk boundaries. Errors match
the corresponding strict ByteString untyped decoder.

The Text parsers validate Base64 independently of the decoded payload's
UTF-8 validity. For example, `parseBase64Padded "_w=="` and
`parseBase64Unpadded "_w"` succeed even though the decoded byte is not valid
UTF-8. Use a ByteString decoder for binary payloads, or an untyped `With`
decoder with a checked conversion such as `decodeUtf8'`.

## Replace Boolean checks

Import `isRight` from `Data.Either`. When a specific padding policy is
required, use `isRight . B64U.parseBase64Padded` or
`isRight . B64U.parseBase64Unpadded`.

To accept either padding policy, replace `isBase64Url input` with:

```haskell
isRight (B64U.parseBase64Padded input)
  || isRight (B64U.parseBase64Unpadded input)
```

The parsers are stricter than the old shape-only `isValidBase64Url` check:

| Input | Padded parser | Unpadded parser |
| --- | --- | --- |
| `""` | Accept | Accept |
| `"Zm9v"` | Accept | Accept |
| `"Zg=="` | Accept | Reject padding |
| `"Zg"` | Require padding | Accept |
| `"Zg="` | Require complete padding | Reject padding |
| `"ZE=="` | Reject noncanonical bits | Reject padding |
| `"ZE"` | Require padding | Reject noncanonical bits |
| `"PDw/"` | Reject standard-alphabet character | Reject standard-alphabet character |

If your application intentionally accepts incomplete or noncanonical input,
keep that policy explicit; the parsers do not reproduce the permissive
behavior of `isValidBase64Url`.

To decode validated input when either padding policy is accepted,
`decodeBase64` consumes both URL alphabet types:

```haskell
decodeEitherPadding input =
  case B64U.parseBase64Padded input of
    Right encoded -> Right (B64U.decodeBase64 encoded)
    Left _ -> fmap B64U.decodeBase64 (B64U.parseBase64Unpadded input)
```

If only decoded bytes are needed, `B64U.decodeBase64Untyped` continues to
accept either padding policy directly.

## Update package bounds

After migrating callers, admit 2.1 with `base64 >= 2.1 && < 2.2` or
`base64 ^>= 2.1`. GHC 9.6 through 9.14 remains the supported compiler range.
