-- |
-- Module       : Data.ByteString.Short.Base64.URL
-- Copyright    : (c) 2019 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- This module contains the combinators implementing the
-- RFC 4648 specification for the Base64url encoding including
-- unpadded and lenient variants
--
module Data.ByteString.Short.Base64.URL
( encodeBase64
, encodeBase64'
, decodeBase64
, decodeBase64Lenient
, isBase64
, isValidBase64
) where


import Data.Bytestring.Base64.URL
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Either (isRight)
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Text.Short.Unsafe (fromShortByteStringUnsafe)

-- | Encode a 'ShortByteString' value as a Base64url 'Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
encodeBase64 :: ShortByteString -> ShortText
encodeBase64 = fromShortByteStringUnsafe . encodeBase64'
{-# INLINE encodeBase64 #-}

-- | Encode a 'ShortByteString' as a Base64url 'ShortByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
encodeBase64' :: ShortByteString -> ShortByteString
encodeBase64' = toShort . encodeBase64' . fromShort

-- | Decode a padded Base64url encoded 'ShortByteString' value. If its length is not a multiple
-- of 4, then padding chars will be added to fill out the input to a multiple of
-- 4 for safe decoding as Base64url-encoded values are optionally padded.
--
-- For a decoder that fails on unpadded input of incorrect size, use 'decodeBase64Unpadded'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64 :: ShortByteString -> Either Text ShortByteString
decodeBase64 = fmap toShort . decodeBase64 . fromShort

{-# INLINE decodeBase64 #-}

-- | Encode a 'ShortByteString' value as Base64url 'Text' without padding. Note that for Base64url,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base64url and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase64Unpadded :: ShortByteString -> ShortText
encodeBase64Unpadded = fromShortByteStringUnsafe . encodeBase64Unpadded'
{-# INLINE encodeBase64Unpadded #-}

-- | Encode a 'ShortByteString' value as Base64url without padding. Note that for Base64url,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base64url and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase64Unpadded' :: ShortByteString -> ShortByteString
encodeBase64Unpadded' = toShort . encodeBase64Unpadded' . fromShort

-- | Decode an unpadded Base64url-encoded 'ShortByteString' value. Input strings are
-- required to be unpadded, and will undergo validation prior to decoding to
-- confirm.
--
-- In general, unless unpadded Base64url is explicitly required, it is
-- safer to call 'decodeBase64'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64Unpadded :: ShortByteString -> Either Text ShortByteString
decodeBase64Unpadded = fmap toShort . decodeBase64Unpadded . fromShort
{-# INLINE decodeBase64Unpadded #-}

-- | Decode a padded Base64url-encoded 'ShortByteString' value. Input strings are
-- required to be correctly padded, and will be validated prior to decoding
-- to confirm.
--
-- In general, unless padded Base64url is explicitly required, it is
-- safer to call 'decodeBase64'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64Padded :: ShortByteString -> Either Text ShortByteString
decodeBase64Padded = fmap toShort . decodeBase64Padded . fromShort
{-# INLINE decodeBase64Padded #-}

-- | Leniently decode an unpadded Base64url-encoded 'ShortByteString'. This function
-- will not generate parse errors. If input data contains padding chars,
-- then the input will be parsed up until the first pad character.
--
-- __Note:__ This is not RFC 4648-compliant.
--
decodeBase64Lenient :: ShortByteString -> ShortByteString
decodeBase64Lenient = toShort . decodeBase64Lenient . fromShort
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether a 'ShortByteString' is Base64url-encoded.
--
isBase64Url :: ShortByteString -> Bool
isBase64Url bs = isValidBase64Url bs && isRight (decodeBase64 bs)
{-# INLINE isBase64Url #-}

-- | Tell whether a 'ShortByteString' is a valid Base64url format.
--
-- This will not tell you whether or not this is a correct Base64url representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base64 encoded 'ShortByteString' value, use 'isBase64Url'.
--
isValidBase64Url :: ShortByteString -> Bool
isValidBase64Url = isValidBase64 . fromShort
{-# INLINE isValidBase64Url #-}
