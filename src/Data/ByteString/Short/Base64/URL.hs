{-# LANGUAGE DataKinds #-}
-- |
-- Module       : Data.ByteString.Short.Base64.URL
-- Copyright    : (c) 2019-2023 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.Short.ShortByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base64url
-- encoding format. This includes strictly padded/unpadded and lenient decoding
-- variants, as well as internal and external validation for canonicity.
--
module Data.ByteString.Short.Base64.URL
( -- * Encoding
  encodeBase64
, encodeBase64'
, encodeBase64Unpadded
, encodeBase64Unpadded'
  -- * Decoding
, decodeBase64
, decodeBase64Untyped
, decodeBase64Unpadded
, decodeBase64UnpaddedUntyped
, decodeBase64Padded
, decodeBase64PaddedUntyped
, decodeBase64Lenient
  -- * Validation
, isBase64Url
, isValidBase64Url
) where


import Data.Base64.Types

import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Text.Short.Unsafe (fromShortByteStringUnsafe)


-- $setup
--
-- >>> import Data.Base64.Types
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
--

-- | Encode a 'ShortByteString' value as a Base64url 'Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
-- === __Examples__:
--
-- >>> encodeBase64 "<<?>>"
-- "PDw_Pj4="
--
encodeBase64 :: ShortByteString -> Base64 'UrlPadded ShortText
encodeBase64 = fmap fromShortByteStringUnsafe . encodeBase64'
{-# INLINE encodeBase64 #-}

-- | Encode a 'ShortByteString' as a Base64url 'ShortByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
-- === __Examples__:
--
-- >>> encodeBase64' "<<?>>"
-- "PDw_Pj4="
--
encodeBase64' :: ShortByteString -> Base64 'UrlPadded ShortByteString
encodeBase64' = fmap toShort . B64U.encodeBase64' . fromShort

-- | Decode a Base64url encoded 'ShortByteString' value, either padded or unpadded.
-- The correct decoding function is dispatched based on the existence of padding.
--
-- For typed values:
--   - If a padded value is required, use 'decodeBase64Padded'
--   - If an unpadded value is required, use 'decodeBase64Unpadded'
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64 $ assertBase64 @'UrlPadded "PDw_Pj4="
-- "<<?>>"
--
-- >>> decodeBase64 $ assertBase64 @'UrlUnpadded "PDw_Pj4"
-- "<<?>>"
--
decodeBase64
  :: UrlAlphabet k
  => Base64 k ShortByteString
  -> ShortByteString
decodeBase64 = toShort . B64U.decodeBase64 . fmap fromShort

-- | Decode an untyped Base64url encoded 'ByteString' value. If its length is not a multiple
-- of 4, then padding chars will be added to fill out the input to a multiple of
-- 4 for safe decoding as Base64url-encoded values are optionally padded.
--
-- For a decoder that fails to decode untyped values of incorrect size:
--   - If a padded value is required, use 'decodeBase64PaddedUntyped'
--   - If an unpadded value is required, use 'decodeBase64UnpaddedUntyped'
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64Untyped "PDw_Pj4="
-- Right "<<?>>"
--
-- >>> decodeBase64Untyped "PDw_Pj4"
-- Right "<<?>>"
--
-- >>> decodeBase64Untyped "PDw-Pg="
-- Left "Base64-encoded bytestring has invalid padding"
--
-- >>> decodeBase64Untyped "PDw-Pg"
-- Right "<<>>"
--
decodeBase64Untyped :: ShortByteString -> Either Text ShortByteString
decodeBase64Untyped = fmap toShort . B64U.decodeBase64Untyped . fromShort
{-# inline decodeBase64Untyped #-}

-- | Encode a 'ShortByteString' value as Base64url 'Text' without padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
-- === __Examples__:
--
-- >>> encodeBase64Unpadded "<<?>>"
-- "PDw_Pj4"
--
encodeBase64Unpadded :: ShortByteString -> Base64 'UrlUnpadded ShortText
encodeBase64Unpadded = fmap fromShortByteStringUnsafe . encodeBase64Unpadded'
{-# INLINE encodeBase64Unpadded #-}

-- | Encode a 'ShortByteString' value as Base64url without padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
-- === __Examples__:
--
-- >>> encodeBase64Unpadded' "<<?>>"
-- "PDw_Pj4"
--
encodeBase64Unpadded' :: ShortByteString -> Base64 'UrlUnpadded ShortByteString
encodeBase64Unpadded' = fmap toShort . B64U.encodeBase64Unpadded' . fromShort

-- | Decode an unpadded Base64url-encoded 'ShortByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64Unpadded $ assertBase64 @'UrlUnpadded "PDw_Pj4"
-- "<<?>>"
--
decodeBase64Unpadded :: Base64 'UrlUnpadded ShortByteString -> ShortByteString
decodeBase64Unpadded = toShort . B64U.decodeBase64Unpadded . fmap fromShort
{-# INLINE decodeBase64Unpadded #-}

-- | Decode an unpadded, untyped Base64url encoded 'ByteString' value.
-- If its length is not a multiple of 4, then padding chars will be added
-- to fill out the input to a multiple of 4 for safe decoding as
-- Base64url-encoded values are optionally padded.
--
-- In general, unless unpadded Base64url is explicitly required, it is
-- safer to call 'decodeBase64'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64UnpaddedUntyped "PDw_Pj4"
-- Right "<<?>>"
--
-- >>> decodeBase64UnpaddedUntyped "PDw-Pg="
-- Left "Base64-encoded bytestring has invalid padding"
--
-- >>> decodeBase64UnpaddedUntyped "PDw-Pg"
-- Right "<<>>"
--
decodeBase64UnpaddedUntyped :: ShortByteString -> Either Text ShortByteString
decodeBase64UnpaddedUntyped = fmap toShort
  . B64U.decodeBase64UnpaddedUntyped
  . fromShort
{-# inline decodeBase64UnpaddedUntyped #-}

-- | Decode a padded Base64url-encoded 'ShortByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64Padded $ assertBase64 @'UrlPadded "PDw_Pj4="
-- "<<?>>"
--
decodeBase64Padded :: Base64 'UrlPadded ShortByteString -> ShortByteString
decodeBase64Padded = toShort . B64U.decodeBase64Padded . fmap fromShort
{-# INLINE decodeBase64Padded #-}

-- | Decode a padded, untyped Base64url encoded 'ByteString' value.
--
-- For a decoder that fails on unpadded input of incorrect size,
-- use 'decodeBase64UnpaddedUntyped'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64PaddedUntyped "PDw_Pj4="
-- Right "<<?>>"
--
-- >>> decodeBase64PaddedUntyped "PDw_Pj4"
-- Left "Base64-encoded bytestring requires padding"
--
decodeBase64PaddedUntyped :: ShortByteString -> Either Text ShortByteString
decodeBase64PaddedUntyped = fmap toShort
  . B64U.decodeBase64PaddedUntyped
  . fromShort
{-# inline decodeBase64PaddedUntyped #-}

-- | Leniently decode an unpadded, untyped Base64url-encoded 'ShortByteString'. This function
-- will not generate parse errors. If input data contains padding chars,
-- then the input will be parsed up until the first pad character.
--
-- __Note:__ This is not RFC 4648-compliant.
--
-- === __Examples__:
--
-- >>> decodeBase64Lenient "PDw_Pj4="
-- "<<?>>"
--
-- >>> decodeBase64Lenient "PDw_%%%$}Pj4"
-- "<<?>>"
--
decodeBase64Lenient :: ShortByteString -> ShortByteString
decodeBase64Lenient = toShort . B64U.decodeBase64Lenient . fromShort
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether an untyped 'ShortByteString' is Base64url-encoded.
--
-- === __Examples__:
--
-- >>> isBase64Url "PDw_Pj4="
-- True
--
-- >>> isBase64Url "PDw_Pj4"
-- True
--
-- >>> isBase64Url "PDw_Pj"
-- False
--
isBase64Url :: ShortByteString -> Bool
isBase64Url = B64U.isBase64Url . fromShort
{-# INLINE isBase64Url #-}

-- | Tell whether an untyped 'ShortByteString' is a valid Base64url format.
--
-- This will not tell you whether or not this is a correct Base64url representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base64 encoded 'ShortByteString' value, use 'isBase64Url'.
--
-- === __Examples__:
--
-- >>> isValidBase64Url "PDw_Pj4="
-- True
--
-- >>> isValidBase64Url "PDw_Pj"
-- True
--
-- >>> isValidBase64Url "%"
-- False
--
isValidBase64Url :: ShortByteString -> Bool
isValidBase64Url = B64U.isValidBase64Url . fromShort
{-# INLINE isValidBase64Url #-}
