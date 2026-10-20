{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module       : Data.ByteString.Base64.URL
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.ByteString'-valued combinators for
-- implementing the RFC 4648 specification of the url-safe Base64 (Base64url)
-- encoding format. This includes strictly padded/unpadded and lenient decoding
-- variants, as well as internal and external validation for canonicity.
--
module Data.ByteString.Base64.URL
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
  -- * Parsing
, parseBase64Padded
, parseBase64Unpadded
) where


import Data.Base64.Types
import Data.Base64.Types.Internal

import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Base64.Internal
import Data.ByteString.Base64.Internal.Head
import Data.ByteString.Base64.Internal.Tables
import Data.Text (Text)
import qualified Data.Text.Encoding as T

import System.IO.Unsafe


-- $setup
--
-- >>> import Data.Base64.Types
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
--

-- | Encode a 'ByteString' value as a Base64url 'Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
-- === __Examples__:
--
-- >>> encodeBase64 "<<?>>"
-- "PDw_Pj4="
--
encodeBase64 :: ByteString -> Base64 'UrlPadded Text
encodeBase64 = fmap T.decodeUtf8 . encodeBase64'
{-# INLINE encodeBase64 #-}

-- | Encode a 'ByteString' as a Base64url 'ByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
-- === __Examples__:
--
-- >>> encodeBase64' "<<?>>"
-- "PDw_Pj4="
--
encodeBase64' :: ByteString -> Base64 'UrlPadded ByteString
encodeBase64' = assertBase64 . encodeBase64_ base64UrlTable

-- | Decode a Base64url encoded 'ByteString' value, either padded or unpadded.
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
  => Base64 k ByteString
  -> ByteString
decodeBase64 b64@(Base64 bs)
  | not $ BS.null bs, BS.last bs == 0x3d = decodeBase64Padded $ coerceBase64 b64
  | otherwise = decodeBase64Unpadded $ coerceBase64 b64
{-# inline decodeBase64 #-}

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
decodeBase64Untyped :: ByteString -> Either Text ByteString
decodeBase64Untyped bs@(PS _ _ !l)
  | l == 0 = Right mempty
  | r == 0 = unsafeDupablePerformIO $ decodeBase64_ decodeB64UrlTable bs
  | r == 2 = unsafeDupablePerformIO $ decodeBase64_ decodeB64UrlTable $ BS.append bs "=="
  | r == 3 = validateLastPad bs $ decodeBase64_ decodeB64UrlTable $ BS.append bs "="
  | otherwise = Left "Base64-encoded bytestring has invalid size"
  where
    !r = l `rem` 4
{-# INLINE decodeBase64Untyped #-}

-- | Encode a 'ByteString' value as Base64url 'Text' without padding. Note that for Base64url,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base64url and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
-- === __Examples__:
--
-- >>> encodeBase64Unpadded "<<?>>"
-- "PDw_Pj4"
--
encodeBase64Unpadded :: ByteString -> Base64 'UrlUnpadded Text
encodeBase64Unpadded = fmap T.decodeUtf8 . encodeBase64Unpadded'
{-# INLINE encodeBase64Unpadded #-}

-- | Encode a 'ByteString' value as Base64url without padding. Note that for Base64url,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base64url and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
-- === __Examples__:
--
-- >>> encodeBase64Unpadded' "<<?>>"
-- "PDw_Pj4"
--
encodeBase64Unpadded' :: ByteString -> Base64 'UrlUnpadded ByteString
encodeBase64Unpadded' = assertBase64 . encodeBase64Nopad_ base64UrlTable

-- | Decode an unpadded Base64url-encoded 'ByteString' value. Input strings are
-- required to be unpadded, and will undergo validation prior to decoding to
-- confirm.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64Unpadded $ assertBase64 @'UrlUnpadded "PDw_Pj4"
-- "<<?>>"
--
decodeBase64Unpadded :: Base64 'UrlUnpadded ByteString -> ByteString
decodeBase64Unpadded b64@(Base64 (PS _ _ !l))
    | r == 2 = decodeBase64Padded $ coerceBase64 $ (`BS.append` "==") <$> b64
    | r == 3 = decodeBase64Padded $ coerceBase64 $ (`BS.append` "=") <$> b64
    | otherwise = decodeBase64Padded $ coerceBase64 b64
  where
    !r = l `rem` 4

-- | Decode a padded, untyped Base64url-encoded 'ByteString' value. Input strings are
-- required to be correctly padded, and will be validated prior to decoding
-- to confirm.
--
-- In general, unless padded Base64url is explicitly required, it is
-- safer to call 'decodeBase64Untyped'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64PaddedUntyped "PDw_Pj4="
-- Right "<<?>>"
--
decodeBase64PaddedUntyped :: ByteString -> Either Text ByteString
decodeBase64PaddedUntyped bs@(PS _ _ !l)
    | l == 0 = Right mempty
    | r == 1 = Left "Base64-encoded bytestring has invalid size"
    | r /= 0 = Left "Base64-encoded bytestring requires padding"
    | otherwise = unsafeDupablePerformIO $ decodeBase64_ decodeB64UrlTable bs
  where
    !r = l `rem` 4
{-# INLINE decodeBase64PaddedUntyped #-}

-- | Decode a padded Base64url-encoded 'ByteString' value. Input strings are
-- required to be correctly padded, and will be validated prior to decoding
-- to confirm.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64Padded $ assertBase64 @'UrlPadded "PDw_Pj4="
-- "<<?>>"
--
decodeBase64Padded :: Base64 'UrlPadded ByteString -> ByteString
decodeBase64Padded = decodeBase64Typed_ decodeB64UrlTable
{-# INLINE decodeBase64Padded #-}

-- | Decode an unpadded, untyped Base64url-encoded 'ByteString' value. Input strings are
-- required to be unpadded, and will undergo validation prior to decoding to
-- confirm.
--
-- In general, unless unpadded Base64url is explicitly required, it is
-- safer to call 'decodeBase64Untyped'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64UnpaddedUntyped "PDw_Pj4"
-- Right "<<?>>"
--
-- >>> decodeBase64UnpaddedUntyped "PDw_Pj4="
-- Left "Base64-encoded bytestring has invalid padding"
--
decodeBase64UnpaddedUntyped :: ByteString -> Either Text ByteString
decodeBase64UnpaddedUntyped bs@(PS _ _ !l)
    | l == 0 = Right mempty
    | r == 0 = validateLastPad bs $ decodeBase64_ decodeB64UrlTable bs
    | r == 2 = validateLastPad bs $ decodeBase64_ decodeB64UrlTable $ BS.append bs "=="
    | r == 3 = validateLastPad bs $ decodeBase64_ decodeB64UrlTable $ BS.append bs "="
    | otherwise = Left "Base64-encoded bytestring has invalid size"
  where
    !r = l `rem` 4
{-# INLINE decodeBase64UnpaddedUntyped #-}

-- | Leniently decode an unpadded, untyped Base64url-encoded 'ByteString'. This function
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
decodeBase64Lenient :: ByteString -> ByteString
decodeBase64Lenient = decodeBase64Lenient_ decodeB64UrlTable
{-# INLINE decodeBase64Lenient #-}

-- | Parse a canonical, padded Base64url-encoded 'ByteString' value.
--
-- Checks the URL-safe alphabet, length, padding, and unused padding bits.
-- Returns the original encoded value wrapped in 'Base64', ready for
-- 'decodeBase64Padded' or 'decodeBase64'. Errors match the strict ByteString
-- 'Data.ByteString.Base64.URL.decodeBase64PaddedUntyped' decoder.
-- The input is neither decoded nor normalized in the returned value.
--
-- === __Examples__:
--
-- >>> parseBase64Padded "PDw_Pj4="
-- Right "PDw_Pj4="
--
-- >>> fmap decodeBase64Padded (parseBase64Padded "PDw_Pj4=")
-- Right "<<?>>"
--
-- >>> parseBase64Padded "PDw_Pj4"
-- Left "Base64-encoded bytestring requires padding"
--
-- >>> parseBase64Padded "ZE=="
-- Left "non-canonical encoding detected at offset: 1"
--
-- @since 2.0
--
parseBase64Padded :: ByteString -> Either Text (Base64 'UrlPadded ByteString)
parseBase64Padded bs = assertBase64 bs <$ decodeBase64PaddedUntyped bs
{-# INLINE parseBase64Padded #-}

-- | Parse a canonical, unpadded Base64url-encoded 'ByteString' value.
--
-- Checks the URL-safe alphabet, length, padding, and unused padding bits.
-- Returns the original encoded value wrapped in 'Base64', ready for
-- 'decodeBase64Unpadded' or 'decodeBase64'. Errors match the strict ByteString
-- 'Data.ByteString.Base64.URL.decodeBase64UnpaddedUntyped' decoder.
-- The input is neither decoded nor normalized in the returned value.
--
-- === __Examples__:
--
-- >>> parseBase64Unpadded "PDw_Pj4"
-- Right "PDw_Pj4"
--
-- >>> fmap decodeBase64Unpadded (parseBase64Unpadded "PDw_Pj4")
-- Right "<<?>>"
--
-- >>> parseBase64Unpadded "PDw_Pj4="
-- Left "Base64-encoded bytestring has invalid padding"
--
-- >>> parseBase64Unpadded "ZE"
-- Left "non-canonical encoding detected at offset: 1"
--
-- @since 2.0
--
parseBase64Unpadded :: ByteString -> Either Text (Base64 'UrlUnpadded ByteString)
parseBase64Unpadded bs = assertBase64 bs <$ decodeBase64UnpaddedUntyped bs
{-# INLINE parseBase64Unpadded #-}
