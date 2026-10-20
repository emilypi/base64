{-# LANGUAGE DataKinds #-}
-- |
-- Module       : Data.Text.Lazy.Encoding.Base64.URL
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.Text.Lazy.Text'-valued combinators for
-- implementing the RFC 4648 specification of the Base64url
-- encoding format. This includes strictly padded/unpadded and lenient decoding
-- variants, as well as internal and external validation for canonicity.
--
module Data.Text.Lazy.Encoding.Base64.URL
( -- * Encoding
  encodeBase64
, encodeBase64Unpadded
  -- * Decoding
, decodeBase64
, decodeBase64Untyped
, decodeBase64UntypedWith
, decodeBase64Unpadded
, decodeBase64UnpaddedUntyped
, decodeBase64UnpaddedUntypedWith
, decodeBase64Padded
, decodeBase64PaddedUntyped
, decodeBase64PaddedUntypedWith
, decodeBase64Lenient
  -- * Parsing
, parseBase64Padded
, parseBase64Unpadded
) where

import Data.Base64.Types

import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Base64.URL as BL64U

import qualified Data.Text as T
import Data.Text.Encoding.Base64.Error
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL



-- $setup
--
-- >>> import Data.Base64.Types
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
--

-- | Encode a 'TL.Text' value in Base64url with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
-- === __Examples__:
--
-- >>> encodeBase64 "<<?>>"
-- "PDw_Pj4="
--
encodeBase64 :: TL.Text -> Base64 'UrlPadded TL.Text
encodeBase64 = BL64U.encodeBase64 . TL.encodeUtf8
{-# INLINE encodeBase64 #-}

-- | Decode an arbitrarily Base64url-encoded 'TL.Text' value.
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
decodeBase64 :: UrlAlphabet k => Base64 k TL.Text -> TL.Text
decodeBase64 = TL.decodeUtf8 . BL64U.decodeBase64 . fmap TL.encodeUtf8
{-# INLINE decodeBase64 #-}

-- | Decode an untyped Base64url-encoded 'TL.Text' value. If its length is not a multiple
-- of 4, then padding chars will be added to fill out the input to a multiple of
-- 4 for safe decoding as base64url encodings are optionally padded.
--
-- For a decoder that fails on unpadded input, use 'decodeBase64Unpadded'.
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
decodeBase64Untyped :: TL.Text -> Either T.Text TL.Text
decodeBase64Untyped = fmap TL.decodeUtf8
  . BL64U.decodeBase64Untyped
  . TL.encodeUtf8
{-# INLINE decodeBase64Untyped #-}

-- | Attempt to decode an untyped lazy 'ByteString' value as Base64url, converting from
-- 'ByteString' to 'TL.Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- @
-- 'decodeBase64With' 'TL.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base64Error' 'UnicodeException') 'TL.Text'
-- @
--
decodeBase64UntypedWith
    :: (ByteString -> Either err TL.Text)
      -- ^ convert a bytestring to text (e.g. 'TL.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base64Error err) TL.Text
decodeBase64UntypedWith f t = case BL64U.decodeBase64Untyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase64UntypedWith #-}

-- | Encode a 'TL.Text' value in Base64url without padding. Note that for Base64url,
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
encodeBase64Unpadded :: TL.Text -> Base64 'UrlUnpadded TL.Text
encodeBase64Unpadded = BL64U.encodeBase64Unpadded . TL.encodeUtf8
{-# INLINE encodeBase64Unpadded #-}

-- | Decode an unpadded Base64url encoded 'Text' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64Unpadded $ assertBase64 @'UrlUnpadded "PDw_Pj4"
-- "<<?>>"
--
decodeBase64Unpadded :: Base64 'UrlUnpadded TL.Text -> TL.Text
decodeBase64Unpadded = TL.decodeUtf8
  . BL64U.decodeBase64Unpadded
  . fmap TL.encodeUtf8
{-# INLINE decodeBase64Unpadded #-}

-- | Decode an unpadded, untyped Base64url encoded 'TL.Text' value.
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
decodeBase64UnpaddedUntyped :: TL.Text -> Either T.Text TL.Text
decodeBase64UnpaddedUntyped = fmap TL.decodeUtf8
    . BL64U.decodeBase64UnpaddedUntyped
    . TL.encodeUtf8
{-# INLINE decodeBase64UnpaddedUntyped #-}

-- | Attempt to decode an unpadded, untyped lazy 'ByteString' value as Base64url, converting from
-- 'ByteString' to 'TL.Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- @
-- 'decodeBase64UnpaddedUntypedWith' 'TL.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base64Error' 'UnicodeException') 'TL.Text'
-- @
--
decodeBase64UnpaddedUntypedWith
    :: (ByteString -> Either err TL.Text)
      -- ^ convert a bytestring to text (e.g. 'TL.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base64Error err) TL.Text
decodeBase64UnpaddedUntypedWith f t = case BL64U.decodeBase64UnpaddedUntyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase64UnpaddedUntypedWith #-}

-- | Decode a padded Base64url encoded 'TL.Text' value
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64Padded $ assertBase64 @'UrlPadded "PDw_Pj4="
-- "<<?>>"
--
decodeBase64Padded :: Base64 'UrlPadded TL.Text -> TL.Text
decodeBase64Padded = TL.decodeUtf8
  . BL64U.decodeBase64Padded
  . fmap TL.encodeUtf8
{-# INLINE decodeBase64Padded #-}

-- | Decode an untyped, padded Base64url encoded 'Text' value
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64PaddedUntyped "PDw_Pj4="
-- Right "<<?>>"
--
decodeBase64PaddedUntyped :: TL.Text -> Either T.Text TL.Text
decodeBase64PaddedUntyped = fmap TL.decodeUtf8
  . BL64U.decodeBase64PaddedUntyped
  . TL.encodeUtf8
{-# inline decodeBase64PaddedUntyped #-}

-- | Attempt to decode a padded, untyped lazy 'ByteString' value as Base64url, converting from
-- 'ByteString' to 'TL.Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Example__:
--
-- @
-- 'decodeBase64PaddedWith' 'T.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base64Error' 'UnicodeException') 'Text'
-- @
--
decodeBase64PaddedUntypedWith
    :: (ByteString -> Either err TL.Text)
      -- ^ convert a bytestring to text (e.g. 'TL.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base64Error err) TL.Text
decodeBase64PaddedUntypedWith f t = case BL64U.decodeBase64PaddedUntyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase64PaddedUntypedWith #-}

-- | Leniently decode an untyped Base64url-encoded 'TL.Text'. This function
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
decodeBase64Lenient :: TL.Text -> TL.Text
decodeBase64Lenient = TL.decodeUtf8
    . BL64U.decodeBase64Lenient
    . TL.encodeUtf8
{-# INLINE decodeBase64Lenient #-}

-- | Parse a canonical, padded Base64url-encoded 'TL.Text' value.
--
-- Checks the URL-safe alphabet, length, padding, and unused padding bits.
-- Returns the original encoded value wrapped in 'Base64', ready for
-- 'decodeBase64Padded' or 'decodeBase64'. Errors match the strict ByteString
-- 'Data.ByteString.Base64.URL.decodeBase64PaddedUntyped' decoder.
-- The input is neither decoded nor normalized in the returned value.
--
-- The entire input is validated before returning 'Right'; the original
-- chunk boundaries are preserved in the result.
--
-- This validates the Base64url encoding, independently of whether the
-- decoded payload is valid UTF-8.
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
-- @since 2.1
--
parseBase64Padded :: TL.Text -> Either T.Text (Base64 'UrlPadded TL.Text)
parseBase64Padded bs = assertBase64 bs <$ BL64U.parseBase64Padded (TL.encodeUtf8 bs)
{-# INLINE parseBase64Padded #-}

-- | Parse a canonical, unpadded Base64url-encoded 'TL.Text' value.
--
-- Checks the URL-safe alphabet, length, padding, and unused padding bits.
-- Returns the original encoded value wrapped in 'Base64', ready for
-- 'decodeBase64Unpadded' or 'decodeBase64'. Errors match the strict ByteString
-- 'Data.ByteString.Base64.URL.decodeBase64UnpaddedUntyped' decoder.
-- The input is neither decoded nor normalized in the returned value.
--
-- The entire input is validated before returning 'Right'; the original
-- chunk boundaries are preserved in the result.
--
-- This validates the Base64url encoding, independently of whether the
-- decoded payload is valid UTF-8.
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
-- @since 2.1
--
parseBase64Unpadded :: TL.Text -> Either T.Text (Base64 'UrlUnpadded TL.Text)
parseBase64Unpadded bs = assertBase64 bs <$ BL64U.parseBase64Unpadded (TL.encodeUtf8 bs)
{-# INLINE parseBase64Unpadded #-}
