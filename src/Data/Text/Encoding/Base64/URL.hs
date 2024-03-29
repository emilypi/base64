{-# LANGUAGE DataKinds #-}
-- |
-- Module       : Data.Text.Encoding.Base64.URL
-- Copyright    : (c) 2019-2023 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.Text.Text'-valued combinators for
-- implementing the RFC 4648 specification of the Base64url
-- encoding format. This includes strictly padded/unpadded and lenient decoding
-- variants, as well as internal and external validation for canonicity.
--
module Data.Text.Encoding.Base64.URL
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
  -- * Validation
, isBase64Url
, isValidBase64Url
) where


import Data.Base64.Types

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as B64U

import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Base64.Error


-- $setup
--
-- >>> import Data.Base64.Types
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
--

-- | Encode a 'Text' value in Base64url with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
-- === __Examples__:
--
-- >>> encodeBase64 "<<?>>"
-- "PDw_Pj4="
--
encodeBase64 :: Text -> Base64 'UrlPadded Text
encodeBase64 = B64U.encodeBase64 . T.encodeUtf8
{-# INLINE encodeBase64 #-}

-- | Decode a Base64url encoded 'Text' value, either padded or unpadded.
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
decodeBase64 :: UrlAlphabet k => Base64 k Text -> Text
decodeBase64 = T.decodeUtf8 . B64U.decodeBase64 . fmap T.encodeUtf8
{-# INLINE decodeBase64 #-}

-- | Decode an untyped Base64url encoded 'Text' value. If its length is not a multiple
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
decodeBase64Untyped :: Text -> Either Text Text
decodeBase64Untyped = fmap T.decodeUtf8
  . B64U.decodeBase64Untyped
  . T.encodeUtf8
{-# inline decodeBase64Untyped #-}

-- | Attempt to decode an untyped 'ByteString' value as Base64url, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- @
-- 'decodeBase64UntypedWith' 'T.decodeUtf8''
--   :: 'Text' -> 'Either' ('Base64Error' 'UnicodeException') 'Text'
-- @
--
decodeBase64UntypedWith
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base64Error err) Text
decodeBase64UntypedWith f t = case B64U.decodeBase64Untyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase64UntypedWith #-}

-- | Encode a 'Text' value in Base64url without padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
-- === __Examples__:
--
-- >>> encodeBase64Unpadded "<<?>>"
-- "PDw_Pj4"
--
encodeBase64Unpadded :: Text -> Base64 'UrlUnpadded Text
encodeBase64Unpadded = B64U.encodeBase64Unpadded . T.encodeUtf8
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
decodeBase64Unpadded :: Base64 'UrlUnpadded Text -> Text
decodeBase64Unpadded = T.decodeUtf8
  . B64U.decodeBase64Unpadded
  . fmap T.encodeUtf8
{-# INLINE decodeBase64Unpadded #-}

-- | Decode a unpadded, untyped Base64url-encoded 'Text' value. If its length is not a multiple
-- of 4, then padding chars will be added to fill out the input to a multiple of
-- 4 for safe decoding as base64url encodings are optionally padded.
--
-- For a decoder that fails on unpadded input, use 'decodeBase64PaddedUntyped'
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
decodeBase64UnpaddedUntyped :: Text -> Either Text Text
decodeBase64UnpaddedUntyped = fmap T.decodeUtf8
  . B64U.decodeBase64UnpaddedUntyped
  . T.encodeUtf8
{-# inline decodeBase64UnpaddedUntyped #-}


-- | Attempt to decode an untyped, unpadded 'ByteString' value as Base64url, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Example__:
--
-- @
-- 'decodeBase64UnpaddedUntypedWith' 'T.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base64Error' 'UnicodeException') 'Text'
-- @
--
decodeBase64UnpaddedUntypedWith
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base64Error err) Text
decodeBase64UnpaddedUntypedWith f t = case B64U.decodeBase64UnpaddedUntyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase64UnpaddedUntypedWith #-}

-- | Decode a padded Base64url encoded 'Text' value
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64Padded $ assertBase64 @'UrlPadded "PDw_Pj4="
-- "<<?>>"
--
decodeBase64Padded :: Base64 'UrlPadded Text -> Text
decodeBase64Padded = T.decodeUtf8
  . B64U.decodeBase64Padded
  . fmap T.encodeUtf8
{-# INLINE decodeBase64Padded #-}

-- | Decode an untyped, padded Base64url encoded 'Text' value
--
-- For a decoder that fails on padded input, use 'decodeBase64UnpaddedUntyped'
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64PaddedUntyped "PDw_Pj4="
-- Right "<<?>>"
--
decodeBase64PaddedUntyped :: Text -> Either Text Text
decodeBase64PaddedUntyped = fmap T.decodeUtf8
  . B64U.decodeBase64PaddedUntyped
  . T.encodeUtf8
{-# inline decodeBase64PaddedUntyped #-}

-- | Attempt to decode a padded, untyped 'ByteString' value as Base64url, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
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
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base64Error err) Text
decodeBase64PaddedUntypedWith f t = case B64U.decodeBase64PaddedUntyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase64PaddedUntypedWith #-}

-- | Leniently decode an untyped Base64url-encoded 'Text'. This function
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
decodeBase64Lenient :: Text -> Text
decodeBase64Lenient = T.decodeUtf8
    . B64U.decodeBase64Lenient
    . T.encodeUtf8
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether an untyped 'Text' value is Base64url-encoded.
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
isBase64Url :: Text -> Bool
isBase64Url = B64U.isBase64Url . T.encodeUtf8
{-# INLINE isBase64Url #-}

-- | Tell whether an untyped 'Text' value is a valid Base64url format.
--
-- This will not tell you whether or not this is a correct Base64url representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base64 encoded 'Text' value, use 'isBase64Url'.
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
isValidBase64Url :: Text -> Bool
isValidBase64Url = B64U.isValidBase64Url . T.encodeUtf8
{-# INLINE isValidBase64Url #-}
