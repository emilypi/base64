{-# LANGUAGE DataKinds #-}
-- |
-- Module       : Data.Text.Short.Encoding.Base64
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.Text.Short.ShortText'-valued combinators
-- implementing the RFC 4648 specification for the Base64
-- encoding format. This includes lenient decoding variants, and
-- external + internal validations for canonicity.
--
module Data.Text.Short.Encoding.Base64
( -- * Encoding
  encodeBase64
  -- * Decoding
, decodeBase64
, decodeBase64Untyped
, decodeBase64UntypedWith
, decodeBase64Lenient
  -- * Parsing
, parseBase64
) where

import Data.Base64.Types

import Data.Bifunctor (first)
import Data.ByteString.Short (ShortByteString, fromShort)
import qualified Data.ByteString.Short.Base64 as BS64
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Base64 as B64T
import Data.Text.Encoding.Base64.Error
import Data.Text.Short


-- $setup
--
-- >>> import Data.Base64.Types
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
--

-- | Encode a 'ShortText' value in Base64 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> encodeBase64 "Sun"
-- "U3Vu"
--
encodeBase64 :: ShortText -> Base64 'StdPadded ShortText
encodeBase64 = BS64.encodeBase64 . toShortByteString
{-# INLINE encodeBase64 #-}

-- | Decode a padded Base64-encoded 'ShortText' value
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64 $ assertBase64 @'StdPadded "U3Vu"
-- "Sun"
--
decodeBase64 :: StdAlphabet k => Base64 k ShortText -> ShortText
decodeBase64 = decodeShortUtf8 . BS64.decodeBase64 . fmap toShortByteString
{-# INLINE decodeBase64 #-}

-- | Decode a padded Base64-encoded 'ShortText' value
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64Untyped "U3Vu"
-- Right "Sun"
--
-- >>> decodeBase64Untyped "U3V"
-- Left "Base64-encoded bytestring requires padding"
--
-- >>> decodeBase64Untyped "U3V="
-- Left "non-canonical encoding detected at offset: 2"
--
decodeBase64Untyped :: ShortText -> Either Text ShortText
decodeBase64Untyped = fmap decodeShortUtf8 . BS64.decodeBase64Untyped . toShortByteString
{-# INLINE decodeBase64Untyped #-}

-- | Attempt to decode an untyped 'ShortByteString' value as Base64, converting from
-- 'ByteString' to 'ShortText' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Example__:
--
-- @
-- 'decodeBase64UntypedWith' 'T.decodeUtf8''
--   :: 'ShortByteString' -> 'Either' ('Base64Error' 'UnicodeException') 'ShortText'
-- @
--
decodeBase64UntypedWith
    :: (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ShortByteString
      -- ^ Input text to decode
    -> Either (Base64Error err) ShortText
decodeBase64UntypedWith f t = case BS64.decodeBase64Untyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase64UntypedWith #-}

-- | Leniently decode an untyped Base64-encoded 'ShortText' value. This function
-- will not generate parse errors. If input data contains padding chars,
-- then the input will be parsed up until the first pad character.
--
-- __Note:__ This is not RFC 4648-compliant.
--
-- === __Examples__:
--
-- >>> decodeBase64Lenient "U3Vu"
-- "Sun"
--
-- >>> decodeBase64Lenient "U3V"
-- "Su"
--
-- >>> decodeBase64Lenient "U3V="
-- "Su"
--
decodeBase64Lenient :: ShortText -> ShortText
decodeBase64Lenient = fromText . B64T.decodeBase64Lenient . toText
{-# INLINE decodeBase64Lenient #-}

-- | Parse a canonical, padded Base64-encoded 'ShortText' value.
--
-- Checks the alphabet, length, padding, and unused padding bits. On success,
-- returns the original encoded value wrapped in 'Base64', ready for
-- 'decodeBase64'. On failure, returns the same error as 'decodeBase64Untyped'.
-- The input is neither decoded nor normalized in the returned value.
--
-- This validates the Base64 encoding, not the UTF-8 validity of the decoded
-- bytes. Use 'decodeBase64UntypedWith' to check the decoded text encoding.
--
-- === __Examples__:
--
-- >>> parseBase64 "U3Vu"
-- Right "U3Vu"
--
-- >>> fmap decodeBase64 (parseBase64 "U3Vu")
-- Right "Sun"
--
-- >>> parseBase64 "U3V"
-- Left "Base64-encoded bytestring requires padding"
--
-- >>> parseBase64 "U3V="
-- Left "non-canonical encoding detected at offset: 2"
--
-- @since 2.0
--
parseBase64 :: ShortText -> Either Text (Base64 'StdPadded ShortText)
parseBase64 bs = assertBase64 bs <$ BS64.parseBase64 (toShortByteString bs)
{-# INLINE parseBase64 #-}

-- Share valid UTF-8 storage directly. Preserve the existing decodeUtf8
-- exception for invalid decoded payloads instead of constructing invalid text.
decodeShortUtf8 :: ShortByteString -> ShortText
decodeShortUtf8 bytes = case fromShortByteString bytes of
  Just text -> text
  Nothing -> fromText (T.decodeUtf8 (fromShort bytes))
{-# INLINE decodeShortUtf8 #-}
