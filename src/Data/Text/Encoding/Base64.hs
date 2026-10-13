{-# LANGUAGE DataKinds #-}
-- |
-- Module       : Data.Text.Encoding.Base64
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.Text.Text'-valued combinators for
-- implementing the RFC 4648 specification of the Base64
-- encoding format. This includes lenient decoding variants, as well as
-- internal and external validation for canonicity.
--
module Data.Text.Encoding.Base64
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
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64

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

-- | Encode a 'Text' value in Base64 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> encodeBase64 "Sun"
-- "U3Vu"
--
encodeBase64 :: Text -> Base64 'StdPadded Text
encodeBase64 = B64.encodeBase64 . T.encodeUtf8
{-# INLINE encodeBase64 #-}

-- | Decode a padded Base64-encoded 'Text' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64 $ assertBase64 @'StdPadded "U3Vu"
-- "Sun"
--
decodeBase64 :: StdAlphabet k => Base64 k Text -> Text
decodeBase64 = T.decodeUtf8 . B64.decodeBase64 . fmap T.encodeUtf8
{-# INLINE decodeBase64 #-}

-- | Decode a padded, untyped Base64-encoded 'Text' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64Untyped "U3Vu"
-- Right "Sun"
--
decodeBase64Untyped :: Text -> Either Text Text
decodeBase64Untyped = fmap T.decodeUtf8
  . B64.decodeBase64Untyped
  . T.encodeUtf8
{-# INLINE decodeBase64Untyped #-}

-- | Attempt to decode an untyped 'Text' value as Base64, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Example__:
--
-- @
-- 'decodeBase64UntypedWith' 'T.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base64Error' 'UnicodeException') 'Text'
-- @
--
decodeBase64UntypedWith
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base64Error err) Text
decodeBase64UntypedWith f t = case B64.decodeBase64Untyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase64UntypedWith #-}

-- | Leniently decode an untyped Base64-encoded 'Text' value. This function
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
decodeBase64Lenient :: Text -> Text
decodeBase64Lenient = T.decodeUtf8
    . B64.decodeBase64Lenient
    . T.encodeUtf8
{-# INLINE decodeBase64Lenient #-}

-- | Parse a canonical, padded Base64-encoded 'Text' value.
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
parseBase64 :: Text -> Either Text (Base64 'StdPadded Text)
parseBase64 bs = assertBase64 bs <$ B64.parseBase64 (T.encodeUtf8 bs)
{-# INLINE parseBase64 #-}
