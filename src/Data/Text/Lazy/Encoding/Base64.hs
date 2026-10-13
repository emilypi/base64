{-# LANGUAGE DataKinds #-}
-- |
-- Module       : Data.Text.Lazy.Encoding.Base64
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.Text.Lazy.Text'-valued combinators
-- implementing the RFC 4648 specification for the Base64
-- encoding format. This includes lenient decoding variants, and
-- external + internal validations for canonicity.
--
module Data.Text.Lazy.Encoding.Base64
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
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Base64 as BL64

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

-- | Encode a 'TL.Text' value in Base64 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> encodeBase64 "Sun"
-- "U3Vu"
--
encodeBase64 :: TL.Text -> Base64 'StdPadded TL.Text
encodeBase64 = BL64.encodeBase64 . TL.encodeUtf8
{-# INLINE encodeBase64 #-}

-- | Decode a padded Base64-encoded 'TL.Text' value
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64 $ assertBase64 @'StdPadded "U3Vu"
-- "Sun"
--
decodeBase64 :: StdAlphabet k => Base64 k TL.Text -> TL.Text
decodeBase64 = TL.decodeUtf8 . BL64.decodeBase64 . fmap TL.encodeUtf8
{-# INLINE decodeBase64 #-}

-- | Decode a padded, untyped Base64-encoded 'TL.Text' value
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
decodeBase64Untyped :: TL.Text -> Either T.Text TL.Text
decodeBase64Untyped = fmap TL.decodeUtf8 . BL64.decodeBase64Untyped . TL.encodeUtf8
{-# INLINE decodeBase64Untyped #-}

-- | Attempt to decode a 'ByteString' value as Base64, converting from
-- 'ByteString' to 'TL.Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Example__:
--
-- @
-- 'decodeBase64UntypedWith' 'TL.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base64Error' 'UnicodeException') 'TL.Text'
-- @
--
decodeBase64UntypedWith
    :: (ByteString -> Either err TL.Text)
      -- ^ convert a bytestring to text (e.g. 'TL.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base64Error err) TL.Text
decodeBase64UntypedWith f t = case BL64.decodeBase64Untyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase64UntypedWith #-}

-- | Leniently decode an untyped Base64-encoded 'TL.Text' value. This function
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
decodeBase64Lenient :: TL.Text -> TL.Text
decodeBase64Lenient = TL.decodeUtf8
  . BL64.decodeBase64Lenient
  . TL.encodeUtf8
{-# INLINE decodeBase64Lenient #-}

-- | Parse a canonical, padded Base64-encoded 'TL.Text' value.
--
-- Checks the alphabet, length, padding, and unused padding bits. On success,
-- returns the original encoded value wrapped in 'Base64', ready for
-- 'decodeBase64'. On failure, returns the same error as 'decodeBase64Untyped'.
-- The input is neither decoded nor normalized in the returned value.
--
-- This validates the Base64 encoding, not the UTF-8 validity of the decoded
-- bytes. Use 'decodeBase64UntypedWith' to check the decoded text encoding.
--
-- The entire input is validated before returning 'Right'; the original
-- chunk boundaries are preserved in the result.
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
parseBase64 :: TL.Text -> Either T.Text (Base64 'StdPadded TL.Text)
parseBase64 bs = assertBase64 bs <$ BL64.parseBase64 (TL.encodeUtf8 bs)
{-# INLINE parseBase64 #-}
