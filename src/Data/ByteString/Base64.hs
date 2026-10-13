{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.ByteString.Base64
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.ByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base64
-- encoding format. This includes lenient decoding variants, as well as
-- internal and external validation for canonicity.
--
module Data.ByteString.Base64
( -- * Encoding
  encodeBase64
, encodeBase64'
  -- * Decoding
, decodeBase64
, decodeBase64Untyped
, decodeBase64Lenient
  -- * Parsing
, parseBase64
) where

import Data.Base64.Types

import Data.ByteString.Internal (ByteString(..))
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

-- | Encode a 'ByteString' value as Base64 'Text' with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> encodeBase64 "Sun"
-- "U3Vu"
--
encodeBase64 :: ByteString -> Base64 'StdPadded Text
encodeBase64 = fmap T.decodeUtf8 . encodeBase64'
{-# inline encodeBase64 #-}

-- | Encode a 'ByteString' value as a Base64 'ByteString'  value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> encodeBase64' "Sun"
-- "U3Vu"
--
encodeBase64' :: ByteString -> Base64 'StdPadded ByteString
encodeBase64' = assertBase64 . encodeBase64_ base64Table
{-# inline encodeBase64' #-}

-- | Decode a padded Base64-encoded 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64 $ assertBase64 @'StdPadded "U3Vu"
-- "Sun"
--
decodeBase64 :: StdAlphabet k => Base64 k ByteString -> ByteString
decodeBase64 = decodeBase64Typed_ decodeB64Table
{-# inline decodeBase64 #-}

-- | Decode a padded untyped Base64-encoded 'ByteString' value.
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
decodeBase64Untyped :: ByteString -> Either Text ByteString
decodeBase64Untyped bs@(PS _ _ !l)
    | l == 0 = Right mempty
    | r == 1 = Left "Base64-encoded bytestring has invalid size"
    | r /= 0 = Left "Base64-encoded bytestring requires padding"
    | otherwise = unsafeDupablePerformIO $ decodeBase64_ decodeB64Table bs
  where
    !r = l `rem` 4
{-# inline decodeBase64Untyped #-}

-- | Leniently decode an untyped Base64-encoded 'ByteString' value. This function
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
decodeBase64Lenient :: ByteString -> ByteString
decodeBase64Lenient = decodeBase64Lenient_ decodeB64Table
{-# inline decodeBase64Lenient #-}

-- | Parse a canonical, padded Base64-encoded 'ByteString' value.
--
-- Checks the alphabet, length, padding, and unused padding bits. On success,
-- returns the original encoded value wrapped in 'Base64', ready for
-- 'decodeBase64'. On failure, returns the same error as 'decodeBase64Untyped'.
-- The input is neither decoded nor normalized in the returned value.
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
parseBase64 :: ByteString -> Either Text (Base64 'StdPadded ByteString)
parseBase64 bs = assertBase64 bs <$ decodeBase64Untyped bs
{-# INLINE parseBase64 #-}
