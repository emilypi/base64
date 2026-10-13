{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
-- |
-- Module       : Data.ByteString.Lazy.Base64
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.Lazy.ByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base64
-- encoding format. This includes lenient decoding variants, as well as
-- internal and external validation for canonicity.
--
module Data.ByteString.Lazy.Base64
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Base64.Internal.Utils (reChunkN)
import Data.ByteString.Lazy (fromChunks, toChunks)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Internal (ByteString(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

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
encodeBase64 :: ByteString -> Base64 'StdPadded TL.Text
encodeBase64 = fmap TL.decodeUtf8 . encodeBase64'
{-# INLINE encodeBase64 #-}

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
encodeBase64' = assertBase64
  . fromChunks
  . fmap (extractBase64 . B64.encodeBase64')
  . reChunkN 3
  . toChunks
{-# INLINE encodeBase64' #-}

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
decodeBase64 = fromChunks
  . pure
  . B64.decodeBase64
  . fmap (BS.concat . toChunks)
{-# INLINE decodeBase64 #-}

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
decodeBase64Untyped :: ByteString -> Either T.Text ByteString
decodeBase64Untyped = fmap (fromChunks . pure)
  . B64.decodeBase64Untyped
  . BS.concat
  . toChunks
{-# inline decodeBase64Untyped #-}

-- | Leniently decode an unpadded Base64-encoded 'ByteString' value. This function
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
decodeBase64Lenient = fromChunks
    . fmap B64.decodeBase64Lenient
    . reChunkN 4
    . fmap (BS.filter (`BL.elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="))
    . toChunks
{-# INLINE decodeBase64Lenient #-}

-- | Parse a canonical, padded Base64-encoded 'ByteString' value.
--
-- Checks the alphabet, length, padding, and unused padding bits. On success,
-- returns the original encoded value wrapped in 'Base64', ready for
-- 'decodeBase64'. On failure, returns the same error as 'decodeBase64Untyped'.
-- The input is neither decoded nor normalized in the returned value.
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
-- @since 2.0
--
parseBase64 :: ByteString -> Either T.Text (Base64 'StdPadded ByteString)
parseBase64 bs = assertBase64 bs <$ B64.parseBase64 (BS.concat $ toChunks bs)
{-# INLINE parseBase64 #-}
