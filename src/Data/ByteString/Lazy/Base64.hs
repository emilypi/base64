{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Lazy.Base64
-- Copyright    : (c) 2019-2020 Emily Pillmore
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
, decodeBase64Lenient
  -- * Validation
, isBase64
, isValidBase64
) where


import Prelude hiding (all, elem)

import Data.Base64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Base64.Internal.Utils (reChunkN)
import Data.ByteString.Lazy (elem, fromChunks, toChunks)
import Data.ByteString.Lazy.Internal (ByteString(..))
import Data.Either (isRight)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

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
-- >>> decodeBase64 "U3Vu"
-- Right "Sun"
--
-- >>> decodeBase64 "U3V"
-- Left "Base64-encoded bytestring requires padding"
--
-- >>> decodebase64 "U3V="
-- Left "non-canonical encoding detected at offset: 2"
--
decodeBase64 :: StdAlphabet k => Base64 k ByteString -> Either T.Text ByteString
decodeBase64 = fmap (fromChunks . (:[]))
  . B64.decodeBase64
  . fmap (BS.concat . toChunks)
{-# INLINE decodeBase64 #-}

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
-- >>> decodebase64Lenient "U3V="
-- "Su"
--
decodeBase64Lenient :: Base64 k ByteString -> ByteString
decodeBase64Lenient = fromChunks
    . fmap B64.decodeBase64Lenient
    . fmap assertBase64
    . reChunkN 4
    . fmap (BS.filter (`elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="))
    . toChunks
    . extractBase64
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether a 'ByteString' value is base64 encoded.
--
-- This function will also detect non-canonical encodings such as @ZE==@, which are
-- externally valid Base64url-encoded values, but are internally inconsistent "impossible"
-- values.
--
-- === __Examples__:
--
-- >>> isBase64 "U3Vu"
-- True
--
-- >>> isBase64 "U3V"
-- False
--
-- >>> isBase64 "U3V="
-- False
--
isBase64 :: ByteString -> Bool
isBase64 bs
  = isValidBase64 bs
  && isRight (decodeBase64 $ assertBase64 @'StdPadded bs)
{-# INLINE isBase64 #-}

-- | Tell whether a 'ByteString' value is a valid Base64 format.
--
-- This will not tell you whether or not this is a correct Base64url representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base64 encoded 'ByteString' value, use 'isBase64'.
--
-- === __Examples__:
--
-- >>> isValidBase64 "U3Vu"
-- True
--
-- >>> isValidBase64 "U3V"
-- True
--
-- >>> isValidBase64 "U3V="
-- True
--
-- >>> isValidBase64 "%"
-- False
--
isValidBase64 :: ByteString -> Bool
isValidBase64 = go . toChunks
  where
    go [] = True
    go [c] = B64.isValidBase64 c
    go (c:cs) = -- note the lack of padding char
      BS.all (`elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/") c
      && go cs
{-# INLINE isValidBase64 #-}
