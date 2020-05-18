{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.ByteString.Lazy.Base64
-- Copyright    : (c) 2019-2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- This module contains the combinators implementing the
-- RFC 4648 specification for the Base64 encoding including
-- unpadded and lenient variants
--
module Data.ByteString.Lazy.Base64
( encodeBase64
, encodeBase64'
, decodeBase64
, decodeBase64Lenient
, isBase64
, isValidBase64
) where


import Prelude hiding (all, elem)

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
encodeBase64 :: ByteString -> TL.Text
encodeBase64 = TL.decodeUtf8 . encodeBase64'
{-# INLINE encodeBase64 #-}

-- | Encode a 'ByteString' value as a Base64 'ByteString'  value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
encodeBase64' :: ByteString -> ByteString
encodeBase64' = fromChunks
  . fmap B64.encodeBase64'
  . reChunkN 3
  . toChunks
{-# INLINE encodeBase64' #-}

-- | Decode a padded Base64-encoded 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64 :: ByteString -> Either T.Text ByteString
decodeBase64 = fmap (fromChunks . (:[]))
  . B64.decodeBase64
  . BS.concat
  . toChunks
{-# INLINE decodeBase64 #-}

-- | Leniently decode an unpadded Base64-encoded 'ByteString' value. This function
-- will not generate parse errors. If input data contains padding chars,
-- then the input will be parsed up until the first pad character.
--
-- __Note:__ This is not RFC 4648-compliant.
--
decodeBase64Lenient :: ByteString -> ByteString
decodeBase64Lenient = fromChunks
    . fmap B64.decodeBase64Lenient
    . reChunkN 4
    . fmap (BS.filter (flip elem "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="))
    . toChunks
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether a 'ByteString' value is base64 encoded.
--
isBase64 :: ByteString -> Bool
isBase64 bs = isValidBase64 bs && isRight (decodeBase64 bs)
{-# INLINE isBase64 #-}

-- | Tell whether a 'ByteString' value is a valid Base64 format.
--
-- This will not tell you whether or not this is a correct Base64url representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base64 encoded 'ByteString' value, use 'isBase64'.
--
isValidBase64 :: ByteString -> Bool
isValidBase64 = go . toChunks
  where
    go [] = True
    go [c] = B64.isValidBase64 c
    go (c:cs) = -- note the lack of padding char
      BS.all (flip elem "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/") c
      && go cs
{-# INLINE isValidBase64 #-}
