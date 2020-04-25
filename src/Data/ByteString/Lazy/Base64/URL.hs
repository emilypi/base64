{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.ByteString.Lazy.Base64.URL
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
module Data.ByteString.Lazy.Base64.URL
( encodeBase64
, encodeBase64'
, encodeBase64Unpadded
, encodeBase64Unpadded'
, decodeBase64
, decodeBase64Padded
, decodeBase64Unpadded
, decodeBase64Lenient
, isBase64Url
, isValidBase64Url
) where


import Prelude hiding (all, elem)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Base64.Internal.Utils (reChunk)
import Data.ByteString.Lazy (elem, fromChunks, toChunks)
import Data.ByteString.Lazy.Internal (ByteString(..))
import Data.Either (isRight)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL


-- | Encode a 'ByteString' value as a Base64url 'Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
encodeBase64 :: ByteString -> TL.Text
encodeBase64 = TL.decodeUtf8 . encodeBase64'
{-# INLINE encodeBase64 #-}

-- | Encode a 'ByteString' as a Base64url 'ByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
encodeBase64' :: ByteString -> ByteString
encodeBase64' Empty = Empty
encodeBase64' (Chunk c cs) = Chunk (B64U.encodeBase64' c) (encodeBase64' cs)

-- | Decode a padded Base64url encoded 'ByteString' value. If its length is not a multiple
-- of 4, then padding chars will be added to fill out the input to a multiple of
-- 4 for safe decoding as Base64url-encoded values are optionally padded.
--
-- For a decoder that fails on unpadded input of incorrect size, use 'decodeBase64Unpadded'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64 :: ByteString -> Either T.Text ByteString
decodeBase64 Empty = Right Empty
decodeBase64 (Chunk c cs) = Chunk
    <$> B64U.decodeBase64 c
    <*> decodeBase64 cs
{-# INLINE decodeBase64 #-}

-- | Encode a 'ByteString' value as Base64url 'Text' without padding. Note that for Base64url,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base64url and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase64Unpadded :: ByteString -> TL.Text
encodeBase64Unpadded = TL.decodeUtf8 . encodeBase64Unpadded'
{-# INLINE encodeBase64Unpadded #-}

-- | Encode a 'ByteString' value as Base64url without padding. Note that for Base64url,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base64url and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase64Unpadded' :: ByteString -> ByteString
encodeBase64Unpadded' Empty = Empty
encodeBase64Unpadded' (Chunk c cs) = Chunk
    (B64U.encodeBase64Unpadded' c)
    (encodeBase64Unpadded' cs)

-- | Decode an unpadded Base64url-encoded 'ByteString' value. Input strings are
-- required to be unpadded, and will undergo validation prior to decoding to
-- confirm.
--
-- In general, unless unpadded Base64url is explicitly required, it is
-- safer to call 'decodeBase64'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64Unpadded :: ByteString -> Either T.Text ByteString
decodeBase64Unpadded Empty = Right Empty
decodeBase64Unpadded (Chunk c cs) = Chunk
    <$> B64U.decodeBase64Unpadded c
    <*> decodeBase64Unpadded cs
{-# INLINE decodeBase64Unpadded #-}

-- | Decode a padded Base64url-encoded 'ByteString' value. Input strings are
-- required to be correctly padded, and will be validated prior to decoding
-- to confirm.
--
-- In general, unless padded Base64url is explicitly required, it is
-- safer to call 'decodeBase64'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64Padded :: ByteString -> Either T.Text ByteString
decodeBase64Padded Empty = Right Empty
decodeBase64Padded (Chunk c cs) = Chunk
    <$> B64U.decodeBase64Padded c
    <*> decodeBase64Padded cs
{-# INLINE decodeBase64Padded #-}

-- | Leniently decode an unpadded Base64url-encoded 'ByteString'. This function
-- will not generate parse errors. If input data contains padding chars,
-- then the input will be parsed up until the first pad character.
--
-- __Note:__ This is not RFC 4648-compliant.
--
decodeBase64Lenient :: ByteString -> ByteString
decodeBase64Lenient = fromChunks
    . fmap B64U.decodeBase64Lenient
    . reChunk
    . fmap (BS.filter (flip elem "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_="))
    . toChunks
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether a 'ByteString' is Base64url-encoded.
--
isBase64Url :: ByteString -> Bool
isBase64Url bs = isValidBase64Url bs && isRight (decodeBase64 bs)
{-# INLINE isBase64Url #-}

-- | Tell whether a 'ByteString' is a valid Base64url format.
--
-- This will not tell you whether or not this is a correct Base64url representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base64 encoded 'ByteString' value, use 'isBase64Url'.
--
isValidBase64Url :: ByteString -> Bool
isValidBase64Url = go . toChunks
  where
    go [] = True
    go [c] = B64U.isValidBase64Url c
    go (c:cs) = -- note the lack of padding char
      BS.all (flip elem "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_") c
      && go cs
{-# INLINE isValidBase64Url #-}
