{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Base64.URL
-- Copyright    : (c) 2019-2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.ByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base64url
-- encoding format. This includes strictly padded/unpadded and lenient decoding
-- variants, as well as internal and external validation for canonicity.
--
module Data.ByteString.Base64.URL
( encodeBase64
, encodeBase64'
, decodeBase64
, encodeBase64Unpadded
, encodeBase64Unpadded'
, decodeBase64Unpadded
, decodeBase64Padded
, decodeBase64Lenient
, isBase64Url
, isValidBase64Url
) where


import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Base64.Internal
import Data.ByteString.Base64.Internal.Head
import Data.ByteString.Base64.Internal.Tables
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text.Encoding as T

import System.IO.Unsafe


-- | Encode a 'ByteString' value as a Base64url 'Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
encodeBase64 :: ByteString -> Text
encodeBase64 = T.decodeUtf8 . encodeBase64'
{-# INLINE encodeBase64 #-}

-- | Encode a 'ByteString' as a Base64url 'ByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
encodeBase64' :: ByteString -> ByteString
encodeBase64' = encodeBase64_ base64UrlTable

-- | Decode a padded Base64url encoded 'ByteString' value. If its length is not a multiple
-- of 4, then padding chars will be added to fill out the input to a multiple of
-- 4 for safe decoding as Base64url-encoded values are optionally padded.
--
-- For a decoder that fails on unpadded input of incorrect size, use 'decodeBase64Unpadded'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64 :: ByteString -> Either Text ByteString
decodeBase64 bs@(PS _ _ l)
    | l == 0 = Right bs
    | r == 0 = unsafeDupablePerformIO $ decodeBase64_ dlen decodeB64UrlTable bs
    | r == 2 = unsafeDupablePerformIO $ decodeBase64_ dlen decodeB64UrlTable (BS.append bs "==")
    | r == 3 = validateLastPad bs $ decodeBase64_ dlen decodeB64UrlTable (BS.append bs "=")
    | otherwise = Left "Base64-encoded bytestring has invalid size"
  where
    q = l `quot` 4
    r = l `rem` 4
    dlen = q * 3
{-# INLINE decodeBase64 #-}

-- | Encode a 'ByteString' value as Base64url 'Text' without padding. Note that for Base64url,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base64url and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase64Unpadded :: ByteString -> Text
encodeBase64Unpadded = T.decodeUtf8 . encodeBase64Unpadded'
{-# INLINE encodeBase64Unpadded #-}

-- | Encode a 'ByteString' value as Base64url without padding. Note that for Base64url,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base64url and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase64Unpadded' :: ByteString -> ByteString
encodeBase64Unpadded' = encodeBase64Nopad_ base64UrlTable

-- | Decode an unpadded Base64url-encoded 'ByteString' value. Input strings are
-- required to be unpadded, and will undergo validation prior to decoding to
-- confirm.
--
-- In general, unless unpadded Base64url is explicitly required, it is
-- safer to call 'decodeBase64'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64Unpadded :: ByteString -> Either Text ByteString
decodeBase64Unpadded bs@(PS _ _ l)
    | l == 0 = Right bs
    | r == 0 = validateLastPad bs $ decodeBase64_ dlen decodeB64UrlTable bs
    | r == 2 = validateLastPad bs $ decodeBase64_ dlen decodeB64UrlTable (BS.append bs "==")
    | r == 3 = validateLastPad bs $ decodeBase64_ dlen decodeB64UrlTable (BS.append bs "=")
    | otherwise = Left "Base64-encoded bytestring has invalid size"
  where
    q = l `quot` 4
    r = l `rem` 4
    dlen = q * 3
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
decodeBase64Padded :: ByteString -> Either Text ByteString
decodeBase64Padded bs@(PS _ _ l)
    | l == 0 = Right bs
    | r == 1 = Left "Base64-encoded bytestring has invalid size"
    | r /= 0 = Left "Base64-encoded bytestring requires padding"
    | otherwise = unsafeDupablePerformIO $ decodeBase64_ dlen decodeB64UrlTable bs
  where
    q = l `quot` 4
    r = l `rem` 4
    dlen = q * 3
{-# INLINE decodeBase64Padded #-}

-- | Leniently decode an unpadded Base64url-encoded 'ByteString'. This function
-- will not generate parse errors. If input data contains padding chars,
-- then the input will be parsed up until the first pad character.
--
-- __Note:__ This is not RFC 4648-compliant.
--
decodeBase64Lenient :: ByteString -> ByteString
decodeBase64Lenient = decodeBase64Lenient_ decodeB64UrlTable
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether a 'ByteString' is encoded in padded /or/ unpadded Base64url format.
--
-- This function will also detect non-canonical encodings such as @ZE==@, which are
-- externally valid Base64url-encoded values, but are internally inconsistent "impossible"
-- values.
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
isValidBase64Url = validateBase64Url "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
{-# INLINE isValidBase64Url #-}
