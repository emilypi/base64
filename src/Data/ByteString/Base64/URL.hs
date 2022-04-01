{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
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
( -- * Encoding
  encodeBase64
, encodeBase64'
, encodeBase64Unpadded
, encodeBase64Unpadded'
  -- * Decoding
, decodeBase64
, decodeBase64Unpadded
, decodeBase64Padded
, decodeBase64Lenient
  -- * Validation
, isBase64Url
, isValidBase64Url
) where

import Data.Base64
import Data.Base64.Internal
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
-- === __Examples__:
--
-- >>> encodeBase64 "<<?>>"
-- "PDw_Pj4="
--
encodeBase64 :: ByteString -> Base64 'UrlPadded Text
encodeBase64 = mapBase64 T.decodeUtf8 . encodeBase64'
{-# INLINE encodeBase64 #-}

-- | Encode a 'ByteString' as a Base64url 'ByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
-- === __Examples__:
--
-- >>> encodeBase64' "<<?>>"
-- "PDw_Pj4="
--
encodeBase64' :: ByteString -> Base64 'UrlPadded ByteString
encodeBase64' = assertBase64 . encodeBase64_ base64UrlTable

-- | Decode a padded Base64url encoded 'ByteString' value. If its length is not a multiple
-- of 4, then padding chars will be added to fill out the input to a multiple of
-- 4 for safe decoding as Base64url-encoded values are optionally padded.
--
-- For a decoder that fails on unpadded input of incorrect size, use 'decodeBase64Unpadded'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64 "PDw_Pj4="
-- Right "<<?>>"
--
-- >>> decodeBase64 "PDw_Pj4"
-- Right "<<?>>"
--
-- >>> decodeBase64 "PDw-Pg="
-- Left "Base64-encoded bytestring has invalid padding"
--
-- >>> decodeBase64 "PDw-Pg"
-- Right "<<>>"
--
decodeBase64 :: Base64 'UrlPadded ByteString -> Either Text ByteString
decodeBase64 (Base64 bs@(PS _ _ !l))
  | l == 0 = Right bs
  | r == 0 = unsafeDupablePerformIO $ decodeBase64_ decodeB64UrlTable bs
  | r == 2 = unsafeDupablePerformIO $ decodeBase64_ decodeB64UrlTable (BS.append bs "==")
  | r == 3 = validateLastPad bs $ decodeBase64_ decodeB64UrlTable (BS.append bs "=")
  | otherwise = Left "Base64-encoded bytestring has invalid size"
  where
    !r = l `rem` 4
{-# INLINE decodeBase64 #-}

-- | Encode a 'ByteString' value as Base64url 'Text' without padding. Note that for Base64url,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base64url and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
-- === __Examples__:
--
-- >>> encodeBase64Unpadded "<<?>>"
-- "PDw_Pj4"
--
encodeBase64Unpadded :: ByteString -> Base64 'UrlUnpadded Text
encodeBase64Unpadded = mapBase64 T.decodeUtf8 . encodeBase64Unpadded'
{-# INLINE encodeBase64Unpadded #-}

-- | Encode a 'ByteString' value as Base64url without padding. Note that for Base64url,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base64url and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
-- === __Examples__:
--
-- >>> encodeBase64Unpadded' "<<?>>"
-- "PDw_Pj4"
--
encodeBase64Unpadded' :: ByteString -> Base64 'UrlUnpadded ByteString
encodeBase64Unpadded' = assertBase64 . encodeBase64Nopad_ base64UrlTable

-- | Decode an unpadded Base64url-encoded 'ByteString' value. Input strings are
-- required to be unpadded, and will undergo validation prior to decoding to
-- confirm.
--
-- In general, unless unpadded Base64url is explicitly required, it is
-- safer to call 'decodeBase64'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64Unpadded "PDw_Pj4"
-- Right "<<?>>"
--
-- >>> decodeBase64Unpadded "PDw_Pj4="
-- Left "Base64-encoded bytestring has invalid padding"
--
decodeBase64Unpadded :: Base64 'UrlUnpadded ByteString -> Either Text ByteString
decodeBase64Unpadded (Base64 bs@(PS _ _ !l))
    | l == 0 = Right bs
    | r == 0 = validateLastPad bs $ decodeBase64_ decodeB64UrlTable bs
    | r == 2 = validateLastPad bs $ decodeBase64_ decodeB64UrlTable (BS.append bs "==")
    | r == 3 = validateLastPad bs $ decodeBase64_ decodeB64UrlTable (BS.append bs "=")
    | otherwise = Left "Base64-encoded bytestring has invalid size"
  where
    !r = l `rem` 4
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
-- === __Examples__:
--
-- >>> decodeBase64Padded "PDw_Pj4="
-- Right "<<?>>"
--
-- >>> decodeBase64Padded "PDw_Pj4"
-- Left "Base64-encoded bytestring requires padding"
--
decodeBase64Padded :: Base64 'UrlPadded ByteString -> Either Text ByteString
decodeBase64Padded (Base64 bs@(PS _ _ !l))
    | l == 0 = Right bs
    | r == 1 = Left "Base64-encoded bytestring has invalid size"
    | r /= 0 = Left "Base64-encoded bytestring requires padding"
    | otherwise = unsafeDupablePerformIO $ decodeBase64_ decodeB64UrlTable bs
  where
    !r = l `rem` 4
{-# INLINE decodeBase64Padded #-}

-- | Leniently decode an unpadded Base64url-encoded 'ByteString'. This function
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
decodeBase64Lenient :: Base64 'UrlUnpadded ByteString -> ByteString
decodeBase64Lenient = decodeBase64Lenient_ decodeB64UrlTable . extractBase64
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether a 'ByteString' is encoded in padded /or/ unpadded Base64url format.
--
-- This function will also detect non-canonical encodings such as @ZE==@, which are
-- externally valid Base64url-encoded values, but are internally inconsistent "impossible"
-- values.
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
isBase64Url :: ByteString -> Bool
isBase64Url bs = isValidBase64Url bs && isRight (decodeBase64 (assertBase64 bs))
{-# INLINE isBase64Url #-}

-- | Tell whether a 'ByteString' is a valid Base64url format.
--
-- This will not tell you whether or not this is a correct Base64url representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base64 encoded 'ByteString' value, use 'isBase64Url'.
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
isValidBase64Url :: ByteString -> Bool
isValidBase64Url = validateBase64Url "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
{-# INLINE isValidBase64Url #-}
