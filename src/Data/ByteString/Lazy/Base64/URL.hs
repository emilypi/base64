{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Lazy.Base64.URL
-- Copyright    : (c) 2019-2022 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.Lazy.ByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base64url
-- encoding format. This includes strictly padded/unpadded and lenient
-- decoding variants, as well as internal and external validation for canonicity.
--
module Data.ByteString.Lazy.Base64.URL
( -- * Encoding
  encodeBase64
, encodeBase64'
, encodeBase64Unpadded
, encodeBase64Unpadded'
  -- * Decoding
, decodeBase64
, decodeBase64Untyped
, decodeBase64Unpadded
, decodeBase64UnpaddedUntyped
, decodeBase64Padded
, decodeBase64PaddedUntyped
, decodeBase64Lenient
  -- * Validation
, isBase64Url
, isValidBase64Url
) where


import Data.Base64.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Base64.Internal.Utils (reChunkN)
import Data.ByteString.Lazy (fromChunks, toChunks)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Internal (ByteString(..))
import Data.Either (isRight)
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


-- | Encode a 'ByteString' value as a Base64url 'Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
-- === __Examples__:
--
-- >>> encodeBase64 "<<?>>"
-- "PDw_Pj4="
--
encodeBase64 :: ByteString -> Base64 'UrlPadded TL.Text
encodeBase64 = fmap TL.decodeUtf8 . encodeBase64'
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
encodeBase64' = assertBase64 . fromChunks
  . fmap (extractBase64 . B64U.encodeBase64')
  . reChunkN 3
  . toChunks

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
-- >>> decodeBase64 $ assertBase64 @'UrlPadded "PDw_Pj4="
-- "<<?>>"
--
-- >>> decodeBase64 $ assertBase64 @'UrlUnpadded "PDw_Pj4"
-- "<<?>>"
--
-- >>> decodeBase64 $ assertBase64 @'UrlUnpadded "PDw-Pg"
-- "<<>>"
--
decodeBase64
  :: UrlAlphabet k
  => Base64 k ByteString
  -> ByteString
decodeBase64 = fromChunks
  . pure
  . B64U.decodeBase64
  . fmap (BS.concat . toChunks)
{-# INLINE decodeBase64 #-}

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
-- >>> decodeBase64Untyped "PDw_Pj4="
-- Right "<<?>>"
--
-- >>> decodeBase64Untyped "PDw_Pj4"
-- Right "<<?>>"
--
-- >>> decodeBase64Untyped "PDw-Pg="
-- Left "Base64-encoded bytestring has invalid padding"
--
-- >>> decodeBase64Untyped "PDw-Pg"
-- Right "<<>>"
--
decodeBase64Untyped :: ByteString -> Either T.Text ByteString
decodeBase64Untyped = fmap (fromChunks . pure)
  . B64U.decodeBase64Untyped
  . BS.concat
  . toChunks
{-# INLINE decodeBase64Untyped #-}

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
encodeBase64Unpadded :: ByteString -> Base64 'UrlUnpadded TL.Text
encodeBase64Unpadded = fmap TL.decodeUtf8 . encodeBase64Unpadded'
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
encodeBase64Unpadded' = assertBase64
  . fromChunks
  . fmap (extractBase64 . B64U.encodeBase64Unpadded')
  . reChunkN 3
  . toChunks

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
-- >>> decodeBase64Unpadded $ assertBase64 @'UrlUnpadded "PDw_Pj4"
-- "<<?>>"
--
decodeBase64Unpadded :: Base64 'UrlUnpadded ByteString -> ByteString
decodeBase64Unpadded = fromChunks
  . pure
  . B64U.decodeBase64Unpadded
  . fmap (BS.concat . toChunks)

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
-- >>> decodeBase64UnpaddedUntyped "PDw_Pj4"
-- Right "<<?>>"
--
-- >>> decodeBase64UnpaddedUntyped "PDw_Pj4="
-- Left "Base64-encoded bytestring has invalid padding"
--
decodeBase64UnpaddedUntyped :: ByteString -> Either T.Text ByteString
decodeBase64UnpaddedUntyped = fmap (fromChunks . (:[]))
  . B64U.decodeBase64UnpaddedUntyped
  . BS.concat
  . toChunks
{-# INLINE decodeBase64UnpaddedUntyped #-}

-- | Decode a padded Base64url-encoded 'ByteString' value. Input strings are
-- required to be padded, and will undergo validation prior to decoding to
-- confirm.
--
-- In general, unless padded Base64url is explicitly required, it is
-- safer to call 'decodeBase64'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64Unpadded $ assertBase64 @'UrlUnpadded "PDw_Pj4"
-- "<<?>>"
--
decodeBase64Padded :: Base64 'UrlPadded ByteString -> ByteString
decodeBase64Padded = fromChunks
  . pure
  . B64U.decodeBase64Padded
  . fmap (BS.concat . toChunks)
{-# inline decodeBase64Padded #-}

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
-- >>> decodeBase64PaddedUntyped "PDw_Pj4="
-- Right "<<?>>"
--
-- >>> decodeBase64PaddedUntyped "PDw_Pj4"
-- Left "Base64-encoded bytestring requires padding"
--
decodeBase64PaddedUntyped :: ByteString -> Either T.Text ByteString
decodeBase64PaddedUntyped = fmap (fromChunks . (:[]))
  . B64U.decodeBase64PaddedUntyped
  . BS.concat
  . toChunks
{-# INLINE decodeBase64PaddedUntyped #-}

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
decodeBase64Lenient :: ByteString -> ByteString
decodeBase64Lenient = fromChunks
    . fmap B64U.decodeBase64Lenient
    . reChunkN 4
    . fmap (BS.filter (`BL.elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_="))
    . toChunks
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether a 'ByteString' is Base64url-encoded.
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
isBase64Url bs
  = isValidBase64Url bs
  && isRight (decodeBase64Untyped bs)
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
isValidBase64Url = go . toChunks
  where
    go [] = True
    go [c] = B64U.isValidBase64Url c
    go (c:cs) = -- note the lack of padding char
      BS.all (`BL.elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_") c
      && go cs
{-# INLINE isValidBase64Url #-}
