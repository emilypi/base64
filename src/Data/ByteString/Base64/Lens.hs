-- |
-- Module       : Data.Text.Encoding.Base64.Lens
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains the 'HasBase64' instance for 'Text', which is
-- defined to be the collection of 'Iso's and 'Prism's defining the
-- RFC 4648 specification for the Base64 encoding format.
--
-- In order to expose this file, you must build the package with
-- '-foptics' enabled.
--
module Data.ByteString.Base64.Lens
( -- * Classy Base64
  HasBase64(..)
) where


import Control.Lens

import Data.ByteString
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U


-- | If a particular type @a@ has a base64 representation
-- for any of its focii, this class provides the optical interface
-- for satisfying the RFC 4648
--
class HasBase64 a where

    -- | A prism into a base64-encoded focus of
    -- some type
    --
    _Base64 :: Prism' a a

    -- | A prism into the base64url-encoded focus of
    -- some type
    --
    _Base64Url :: Prism' a a


    -- | An iso representing the lenient conversion
    -- of some focus to its base64 encoding
    --
    _Base64Lenient :: Iso' a a

    -- | An iso representing the lenient conversion
    -- of some focus to its base64url encoding
    --
    _Base64UrlLenient :: Iso' a a

    -- | A prism into the unpadded base64-encoded focus of
    -- some type
    --
    _Base64Unpadded :: Prism' a a

    -- | A prism into the unpadded base64url-encoded focus of
    -- some type
    --
    _Base64UrlUnpadded :: Prism' a a

    -- | An iso representing the lenient conversion
    -- of some focus to its unpadded base64 encoding
    --
    _Base64LenientUnpadded :: Iso' a a

    -- | An iso representing the lenient conversion
    -- of some focus to its unpadded base64url encoding
    --
    _Base64UrlLenientUnpadded :: Iso' a a


instance HasBase64 ByteString where
    _Base64 = prism' B64.encodeBase64 $ \b ->
      case B64.decodeBase64 b of
        Right bs -> Just bs
        _ -> Nothing

    _Base64Url = prism' B64U.encodeBase64 $ \b ->
      case B64U.decodeBase64 b of
        Right bs -> Just bs
        _ -> Nothing

    _Base64Lenient = iso
      B64.encodeBase64
      B64.decodeBase64Lenient

    _Base64UrlLenient = iso
      B64U.encodeBase64
      B64U.decodeBase64Lenient

    _Base64Unpadded = prism' B64.encodeBase64 $ \b ->
      case B64.decodeBase64Unpadded b of
        Right bs -> Just bs
        _ -> Nothing

    _Base64UrlUnpadded = prism' B64U.encodeBase64 $ \b ->
      case B64U.decodeBase64Unpadded b of
        Right bs -> Just bs
        _ -> Nothing

    _Base64LenientUnpadded = iso
      B64.encodeBase64
      B64.decodeBase64Lenient

    _Base64UrlLenientUnpadded = iso
      B64U.encodeBase64
      B64U.decodeBase64Lenient
