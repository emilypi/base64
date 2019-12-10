{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
, HasBase64Unpadded(..)
) where


import Control.Lens

import Data.ByteString
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U


-- | If a particular type @a@ has a base64 representation
-- for any of its focii, this class provides the optical interface
-- for satisfying the padded base64 spec in RFC 4648
--
class HasBase64 s a where
    -- | A prism into a base64-encoded focus of
    -- some type
    --
    _Base64 :: Prism' s a

    -- | A prism into the base64url-encoded focus of
    -- some type
    --
    _Base64Url :: Prism' s a

-- | If a particular type @a@ has an unpadded base64 representation
-- for any of its focii, this class provides the optical interface
-- for satisfying the unpadded base64 spec in RFC 4648
--
class HasBase64Unpadded s a where
    -- | A prism into the unpadded base64-encoded focus of
    -- some type
    --
    _Base64Unpadded :: Prism' s a

    -- | A prism into the unpadded base64url-encoded focus of
    -- some type
    --
    _Base64UrlUnpadded :: Prism' s a


instance HasBase64 ByteString ByteString where
    _Base64 = prism' B64.encodeBase64 $ \s -> case B64.decodeBase64 s of
      Left _ -> Nothing
      Right a -> Just a

    _Base64Url = prism' B64U.encodeBase64 $ \s -> case B64U.decodeBase64 s of
      Left _ -> Nothing
      Right a -> Just a

instance HasBase64Unpadded ByteString ByteString where
    _Base64Unpadded = prism' B64.encodeBase64 $ \s -> case B64U.decodeBase64 s of
      Left _ -> Nothing
      Right a -> Just a

    _Base64UrlUnpadded = prism' B64.encodeBase64 $ \s -> case B64U.decodeBase64Unpadded s of
      Left _ -> Nothing
      Right a -> Just a
