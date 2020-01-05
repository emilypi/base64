{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module       : Data.Text.Encoding.Base64.Lens
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: TypeFamilies
--
-- This module contains the 'AsBase64' and 'AsBase64Unpadded' instances
-- for 'Text', which defined to be the collection of 'Control.Lens.Type.Prism's defining the
-- RFC 4648 specification for the padded and unpadded Base64 encoding format.
--
-- These typeclasses are re-exported for convenience
--
module Data.Text.Encoding.Base64.Lens
( AsBase64(..)
, AsBase64Unpadded(..)
) where


import Control.Lens

import Data.Text (Text)
import Data.ByteString.Base64.Lens
import qualified Data.Text.Encoding.Base64 as B64T
import qualified Data.Text.Encoding.Base64.URL as B64TU


instance AsBase64 Text where
    type Base64 Text = Text

    _Base64 = prism' B64T.encodeBase64 $ \s -> case B64T.decodeBase64 s of
      Left _ -> Nothing
      Right a -> Just a
    {-# INLINE _Base64 #-}

    _Base64Url = prism' B64TU.encodeBase64 $ \s -> case B64TU.decodeBase64 s of
      Left _ -> Nothing
      Right a -> Just a
    {-# INLINE _Base64Url #-}

instance AsBase64Unpadded Text where
    type Base64Unpadded Text = Text

    _Base64Unpadded = prism' B64T.encodeBase64 $ \s -> case B64T.decodeBase64 s of
      Left _ -> Nothing
      Right a -> Just a
    {-# INLINE _Base64Unpadded #-}

    _Base64UrlUnpadded = prism' B64TU.encodeBase64 $ \s -> case B64TU.decodeBase64Unpadded s of
      Left _ -> Nothing
      Right a -> Just a
    {-# INLINE _Base64UrlUnpadded #-}
