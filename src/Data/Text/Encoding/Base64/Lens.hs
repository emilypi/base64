{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module       : Data.Text.Encoding.Base64.Lens
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains the 'HasBase64' and 'HasBase64Unpadded' instances
-- for 'Text', which defined to be the collection of 'Control.Lens.Type.Prism's defining the
-- RFC 4648 specification for the padded and unpadded Base64 encoding format.
--
-- These typeclasses are re-exported for convenience
--
module Data.Text.Encoding.Base64.Lens
-- ( HasBase64(..)
-- , HasBase64Unpadded(..)
-- ) where
where


import Control.Lens

import Data.Text
import Data.ByteString.Base64.Lens
import qualified Data.Text.Encoding.Base64 as B64T
import qualified Data.Text.Encoding.Base64.URL as B64TU


instance HasBase64 Text where
    type Base64 Text = Text
    _Base64 = prism' B64T.encodeBase64 $ \s -> case B64T.decodeBase64 s of
      Left _ -> Nothing
      Right a -> Just a

    _Base64Url = prism' B64TU.encodeBase64 $ \s -> case B64TU.decodeBase64 s of
      Left _ -> Nothing
      Right a -> Just a

instance HasBase64Unpadded Text where
    type Base64Unpadded Text = Text

    _Base64Unpadded = prism' B64T.encodeBase64 $ \s -> case B64T.decodeBase64 s of
      Left _ -> Nothing
      Right a -> Just a

    _Base64UrlUnpadded = prism' B64TU.encodeBase64 $ \s -> case B64TU.decodeBase64Unpadded s of
      Left _ -> Nothing
      Right a -> Just a
