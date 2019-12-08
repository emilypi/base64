{-# OPTIONS_GHC -fno-warn-orphans #-}
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
module Data.Text.Encoding.Base64.Lens where


import Control.Lens

import Data.Text
import Data.ByteString.Base64.Lens
import qualified Data.Text.Encoding.Base64 as B64T
import qualified Data.Text.Encoding.Base64.URL as B64TU


instance HasBase64 Text Text Text Text where
    _Base64 = lens B64T.encodeBase64 (\_ b -> b)
    _Base64Url = lens B64TU.encodeBase64 (\_ b -> b)
    _Base64Unpadded = lens B64T.encodeBase64Unpadded (\_ b -> b)
    _Base64UrlUnpadded = lens B64TU.encodeBase64Unpadded (\_ b -> b)
