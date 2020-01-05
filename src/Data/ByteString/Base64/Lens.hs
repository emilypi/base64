-- |
-- Module       : Data.Text.Encoding.Base64.Lens
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains 'Control.Lens.Type.Prism's Base64-encoding and
-- decoding 'ByteString' values.
--
module Data.ByteString.Base64.Lens
( -- * Prisms
  _Base64
, _Base64Url
, _Base64Unpadded
, _Base64UrlUnpadded
) where


import Control.Lens

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U

-- | A 'Control.Lens.Type.Prism' into the Base64 encoding of a 'ByteString' value
--
-- >>> _Base64 # "Sun"
-- "UV3u"
--
-- >>> "UV3u" ^? _Base64
-- Just "Sun"
--
_Base64 :: Prism' ByteString ByteString
_Base64 = prism' B64.encodeBase64 $ \s -> case B64.decodeBase64 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base64 #-}

-- | A 'Control.Lens.Type.Prism' into the Base64-url encoding of a 'ByteString' value
--
-- >>> _Base64Url # "Sun"
-- "UV3u"
--
-- >>> "PDw_Pz8-Pg==" ^? _Base64Url
-- Just "<<???>>"
--
_Base64Url :: Prism' ByteString ByteString
_Base64Url = prism' B64U.encodeBase64 $ \s -> case B64U.decodeBase64 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base64Url #-}

-- | A 'Control.Lens.Type.Prism' into the unpadded Base64 encoding of a
-- 'ByteString' value
--
-- Please note that unpadded variants should only be used
-- when assumptions about the data can be made. In particular, if the length of
-- the input is divisible by 3, then this is a safe function to call.
--
-- >>> _Base64Unpadded # "Sun"
-- "UV3u"
--
-- >>> "UV3u" ^? _Base64Unpadded
-- Just "Sun"
--
_Base64Unpadded :: Prism' ByteString ByteString
_Base64Unpadded = prism' B64.encodeBase64Unpadded $ \s -> case B64.decodeBase64Unpadded s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base64Unpadded #-}

-- | A 'Control.Lens.Type.Prism' into the Base64-url encoding of a 'ByteString' value
--
-- Please note that unpadded variants should only be used
-- when assumptions about the data can be made. In particular, if the length of
-- the input is divisible by 3, then this is a safe function to call.
--
-- >>> _Base64UrlUnpadded # "<<??>>"
-- "PDw_Pz4-"
--
-- >>> "PDw_Pz4-" ^? _Base64UrlUnpadded
-- Just "<<??>>"
--
_Base64UrlUnpadded :: Prism' ByteString ByteString
_Base64UrlUnpadded = prism' B64U.encodeBase64Unpadded $ \s -> case B64U.decodeBase64Unpadded s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base64UrlUnpadded #-}
