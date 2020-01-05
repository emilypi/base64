{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module       : Data.Text.Encoding.Base64.Lens
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: non-portable
--
-- This module contains 'Control.Lens.Type.Prism's Base64-encoding and
-- decoding 'Text' values.
--
module Data.Text.Encoding.Base64.Lens
( -- * Prisms
  _Base64
, _Base64Url
, _Base64Unpadded
, _Base64UrlUnpadded
  -- * Patterns
, pattern Base64
, pattern Base64Url
, pattern Base64Unpadded
, pattern Base64UrlUnpadded
) where


import Control.Lens

import Data.Text (Text)
import qualified Data.Text.Encoding.Base64 as B64T
import qualified Data.Text.Encoding.Base64.URL as B64TU


-- -------------------------------------------------------------------------- --
-- Optics

-- | A 'Control.Lens.Type.Prism' into the Base64 encoding of a 'Text' value.
--
-- >>> _Base64 # "Sun"
-- "UV3u"
--
-- >>> "UV3u" ^? _Base64
-- Just "Sun"
--
_Base64 :: Prism' Text Text
_Base64 = prism' B64T.encodeBase64 $ \s -> case B64T.decodeBase64 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base64 #-}

-- | A 'Control.Lens.Type.Prism' into the Base64-url encoding of a 'Text' value.
--
-- >>> _Base64Url # "Sun"
-- "UV3u"
--
-- >>> "PDw_Pz8-Pg==" ^? _Base64Url
-- Just "<<???>>"
--
_Base64Url :: Prism' Text Text
_Base64Url = prism' B64TU.encodeBase64 $ \s -> case B64TU.decodeBase64 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base64Url #-}

-- | A 'Control.Lens.Type.Prism' into the unpadded Base64 encoding of a
-- 'Text' value.
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
_Base64Unpadded :: Prism' Text Text
_Base64Unpadded = prism' B64T.encodeBase64Unpadded $ \s -> case B64T.decodeBase64Unpadded s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base64Unpadded #-}

-- | A 'Control.Lens.Type.Prism' into the Base64-url encoding of a 'Text' value.
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
_Base64UrlUnpadded :: Prism' Text Text
_Base64UrlUnpadded = prism' B64TU.encodeBase64Unpadded $ \s -> case B64TU.decodeBase64Unpadded s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base64UrlUnpadded #-}

-- -------------------------------------------------------------------------- --
-- Patterns

pattern Base64 :: Text -> Text
pattern Base64 a <- (preview _Base64 -> Just a) where
    Base64 a = _Base64 # a

pattern Base64Url :: Text -> Text
pattern Base64Url a <- (preview _Base64Url -> Just a) where
    Base64Url a = _Base64Url # a

pattern Base64Unpadded :: Text -> Text
pattern Base64Unpadded a <- (preview _Base64Unpadded -> Just a) where
    Base64Unpadded a = _Base64Unpadded # a

pattern Base64UrlUnpadded :: Text -> Text
pattern Base64UrlUnpadded a <- (preview _Base64UrlUnpadded -> Just a) where
    Base64UrlUnpadded a = _Base64UrlUnpadded # a
