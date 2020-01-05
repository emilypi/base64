-- |
-- Module       : Data.Text.Encoding.Base64.Lens
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains the 'AsBase64Text' and 'AsBase64UnpaddedText' instances
-- for 'Text', which defined to be the collection of 'Control.Lens.Type.Prism's defining the
-- RFC 4648 specification for the padded and unpadded Base64 encoding format.
--
module Data.Text.Encoding.Base64.Lens
( AsBase64Text(..)
, AsBase64UnpaddedText(..)
) where


import Control.Lens

import Data.Text (Text)
import qualified Data.Text.Encoding.Base64 as B64T
import qualified Data.Text.Encoding.Base64.URL as B64TU

-- | If a particular type @s@ has a base64 representation
-- for any of its focii, this class provides the optical interface
-- for satisfying the padded base64 spec in RFC 4648
--
class AsBase64Text s where
    -- | A prism into a base64-encoded focus of
    -- some type
    --
    -- Examples:
    --
    -- >>> _Base64Text @Text # "Sun"
    -- "UV3u"
    --    --
    -- >>> "PDw/Pz8+Pg==" ^? _Base64Text
    -- Just "<<???>>"
    --
    _Base64Text :: Prism' s Text

    -- | A prism into the base64url-encoded focus of
    -- some type
    --
    -- Examples:
    --
    -- >>> _Base64UrlText @Text # "Sun"
    -- "UV3u"
    --
    -- >>> "PDw_Pz8-Pg==" ^? _Base64UrlText
    -- Just "<<???>>"
    --
    _Base64UrlText :: Prism' s Text

-- | If a particular type @a@ has an unpadded base64 representation
-- for any of its focii, this class provides the optical interface
-- for satisfying the unpadded base64 spec in RFC 4648
--
class AsBase64UnpaddedText s where
    -- | A prism into the unpadded base64-encoded focus of
    -- some type
    --
    _Base64UnpaddedText :: Prism' s Text

    -- | A prism into the unpadded base64url-encoded focus of
    -- some type
    --
    _Base64UrlUnpaddedText :: Prism' s Text


instance AsBase64Text Text where
    _Base64Text = prism' B64T.encodeBase64 $ \s -> case B64T.decodeBase64 s of
      Left _ -> Nothing
      Right a -> Just a
    {-# INLINE _Base64Text #-}

    _Base64UrlText = prism' B64TU.encodeBase64 $ \s -> case B64TU.decodeBase64 s of
      Left _ -> Nothing
      Right a -> Just a
    {-# INLINE _Base64UrlText #-}

instance AsBase64UnpaddedText Text where
    _Base64UnpaddedText = prism' B64T.encodeBase64 $ \s -> case B64T.decodeBase64 s of
      Left _ -> Nothing
      Right a -> Just a
    {-# INLINE _Base64UnpaddedText #-}

    _Base64UrlUnpaddedText = prism' B64TU.encodeBase64 $ \s -> case B64TU.decodeBase64Unpadded s of
      Left _ -> Nothing
      Right a -> Just a
    {-# INLINE _Base64UrlUnpaddedText #-}
