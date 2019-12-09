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
, HasBase64'
) where


import Control.Lens

import Data.ByteString
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U


-- | If a particular type @a@ has a base64 representation
-- for any of its focii, this class provides the optical interface
-- for satisfying the RFC 4648
--
class HasBase64 s t a b | s -> a, t -> b, s b -> t, t a -> s where
    -- | A prism into a base64-encoded focus of
    -- some type
    --
    _Base64 :: Lens s t a b

    -- | A prism into the base64url-encoded focus of
    -- some type
    --
    _Base64Url :: Lens s t a b

    -- | A prism into the unpadded base64-encoded focus of
    -- some type
    --
    _Base64Unpadded :: Lens s t a b

    -- | A prism into the unpadded base64url-encoded focus of
    -- some type
    --
    _Base64UrlUnpadded :: Lens s t a b


type HasBase64' s a = HasBase64 s s a a

instance HasBase64 ByteString ByteString ByteString ByteString where
    _Base64 = lens B64.encodeBase64 (\_ b -> b)
    _Base64Url = lens B64U.encodeBase64 (\_ b -> b)
    _Base64Unpadded = lens B64.encodeBase64Unpadded (\_ b -> b)
    _Base64UrlUnpadded = lens B64U.encodeBase64Unpadded (\_ b -> b)
