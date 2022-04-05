{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}
-- |
-- Module       : Data.Text.Encoding.Base64.Error
-- Copyright    : (c) 2019-2022 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains the error types raised (not as exceptions!)
-- in the decoding process.
--
module Data.Text.Encoding.Base64.Error
( Base64Error(..)
) where


import Control.DeepSeq (NFData(..))
import Control.Exception (Exception(..))

import Data.Text (Text)

import GHC.Generics

-- | This data type represents the type of decoding errors of
-- various kinds as they pertain to decoding 'Text' values.
-- Namely, to distinguish between decoding errors from opaque
-- unicode exceptions caught in the unicode decoding process.
--
data Base64Error e
  = DecodeError Text
    -- ^ The error associated with decoding failure
    -- as a result of the Base64 decoding process
  | ConversionError e
    -- ^ The error associated with the decoding failure
    -- as a result of the conversion process
  deriving
    ( Eq, Show
    , Generic
      -- ^ @since 4.2.2
    )

-- |
--
-- @since 4.2.2
--
instance Exception e => Exception (Base64Error e)


-- |
--
-- @since 4.2.2
--
instance NFData e => NFData (Base64Error e)
