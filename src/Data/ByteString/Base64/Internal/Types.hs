{-# LANGUAGE BangPatterns #-}
-- |
-- Module       : Data.ByteString.Base64.Internal.Types
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- Shared internal types
--
module Data.ByteString.Base64.Internal.Types
( EncodingTable(..)
, Padding(..)
) where


import Foreign.ForeignPtr
import Foreign.Ptr

import GHC.Word


-- | A type isomorphic to 'Bool' marking support for padding out bytestrings (@Pad),
-- or not (@Nopad@).
--
data Padding
    = Pad
      -- ^ Do we pad out the bytestring?
    | NoPad
      -- ^ Do we not pad out the bytestring?
    deriving Eq

-- | Only the lookup table need be a foreignptr,
-- and then, only so that we can automate some touches to keep it alive
--
data EncodingTable = EncodingTable
  {-# UNPACK #-} !(Ptr Word8)
  {-# UNPACK #-} !(ForeignPtr Word16)
