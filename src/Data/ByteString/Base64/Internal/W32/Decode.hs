{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base64.Internal.W32.Decode
-- Copyright 	: (c) 2019-2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- 'Word32'-optimized inner loop
--
module Data.ByteString.Base64.Internal.W32.Decode
( decodeLoop
) where

import Data.Bits
import Data.ByteString.Internal
import Data.Text (Text)
import qualified Data.Text as T

import Foreign.Ptr
import Foreign.Storable

import GHC.Word

decodeLoop = undefined
