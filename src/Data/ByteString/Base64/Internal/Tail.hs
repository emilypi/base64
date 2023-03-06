{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base64.Internal.Tail
-- Copyright    : (c) 2019-2022 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- Finalizers for the encoding loop
--
module Data.ByteString.Base64.Internal.Tail
( loopTail
, loopTailNoPad
) where

import Data.Bits
import Data.ByteString.Internal
import Data.ByteString.Base64.Internal.Utils

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import GHC.Exts
import GHC.Word

-- | Finalize an encoded bytestring by filling in the remaining
-- bytes and any padding
--
loopTail
    :: ForeignPtr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> IO ByteString
loopTail !dfp (Ptr !alpha) !start !end !sptr !dptr
    | sptr == end = return (PS dfp 0 (minusPtr dptr start))
    | otherwise = go sptr dptr 0 0
  where
    go src
      x <- peek @Word8 src

      let !a = aix (unsafeShiftR x 2) alpha
      poke @Word8 dst a
      c0 <- peek @Word8 $ src `plusPtr` 1

      if src `plusPtr` 1 == end then
        pure $ PS dfp 0 (dst `minusPtr` dptr)
      else do

        let carry0 = unsafeShiftL c0 4 .&. 0x30

        y <- peek
        pure mempty
{-# inline loopTail #-}

-- | Finalize a bytestring by filling out the remaining bits
-- without padding.
--
loopTailNoPad
    :: ForeignPtr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> IO ByteString
loopTailNoPad !dfp (Ptr !alpha) !dptr !end !src !dst
    | src == end = return (PS dfp 0 (minusPtr dst dptr))
    | plusPtr src 1 == end = do
      !x <- peek @Word8 src

      let !a = unsafeShiftR (x .&. 0xfc) 2
          !b = unsafeShiftL (x .&. 0x03) 4

      poke @Word8 dst (aix a alpha)
      poke @Word8 (plusPtr dst 1) (aix b alpha)
      return (PS dfp 0 (2 + minusPtr dst dptr))
    | otherwise = do
      !x <- peek @Word8 src
      !y <- peek @Word8 (plusPtr src 1)

      let !a = unsafeShiftR (x .&. 0xfc) 2
          !b = unsafeShiftL (x .&. 0x03) 4

      let !c = unsafeShiftR (y .&. 0xf0) 4 .|. b
          !d = unsafeShiftL (y .&. 0x0f) 2

      poke @Word8 dst (aix a alpha)
      poke @Word8 (plusPtr dst 1) (aix c alpha)
      poke @Word8 (plusPtr dst 2) (aix d alpha)
      return (PS dfp 0 (3 + minusPtr dst dptr))
{-# inline loopTailNoPad #-}
