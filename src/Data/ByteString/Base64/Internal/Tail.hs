{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base64.Internal.W32.Loop
-- Copyright    : (c) 2019 Emily Pillmore
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
import Data.Text

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import GHC.Exts
import GHC.Word

-- | Finalize an encoded bytestring by filling in the remaining
-- bytes and any padding
--
loopTail :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
loopTail (Ptr !alpha) !end !src !dst
    | src == end = return ()
    | otherwise = do
      !k <- peekByteOff src 0

      let !a = shiftR (k .&. 0xfc) 2
          !b = shiftL (k .&. 0x03) 4

      pokeByteOff dst 0 (aix a alpha)

      if plusPtr src 2 /= end
      then do
        pokeByteOff dst 1 (aix b alpha)
        pokeByteOff @Word8 dst 2 0x3d
        pokeByteOff @Word8 dst 3 0x3d
      else do
        !k' <- peekByteOff src 1

        let !b' = shiftR (k' .&. 0xf0) 4 .|. b
            !c' = shiftL (k' .&. 0x0f) 2

        pokeByteOff dst 1 (aix b' alpha)
        pokeByteOff dst 2 (aix c' alpha)
        pokeByteOff @Word8 dst 3 0x3d
{-# INLINE loopTail #-}


-- | Finalize a bytestring by filling out the remaining bits
-- without padding.
--
loopTailNoPad
    :: ForeignPtr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Int
    -> IO ByteString
loopTailNoPad !dfp (Ptr !alpha) !end !src !dst !n
      | src == end = return (PS dfp 0 n)
      | otherwise = do
        !k <- peekByteOff src 0

        let !a = shiftR (k .&. 0xfc) 2
            !b = shiftL (k .&. 0x03) 4

        pokeByteOff dst 0 (aix a alpha)

        if plusPtr src 2 /= end
        then do
          pokeByteOff dst 1 (aix b alpha)
          return (PS dfp 0 (n + 2))
        else do
          !k' <- peekByteOff src 1

          let !b' = shiftR (k' .&. 0xf0) 4 .|. b
              !c' = shiftL (k' .&. 0x0f) 2

          pokeByteOff dst 1 (aix b' alpha)
          pokeByteOff dst 2 (aix c' alpha)
          return (PS dfp 0 (n + 3))
{-# INLINE loopTailNoPad #-}

decodeTail
    :: ForeignPtr Word8
    -> Ptr Word8
    -> Ptr Word32
    -> Int
    -> IO (Either Text ByteString)
decodeTail !dfp !sptr !dptr !n = undefined
