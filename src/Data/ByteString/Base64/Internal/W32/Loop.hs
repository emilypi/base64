{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base64.Internal.W32.Loop
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- Internal module defining the encoding and decoding
-- processes and tables.
--
module Data.ByteString.Base64.Internal.W32.Loop
( -- * padded
  innerLoop
, loopTail

  -- * unpadded
, innerLoopNopad
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

-- | Encoding inner loop. Packs 3 bytes from src pointer into
-- the first 6 bytes of 4 'Word8''s (using the encoding table,
-- as 2 'Word12''s ), writing these to the dst pointer.
--
innerLoop
    :: Ptr Word16
    -> Ptr Word8
    -> Ptr Word16
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word8 -> IO ())
    -> IO ()
innerLoop !etable !sptr !dptr !end finish = go (castPtr sptr) dptr
  where
    go !src !dst
      | plusPtr src 2 >= end = finish (castPtr src) (castPtr dst)
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        !w <- peek @Word32 src
#else
        !w <- byteSwap32 <$> peek @Word32 src
#endif
        !x <- peekElemOff etable (fromIntegral (unsafeShiftR w 20))
        !y <- peekElemOff etable (fromIntegral ((unsafeShiftR w 8) .&. 0xfff))

        poke dst x
        poke (plusPtr dst 2) y

        go (plusPtr src 3) (plusPtr dst 4)
{-# INLINE innerLoop #-}

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

-- | Unpadded encoding loop, finalized as a bytestring using the
-- resultant length count.
--
innerLoopNopad
    :: Ptr Word16
    -> Ptr Word8
    -> Ptr Word16
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word8 -> Int -> IO ByteString)
    -> IO ByteString
innerLoopNopad !etable !sptr !dptr !end finish = go (castPtr sptr) dptr 0
  where
    go !src !dst !n
      | plusPtr src 2 >= end = finish (castPtr src) (castPtr dst) n
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        w <- peek @Word32 src
#else
        w <- byteSwap32 <$> peek @Word32 src
#endif
        !x <- peekElemOff etable (fromIntegral (unsafeShiftR w 20))
        !y <- peekElemOff etable (fromIntegral ((unsafeShiftR w 8) .&. 0xfff))

        poke dst x
        poke (plusPtr dst 2) y

        go (plusPtr src 3) (plusPtr dst 4) (n + 4)
{-# INLINE innerLoopNopad #-}

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

          -- ideally, we'd want to pack these is in a single write
          --
          pokeByteOff dst 1 (aix b' alpha)
          pokeByteOff dst 2 (aix c' alpha)
          return (PS dfp 0 (n + 3))
{-# INLINE loopTailNoPad #-}
