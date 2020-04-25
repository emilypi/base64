{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
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
, decodeTail
) where

import Data.Bits
import Data.ByteString.Internal
import Data.ByteString.Base64.Internal.Utils
import Data.Text (Text)
import qualified Data.Text as T

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
loopTail !dfp (Ptr !alpha) !dptr !end !src !dst
    | src == end = return (PS dfp 0 (minusPtr dst dptr))
    | plusPtr src 1 == end = do
      !x <- peek @Word8  src

      let !a = shiftR (x .&. 0xfc) 2
          !b = shiftL (x .&. 0x03) 4

      poke @Word8 dst (aix a alpha)
      poke @Word8 (plusPtr dst 1) (aix b alpha)
      poke @Word8 (plusPtr dst 2) 0x3d
      poke @Word8 (plusPtr dst 3) 0x3d
      return (PS dfp 0 (4 + minusPtr dst dptr))

    | otherwise = do
      !x <- peek @Word8  src
      !y <- peek @Word8 (plusPtr src 1)

      let !a = shiftR (x .&. 0xfc) 2
          !b = shiftL (x .&. 0x03) 4

      let !c = shiftR (y .&. 0xf0) 4 .|. b
          !d = shiftL (y .&. 0x0f) 2

      poke @Word8 dst (aix a alpha)
      poke @Word8 (plusPtr dst 1) (aix c alpha)
      poke @Word8 (plusPtr dst 2) (aix d alpha)
      poke @Word8 (plusPtr dst 3) 0x3d
      return (PS dfp 0 (4 + minusPtr dst dptr))
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
    -> Ptr Word8
    -> IO ByteString
loopTailNoPad !dfp (Ptr !alpha) !dptr !end !src !dst
      | src == end = return (PS dfp 0 (minusPtr dst dptr))
      | plusPtr src 1 == end = do
        !x <- peek @Word8 src

        let !a = shiftR (x .&. 0xfc) 2
            !b = shiftL (x .&. 0x03) 4

        poke @Word8 dst (aix a alpha)
        poke @Word8 (plusPtr dst 1) (aix b alpha)
        return (PS dfp 0 (2 + (minusPtr dst dptr)))
      | otherwise = do
        !x <- peek @Word8 src
        !y <- peek @Word8 (plusPtr src 1)

        let !a = shiftR (x .&. 0xfc) 2
            !b = shiftL (x .&. 0x03) 4

        let !c = shiftR (y .&. 0xf0) 4 .|. b
            !d = shiftL (y .&. 0x0f) 2

        poke @Word8 dst (aix a alpha)
        poke @Word8 (plusPtr dst 1) (aix c alpha)
        poke @Word8 (plusPtr dst 2) (aix d alpha)
        return (PS dfp 0 (3 + (minusPtr dst dptr)))
{-# INLINE loopTailNoPad #-}

decodeTail
    :: ForeignPtr Word8
      -- ^ dst bytestring foreign ptr
    -> Ptr Word8
      -- ^ decode table
    -> Ptr Word8
      -- ^ original decode static pointer
    -> Ptr Word8
      -- ^ end of the src ptr
    -> Ptr Word8
      -- ^ dst ptr
    -> Ptr Word8
      -- ^ src ptr
    -> IO (Either Text ByteString)
decodeTail !dfp !dtable !dptr !end !dst !src
    | src >= end = return (Right (PS dfp 0 (minusPtr dst dptr)))
    | otherwise = do
      !w <- peek @Word8 src
      !x <- peek @Word8 (plusPtr src 1)
      !y <- peek @Word8 (plusPtr src 2)
      !z <- peek @Word8 (plusPtr src 3)

      !a <- w32 <$> peekByteOff @Word8 dtable (fromIntegral w)
      !b <- w32 <$> peekByteOff @Word8 dtable (fromIntegral x)
      !c <- w32 <$> peekByteOff @Word8 dtable (fromIntegral y)
      !d <- w32 <$> peekByteOff @Word8 dtable (fromIntegral z)

      if
        | a == 0xff -> err src
        | b == 0xff -> err (plusPtr src 1)
        | c == 0xff -> err (plusPtr src 2)
        | d == 0xff -> err (plusPtr src 3)
        | a == 0x63 -> padErr src
        | b == 0x63 -> padErr (plusPtr src 1)
        | c == 0x63, d /= 0x63 -> padErr (plusPtr src 3)
        | otherwise -> do

          let !ww = (unsafeShiftL a 18)
                .|. (unsafeShiftL b 12)
                .|. (unsafeShiftL c 6)
                .|. d

          if
            | c == 0x63, d == 0x63 -> do
              poke @Word8 dst (fromIntegral (unsafeShiftR ww 16))
              return $ Right (PS dfp 0 (1 + (minusPtr dst dptr)))
            | d == 0x63 -> do
              poke @Word8 dst (fromIntegral (unsafeShiftR ww 16))
              poke @Word8 (plusPtr dst 1) (fromIntegral (unsafeShiftR ww 8))
              return $ Right (PS dfp 0 (2 + (minusPtr dst dptr)))
            | otherwise -> do
              poke @Word8 dst (fromIntegral (unsafeShiftR ww 16))
              poke @Word8 (plusPtr dst 1) (fromIntegral (unsafeShiftR ww 8))
              poke @Word8 (plusPtr dst 2) (fromIntegral ww)
              return $ Right (PS dfp 0 (3 + (minusPtr dst dptr)))
  where
    err p = return . Left . T.pack
      $ "invalid character at offset: "
      ++ show (p `minusPtr` src)

    padErr p =  return . Left . T.pack
      $ "invalid padding at offset: "
      ++ show (p `minusPtr` src)
