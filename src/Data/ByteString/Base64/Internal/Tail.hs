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
    -> Int
    -> IO ByteString
loopTail !dfp (Ptr !alpha) !end !src !dst !n
    | src == end = return (PS dfp 0 n)
    | plusPtr src 1 == end = do
      !x <- peek @Word8  src

      let !a = shiftR (x .&. 0xfc) 2
          !b = shiftL (x .&. 0x03) 4

      poke @Word8 dst (aix a alpha)
      poke @Word8 (plusPtr dst 1) (aix b alpha)
      poke @Word8 (plusPtr dst 2) 0x3d
      poke @Word8 (plusPtr dst 3) 0x3d
      return (PS dfp 0 (n + 4))

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
      return (PS dfp 0 (n + 4))
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
      | plusPtr src 1 == end = do
        !x <- peek @Word8 src

        let !a = shiftR (x .&. 0xfc) 2
            !b = shiftL (x .&. 0x03) 4

        poke @Word8 dst (aix a alpha)
        poke @Word8 (plusPtr dst 1) (aix b alpha)
        return (PS dfp 0 (n + 2))
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
        return (PS dfp 0 (n + 3))
{-# INLINE loopTailNoPad #-}

decodeTail
    :: ForeignPtr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Int
    -> IO (Either Text ByteString)
decodeTail !dfp !dtable !end !dst !src !n
    | src >= end = return (Right (PS dfp 0 n))
    | otherwise = do
      !a <- look src
      !b <- look (src `plusPtr` 1)
      !c <- look (src `plusPtr` 2)
      !d <- look (src `plusPtr` 3)

      if
        | a == 0x63 -> padErr src
        | b == 0x63 -> padErr (plusPtr src 1)
        | a == 0xff -> err src
        | b == 0xff -> err (plusPtr src 1)
        | c == 0xff -> err (plusPtr src 2)
        | d == 0xff -> err (plusPtr src 3)
        | otherwise -> do

          let !w = (unsafeShiftL a 18)
                .|. (unsafeShiftL b 12)
                .|. (unsafeShiftL c 6)
                .|. d

          poke @Word8 dst (fromIntegral (unsafeShiftR w 16))

          if
            | c == 0x63 -> return $ Right (PS dfp 0 (n + 1))
            | d == 0x63 -> do
              poke @Word8 (plusPtr dst 1) (fromIntegral (unsafeShiftR w 8))
              return $ Right (PS dfp 0 (n + 2))
            | otherwise -> do
              poke @Word8 (plusPtr dst 1) (fromIntegral (unsafeShiftR w 8))
              poke @Word8 (plusPtr dst 2) (fromIntegral w)
              return $ Right (PS dfp 0 (n + 3))
  where
    look :: Ptr Word8 -> IO Word32
    look !p = do
      !i <- peekByteOff @Word8 p 0
      !v <- peekByteOff @Word8 dtable (fromIntegral i)
      return (fromIntegral v)

    err p = return . Left . T.pack
      $ "invalid character at offset: "
      ++ show (p `minusPtr` src)

    padErr p =  return . Left . T.pack
      $ "invalid padding at offset: "
      ++ show (p `minusPtr` src)
