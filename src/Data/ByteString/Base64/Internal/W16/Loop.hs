{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base64.Internal.W16.Loop
-- Copyright    : (c) 2019-2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- 'Word8' fallback loop
--
module Data.ByteString.Base64.Internal.W16.Loop
( innerLoop
, decodeLoop
, lenientLoop
) where


import Data.Bits
import Data.ByteString.Internal
import Data.ByteString.Base64.Internal.Utils
import Data.Text (Text)
import qualified Data.Text as T

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

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
    -> (Ptr Word8 -> Ptr Word8 -> Int -> IO ByteString)
    -> Int
    -> IO ByteString
innerLoop !etable !sptr !dptr !end finish !nn = go sptr dptr nn
  where
    go !src !dst !n
      | plusPtr src 2 >= end = finish src (castPtr dst) n
      | otherwise = do

        !i <- w32 <$> peek src
        !j <- w32 <$> peek (plusPtr src 1)
        !k <- w32 <$> peek (plusPtr src 2)

        let !w = (shiftL i 16) .|. (shiftL j 8) .|. k

        !x <- peekElemOff etable (fromIntegral (shiftR w 12))
        !y <- peekElemOff etable (fromIntegral (w .&. 0xfff))

        poke dst x
        poke (plusPtr dst 2) y

        go (plusPtr src 3) (plusPtr dst 4) (n + 4)
{-# INLINE innerLoop #-}

decodeLoop
    :: Ptr Word8
        -- ^ decode lookup table
    -> Ptr Word8
        -- ^ src pointer
    -> Ptr Word8
        -- ^ dst pointer
    -> Ptr Word8
        -- ^ end of src ptr
    -> ForeignPtr Word8
        -- ^ dst foreign ptr (for consing bs)
    -> Int
    -> IO (Either Text ByteString)
decodeLoop !dtable !sptr !dptr !end !dfp !nn = go dptr sptr nn
  where
    err p = return . Left . T.pack
      $ "invalid character at offset: "
      ++ show (p `minusPtr` sptr)

    padErr p =  return . Left . T.pack
      $ "invalid padding at offset: "
      ++ show (p `minusPtr` sptr)

    look :: Ptr Word8 -> IO Word32
    look !p = do
      !i <- peekByteOff @Word8 p 0
      !v <- peekByteOff @Word8 dtable (fromIntegral i)
      return (fromIntegral v)

    go !dst !src !n
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
                go (plusPtr dst 3) (plusPtr src 4) (n + 3)
{-# INLINE decodeLoop #-}

lenientLoop
    :: Ptr Word8
        -- ^ decode lookup table
    -> Ptr Word8
        -- ^ src pointer
    -> Ptr Word8
        -- ^ dst pointer
    -> Ptr Word8
        -- ^ end of src ptr
    -> ForeignPtr Word8
        -- ^ dst foreign ptr (for consing bs)
    -> IO ByteString
lenientLoop !dtable !sptr !dptr !end !dfp = go dptr sptr 0
  where
    finalize !n = return (PS dfp 0 n)
    {-# INLINE finalize #-}

    look !skip !p_ f = k p_
      where
        k !p
          | p >= end = f (plusPtr end (-1)) (0x63 :: Word32)
          | otherwise = do
            !i <- peekByteOff @Word8 p 0
            !v <- peekByteOff @Word8 dtable (fromIntegral i)

            if
              | v == 0xff -> k (plusPtr p 1)
              | v == 0x63, skip -> k (plusPtr p 1)
              | otherwise -> f (plusPtr p 1) (fromIntegral v)

    go !dst !src !n
      | src >= end = finalize n
      | otherwise =
        look True src $ \ap a ->
        look True ap $ \bp b ->
          if
            | a == 0x63 -> finalize n
            | b == 0x63 -> finalize n
            | otherwise ->
              look False bp $ \cp c ->
              look False cp $ \dp d -> do
                let !w = (shiftL a 18) .|. (shiftL b 12) .|. (shiftL c 6) .|. d

                poke @Word8 dst (fromIntegral (shiftR w 16))
                if c == 0x63
                then finalize (n + 1)
                else do
                  poke @Word8 (plusPtr dst 1) (fromIntegral (w `shiftR` 8))
                  if d == 0x63
                  then finalize (n + 2)
                  else do
                    poke @Word8 (plusPtr dst 2) (fromIntegral w)
                    go (plusPtr dst 3) dp (n + 3)
{-# INLINE lenientLoop #-}
