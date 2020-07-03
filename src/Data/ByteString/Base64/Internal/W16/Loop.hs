{-# LANGUAGE BangPatterns #-}
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
    -> (Ptr Word8 -> Ptr Word8 -> IO ByteString)
    -> IO ByteString
innerLoop !etable !sptr !dptr !end finish = go sptr dptr
  where
    go !src !dst
      | plusPtr src 2 >= end = finish src (castPtr dst)
      | otherwise = do

        !i <- w32 <$> peek src
        !j <- w32 <$> peek (plusPtr src 1)
        !k <- w32 <$> peek (plusPtr src 2)

        let !w = (unsafeShiftL i 16) .|. (unsafeShiftL j 8) .|. k

        !x <- peekElemOff etable (fromIntegral (unsafeShiftR w 12))
        !y <- peekElemOff etable (fromIntegral (w .&. 0xfff))

        poke dst x
        poke (plusPtr dst 2) y

        go (plusPtr src 3) (plusPtr dst 4)
{-# inline innerLoop #-}

decodeLoop
    :: Ptr Word8
        -- ^ decode lookup table
    -> Ptr Word8
        -- ^ src pointer
    -> Ptr Word8
        -- ^ dst pointer
    -> Ptr Word8
    -> ForeignPtr Word8
    -> IO (Either Text ByteString)
decodeLoop !dtable !sptr !dptr !end dfp = go dptr sptr
  where
    err :: Ptr Word8 -> IO (Either Text ByteString)
    err p = return . Left . T.pack
      $ "invalid character at offset: "
      ++ show (p `minusPtr` sptr)

    padErr :: Ptr Word8 -> IO (Either Text ByteString)
    padErr p =  return . Left . T.pack
      $ "invalid padding at offset: "
      ++ show (p `minusPtr` sptr)

    canonErr :: Ptr Word8 -> IO (Either Text ByteString)
    canonErr p = return . Left . T.pack
      $ "non-canonical encoding detected at offset: "
      ++ show (p `minusPtr` sptr)

    look :: Ptr Word8 -> IO Word32
    look !p = do
      !i <- peekByteOff @Word8 p 0
      !v <- peekByteOff @Word8 dtable (fromIntegral i)
      return (fromIntegral v)

    go !dst !src
      | plusPtr src 4 >= end = do
        a <- look src
        b <- look (src `plusPtr` 1)
        c <- look (src `plusPtr` 2)
        d <- look (src `plusPtr` 3)
        finalChunk dst src a b c d

      | otherwise = do
        a <- look src
        b <- look (src `plusPtr` 1)
        c <- look (src `plusPtr` 2)
        d <- look (src `plusPtr` 3)
        decodeChunk dst src a b c d

    -- | Decodes chunks of 4 bytes at a time, recombining into
    -- 3 bytes. Note that in the inner loop stage, no padding
    -- characters are admissible.
    --
    decodeChunk !dst !src a b c d
     | a == 0x63 = padErr src
     | b == 0x63 = padErr (plusPtr src 1)
     | c == 0x63 = padErr (plusPtr src 2)
     | d == 0x63 = padErr (plusPtr src 3)
     | a == 0xff = err src
     | b == 0xff = err (plusPtr src 1)
     | c == 0xff = err (plusPtr src 2)
     | d == 0xff = err (plusPtr src 3)
     | otherwise = do

       let !w = ((unsafeShiftL a 18)
             .|. (unsafeShiftL b 12)
             .|. (unsafeShiftL c 6)
             .|. d) :: Word32

       poke @Word8 dst (fromIntegral (unsafeShiftR w 16))
       poke @Word8 (plusPtr dst 1) (fromIntegral (unsafeShiftR w 8))
       poke @Word8 (plusPtr dst 2) (fromIntegral w)
       go (plusPtr dst 3) (plusPtr src 4)

    -- | Decode the final 4 bytes in the string, recombining into
    -- 3 bytes. Note that in this stage, we can have padding chars
    -- but only in the final 2 positions.
    --
    finalChunk !dst !src a b c d
      | a == 0x63 = padErr src
      | b == 0x63 = padErr (plusPtr src 1)
      | c == 0x63 && d /= 0x63 = err (plusPtr src 3) -- make sure padding is coherent.
      | a == 0xff = err src
      | b == 0xff = err (plusPtr src 1)
      | c == 0xff = err (plusPtr src 2)
      | d == 0xff = err (plusPtr src 3)
      | otherwise = do
        let !w = ((unsafeShiftL a 18)
              .|. (unsafeShiftL b 12)
              .|. (unsafeShiftL c 6)
              .|. d) :: Word32

        poke @Word8 dst (fromIntegral (unsafeShiftR w 16))

        if c == 0x63 && d == 0x63
        then
          if validateLastPos b mask_4bits
          then return $ Right $ PS dfp 0 (1 + (dst `minusPtr` dptr))
          else canonErr (plusPtr src 1)
        else if d == 0x63
          then if validateLastPos c mask_2bits
            then do
               poke @Word8 (plusPtr dst 1) (fromIntegral (unsafeShiftR w 8))
               return $ Right $ PS dfp 0 (2 + (dst `minusPtr` dptr))
            else canonErr (plusPtr src 2)
          else do
            poke @Word8 (plusPtr dst 1) (fromIntegral (unsafeShiftR w 8))
            poke @Word8 (plusPtr dst 2) (fromIntegral w)
            return $ Right $ PS dfp 0 (3 + (dst `minusPtr` dptr))
{-# inline decodeLoop #-}

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
    finalize !n = return $ PS dfp 0 n
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
                let !w = (unsafeShiftL a 18) .|. (unsafeShiftL b 12) .|. (unsafeShiftL c 6) .|. d

                poke @Word8 dst (fromIntegral (unsafeShiftR w 16))
                if c == 0x63
                then finalize (n + 1)
                else do
                  poke @Word8 (plusPtr dst 1) (fromIntegral (w `unsafeShiftR` 8))
                  if d == 0x63
                  then finalize (n + 2)
                  else do
                    poke @Word8 (plusPtr dst 2) (fromIntegral w)
                    go (plusPtr dst 3) dp (n + 3)
