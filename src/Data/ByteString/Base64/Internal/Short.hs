{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.ByteString.Base64.Internal.Short
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-style
--
-- Direct byte-array encoding and decoding. SIMD consumes a bounded prefix;
-- scalar tails retain padding checks, canonicity checks, and absolute offsets.
-- The direct path supplies missing URL padding virtually, without copying
-- the source.
-- Larger scalar-only inputs reuse the established strict bytestring loops,
-- whose throughput outweighs conversion costs at these sizes.
module Data.ByteString.Base64.Internal.Short
( Padding(..)
, encodeShort
, decodeShort
, decodeShortTyped
) where

import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Data.Base64.Types (Base64, assertBase64, extractBase64)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.Internal.Head as Strict
import Data.ByteString.Base64.Internal.Utils
#ifdef BASE64_SIMD
import qualified Data.ByteString.Base64.Internal.SIMD as SIMD
#endif
import qualified Data.ByteString.Short as SBS
import Data.ByteString.Short.Internal (ShortByteString(..))
import Data.Primitive.ByteArray
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Storable (peekElemOff)
import GHC.Exts (Ptr(..))
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | Padding policy for checked decoding.
data Padding = Required | Optional | Forbidden

-- | Use direct byte arrays for small inputs or native SIMD. For larger
-- scalar-only inputs, the existing strict encoder is faster even with copies.
encodeShort :: Bool -> EncodingTable -> ShortByteString -> ShortByteString
encodeShort padded table input
  | directAt 96 (SBS.length input) = encodeDirect padded table input
  | padded = SBS.toShort (Strict.encodeBase64_ table (SBS.fromShort input))
  | otherwise = SBS.toShort (Strict.encodeBase64Nopad_ table (SBS.fromShort input))
{-# INLINE encodeShort #-}

encodeDirect :: Bool -> EncodingTable -> ShortByteString -> ShortByteString
encodeDirect padded (EncodingTable !(Ptr alpha) _) (SBS src#) = runST $ do
  dst <- newByteArray dlen
#ifdef BASE64_SIMD
  consumed <- SIMD.encodeByteArrayPrefix src slen dst (Ptr alpha)
#else
  let consumed = 0
#endif
  let put !offset !value = writeByteArray dst offset (aix value alpha)
      go !soff !doff
        | soff + 2 < slen = do
            let !a = byte soff
                !b = byte (soff + 1)
                !c = byte (soff + 2)
            put doff (a `unsafeShiftR` 2)
            put (doff + 1) ((a .&. 3) `unsafeShiftL` 4 .|. b `unsafeShiftR` 4)
            put (doff + 2) ((b .&. 15) `unsafeShiftL` 2 .|. c `unsafeShiftR` 6)
            put (doff + 3) (c .&. 63)
            go (soff + 3) (doff + 4)
        | soff == slen = pure ()
        | otherwise = do
            let !a = byte soff
            put doff (a `unsafeShiftR` 2)
            if soff + 1 == slen
              then do
                put (doff + 1) ((a .&. 3) `unsafeShiftL` 4)
                if padded
                  then do
                    writeByteArray dst (doff + 2) (61 :: Word8)
                    writeByteArray dst (doff + 3) (61 :: Word8)
                  else pure ()
              else do
                let !b = byte (soff + 1)
                put (doff + 1) ((a .&. 3) `unsafeShiftL` 4 .|. b `unsafeShiftR` 4)
                put (doff + 2) ((b .&. 15) `unsafeShiftL` 2)
                if padded then writeByteArray dst (doff + 3) (61 :: Word8) else pure ()
  go consumed ((consumed `quot` 3) * 4)
  finish dst dlen
  where
    !src = ByteArray src#
    !slen = sizeofByteArray src
    byte i = indexByteArray src i :: Word8
    (!quanta, !remainder) = slen `quotRem` 3
    !tailLength
      | remainder == 0 = 0
      | padded = 4
      | otherwise = remainder + 1
    !dlen
      | quanta > (maxBound - tailLength) `quot` 4 = error "base64: encoded output too large"
      | otherwise = quanta * 4 + tailLength
{-# INLINE encodeDirect #-}

-- | Checked decoding with exactly the same error precedence as strict bytestrings.
decodeShort :: Padding -> ForeignPtr Word8 -> ShortByteString -> Either Text ShortByteString
decodeShort padding table input
  | slen == 0 = Right SBS.empty
  | remainder == 1 = Left "Base64-encoded bytestring has invalid size"
  | Required <- padding, remainder /= 0 = Left "Base64-encoded bytestring requires padding"
  | Forbidden <- padding, lastIsPad = invalidPadding
  | Optional <- padding, remainder == 3, lastIsPad = invalidPadding
  | otherwise = decodeWith True table input
  where
    !slen = SBS.length input
    !remainder = slen `rem` 4
    lastIsPad = SBS.index input (slen - 1) == 61
    invalidPadding = Left "Base64-encoded bytestring has invalid padding"
{-# INLINE decodeShort #-}

-- | Typed decoding omits validation. The direct path supplies optional URL
-- padding in place; larger scalar-only inputs use the strict fallback.
decodeShortTyped :: ForeignPtr Word8 -> Base64 k ShortByteString -> ShortByteString
decodeShortTyped table encoded = case decodeWith False table (extractBase64 encoded) of
  Right result -> result
  Left _ -> error "base64: impossible typed decoding failure"
{-# INLINE decodeShortTyped #-}

decodeWith :: Bool -> ForeignPtr Word8 -> ShortByteString -> Either Text ShortByteString
decodeWith !checked !table input@(SBS src#)
  | slen == 0 = Right SBS.empty
  | not (directAt 128 slen) =
      if checked
        then fmap SBS.toShort (unsafeDupablePerformIO (Strict.decodeBase64_ table padded))
        else Right (SBS.toShort (Strict.decodeBase64Typed_ table (assertBase64 padded)))
  | otherwise = unsafeDupablePerformIO $ withForeignPtr table $ \dtable -> unsafeSTToIO $ do
      dst <- newByteArray capacity
#ifdef BASE64_SIMD
      consumed <- SIMD.decodeByteArrayPrefix src slen dst dtable
#else
      let consumed = 0
#endif
      result <- decodeLoop checked dtable src slen dst consumed
      case result of
        Left err -> pure (Left err)
        Right len -> Right <$> finish dst len
  where
    !src = ByteArray src#
    !slen = sizeofByteArray src
    (!quanta, !remainder) = slen `quotRem` 4
    !capacity = quanta * 3 + if remainder == 0 then 0 else remainder - 1
    padded = case remainder of
      2 -> SBS.fromShort input `BS.append` "=="
      3 -> SBS.fromShort input `BS.append` "="
      _ -> SBS.fromShort input
{-# INLINE decodeWith #-}

-- Keep small allocation savings without replacing faster bulk scalar loops.
-- The cutoffs also match the minimum lengths for the SIMD prefix calls.
directAt :: Int -> Int -> Bool
directAt threshold len = len < threshold
#ifdef BASE64_SIMD
  || SIMD.available
#endif
{-# INLINE directAt #-}

-- | Freeze only the populated bytes. Shrinking cannot move the mutable array.
finish :: MutableByteArray s -> Int -> ST s ShortByteString
finish dst len = do
  shrinkMutableByteArray dst len
  ByteArray result# <- unsafeFreezeByteArray dst
  pure (SBS result#)
{-# INLINE finish #-}

decodeLoop
  :: Bool -> Ptr Word8 -> ByteArray -> Int -> MutableByteArray s -> Int
  -> ST s (Either Text Int)
decodeLoop !checked !table !src !slen !dst !consumed = go consumed ((consumed `quot` 4) * 3)
  where
    err = failure "invalid base64 encoding near offset: "
    padErr = failure "invalid padding near offset: "
    canonErr = failure "non-canonical encoding detected at offset: "
    failure message offset = pure (Left (message <> T.pack (show offset)))

    look i
      | i >= slen = pure (0x63 :: Word32)
      | otherwise = unsafeIOToST $ fromIntegral <$>
          peekElemOff table (fromIntegral (indexByteArray src i :: Word8))

    go !soff !doff = do
      a <- look soff
      b <- look (soff + 1)
      c <- look (soff + 2)
      d <- look (soff + 3)
      if soff + 4 >= slen
        then finalChunk soff doff a b c d
        else decodeChunk soff doff a b c d

    writeChunk doff a b c d = do
      let !w = a `unsafeShiftL` 18 .|. b `unsafeShiftL` 12 .|. c `unsafeShiftL` 6 .|. d
      writeByteArray dst doff (fromIntegral (w `unsafeShiftR` 16) :: Word8)
      writeByteArray dst (doff + 1) (fromIntegral (w `unsafeShiftR` 8) :: Word8)
      writeByteArray dst (doff + 2) (fromIntegral w :: Word8)

    decodeChunk soff doff a b c d
      | checked, a == 0x63 = padErr soff
      | checked, b == 0x63 = padErr (soff + 1)
      | checked, c == 0x63 = padErr (soff + 2)
      | checked, d == 0x63 = padErr (soff + 3)
      | checked, a == 0xff = err soff
      | checked, b == 0xff = err (soff + 1)
      | checked, c == 0xff = err (soff + 2)
      | checked, d == 0xff = err (soff + 3)
      | otherwise = do
          writeChunk doff a b c d
          go (soff + 4) (doff + 3)

    finalChunk soff doff a b c d
      | checked, a == 0x63 = padErr soff
      | checked, b == 0x63 = padErr (soff + 1)
      | checked, c == 0x63, d /= 0x63 = err (soff + 3)
      | checked, a == 0xff = err soff
      | checked, b == 0xff = err (soff + 1)
      | checked, c == 0xff = err (soff + 2)
      | checked, d == 0xff = err (soff + 3)
      | c == 0x63, d == 0x63 =
          if checked && not (validateLastPos b mask_4bits)
            then canonErr (soff + 1)
            else do
              writeByteArray dst doff (fromIntegral (a `unsafeShiftL` 2 .|. b `unsafeShiftR` 4) :: Word8)
              pure (Right (doff + 1))
      | d == 0x63 =
          if checked && not (validateLastPos c mask_2bits)
            then canonErr (soff + 2)
            else do
              writeByteArray dst doff (fromIntegral (a `unsafeShiftL` 2 .|. b `unsafeShiftR` 4) :: Word8)
              writeByteArray dst (doff + 1) (fromIntegral (b `unsafeShiftL` 4 .|. c `unsafeShiftR` 2) :: Word8)
              pure (Right (doff + 2))
      | otherwise = do
          writeChunk doff a b c d
          pure (Right (doff + 3))
{-# INLINE decodeLoop #-}
