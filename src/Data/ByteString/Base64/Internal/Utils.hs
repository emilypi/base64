{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
-- |
-- Module       : Data.ByteString.Base64.Internal
-- Copyright    : (c) 2019 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- Shared internal utils
--
module Data.ByteString.Base64.Internal.Utils
( EncodingTable(..)
, Padding(..)
, aix
, packTable
, w32
, w64
, w32_16
, writeNPlainForeignPtrBytes
) where


import System.IO.Unsafe

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import GHC.Exts
import GHC.ForeignPtr
import GHC.Word


-- | Only the lookup table need be a foreignptr,
-- and then, only so that we can automate some touches to keep it alive
--
data EncodingTable = EncodingTable
  {-# UNPACK #-} !(Ptr Word8)
  {-# UNPACK #-} !(ForeignPtr Word16)


-- | A type isomorphic to 'Bool' marking support for padding out bytestrings (@Pad),
-- or not (@Nopad@).
--
data Padding
    = Padded
      -- ^ Do we require padding?
    | Unpadded
      -- ^ Do we not require padding?
    | Don'tCare
      -- ^ Do we not care? (should we pad if it needs it?)
    deriving Eq

-- | Read 'Word8' index off alphabet addr
--
aix :: Word8 -> Addr# -> Word8
aix (W8# i) alpha = W8# (indexWord8OffAddr# alpha (word2Int# i))
{-# INLINE aix #-}

-- | Convert 'Word8''s into 'Word32''s
--
w32 :: Word8 -> Word32
w32 = fromIntegral
{-# INLINE w32 #-}

-- | Convert 'Word8''s into 'Word32''s
--
w64 :: Word16 -> Word64
w64 = fromIntegral
{-# INLINE w64 #-}

w32_16 :: Word16 -> Word32
w32_16 = fromIntegral
{-# INLINE w32_16 #-}

-- | Allocate and fill @n@ bytes with some data
--
writeNPlainForeignPtrBytes
    :: ( Storable a
       , Storable b
       )
    => Int
    -> [a]
    -> ForeignPtr b
writeNPlainForeignPtrBytes !n as = unsafeDupablePerformIO $ do
    fp <- mallocPlainForeignPtrBytes n
    withForeignPtr fp $ \p -> go p as
    return (castForeignPtr fp)
  where
    go !_ [] = return ()
    go !p (x:xs) = poke p x >> go (plusPtr p 1) xs

-- | Pack an 'Addr#' into an encoding table of 'Word16's
--
packTable :: Addr# -> EncodingTable
packTable alphabet = etable
  where
    ix (I# n) = W8# (indexWord8OffAddr# alphabet n)

    !etable =
      let bs = concat
            [ [ ix i, ix j ]
            | !i <- [0..63]
            , !j <- [0..63]
            ]
      in EncodingTable (Ptr alphabet) (writeNPlainForeignPtrBytes 8192 bs)
