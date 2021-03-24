{-# LANGUAGE Haskell2010 #-}

module Main(main) where

import qualified Data.List             as L
import           Data.Word             (Word16, Word32, Word64, Word8)
import qualified GHC.ByteOrder         as IUT

-- Haskell2010
import qualified Foreign.Marshal.Array as FFI
import qualified Foreign.Marshal.Utils as FFI
import qualified Foreign.Ptr           as FFI
import qualified Foreign.Storable      as FFI

asOctets :: FFI.Storable a => a -> IO [Word8]
asOctets w = FFI.with w $ \p -> FFI.peekArray (FFI.sizeOf w) (FFI.castPtr p)

runtimeByteOrder :: IO IUT.ByteOrder
runtimeByteOrder = do
    w16octets <- asOctets (0x1234             :: Word16)
    w32octets <- asOctets (0x12345678         :: Word32)
    w64octets <- asOctets (0x123456789abcdef0 :: Word64)

    let octectsLst = [w16octets,w32octets,w64octets]

    case (L.nub $ zipWith inferBO octectsLst [beOctets16,beOctets32,beOctets64]) of
      [Just x] -> return x
      _        -> fail ("runtimeByteOrder failed " ++ show octectsLst)

  where
    beOctets64 = [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0]
    beOctets32 = take 4 beOctets64
    beOctets16 = take 2 beOctets64

    inferBO host beRef
      | host == beRef           = Just IUT.BigEndian
      | host == L.reverse beRef = Just IUT.LittleEndian
      | otherwise               = Nothing

main :: IO ()
main = do
  putStrLn $ "GHC.ByteOrder.targetByteOrder = " ++ show IUT.targetByteOrder

  hostBO <- runtimeByteOrder
  putStrLn $ "probed byte-order = " ++ show hostBO

  if IUT.targetByteOrder == hostBO
   then putStrLn "success!"
   else do
    print =<< asOctets (0x1234             :: Word16)
    print =<< asOctets (0x12345678         :: Word32)
    print =<< asOctets (0x123456789abcdef0 :: Word64)
    fail "static 'targetByteOrder' doesn't match probed ByteOrder"

