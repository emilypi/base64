{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
module Data.ByteString.Base64.Internal where


import Data.ByteString
import Data.ByteString.Internal

import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr


withByteArray :: ByteString -> (Ptr p -> IO a) -> IO a
withByteArray (PS fptr off _) f =
    withForeignPtr fptr $ \ptr -> f $! (ptr `plusPtr` off)

allocRet :: Int -> (Ptr p -> IO a) -> IO (a, ByteString)
allocRet sz f = do
    fptr <- mallocByteString sz
    r <- withForeignPtr fptr (f . castPtr)
    return (r, PS fptr 0 sz)
