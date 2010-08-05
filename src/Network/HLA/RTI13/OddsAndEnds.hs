{-# LANGUAGE ForeignFunctionInterface #-}
module Network.HLA.RTI13.OddsAndEnds where

import Data.ByteString
import Data.ByteString.Unsafe
import Foreign
import Foreign.C


foreign import ccall "string.h strlen"
    strlen :: CString -> IO CSize
foreign import ccall "oddsAndEnds.h delete_cString"
    delete_cString :: CString -> IO ()

unsafePackNewCStringLen :: CString -> Int -> IO ByteString
unsafePackNewCStringLen str len = 
    unsafePackCStringFinalizer (castPtr str) len (delete_cString str)

-- |Like unsafePackMallocCString but for strings that were allocated with
-- new[] instead of malloc.
unsafePackNewCString :: CString -> IO ByteString
unsafePackNewCString str = do
    len <- strlen str
    unsafePackNewCStringLen str (fromIntegral len)

