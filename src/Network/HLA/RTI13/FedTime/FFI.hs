{-# LANGUAGE ForeignFunctionInterface #-}
module Network.HLA.RTI13.FedTime.FFI where

import Network.HLA.RTI13.FedTime.Types

import Foreign hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.C

import Network.HLA.RTI13.RTIException

foreign import ccall unsafe "wrap/fedtime.h wrap_new_RTIfedTime"
    wrap_new_RTIfedTime :: CDouble -> Ptr (Ptr RTIException) -> IO (Ptr RTIFedTime)
new_RTIFedTime :: Double -> IO (Ptr RTIFedTime)
new_RTIFedTime t = wrapExceptions (wrap_new_RTIfedTime (realToFrac t))

foreign import ccall unsafe "wrap/fedtime.h wrap_delete_RTIfedTime"
    wrap_delete_RTIfedTime :: Ptr RTIFedTime -> Ptr (Ptr RTIException) -> IO ()
delete_RTIFedTime :: Ptr RTIFedTime -> IO ()
delete_RTIFedTime rtiFedTime = 
    wrapExceptions (wrap_delete_RTIfedTime rtiFedTime)

foreign import ccall unsafe "wrap/fedtime.h wrap_RTIfedTime_getTime"
    wrap_getTime :: Ptr RTIFedTime -> Ptr (Ptr RTIException) -> IO CDouble
getTime :: Ptr RTIFedTime -> IO Double
getTime fedtime = do
    t <- wrapExceptions (wrap_getTime fedtime)
    return (realToFrac t)

foreign import ccall unsafe "wrap/fedtime.h wrap_RTIfedTime_setTime"
    wrap_setTime :: Ptr RTIFedTime -> CDouble -> Ptr (Ptr RTIException) -> IO ()
setTime :: Ptr RTIFedTime -> Double -> IO ()
setTime fedtime t =
    wrapExceptions (wrap_setTime fedtime (realToFrac t))

mkRTIFedTime :: Double -> IO RTIFedTime
mkRTIFedTime t = do
    rt <- new_RTIFedTime t
    fp <- newForeignPtr rt (delete_RTIFedTime rt)
    return (RTIFedTime fp)

