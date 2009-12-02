{-# LANGUAGE
        ForeignFunctionInterface,
        EmptyDataDecls, 
        MultiParamTypeClasses, FlexibleInstances,
        TypeFamilies
        
  #-}
module Network.HLA.RTI13.FedTime (RTIFedTime) where

import Foreign hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.C
import Data.StateRef
import Control.Exception

import Network.HLA.RTI13.BaseTypes
import Network.HLA.RTI13.RTITypes
import Network.HLA.RTI13.RTIException

newtype RTIFedTime = RTIFedTime (ForeignPtr RTIFedTime)
    deriving (Eq, Ord, Show)

foreign import ccall unsafe "wrap/fedtime.h wrap_new_RTIfedTime"
    raw_new_RTIfedTime :: CDouble -> Ptr (Ptr RTIException) -> IO (Ptr RTIFedTime)
new_RTIFedTime :: Double -> IO (Ptr RTIFedTime)
new_RTIFedTime t = wrapExceptions (raw_new_RTIfedTime (realToFrac t))

foreign import ccall unsafe "wrap/fedtime.h wrap_delete_RTIfedTime"
    raw_delete_RTIfedTime :: Ptr RTIFedTime -> Ptr (Ptr RTIException) -> IO ()
delete_RTIFedTime :: Ptr RTIFedTime -> IO ()
delete_RTIFedTime rtiFedTime = 
    wrapExceptions (raw_delete_RTIfedTime rtiFedTime)

foreign import ccall unsafe "wrap/fedtime.h wrap_getTime"
    raw_getTime :: Ptr RTIFedTime -> Ptr (Ptr RTIException) -> IO CDouble
getTime :: Ptr RTIFedTime -> IO Double
getTime fedtime = do
    t <- wrapExceptions (raw_getTime fedtime)
    return (realToFrac t)

foreign import ccall unsafe "wrap/fedtime.h wrap_setTime"
    raw_setTime :: Ptr RTIFedTime -> CDouble -> Ptr (Ptr RTIException) -> IO ()
setTime :: Ptr RTIFedTime -> Double -> IO ()
setTime fedtime t =
    wrapExceptions (raw_setTime fedtime (realToFrac t))

mkRTIFedTime :: Double -> IO RTIFedTime
mkRTIFedTime t = do
    rt <- new_RTIFedTime t
    fp <- newForeignPtr rt (delete_RTIFedTime rt)
    return (RTIFedTime fp)

instance NewRef RTIFedTime IO Double where
    newRef = mkRTIFedTime

instance ReadRef RTIFedTime IO Double where
    readRef (RTIFedTime fp) = withForeignPtr fp $ \rt -> do
        getTime rt

instance WriteRef RTIFedTime IO Double where
    writeRef (RTIFedTime fp) t = withForeignPtr fp $ \rt -> do
        setTime rt t

instance FedTimeImpl RTIFedTime where
    type FedTimeRepr RTIFedTime = Double
    withFedTime d f = bracket (new_RTIFedTime d) (delete_RTIFedTime) $ \time -> do
        result  <- f time
        newTime <- getTime time
        return (newTime, result)
    withFedTime_ d = bracket (new_RTIFedTime d) (delete_RTIFedTime)
    importFedTime = getTime
-- instance FedTimeType RTIFedTime where
--     withFedTime (RTIFedTime fp) action = withForeignPtr fp $ \fedtime -> 
--         action (castPtr fedtime)
--     importFedTime t = do
--         t <- newForeignPtr_ t
--         return (RTIFedTime t)