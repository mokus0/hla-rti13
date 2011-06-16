{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module Network.HLA.RTI13.FedTime (RTIFedTime) where

import Network.HLA.RTI13.FedTime.FFI

import Foreign hiding (newForeignPtr, addForeignPtrFinalizer)
import Data.StateRef
import Control.Exception

import Network.HLA.RTI13.RTITypes

instance NewRef RTIFedTime IO Double where
    newReference = mkRTIFedTime

instance ReadRef RTIFedTime IO Double where
    readReference (RTIFedTime fp) = 
        withForeignPtr fp $ \rt ->
            getTime rt

instance WriteRef RTIFedTime IO Double where
    writeReference (RTIFedTime fp) t = 
        withForeignPtr fp $ \rt -> 
            setTime rt t

instance FedTimeImpl RTIFedTime where
    type FedTime RTIFedTime = Double
    withFedTimeIn  t      = bracket (new_RTIFedTime t) (delete_RTIFedTime)
    withFedTimeOut action = withFedTimeIn 0 $ \t -> action t >> importFedTime t
    importFedTime = getTime
