{-# LANGUAGE
        ForeignFunctionInterface, TypeFamilies
  #-}
module Network.HLA.RTI13.NullFederateAmbassador where

import Foreign hiding (newForeignPtr)
import Foreign.Concurrent

import Network.HLA.RTI13.BaseTypes
import Network.HLA.RTI13.RTITypes
import Network.HLA.RTI13.RTIException

newtype NullFederateAmbassador = NullFederateAmbassador (ForeignPtr NullFederateAmbassador)

foreign import ccall unsafe "wrap/NullFederateAmbassador.h wrap_new_NullFederateAmbassador"
    wrap_new_NullFederateAmbassador :: Ptr (Ptr RTIException) -> IO (Ptr NullFederateAmbassador)
foreign import ccall unsafe "wrap/NullFederateAmbassador.h wrap_delete_NullFederateAmbassador"
    wrap_delete_NullFederateAmbassador :: Ptr NullFederateAmbassador -> Ptr (Ptr RTIException) -> IO ()

newNullFederateAmbassador :: IO NullFederateAmbassador
newNullFederateAmbassador = do
    fedAmb <- wrapExceptions wrap_new_NullFederateAmbassador
    fedAmb <- newForeignPtr fedAmb (delete_NullFederateAmbassador fedAmb)
    return (NullFederateAmbassador fedAmb)
    
    where
        delete_NullFederateAmbassador fedAmb = wrapExceptions (wrap_delete_NullFederateAmbassador fedAmb)

instance FederateAmbassador NullFederateAmbassador where
    type FedAmbTime NullFederateAmbassador = SomeFedTime
    withFederateAmbassador (NullFederateAmbassador fedAmb) action = 
        withForeignPtr fedAmb $ \fedAmb -> 
            action (castPtr fedAmb)
