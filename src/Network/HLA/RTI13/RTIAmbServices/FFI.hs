{-# LANGUAGE
        EmptyDataDecls, ForeignFunctionInterface
  #-}
module Network.HLA.RTI13.RTIAmbServices.FFI where

import Foreign
import Foreign.C
import Foreign.C.String
import Network.HLA.RTI13.BaseTypes
import Network.HLA.RTI13.RTITypes
import Network.HLA.RTI13.RTIException
import Data.StateRef

data RTIAmbassador fedAmb = RTIAmbassador
    { rtiAmbPtr :: ForeignPtr (RTIAmbassador fedAmb)
    , rtiFedAmb :: IORef (Maybe fedAmb) -- used to keep fedamb alive after joining federation
    }
instance Show (RTIAmbassador fedAmb) where showsPrec p (RTIAmbassador x _) = showsPrec p x

withRTIAmbassador :: RTIAmbassador fedAmb -> (Ptr (RTIAmbassador fedAmb) -> IO a) -> IO a
withRTIAmbassador (RTIAmbassador rtiAmb _) = withForeignPtr rtiAmb

new_RTIambassador :: IO (Ptr (RTIAmbassador fedAmb))
new_RTIambassador = wrapExceptions wrap_new_RTIambassador
foreign import ccall unsafe "wrap/RTIambServices.h wrap_new_RTIambassador" 
    wrap_new_RTIambassador :: Ptr (Ptr RTIException) -> IO (Ptr (RTIAmbassador fedAmb))

delete_RTIambassador :: Ptr (RTIAmbassador fedAmb) -> IO ()
delete_RTIambassador rtiAmb = wrapExceptions (wrap_delete_RTIambassador rtiAmb)
foreign import ccall unsafe "wrap/RTIambServices.h wrap_delete_RTIambassador" 
    wrap_delete_RTIambassador :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

------------------------------------
-- Federation Management Services --
------------------------------------

foreign import ccall unsafe "wrap/RTIambServices.h wrap_createFederationExecution"
    wrap_createFederationExecution :: Ptr (RTIAmbassador fedAmb) -> CString -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_destroyFederationExecution"
    wrap_destroyFederationExecution :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIambServices.h wrap_joinFederationExecution"
    wrap_joinFederationExecution :: Ptr (RTIAmbassador fedAmb) -> CString -> CString -> Ptr SomeFederateAmbassador -> Ptr (Ptr RTIException) -> IO FederateHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_resignFederationExecution"
    wrap_resignFederationExecution :: Ptr (RTIAmbassador fedAmb) -> CInt -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_registerFederationSynchronizationPoint"
    wrap_registerFederationSynchronizationPoint :: Ptr (RTIAmbassador fedAmb) -> CString -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_registerFederationSynchronizationPoint_with_syncSet"
    wrap_registerFederationSynchronizationPoint_with_syncSet :: Ptr (RTIAmbassador fedAmb) -> CString -> CString -> Ptr FederateHandleSet -> Ptr (Ptr RTIException) -> IO ()

-------------------------------------
-- Declaration Management Services --
-------------------------------------

foreign import ccall "wrap/RTIambServices.h wrap_publishObjectClass"
    wrap_publishObjectClass :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIambServices.h wrap_unpublishObjectClass"
    wrap_unpublishObjectClass :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIambServices.h wrap_publishInteractionClass"
    wrap_publishInteractionClass :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIambServices.h wrap_unpublishInteractionClass"
    wrap_unpublishInteractionClass :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_subscribeObjectClassAttributes"
    wrap_subscribeObjectClassAttributes :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_unsubscribeObjectClass"
    wrap_unsubscribeObjectClass :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_subscribeInteractionClass"
    wrap_subscribeInteractionClass :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Bool -> Ptr (Ptr RTIException) -> IO ()
    
foreign import ccall "wrap/RTIAmbServices.h wrap_unsubscribeInteractionClass"
    wrap_unsubscribeInteractionClass :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO ()

--------------------------------
-- Object Management Services --
--------------------------------

foreign import ccall "wrap/RTIAmbServices.h wrap_registerObjectInstance_withName"
    wrap_registerObjectInstance_withName :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> CString -> Ptr (Ptr RTIException) -> IO ObjectHandle

foreign import ccall "wrap/RTIAmbServices.h wrap_registerObjectInstance"
    wrap_registerObjectInstance :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO ObjectHandle

foreign import ccall "wrap/RTIAmbServices.h wrap_sendInteractionAtTime"
    wrap_sendInteractionAtTime :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> Ptr (FedAmbTime fedAmb) -> CString -> Ptr UniqueID -> Ptr FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_sendInteraction"
    wrap_sendInteraction :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_requestClassAttributeValueUpdate"
    wrap_requestClassAttributeValueUpdate :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()

-----------------------------------
-- Ownership Management Services --
-----------------------------------


------------------------------
-- Time Management Services --
------------------------------

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_enableTimeRegulation" 
    wrap_enableTimeRegulation :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_disableTimeRegulation" 
    wrap_disableTimeRegulation :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_enableTimeConstrained"
    wrap_enableTimeConstrained :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_disableTimeConstrained" 
    wrap_disableTimeConstrained :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_timeAdvanceRequest" 
    wrap_timeAdvanceRequest :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

----------------------------------
-- Data Distribution Management --
----------------------------------

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_createRegion" 
    wrap_createRegion :: Ptr (RTIAmbassador fedAmb) -> SpaceHandle -> ULong -> Ptr (Ptr RTIException) -> IO (Ptr Region)

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_notifyAboutRegionModification" 
    wrap_notifyAboutRegionModification :: Ptr (RTIAmbassador fedAmb) -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_deleteRegion" 
    wrap_deleteRegion :: Ptr (RTIAmbassador fedAmb) -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_registerObjectInstanceWithRegion_withName" 
    wrap_registerObjectInstanceWithRegion :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> CString -> Ptr AttributeHandle -> Ptr (Ptr Region) -> ULong -> Ptr (Ptr RTIException) -> IO ObjectHandle

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_subscribeInteractionClassWithRegion" 
    wrap_subscribeInteractionClassWithRegion :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr Region -> Bool -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_unsubscribeInteractionClassWithRegion" 
    wrap_unsubscribeInteractionClassWithRegion :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()

--------------------------
-- RTI Support Services --
--------------------------

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_getObjectClassHandle"
    wrap_getObjectClassHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO ObjectClassHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getAttributeHandle"
    wrap_getAttributeHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO AttributeHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getInteractionClassHandle"
    wrap_getInteractionClassHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO InteractionClassHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getParameterHandle"
    wrap_getParameterHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO ParameterHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getRoutingSpaceHandle"
    wrap_getRoutingSpaceHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO SpaceHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getDimensionHandle"
    wrap_getDimensionHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> SpaceHandle -> Ptr (Ptr RTIException) -> IO DimensionHandle

foreign import ccall "wrap/RTIAmbServices.h wrap_enableAttributeRelevanceAdvisorySwitch"
    wrap_enableAttributeRelevanceAdvisorySwitch :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_tick_minimum_maximum"
    wrap_tick_minimum_maximum :: Ptr (RTIAmbassador fedAmb) -> TickTime -> TickTime -> Ptr (Ptr RTIException) -> IO Bool
