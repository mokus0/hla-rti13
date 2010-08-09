{-# LANGUAGE
        GADTs, ForeignFunctionInterface
  #-}
module Network.HLA.RTI13.RTIAmbServices.FFI where

import Data.ByteString (ByteString)
import Data.IORef
import qualified Data.Map as M (Map)
import qualified Data.Set as S (Set)
import Foreign
import Foreign.C
import Network.HLA.RTI13.BaseTypes
import Network.HLA.RTI13.RTITypes
import Network.HLA.RTI13.RTIException

data SomeFedAmb t where
    SomeFedAmb :: FederateAmbassador fedAmb => !fedAmb -> SomeFedAmb (FedAmbTime fedAmb)

data RTIAmbassador t = RTIAmbassador
    { rtiAmbPtr :: ForeignPtr (RTIAmbassador t)
    , rtiFedAmb :: IORef (Maybe (SomeFedAmb t)) -- used to keep fedamb alive after joining federation
    }
instance Show (RTIAmbassador t) where showsPrec p (RTIAmbassador x _) = showsPrec p x

withRTIAmbassador :: RTIAmbassador t -> (Ptr (RTIAmbassador t) -> IO a) -> IO a
withRTIAmbassador (RTIAmbassador rtiAmb _) = withForeignPtr rtiAmb

new_RTIambassador :: IO (Ptr (RTIAmbassador t))
new_RTIambassador = wrapExceptions wrap_new_RTIambassador
foreign import ccall unsafe "wrap/RTIambServices.h wrap_new_RTIambassador" 
    wrap_new_RTIambassador :: Ptr (Ptr RTIException) -> IO (Ptr (RTIAmbassador t))

delete_RTIambassador :: Ptr (RTIAmbassador t) -> IO ()
delete_RTIambassador rtiAmb = wrapExceptions (wrap_delete_RTIambassador rtiAmb)
foreign import ccall unsafe "wrap/RTIambServices.h wrap_delete_RTIambassador" 
    wrap_delete_RTIambassador :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

-------------------------------------
-- * Federation Management Services
-------------------------------------

foreign import ccall unsafe "wrap/RTIambServices.h wrap_createFederationExecution"
    createFederationExecution :: Ptr (RTIAmbassador t) -> CString -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_destroyFederationExecution"
    destroyFederationExecution :: Ptr (RTIAmbassador t) -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIambServices.h wrap_joinFederationExecution"
    joinFederationExecution :: Ptr (RTIAmbassador t) -> CString -> CString -> Ptr fedAmb -> Ptr (Ptr RTIException) -> IO FederateHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_resignFederationExecution"
    resignFederationExecution :: Ptr (RTIAmbassador t) -> CInt -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_registerFederationSynchronizationPoint"
    registerFederationSynchronizationPoint :: Ptr (RTIAmbassador t) -> CString -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_registerFederationSynchronizationPoint_with_syncSet"
    registerFederationSynchronizationPoint_with_syncSet :: Ptr (RTIAmbassador t) -> CString -> CString -> Ptr FederateHandleSet -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_synchronizationPointAchieved"
    synchronizationPointAchieved :: Ptr (RTIAmbassador t) -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_requestFederationSave"
    requestFederationSave :: Ptr (RTIAmbassador t) -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_requestFederationSaveAtTime"
    requestFederationSaveAtTime :: Ptr (RTIAmbassador t) -> CString -> Ptr t -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_federateSaveBegun"
    federateSaveBegun :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_federateSaveComplete"
    federateSaveComplete :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_federateSaveNotComplete"
    federateSaveNotComplete :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_requestFederationRestore"
    requestFederationRestore :: Ptr (RTIAmbassador t) -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_federateRestoreComplete"
    federateRestoreComplete :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_federateRestoreNotComplete"
    federateRestoreNotComplete :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

--------------------------------------
-- * Declaration Management Services
--------------------------------------

foreign import ccall "wrap/RTIambServices.h wrap_publishObjectClass"
    publishObjectClass :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> Ptr (S.Set AttributeHandle) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIambServices.h wrap_unpublishObjectClass"
    unpublishObjectClass :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIambServices.h wrap_publishInteractionClass"
    publishInteractionClass :: Ptr (RTIAmbassador t) -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIambServices.h wrap_unpublishInteractionClass"
    unpublishInteractionClass :: Ptr (RTIAmbassador t) -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_subscribeObjectClassAttributes"
    subscribeObjectClassAttributes :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> Ptr (S.Set AttributeHandle) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_unsubscribeObjectClass"
    unsubscribeObjectClass :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_subscribeInteractionClass"
    subscribeInteractionClass :: Ptr (RTIAmbassador t) -> InteractionClassHandle -> Bool -> Ptr (Ptr RTIException) -> IO ()
    
foreign import ccall "wrap/RTIAmbServices.h wrap_unsubscribeInteractionClass"
    unsubscribeInteractionClass :: Ptr (RTIAmbassador t) -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO ()

---------------------------------
-- * Object Management Services
---------------------------------

foreign import ccall "wrap/RTIAmbServices.h wrap_registerObjectInstance_withName"
    registerObjectInstance_withName :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> CString -> Ptr (Ptr RTIException) -> IO ObjectHandle

foreign import ccall "wrap/RTIAmbServices.h wrap_updateAttributeValuesAtTime"
    updateAttributeValuesAtTime :: Ptr (RTIAmbassador t) -> ObjectHandle -> Ptr (M.Map AttributeHandle ByteString) -> Ptr t -> CString -> Ptr UniqueID -> Ptr FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_updateAttributeValues"
    updateAttributeValues :: Ptr (RTIAmbassador t) -> ObjectHandle -> Ptr (M.Map AttributeHandle ByteString) -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_registerObjectInstance"
    registerObjectInstance :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO ObjectHandle

foreign import ccall "wrap/RTIAmbServices.h wrap_sendInteractionAtTime"
    sendInteractionAtTime :: Ptr (RTIAmbassador t) -> InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> Ptr t -> CString -> Ptr UniqueID -> Ptr FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_sendInteraction"
    sendInteraction :: Ptr (RTIAmbassador t) -> InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_deleteObjectInstanceAtTime"
    deleteObjectInstanceAtTime :: Ptr (RTIAmbassador t) -> ObjectHandle -> Ptr t -> CString -> Ptr UniqueID -> Ptr FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_deleteObjectInstance"
    deleteObjectInstance :: Ptr (RTIAmbassador t) -> ObjectHandle -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_localDeleteObjectInstance"
    localDeleteObjectInstance :: Ptr (RTIAmbassador t) -> ObjectHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_changeAttributeTransportationType"
    changeAttributeTransportationType :: Ptr (RTIAmbassador t) -> ObjectHandle -> Ptr (S.Set AttributeHandle) -> TransportationHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_changeInteractionTransportationType"
    changeInteractionTransportationType :: Ptr (RTIAmbassador t) -> InteractionClassHandle -> TransportationHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_requestObjectAttributeValueUpdate"
    requestObjectAttributeValueUpdate :: Ptr (RTIAmbassador t) -> ObjectHandle -> Ptr (S.Set AttributeHandle) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_requestClassAttributeValueUpdate"
    requestClassAttributeValueUpdate :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> Ptr (S.Set AttributeHandle) -> Ptr (Ptr RTIException) -> IO ()

------------------------------------
-- * Ownership Management Services
------------------------------------

foreign import ccall "wrap/RTIAmbServices.h wrap_unconditionalAttributeOwnershipDivestiture"
    unconditionalAttributeOwnershipDivestiture :: Ptr (RTIAmbassador t) -> ObjectHandle -> Ptr (S.Set AttributeHandle) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_negotiatedAttributeOwnershipDivestiture"
    negotiatedAttributeOwnershipDivestiture :: Ptr (RTIAmbassador t) -> ObjectHandle -> Ptr (S.Set AttributeHandle) -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_attributeOwnershipAcquisition"
    attributeOwnershipAcquisition :: Ptr (RTIAmbassador t) -> ObjectHandle -> Ptr (S.Set AttributeHandle) -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_attributeOwnershipAcquisitionIfAvailable"
    attributeOwnershipAcquisitionIfAvailable :: Ptr (RTIAmbassador t) -> ObjectHandle -> Ptr (S.Set AttributeHandle) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_attributeOwnershipReleaseResponse"
    attributeOwnershipReleaseResponse :: Ptr (RTIAmbassador t) -> ObjectHandle -> Ptr (S.Set AttributeHandle) -> Ptr (Ptr RTIException) -> IO (Ptr (S.Set AttributeHandle))

foreign import ccall "wrap/RTIAmbServices.h wrap_cancelNegotiatedAttributeOwnershipDivestiture"
    cancelNegotiatedAttributeOwnershipDivestiture :: Ptr (RTIAmbassador t) -> ObjectHandle -> Ptr (S.Set AttributeHandle) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_cancelAttributeOwnershipAcquisition"
    cancelAttributeOwnershipAcquisition :: Ptr (RTIAmbassador t) -> ObjectHandle -> Ptr (S.Set AttributeHandle) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_queryAttributeOwnership"
    queryAttributeOwnership :: Ptr (RTIAmbassador t) -> ObjectHandle -> AttributeHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_isAttributeOwnedByFederate"
    isAttributeOwnedByFederate :: Ptr (RTIAmbassador t) -> ObjectHandle -> AttributeHandle -> Ptr (Ptr RTIException) -> IO Bool

-------------------------------
-- * Time Management Services
-------------------------------

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_enableTimeRegulation" 
    enableTimeRegulation :: Ptr (RTIAmbassador t) -> Ptr t -> Ptr t -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_disableTimeRegulation" 
    disableTimeRegulation :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_enableTimeConstrained"
    enableTimeConstrained :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_disableTimeConstrained" 
    disableTimeConstrained :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_timeAdvanceRequest" 
    timeAdvanceRequest :: Ptr (RTIAmbassador t) -> Ptr t -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_timeAdvanceRequestAvailable"
    timeAdvanceRequestAvailable :: Ptr (RTIAmbassador t) -> Ptr t -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_nextEventRequest"
    nextEventRequest :: Ptr (RTIAmbassador t) -> Ptr t -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_nextEventRequestAvailable"
    nextEventRequestAvailable :: Ptr (RTIAmbassador t) -> Ptr t -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_flushQueueRequest"
    flushQueueRequest :: Ptr (RTIAmbassador t) -> Ptr t -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_enableAsynchronousDelivery"
    enableAsynchronousDelivery :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_disableAsynchronousDelivery"
    disableAsynchronousDelivery :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_queryLBTS"
    queryLBTS :: Ptr (RTIAmbassador t) -> Ptr t -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_queryFederateTime"
    queryFederateTime :: Ptr (RTIAmbassador t) -> Ptr t -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_queryMinNextEventTime"
    queryMinNextEventTime :: Ptr (RTIAmbassador t) -> Ptr t -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_modifyLookahead"
    modifyLookahead :: Ptr (RTIAmbassador t) -> Ptr t -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_queryLookahead"
    queryLookahead :: Ptr (RTIAmbassador t) -> Ptr t -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_retract"
    retract :: Ptr (RTIAmbassador t) -> UniqueID -> FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_changeAttributeOrderType"
    changeAttributeOrderType :: Ptr (RTIAmbassador t) -> ObjectHandle -> Ptr (S.Set AttributeHandle) -> OrderingHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_changeInteractionOrderType"
    changeInteractionOrderType :: Ptr (RTIAmbassador t) -> InteractionClassHandle -> OrderingHandle -> Ptr (Ptr RTIException) -> IO ()

-----------------------------------
-- * Data Distribution Management
-----------------------------------

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_createRegion" 
    createRegion :: Ptr (RTIAmbassador t) -> SpaceHandle -> ULong -> Ptr (Ptr RTIException) -> IO (Ptr Region)

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_notifyAboutRegionModification" 
    notifyAboutRegionModification :: Ptr (RTIAmbassador t) -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_deleteRegion" 
    deleteRegion :: Ptr (RTIAmbassador t) -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_registerObjectInstanceWithRegion_withName" 
    registerObjectInstanceWithRegion_withName :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> CString -> Ptr AttributeHandle -> Ptr (Ptr Region) -> ULong -> Ptr (Ptr RTIException) -> IO ObjectHandle

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_registerObjectInstanceWithRegion" 
    registerObjectInstanceWithRegion :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> Ptr AttributeHandle -> Ptr (Ptr Region) -> ULong -> Ptr (Ptr RTIException) -> IO ObjectHandle

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_associateRegionForUpdates" 
    associateRegionForUpdates :: Ptr (RTIAmbassador t) -> Ptr Region -> ObjectHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_unassociateRegionForUpdates" 
    unassociateRegionForUpdates :: Ptr (RTIAmbassador t) -> Ptr Region -> ObjectHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_subscribeObjectClassAttributesWithRegion" 
    subscribeObjectClassAttributesWithRegion :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> Ptr Region -> Ptr (S.Set AttributeHandle) -> Bool -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_unsubscribeObjectClassWithRegion" 
    unsubscribeObjectClassWithRegion :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_subscribeInteractionClassWithRegion" 
    subscribeInteractionClassWithRegion :: Ptr (RTIAmbassador t) -> InteractionClassHandle -> Ptr Region -> Bool -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_unsubscribeInteractionClassWithRegion" 
    unsubscribeInteractionClassWithRegion :: Ptr (RTIAmbassador t) -> InteractionClassHandle -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_sendInteractionWithRegionAtTime" 
    sendInteractionWithRegionAtTime :: Ptr (RTIAmbassador t) -> InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> Ptr t -> CString -> Ptr Region -> Ptr UniqueID -> Ptr FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_sendInteractionWithRegion" 
    sendInteractionWithRegion :: Ptr (RTIAmbassador t) -> InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> CString -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_requestClassAttributeValueUpdateWithRegion" 
    requestClassAttributeValueUpdateWithRegion :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> Ptr (S.Set AttributeHandle) -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()


---------------------------
-- * RTI Support Services
---------------------------

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_getObjectClassHandle"
    getObjectClassHandle :: Ptr (RTIAmbassador t) -> CString -> Ptr (Ptr RTIException) -> IO ObjectClassHandle

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_getObjectClassName"
    getObjectClassName :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getAttributeHandle"
    getAttributeHandle :: Ptr (RTIAmbassador t) -> CString -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO AttributeHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getAttributeName"
    getAttributeName :: Ptr (RTIAmbassador t) -> AttributeHandle -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getInteractionClassHandle"
    getInteractionClassHandle :: Ptr (RTIAmbassador t) -> CString -> Ptr (Ptr RTIException) -> IO InteractionClassHandle
    
foreign import ccall unsafe "wrap/RTIambServices.h wrap_getInteractionClassName"
    getInteractionClassName :: Ptr (RTIAmbassador t) -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getParameterHandle"
    getParameterHandle :: Ptr (RTIAmbassador t) -> CString -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO ParameterHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getParameterName"
    getParameterName :: Ptr (RTIAmbassador t) -> ParameterHandle -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getObjectInstanceHandle"
    getObjectInstanceHandle :: Ptr (RTIAmbassador t) -> CString -> Ptr (Ptr RTIException) -> IO ObjectHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getObjectInstanceName"
    getObjectInstanceName :: Ptr (RTIAmbassador t) -> ObjectHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getRoutingSpaceHandle"
    getRoutingSpaceHandle :: Ptr (RTIAmbassador t) -> CString -> Ptr (Ptr RTIException) -> IO SpaceHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getRoutingSpaceName"
    getRoutingSpaceName :: Ptr (RTIAmbassador t) -> SpaceHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getDimensionHandle"
    getDimensionHandle :: Ptr (RTIAmbassador t) -> CString -> SpaceHandle -> Ptr (Ptr RTIException) -> IO DimensionHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getDimensionName"
    getDimensionName :: Ptr (RTIAmbassador t) -> DimensionHandle -> SpaceHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getAttributeRoutingSpaceHandle"
    getAttributeRoutingSpaceHandle :: Ptr (RTIAmbassador t) -> AttributeHandle -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO SpaceHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getObjectClass"
    getObjectClass :: Ptr (RTIAmbassador t) -> ObjectHandle -> Ptr (Ptr RTIException) -> IO ObjectClassHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getInteractionRoutingSpaceHandle"
    getInteractionRoutingSpaceHandle :: Ptr (RTIAmbassador t) -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO SpaceHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getTransportationHandle"
    getTransportationHandle :: Ptr (RTIAmbassador t) -> CString -> Ptr (Ptr RTIException) -> IO TransportationHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getTransportationName"
    getTransportationName :: Ptr (RTIAmbassador t) -> TransportationHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getOrderingHandle"
    getOrderingHandle :: Ptr (RTIAmbassador t) -> CString -> Ptr (Ptr RTIException) -> IO OrderingHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getOrderingName"
    getOrderingName :: Ptr (RTIAmbassador t) -> OrderingHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall "wrap/RTIAmbServices.h wrap_enableClassRelevanceAdvisorySwitch"
    enableClassRelevanceAdvisorySwitch :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_disableClassRelevanceAdvisorySwitch"
    disableClassRelevanceAdvisorySwitch :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_enableAttributeRelevanceAdvisorySwitch"
    enableAttributeRelevanceAdvisorySwitch :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_disableAttributeRelevanceAdvisorySwitch"
    disableAttributeRelevanceAdvisorySwitch :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_enableAttributeScopeAdvisorySwitch"
    enableAttributeScopeAdvisorySwitch :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_disableAttributeScopeAdvisorySwitch"
    disableAttributeScopeAdvisorySwitch :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_enableInteractionRelevanceAdvisorySwitch"
    enableInteractionRelevanceAdvisorySwitch :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_disableInteractionRelevanceAdvisorySwitch"
    disableInteractionRelevanceAdvisorySwitch :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_tick"
    tick :: Ptr (RTIAmbassador t) -> Ptr (Ptr RTIException) -> IO Bool

foreign import ccall "wrap/RTIAmbServices.h wrap_tick_minimum_maximum"
    tick_minimum_maximum :: Ptr (RTIAmbassador t) -> TickTime -> TickTime -> Ptr (Ptr RTIException) -> IO Bool

foreign import ccall "wrap/RTIAmbServices.h wrap_getRegionToken"
    getRegionToken :: Ptr (RTIAmbassador t) -> Ptr Region -> Ptr (Ptr RTIException) -> IO RegionToken

foreign import ccall "wrap/RTIAmbServices.h wrap_getRegion"
    getRegion :: Ptr (RTIAmbassador t) -> RegionToken -> Ptr (Ptr RTIException) -> IO (Ptr Region)
