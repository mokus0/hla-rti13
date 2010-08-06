{-# LANGUAGE
        EmptyDataDecls, ForeignFunctionInterface
  #-}
module Network.HLA.RTI13.RTIAmbServices.FFI where

import Data.IORef
import Foreign
import Foreign.C
import Network.HLA.RTI13.BaseTypes
import Network.HLA.RTI13.RTITypes
import Network.HLA.RTI13.RTIException

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

-------------------------------------
-- * Federation Management Services
-------------------------------------

foreign import ccall unsafe "wrap/RTIambServices.h wrap_createFederationExecution"
    createFederationExecution :: Ptr (RTIAmbassador fedAmb) -> CString -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_destroyFederationExecution"
    destroyFederationExecution :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIambServices.h wrap_joinFederationExecution"
    joinFederationExecution :: Ptr (RTIAmbassador fedAmb) -> CString -> CString -> Ptr fedAmb -> Ptr (Ptr RTIException) -> IO FederateHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_resignFederationExecution"
    resignFederationExecution :: Ptr (RTIAmbassador fedAmb) -> CInt -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_registerFederationSynchronizationPoint"
    registerFederationSynchronizationPoint :: Ptr (RTIAmbassador fedAmb) -> CString -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_registerFederationSynchronizationPoint_with_syncSet"
    registerFederationSynchronizationPoint_with_syncSet :: Ptr (RTIAmbassador fedAmb) -> CString -> CString -> Ptr FederateHandleSet -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_synchronizationPointAchieved"
    synchronizationPointAchieved :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_requestFederationSave"
    requestFederationSave :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_requestFederationSaveAtTime"
    requestFederationSaveAtTime :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_federateSaveBegun"
    federateSaveBegun :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_federateSaveComplete"
    federateSaveComplete :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_federateSaveNotComplete"
    federateSaveNotComplete :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_requestFederationRestore"
    requestFederationRestore :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_federateRestoreComplete"
    federateRestoreComplete :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_federateRestoreNotComplete"
    federateRestoreNotComplete :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

--------------------------------------
-- * Declaration Management Services
--------------------------------------

foreign import ccall "wrap/RTIambServices.h wrap_publishObjectClass"
    publishObjectClass :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIambServices.h wrap_unpublishObjectClass"
    unpublishObjectClass :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIambServices.h wrap_publishInteractionClass"
    publishInteractionClass :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIambServices.h wrap_unpublishInteractionClass"
    unpublishInteractionClass :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_subscribeObjectClassAttributes"
    subscribeObjectClassAttributes :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_unsubscribeObjectClass"
    unsubscribeObjectClass :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_subscribeInteractionClass"
    subscribeInteractionClass :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Bool -> Ptr (Ptr RTIException) -> IO ()
    
foreign import ccall "wrap/RTIAmbServices.h wrap_unsubscribeInteractionClass"
    unsubscribeInteractionClass :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO ()

---------------------------------
-- * Object Management Services
---------------------------------

foreign import ccall "wrap/RTIAmbServices.h wrap_registerObjectInstance_withName"
    registerObjectInstance_withName :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> CString -> Ptr (Ptr RTIException) -> IO ObjectHandle

foreign import ccall "wrap/RTIAmbServices.h wrap_updateAttributeValuesAtTime"
    updateAttributeValuesAtTime :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleValuePairSet -> Ptr (FedAmbTime fedAmb) -> CString -> Ptr UniqueID -> Ptr FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_updateAttributeValues"
    updateAttributeValues :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleValuePairSet -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_registerObjectInstance"
    registerObjectInstance :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO ObjectHandle

foreign import ccall "wrap/RTIAmbServices.h wrap_sendInteractionAtTime"
    sendInteractionAtTime :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> Ptr (FedAmbTime fedAmb) -> CString -> Ptr UniqueID -> Ptr FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_sendInteraction"
    sendInteraction :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_deleteObjectInstanceAtTime"
    deleteObjectInstanceAtTime :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr (FedAmbTime fedAmb) -> CString -> Ptr UniqueID -> Ptr FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_deleteObjectInstance"
    deleteObjectInstance :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_localDeleteObjectInstance"
    localDeleteObjectInstance :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_changeAttributeTransportationType"
    changeAttributeTransportationType :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> TransportationHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_changeInteractionTransportationType"
    changeInteractionTransportationType :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> TransportationHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_requestObjectAttributeValueUpdate"
    requestObjectAttributeValueUpdate :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_requestClassAttributeValueUpdate"
    requestClassAttributeValueUpdate :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()

------------------------------------
-- * Ownership Management Services
------------------------------------

foreign import ccall "wrap/RTIAmbServices.h wrap_unconditionalAttributeOwnershipDivestiture"
    unconditionalAttributeOwnershipDivestiture :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_negotiatedAttributeOwnershipDivestiture"
    negotiatedAttributeOwnershipDivestiture :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_attributeOwnershipAcquisition"
    attributeOwnershipAcquisition :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_attributeOwnershipAcquisitionIfAvailable"
    attributeOwnershipAcquisitionIfAvailable :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_attributeOwnershipReleaseResponse"
    attributeOwnershipReleaseResponse :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO (Ptr AttributeHandleSet)

foreign import ccall "wrap/RTIAmbServices.h wrap_cancelNegotiatedAttributeOwnershipDivestiture"
    cancelNegotiatedAttributeOwnershipDivestiture :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_cancelAttributeOwnershipAcquisition"
    cancelAttributeOwnershipAcquisition :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_queryAttributeOwnership"
    queryAttributeOwnership :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> AttributeHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_isAttributeOwnedByFederate"
    isAttributeOwnedByFederate :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> AttributeHandle -> Ptr (Ptr RTIException) -> IO Bool

-------------------------------
-- * Time Management Services
-------------------------------

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_enableTimeRegulation" 
    enableTimeRegulation :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_disableTimeRegulation" 
    disableTimeRegulation :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_enableTimeConstrained"
    enableTimeConstrained :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_disableTimeConstrained" 
    disableTimeConstrained :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_timeAdvanceRequest" 
    timeAdvanceRequest :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_timeAdvanceRequestAvailable"
    timeAdvanceRequestAvailable :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_nextEventRequest"
    nextEventRequest :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_nextEventRequestAvailable"
    nextEventRequestAvailable :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_flushQueueRequest"
    flushQueueRequest :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_enableAsynchronousDelivery"
    enableAsynchronousDelivery :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_disableAsynchronousDelivery"
    disableAsynchronousDelivery :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_queryLBTS"
    queryLBTS :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_queryFederateTime"
    queryFederateTime :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_queryMinNextEventTime"
    queryMinNextEventTime :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_modifyLookahead"
    modifyLookahead :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_queryLookahead"
    queryLookahead :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_retract"
    retract :: Ptr (RTIAmbassador fedAmb) -> UniqueID -> FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_changeAttributeOrderType"
    changeAttributeOrderType :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr (AttributeHandleSet) -> OrderingHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_changeInteractionOrderType"
    changeInteractionOrderType :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> OrderingHandle -> Ptr (Ptr RTIException) -> IO ()

-----------------------------------
-- * Data Distribution Management
-----------------------------------

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_createRegion" 
    createRegion :: Ptr (RTIAmbassador fedAmb) -> SpaceHandle -> ULong -> Ptr (Ptr RTIException) -> IO (Ptr Region)

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_notifyAboutRegionModification" 
    notifyAboutRegionModification :: Ptr (RTIAmbassador fedAmb) -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_deleteRegion" 
    deleteRegion :: Ptr (RTIAmbassador fedAmb) -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_registerObjectInstanceWithRegion_withName" 
    registerObjectInstanceWithRegion_withName :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> CString -> Ptr AttributeHandle -> Ptr (Ptr Region) -> ULong -> Ptr (Ptr RTIException) -> IO ObjectHandle

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_registerObjectInstanceWithRegion" 
    registerObjectInstanceWithRegion :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> Ptr AttributeHandle -> Ptr (Ptr Region) -> ULong -> Ptr (Ptr RTIException) -> IO ObjectHandle

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_associateRegionForUpdates" 
    associateRegionForUpdates :: Ptr (RTIAmbassador t) -> Ptr Region -> ObjectHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_unassociateRegionForUpdates" 
    unassociateRegionForUpdates :: Ptr (RTIAmbassador t) -> Ptr Region -> ObjectHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_subscribeObjectClassAttributesWithRegion" 
    subscribeObjectClassAttributesWithRegion :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> Ptr Region -> Ptr AttributeHandleSet -> Bool -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_unsubscribeObjectClassWithRegion" 
    unsubscribeObjectClassWithRegion :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_subscribeInteractionClassWithRegion" 
    subscribeInteractionClassWithRegion :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr Region -> Bool -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_unsubscribeInteractionClassWithRegion" 
    unsubscribeInteractionClassWithRegion :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_sendInteractionWithRegionAtTime" 
    sendInteractionWithRegionAtTime :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> Ptr (FedAmbTime fedAmb) -> CString -> Ptr Region -> Ptr UniqueID -> Ptr FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_sendInteractionWithRegion" 
    sendInteractionWithRegion :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> CString -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_requestClassAttributeValueUpdateWithRegion" 
    requestClassAttributeValueUpdateWithRegion :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr AttributeHandleSet -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()


---------------------------
-- * RTI Support Services
---------------------------

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_getObjectClassHandle"
    getObjectClassHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO ObjectClassHandle

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_getObjectClassName"
    getObjectClassName :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getAttributeHandle"
    getAttributeHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO AttributeHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getAttributeName"
    getAttributeName :: Ptr (RTIAmbassador fedAmb) -> AttributeHandle -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getInteractionClassHandle"
    getInteractionClassHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO InteractionClassHandle
    
foreign import ccall unsafe "wrap/RTIambServices.h wrap_getInteractionClassName"
    getInteractionClassName :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getParameterHandle"
    getParameterHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO ParameterHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getParameterName"
    getParameterName :: Ptr (RTIAmbassador fedAmb) -> ParameterHandle -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getObjectInstanceHandle"
    getObjectInstanceHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO ObjectHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getObjectInstanceName"
    getObjectInstanceName :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getRoutingSpaceHandle"
    getRoutingSpaceHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO SpaceHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getRoutingSpaceName"
    getRoutingSpaceName :: Ptr (RTIAmbassador fedAmb) -> SpaceHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getDimensionHandle"
    getDimensionHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> SpaceHandle -> Ptr (Ptr RTIException) -> IO DimensionHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getDimensionName"
    getDimensionName :: Ptr (RTIAmbassador fedAmb) -> DimensionHandle -> SpaceHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getAttributeRoutingSpaceHandle"
    getAttributeRoutingSpaceHandle :: Ptr (RTIAmbassador fedAmb) -> AttributeHandle -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO SpaceHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getObjectClass"
    getObjectClass :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr (Ptr RTIException) -> IO ObjectClassHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getInteractionRoutingSpaceHandle"
    getInteractionRoutingSpaceHandle :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO SpaceHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getTransportationHandle"
    getTransportationHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO TransportationHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getTransportationName"
    getTransportationName :: Ptr (RTIAmbassador fedAmb) -> TransportationHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getOrderingHandle"
    getOrderingHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO OrderingHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getOrderingName"
    getOrderingName :: Ptr (RTIAmbassador fedAmb) -> OrderingHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall "wrap/RTIAmbServices.h wrap_enableClassRelevanceAdvisorySwitch"
    enableClassRelevanceAdvisorySwitch :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_disableClassRelevanceAdvisorySwitch"
    disableClassRelevanceAdvisorySwitch :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_enableAttributeRelevanceAdvisorySwitch"
    enableAttributeRelevanceAdvisorySwitch :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_disableAttributeRelevanceAdvisorySwitch"
    disableAttributeRelevanceAdvisorySwitch :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_enableAttributeScopeAdvisorySwitch"
    enableAttributeScopeAdvisorySwitch :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_disableAttributeScopeAdvisorySwitch"
    disableAttributeScopeAdvisorySwitch :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_enableInteractionRelevanceAdvisorySwitch"
    enableInteractionRelevanceAdvisorySwitch :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_disableInteractionRelevanceAdvisorySwitch"
    disableInteractionRelevanceAdvisorySwitch :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_tick"
    tick :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO Bool

foreign import ccall "wrap/RTIAmbServices.h wrap_tick_minimum_maximum"
    tick_minimum_maximum :: Ptr (RTIAmbassador fedAmb) -> TickTime -> TickTime -> Ptr (Ptr RTIException) -> IO Bool

foreign import ccall "wrap/RTIAmbServices.h wrap_getRegionToken"
    getRegionToken :: Ptr (RTIAmbassador fedAmb) -> Ptr Region -> Ptr (Ptr RTIException) -> IO RegionToken

foreign import ccall "wrap/RTIAmbServices.h wrap_getRegion"
    getRegion :: Ptr (RTIAmbassador fedAmb) -> RegionToken -> Ptr (Ptr RTIException) -> IO (Ptr Region)
