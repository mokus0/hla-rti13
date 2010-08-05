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

foreign import ccall unsafe "wrap/RTIambServices.h wrap_synchronizationPointAchieved"
    wrap_synchronizationPointAchieved :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_requestFederationSave"
    wrap_requestFederationSave :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_requestFederationSaveAtTime"
    wrap_requestFederationSaveAtTime :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_federateSaveBegun"
    wrap_federateSaveBegun :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_federateSaveComplete"
    wrap_federateSaveComplete :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_federateSaveNotComplete"
    wrap_federateSaveNotComplete :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_requestFederationRestore"
    wrap_requestFederationRestore :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_federateRestoreComplete"
    wrap_federateRestoreComplete :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIambServices.h wrap_federateRestoreNotComplete"
    wrap_federateRestoreNotComplete :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

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

foreign import ccall "wrap/RTIAmbServices.h wrap_updateAttributeValuesAtTime"
    wrap_updateAttributeValuesAtTime :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleValuePairSet -> Ptr (FedAmbTime fedAmb) -> CString -> Ptr UniqueID -> Ptr FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_updateAttributeValues"
    wrap_updateAttributeValues :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleValuePairSet -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_registerObjectInstance"
    wrap_registerObjectInstance :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO ObjectHandle

foreign import ccall "wrap/RTIAmbServices.h wrap_sendInteractionAtTime"
    wrap_sendInteractionAtTime :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> Ptr (FedAmbTime fedAmb) -> CString -> Ptr UniqueID -> Ptr FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_sendInteraction"
    wrap_sendInteraction :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_deleteObjectInstanceAtTime"
    wrap_deleteObjectInstanceAtTime :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr (FedAmbTime fedAmb) -> CString -> Ptr UniqueID -> Ptr FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_deleteObjectInstance"
    wrap_deleteObjectInstance :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_localDeleteObjectInstance"
    wrap_localDeleteObjectInstance :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_changeAttributeTransportationType"
    wrap_changeAttributeTransportationType :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> TransportationHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_changeInteractionTransportationType"
    wrap_changeInteractionTransportationType :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> TransportationHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_requestObjectAttributeValueUpdate"
    wrap_requestObjectAttributeValueUpdate :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_requestClassAttributeValueUpdate"
    wrap_requestClassAttributeValueUpdate :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()

-----------------------------------
-- Ownership Management Services --
-----------------------------------

foreign import ccall "wrap/RTIAmbServices.h wrap_unconditionalAttributeOwnershipDivestiture"
    wrap_unconditionalAttributeOwnershipDivestiture :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_negotiatedAttributeOwnershipDivestiture"
    wrap_negotiatedAttributeOwnershipDivestiture :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_attributeOwnershipAcquisition"
    wrap_attributeOwnershipAcquisition :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_attributeOwnershipAcquisitionIfAvailable"
    wrap_attributeOwnershipAcquisitionIfAvailable :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_attributeOwnershipReleaseResponse"
    wrap_attributeOwnershipReleaseResponse :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO (Ptr AttributeHandleSet)

foreign import ccall "wrap/RTIAmbServices.h wrap_cancelNegotiatedAttributeOwnershipDivestiture"
    wrap_cancelNegotiatedAttributeOwnershipDivestiture :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_cancelAttributeOwnershipAcquisition"
    wrap_cancelAttributeOwnershipAcquisition :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_queryAttributeOwnership"
    wrap_queryAttributeOwnership :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> AttributeHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_isAttributeOwnedByFederate"
    wrap_isAttributeOwnedByFederate :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> AttributeHandle -> Ptr (Ptr RTIException) -> IO Bool

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

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_timeAdvanceRequestAvailable"
    wrap_timeAdvanceRequestAvailable :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_nextEventRequest"
    wrap_nextEventRequest :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_nextEventRequestAvailable"
    wrap_nextEventRequestAvailable :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_flushQueueRequest"
    wrap_flushQueueRequest :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_enableAsynchronousDelivery"
    wrap_enableAsynchronousDelivery :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_disableAsynchronousDelivery"
    wrap_disableAsynchronousDelivery :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_queryLBTS"
    wrap_queryLBTS :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_queryFederateTime"
    wrap_queryFederateTime :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_queryMinNextEventTime"
    wrap_queryMinNextEventTime :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_modifyLookahead"
    wrap_modifyLookahead :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_queryLookahead"
    wrap_queryLookahead :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_retract"
    wrap_retract :: Ptr (RTIAmbassador fedAmb) -> UniqueID -> FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_changeAttributeOrderType"
    wrap_changeAttributeOrderType :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr (AttributeHandleSet) -> OrderingHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_changeInteractionOrderType"
    wrap_changeInteractionOrderType :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> OrderingHandle -> Ptr (Ptr RTIException) -> IO ()

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
    wrap_registerObjectInstanceWithRegion_withName :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> CString -> Ptr AttributeHandle -> Ptr (Ptr Region) -> ULong -> Ptr (Ptr RTIException) -> IO ObjectHandle

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_registerObjectInstanceWithRegion" 
    wrap_registerObjectInstanceWithRegion :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> Ptr AttributeHandle -> Ptr (Ptr Region) -> ULong -> Ptr (Ptr RTIException) -> IO ObjectHandle

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_associateRegionForUpdates" 
    wrap_associateRegionForUpdates :: Ptr (RTIAmbassador t) -> Ptr Region -> ObjectHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_unassociateRegionForUpdates" 
    wrap_unassociateRegionForUpdates :: Ptr (RTIAmbassador t) -> Ptr Region -> ObjectHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_subscribeObjectClassAttributesWithRegion" 
    wrap_subscribeObjectClassAttributesWithRegion :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> Ptr Region -> Ptr AttributeHandleSet -> Bool -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_unsubscribeObjectClassWithRegion" 
    wrap_unsubscribeObjectClassWithRegion :: Ptr (RTIAmbassador t) -> ObjectClassHandle -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_subscribeInteractionClassWithRegion" 
    wrap_subscribeInteractionClassWithRegion :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr Region -> Bool -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_unsubscribeInteractionClassWithRegion" 
    wrap_unsubscribeInteractionClassWithRegion :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_sendInteractionWithRegionAtTime" 
    wrap_sendInteractionWithRegionAtTime :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> Ptr (FedAmbTime fedAmb) -> CString -> Ptr Region -> Ptr UniqueID -> Ptr FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_sendInteractionWithRegion" 
    wrap_sendInteractionWithRegion :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> CString -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_requestClassAttributeValueUpdateWithRegion" 
    wrap_requestClassAttributeValueUpdateWithRegion :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr AttributeHandleSet -> Ptr Region -> Ptr (Ptr RTIException) -> IO ()


--------------------------
-- RTI Support Services --
--------------------------

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_getObjectClassHandle"
    wrap_getObjectClassHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO ObjectClassHandle

foreign import ccall unsafe "wrap/RTIAmbServices.h wrap_getObjectClassName"
    wrap_getObjectClassName :: Ptr (RTIAmbassador fedAmb) -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getAttributeHandle"
    wrap_getAttributeHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO AttributeHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getAttributeName"
    wrap_getAttributeName :: Ptr (RTIAmbassador fedAmb) -> AttributeHandle -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getInteractionClassHandle"
    wrap_getInteractionClassHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO InteractionClassHandle
    
foreign import ccall unsafe "wrap/RTIambServices.h wrap_getInteractionClassName"
    wrap_getInteractionClassName :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getParameterHandle"
    wrap_getParameterHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO ParameterHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getParameterName"
    wrap_getParameterName :: Ptr (RTIAmbassador fedAmb) -> ParameterHandle -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getObjectInstanceHandle"
    wrap_getObjectInstanceHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO ObjectHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getObjectInstanceName"
    wrap_getObjectInstanceName :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getRoutingSpaceHandle"
    wrap_getRoutingSpaceHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO SpaceHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getRoutingSpaceName"
    wrap_getRoutingSpaceName :: Ptr (RTIAmbassador fedAmb) -> SpaceHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getDimensionHandle"
    wrap_getDimensionHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> SpaceHandle -> Ptr (Ptr RTIException) -> IO DimensionHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getDimensionName"
    wrap_getDimensionName :: Ptr (RTIAmbassador fedAmb) -> DimensionHandle -> SpaceHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getAttributeRoutingSpaceHandle"
    wrap_getAttributeRoutingSpaceHandle :: Ptr (RTIAmbassador fedAmb) -> AttributeHandle -> ObjectClassHandle -> Ptr (Ptr RTIException) -> IO SpaceHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getObjectClass"
    wrap_getObjectClass :: Ptr (RTIAmbassador fedAmb) -> ObjectHandle -> Ptr (Ptr RTIException) -> IO ObjectClassHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getInteractionRoutingSpaceHandle"
    wrap_getInteractionRoutingSpaceHandle :: Ptr (RTIAmbassador fedAmb) -> InteractionClassHandle -> Ptr (Ptr RTIException) -> IO SpaceHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getTransportationHandle"
    wrap_getTransportationHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO TransportationHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getTransportationName"
    wrap_getTransportationName :: Ptr (RTIAmbassador fedAmb) -> TransportationHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getOrderingHandle"
    wrap_getOrderingHandle :: Ptr (RTIAmbassador fedAmb) -> CString -> Ptr (Ptr RTIException) -> IO OrderingHandle

foreign import ccall unsafe "wrap/RTIambServices.h wrap_getOrderingName"
    wrap_getOrderingName :: Ptr (RTIAmbassador fedAmb) -> OrderingHandle -> Ptr (Ptr RTIException) -> IO CString

foreign import ccall "wrap/RTIAmbServices.h wrap_enableClassRelevanceAdvisorySwitch"
    wrap_enableClassRelevanceAdvisorySwitch :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_disableClassRelevanceAdvisorySwitch"
    wrap_disableClassRelevanceAdvisorySwitch :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_enableAttributeRelevanceAdvisorySwitch"
    wrap_enableAttributeRelevanceAdvisorySwitch :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_disableAttributeRelevanceAdvisorySwitch"
    wrap_disableAttributeRelevanceAdvisorySwitch :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_enableAttributeScopeAdvisorySwitch"
    wrap_enableAttributeScopeAdvisorySwitch :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_disableAttributeScopeAdvisorySwitch"
    wrap_disableAttributeScopeAdvisorySwitch :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_enableInteractionRelevanceAdvisorySwitch"
    wrap_enableInteractionRelevanceAdvisorySwitch :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_disableInteractionRelevanceAdvisorySwitch"
    wrap_disableInteractionRelevanceAdvisorySwitch :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTIAmbServices.h wrap_tick"
    wrap_tick :: Ptr (RTIAmbassador fedAmb) -> Ptr (Ptr RTIException) -> IO Bool

foreign import ccall "wrap/RTIAmbServices.h wrap_tick_minimum_maximum"
    wrap_tick_minimum_maximum :: Ptr (RTIAmbassador fedAmb) -> TickTime -> TickTime -> Ptr (Ptr RTIException) -> IO Bool

foreign import ccall "wrap/RTIAmbServices.h wrap_getRegionToken"
    wrap_getRegionToken :: Ptr (RTIAmbassador fedAmb) -> Ptr Region -> Ptr (Ptr RTIException) -> IO RegionToken

foreign import ccall "wrap/RTIAmbServices.h wrap_getRegion"
    wrap_getRegion :: Ptr (RTIAmbassador fedAmb) -> RegionToken -> Ptr (Ptr RTIException) -> IO (Ptr Region)
