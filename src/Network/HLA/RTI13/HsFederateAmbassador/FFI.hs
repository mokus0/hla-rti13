{-# LANGUAGE ForeignFunctionInterface #-}
module Network.HLA.RTI13.HsFederateAmbassador.FFI where

import Network.HLA.RTI13.HsFederateAmbassador.Types
import Network.HLA.RTI13.RTIException
import Network.HLA.RTI13.RTITypes

import Foreign
import Foreign.C

foreign import ccall unsafe "hsFederateAmb.h wrap_new_HsFederateAmbassador"
    wrap_new_HsFederateAmbassador :: FunPtr (FunPtr a -> IO ()) -> Ptr (Ptr RTIException) -> IO (Ptr (HsFederateAmbassador t))

foreign import ccall "hsFederateAmb.h wrap_delete_HsFederateAmbassador"
    wrap_delete_HsFederateAmbassador :: Ptr (HsFederateAmbassador t) -> Ptr (Ptr RTIException) -> IO ()


------------------------------------
-- Federation Management Services --
------------------------------------

foreign import ccall "hsFederateAmb.h hsfa_set_synchronizationPointRegistrationSucceeded"
    hsfa_set_synchronizationPointRegistrationSucceeded :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_synchronizationPointRegistrationFailed"
    hsfa_set_synchronizationPointRegistrationFailed :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_announceSynchronizationPoint"
    hsfa_set_announceSynchronizationPoint :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_federationSynchronized"
    hsfa_set_federationSynchronized :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_initiateFederateSave"
    hsfa_set_initiateFederateSave :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_federationSaved"
    hsfa_set_federationSaved :: Ptr (HsFederateAmbassador t) -> FunPtr (IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_federationNotSaved"
    hsfa_set_federationNotSaved :: Ptr (HsFederateAmbassador t) -> FunPtr (IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_requestFederationRestoreSucceeded"
    hsfa_set_requestFederationRestoreSucceeded :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_requestFederationRestoreFailed"
    hsfa_set_requestFederationRestoreFailed :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_federationRestoreBegun"
    hsfa_set_federationRestoreBegun :: Ptr (HsFederateAmbassador t) -> FunPtr (IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_initiateFederateRestore"
    hsfa_set_initiateFederateRestore :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> FederateHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_federationRestored"
    hsfa_set_federationRestored :: Ptr (HsFederateAmbassador t) -> FunPtr (IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_federationNotRestored"
    hsfa_set_federationNotRestored :: Ptr (HsFederateAmbassador t) -> FunPtr (IO ()) -> IO ()

----------------------------
-- Declaration Management --
----------------------------

foreign import ccall "hsFederateAmb.h hsfa_set_turnInteractionsOn"
    hsfa_set_turnInteractionsOn :: Ptr (HsFederateAmbassador t) -> FunPtr (InteractionClassHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_turnInteractionsOff"
    hsfa_set_turnInteractionsOff :: Ptr (HsFederateAmbassador t) -> FunPtr (InteractionClassHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_startRegistrationForObjectClass"
    hsfa_set_startRegistrationForObjectClass :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectClassHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_stopRegistrationForObjectClass"
    hsfa_set_stopRegistrationForObjectClass :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectClassHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_discoverObjectInstance"
    hsfa_set_discoverObjectInstance :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> ObjectClassHandle -> CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_reflectAttributeValues"
    hsfa_set_reflectAttributeValues :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleValuePairSet -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_receiveInteraction"
    hsfa_set_receiveInteraction :: Ptr (HsFederateAmbassador t) -> FunPtr (InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_removeObjectInstance"
    hsfa_set_removeObjectInstance :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_attributesInScope"
    hsfa_set_attributesInScope :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_attributesOutOfScope"
    hsfa_set_attributesOutOfScope :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_provideAttributeValueUpdate"
    hsfa_set_provideAttributeValueUpdate :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_turnUpdatesOnForObjectInstance"
    hsfa_set_turnUpdatesOnForObjectInstance :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_turnUpdatesOffForObjectInstance"
    hsfa_set_turnUpdatesOffForObjectInstance :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

-----------------------------------
-- Ownership Management Services --
-----------------------------------

foreign import ccall "hsFederateAmb.h hsfa_set_requestAttributeOwnershipAssumption"
    hsfa_set_requestAttributeOwnershipAssumption :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_attributeOwnershipDivestitureNotification"
    hsfa_set_attributeOwnershipDivestitureNotification :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_attributeOwnershipAcquisitionNotification"
    hsfa_set_attributeOwnershipAcquisitionNotification :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_attributeOwnershipUnavailable"
    hsfa_set_attributeOwnershipUnavailable :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_requestAttributeOwnershipRelease"
    hsfa_set_requestAttributeOwnershipRelease :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_confirmAttributeOwnershipAcquisitionCancellation"
    hsfa_set_confirmAttributeOwnershipAcquisitionCancellation :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_informAttributeOwnership"
    hsfa_set_informAttributeOwnership :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> AttributeHandle -> FederateHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_attributeIsNotOwned"
    hsfa_set_attributeIsNotOwned :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> AttributeHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_attributeOwnedByRTI"
    hsfa_set_attributeOwnedByRTI :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> AttributeHandle -> IO ()) -> IO ()

---------------------
-- Time Management --
---------------------

foreign import ccall "hsFederateAmb.h hsfa_set_timeRegulationEnabled"
    hsfa_set_timeRegulationEnabled :: Ptr (HsFederateAmbassador t) -> FunPtr (Ptr t -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_timeConstrainedEnabled"
    hsfa_set_timeConstrainedEnabled :: Ptr (HsFederateAmbassador t) -> FunPtr (Ptr t -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_timeAdvanceGrant"
    hsfa_set_timeAdvanceGrant :: Ptr (HsFederateAmbassador t) -> FunPtr (Ptr t -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_requestRetraction"
    hsfa_set_requestRetraction :: Ptr (HsFederateAmbassador t) -> FunPtr (UniqueID -> FederateHandle -> IO ()) -> IO ()

