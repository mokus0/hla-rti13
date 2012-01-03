{-# LANGUAGE ForeignFunctionInterface #-}
module Network.HLA.RTI13.HsFederateAmbassador.FFI where

import Network.HLA.RTI13.HsFederateAmbassador.Types
import Network.HLA.RTI13.RTIException
import Network.HLA.RTI13.RTITypes

import Data.ByteString (ByteString)
import qualified Data.Map as M (Map)
import qualified Data.Set as S (Set)
import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe

{-# NOINLINE freeHaskellFunPtrPtr #-}
freeHaskellFunPtrPtr :: FunPtr (FunPtr a -> IO ())
freeHaskellFunPtrPtr = unsafePerformIO (mkFreeFunPtr freeHaskellFunPtr)

foreign import ccall "wrapper" mkFreeFunPtr
    ::            (FunPtr a -> IO ()) 
    -> IO (FunPtr (FunPtr a -> IO ()))

foreign import ccall unsafe "hsFederateAmb.h wrap_new_HsFederateAmbassador"
    wrap_new_HsFederateAmbassador :: FunPtr (FunPtr a -> IO ()) -> Ptr (Ptr RTIException) -> IO (Ptr (HsFederateAmbassador t))
new_HsFederateAmbassador :: IO (Ptr (HsFederateAmbassador t))
new_HsFederateAmbassador = wrapExceptions (wrap_new_HsFederateAmbassador freeHaskellFunPtrPtr)

foreign import ccall "hsFederateAmb.h wrap_delete_HsFederateAmbassador"
    wrap_delete_HsFederateAmbassador :: Ptr (HsFederateAmbassador t) -> Ptr (Ptr RTIException) -> IO ()
delete_HsFederateAmbassador :: Ptr (HsFederateAmbassador t) -> IO ()
delete_HsFederateAmbassador fedAmb = wrapExceptions (wrap_delete_HsFederateAmbassador fedAmb)

-------------------------------------
-- * Federation Management Services
-------------------------------------

foreign import ccall "hsFederateAmb.h hsfa_set_synchronizationPointRegistrationSucceeded"
    set_synchronizationPointRegistrationSucceeded :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_synchronizationPointRegistrationFailed"
    set_synchronizationPointRegistrationFailed :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_announceSynchronizationPoint"
    set_announceSynchronizationPoint :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_federationSynchronized"
    set_federationSynchronized :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_initiateFederateSave"
    set_initiateFederateSave :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_federationSaved"
    set_federationSaved :: Ptr (HsFederateAmbassador t) -> FunPtr (IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_federationNotSaved"
    set_federationNotSaved :: Ptr (HsFederateAmbassador t) -> FunPtr (IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_requestFederationRestoreSucceeded"
    set_requestFederationRestoreSucceeded :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_requestFederationRestoreFailed"
    set_requestFederationRestoreFailed :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_federationRestoreBegun"
    set_federationRestoreBegun :: Ptr (HsFederateAmbassador t) -> FunPtr (IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_initiateFederateRestore"
    set_initiateFederateRestore :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> FederateHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_federationRestored"
    set_federationRestored :: Ptr (HsFederateAmbassador t) -> FunPtr (IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_federationNotRestored"
    set_federationNotRestored :: Ptr (HsFederateAmbassador t) -> FunPtr (IO ()) -> IO ()

-----------------------------
-- * Declaration Management
-----------------------------

foreign import ccall "hsFederateAmb.h hsfa_set_turnInteractionsOn"
    set_turnInteractionsOn :: Ptr (HsFederateAmbassador t) -> FunPtr (InteractionClassHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_turnInteractionsOff"
    set_turnInteractionsOff :: Ptr (HsFederateAmbassador t) -> FunPtr (InteractionClassHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_startRegistrationForObjectClass"
    set_startRegistrationForObjectClass :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectClassHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_stopRegistrationForObjectClass"
    set_stopRegistrationForObjectClass :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectClassHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_discoverObjectInstance"
    set_discoverObjectInstance :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> ObjectClassHandle -> CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_reflectAttributeValues"
    set_reflectAttributeValues :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr (M.Map AttributeHandle ByteString) -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_receiveInteraction"
    set_receiveInteraction :: Ptr (HsFederateAmbassador t) -> FunPtr (InteractionClassHandle -> Ptr (M.Map ParameterHandle ByteString) -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_removeObjectInstance"
    set_removeObjectInstance :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_attributesInScope"
    set_attributesInScope :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr (S.Set AttributeHandle) -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_attributesOutOfScope"
    set_attributesOutOfScope :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr (S.Set AttributeHandle) -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_provideAttributeValueUpdate"
    set_provideAttributeValueUpdate :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr (S.Set AttributeHandle) -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_turnUpdatesOnForObjectInstance"
    set_turnUpdatesOnForObjectInstance :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr (S.Set AttributeHandle) -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_turnUpdatesOffForObjectInstance"
    set_turnUpdatesOffForObjectInstance :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr (S.Set AttributeHandle) -> IO ()) -> IO ()

------------------------------------
-- * Ownership Management Services
------------------------------------

foreign import ccall "hsFederateAmb.h hsfa_set_requestAttributeOwnershipAssumption"
    set_requestAttributeOwnershipAssumption :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr (S.Set AttributeHandle) -> CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_attributeOwnershipDivestitureNotification"
    set_attributeOwnershipDivestitureNotification :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr (S.Set AttributeHandle) -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_attributeOwnershipAcquisitionNotification"
    set_attributeOwnershipAcquisitionNotification :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr (S.Set AttributeHandle) -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_attributeOwnershipUnavailable"
    set_attributeOwnershipUnavailable :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr (S.Set AttributeHandle) -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_requestAttributeOwnershipRelease"
    set_requestAttributeOwnershipRelease :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr (S.Set AttributeHandle) -> CString -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_confirmAttributeOwnershipAcquisitionCancellation"
    set_confirmAttributeOwnershipAcquisitionCancellation :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr (S.Set AttributeHandle) -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_informAttributeOwnership"
    set_informAttributeOwnership :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> AttributeHandle -> FederateHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_attributeIsNotOwned"
    set_attributeIsNotOwned :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> AttributeHandle -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_attributeOwnedByRTI"
    set_attributeOwnedByRTI :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> AttributeHandle -> IO ()) -> IO ()

----------------------
-- * Time Management
----------------------

foreign import ccall "hsFederateAmb.h hsfa_set_timeRegulationEnabled"
    set_timeRegulationEnabled :: Ptr (HsFederateAmbassador t) -> FunPtr (Ptr t -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_timeConstrainedEnabled"
    set_timeConstrainedEnabled :: Ptr (HsFederateAmbassador t) -> FunPtr (Ptr t -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_timeAdvanceGrant"
    set_timeAdvanceGrant :: Ptr (HsFederateAmbassador t) -> FunPtr (Ptr t -> IO ()) -> IO ()

foreign import ccall "hsFederateAmb.h hsfa_set_requestRetraction"
    set_requestRetraction :: Ptr (HsFederateAmbassador t) -> FunPtr (UniqueID -> FederateHandle -> IO ()) -> IO ()

