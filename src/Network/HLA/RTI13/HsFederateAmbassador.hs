module Network.HLA.RTI13.HsFederateAmbassador where

import Foreign hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C.String
import Control.Monad.Reader
import Control.Exception

import Network.HLA.RTI13.HsFederateAmbassador.FFI
import Network.HLA.RTI13.HsFederateAmbassador.FunPtrWrappers
import Network.HLA.RTI13.HsFederateAmbassador.Types

import Network.HLA.RTI13.RTITypes
import Network.HLA.RTI13.RTIException

newHsFederateAmbassador :: IO (HsFederateAmbassador t)
newHsFederateAmbassador = do
    fedAmb <- wrapExceptions (wrap_new_HsFederateAmbassador freeHaskellFunPtrPtr)
    fedAmb <- newForeignPtr fedAmb (delete_HsFederateAmbassador fedAmb)
    return (HsFederateAmbassador fedAmb)
    
    where
        delete_HsFederateAmbassador fedAmb = wrapExceptions (wrap_delete_HsFederateAmbassador fedAmb)

------------------------------------
-- Federation Management Services --
------------------------------------

onSynchronizationPointRegistrationSucceeded :: (String -> IO ()) -> FedHandlers t ()
onSynchronizationPointRegistrationSucceeded synchronizationPointRegistrationSucceeded = do
    fedAmb <- ask
    liftIO (set_synchronizationPointRegistrationSucceeded fedAmb synchronizationPointRegistrationSucceeded)

set_synchronizationPointRegistrationSucceeded :: HsFederateAmbassador t -> (String -> IO ()) -> IO ()
set_synchronizationPointRegistrationSucceeded fedAmb synchronizationPointRegistrationSucceeded =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \cstr -> do
            str <- peekCString cstr
            synchronizationPointRegistrationSucceeded str
        hsfa_set_synchronizationPointRegistrationSucceeded fedAmb funPtr

onSynchronizationPointRegistrationFailed :: (String -> IO ()) -> FedHandlers t ()
onSynchronizationPointRegistrationFailed synchronizationPointRegistrationFailed = do
    fedAmb <- ask
    liftIO (set_synchronizationPointRegistrationFailed fedAmb synchronizationPointRegistrationFailed)

set_synchronizationPointRegistrationFailed :: HsFederateAmbassador t -> (String -> IO ()) -> IO ()
set_synchronizationPointRegistrationFailed fedAmb synchronizationPointRegistrationFailed =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \cstr -> do
            str <- peekCString cstr
            synchronizationPointRegistrationFailed str
        hsfa_set_synchronizationPointRegistrationFailed fedAmb funPtr

onAnnounceSynchronizationPoint :: (String -> String -> IO ()) -> FedHandlers t ()
onAnnounceSynchronizationPoint announceSynchronizationPoint = do
    fedAmb <- ask
    liftIO (set_announceSynchronizationPoint fedAmb announceSynchronizationPoint)

set_announceSynchronizationPoint :: HsFederateAmbassador t -> (String -> String -> IO ()) -> IO ()
set_announceSynchronizationPoint fedAmb announceSynchronizationPoint =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P2_V $ \cstr1 cstr2 -> do
            str1 <- peekCString cstr1
            str2 <- peekCString cstr2
            announceSynchronizationPoint str1 str2
        hsfa_set_announceSynchronizationPoint fedAmb funPtr

onFederationSynchronized :: (String -> IO ()) -> FedHandlers t ()
onFederationSynchronized federationSynchronized = do
    fedAmb <- ask
    liftIO (set_federationSynchronized fedAmb federationSynchronized)

set_federationSynchronized :: HsFederateAmbassador t -> (String -> IO ()) -> IO ()
set_federationSynchronized fedAmb federationSynchronized =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \cstr -> do
            str <- peekCString cstr
            federationSynchronized str
        hsfa_set_federationSynchronized fedAmb funPtr

onInitiateFederateSave :: (String -> IO ()) -> FedHandlers t ()
onInitiateFederateSave initiateFederateSave = do
    fedAmb <- ask
    liftIO (set_initiateFederateSave fedAmb initiateFederateSave)

set_initiateFederateSave :: HsFederateAmbassador t -> (String -> IO ()) -> IO ()
set_initiateFederateSave fedAmb initiateFederateSave =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \cstr -> do
            str <- peekCString cstr
            initiateFederateSave str
        hsfa_set_initiateFederateSave fedAmb funPtr


onFederationSaved :: IO () -> FedHandlers t ()
onFederationSaved federationSaved = do
    fedAmb <- ask
    liftIO (set_federationSaved fedAmb federationSaved)

set_federationSaved :: HsFederateAmbassador t -> IO () -> IO ()
set_federationSaved fedAmb federationSaved =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_V federationSaved
        hsfa_set_federationSaved fedAmb funPtr

onFederationNotSaved :: IO () -> FedHandlers t ()
onFederationNotSaved federationNotSaved = do
    fedAmb <- ask
    liftIO (set_federationSaved fedAmb federationNotSaved)

set_federationNotSaved :: HsFederateAmbassador t -> IO () -> IO ()
set_federationNotSaved fedAmb federationNotSaved =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_V federationNotSaved
        hsfa_set_federationNotSaved fedAmb funPtr

onRequestFederationRestoreSucceeded :: (String -> IO ()) -> FedHandlers t ()
onRequestFederationRestoreSucceeded requestFederationRestoreSucceeded = do
    fedAmb <- ask
    liftIO (set_requestFederationRestoreSucceeded fedAmb requestFederationRestoreSucceeded)

set_requestFederationRestoreSucceeded :: HsFederateAmbassador t -> (String -> IO ()) -> IO ()
set_requestFederationRestoreSucceeded fedAmb requestFederationRestoreSucceeded =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \cstr -> do
            str <- peekCString cstr
            requestFederationRestoreSucceeded str
        hsfa_set_requestFederationRestoreSucceeded fedAmb funPtr


onRequestFederationRestoreFailed :: (String -> String -> IO ()) -> FedHandlers t ()
onRequestFederationRestoreFailed requestFederationRestoreFailed = do
    fedAmb <- ask
    liftIO (set_requestFederationRestoreFailed fedAmb requestFederationRestoreFailed)

set_requestFederationRestoreFailed :: HsFederateAmbassador t -> (String -> String -> IO ()) -> IO ()
set_requestFederationRestoreFailed fedAmb requestFederationRestoreFailed =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P2_V $ \cstr1 cstr2 -> do
            str1 <- peekCString cstr1
            str2 <- peekCString cstr2
            requestFederationRestoreFailed str1 str2
        hsfa_set_requestFederationRestoreFailed fedAmb funPtr


onFederationRestoreBegun :: IO () -> FedHandlers t ()
onFederationRestoreBegun federationRestoreBegun = do
    fedAmb <- ask
    liftIO (set_federationRestoreBegun fedAmb federationRestoreBegun)

set_federationRestoreBegun :: HsFederateAmbassador t -> IO () -> IO ()
set_federationRestoreBegun fedAmb federationRestoreBegun =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_V federationRestoreBegun
        hsfa_set_federationRestoreBegun fedAmb funPtr

onInitiateFederateRestore :: (String -> FederateHandle -> IO ()) -> FedHandlers t ()
onInitiateFederateRestore initiateFederateRestore = do
    fedAmb <- ask
    liftIO (set_initiateFederateRestore fedAmb initiateFederateRestore)

set_initiateFederateRestore :: HsFederateAmbassador t -> (String -> FederateHandle -> IO ()) -> IO ()
set_initiateFederateRestore fedAmb initiateFederateRestore =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_F_V $ \cstr fh -> do
            str <- peekCString cstr
            initiateFederateRestore str fh
        hsfa_set_initiateFederateRestore fedAmb funPtr


onFederationRestored :: IO () -> FedHandlers t ()
onFederationRestored federationRestored = do
    fedAmb <- ask
    liftIO (set_federationRestored fedAmb federationRestored)

set_federationRestored :: HsFederateAmbassador t -> IO () -> IO ()
set_federationRestored fedAmb federationRestored =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_V federationRestored
        hsfa_set_federationRestored fedAmb funPtr

onFederationNotRestored :: IO () -> FedHandlers t ()
onFederationNotRestored federationNotRestored = do
    fedAmb <- ask
    liftIO (set_federationNotRestored fedAmb federationNotRestored)

set_federationNotRestored :: HsFederateAmbassador t -> IO () -> IO ()
set_federationNotRestored fedAmb federationNotRestored =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_V federationNotRestored
        hsfa_set_federationNotRestored fedAmb funPtr


----------------------------
-- Declaration Management --
----------------------------

onTurnInteractionsOn :: (InteractionClassHandle -> IO ()) -> FedHandlers t ()
onTurnInteractionsOn turnInteractionsOn = do
    fedAmb <- ask
    liftIO (set_turnInteractionsOn fedAmb turnInteractionsOn)

set_turnInteractionsOn :: HsFederateAmbassador t -> (InteractionClassHandle -> IO ()) -> IO ()
set_turnInteractionsOn fedAmb turnInteractionsOn = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_IC_V turnInteractionsOn
        hsfa_set_turnInteractionsOn fedAmb funPtr

onTurnInteractionsOff :: (InteractionClassHandle -> IO ()) -> FedHandlers t ()
onTurnInteractionsOff turnInteractionsOff = do
    fedAmb <- ask
    liftIO (set_turnInteractionsOff fedAmb turnInteractionsOff)

set_turnInteractionsOff :: HsFederateAmbassador t -> (InteractionClassHandle -> IO ()) -> IO ()
set_turnInteractionsOff fedAmb turnInteractionsOff = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_IC_V turnInteractionsOff
        hsfa_set_turnInteractionsOff fedAmb funPtr

-----------------------
-- Object Management --
-----------------------

onStartRegistrationForObjectClass :: (ObjectClassHandle -> IO ()) -> FedHandlers t ()
onStartRegistrationForObjectClass startRegistrationForObjectClass = do
    fedAmb <- ask
    liftIO (set_startRegistrationForObjectClass fedAmb startRegistrationForObjectClass)

set_startRegistrationForObjectClass :: HsFederateAmbassador t -> (ObjectClassHandle -> IO ()) -> IO ()
set_startRegistrationForObjectClass fedAmb startRegistrationForObjectClass = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_OC_V startRegistrationForObjectClass
        hsfa_set_startRegistrationForObjectClass fedAmb funPtr


onStopRegistrationForObjectClass :: (ObjectClassHandle -> IO ()) -> FedHandlers t ()
onStopRegistrationForObjectClass stopRegistrationForObjectClass = do
    fedAmb <- ask
    liftIO (set_stopRegistrationForObjectClass fedAmb stopRegistrationForObjectClass)

set_stopRegistrationForObjectClass :: HsFederateAmbassador t -> (ObjectClassHandle -> IO ()) -> IO ()
set_stopRegistrationForObjectClass fedAmb stopRegistrationForObjectClass = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_OC_V stopRegistrationForObjectClass
        hsfa_set_stopRegistrationForObjectClass fedAmb funPtr


onDiscoverObjectInstance :: (ObjectHandle -> ObjectClassHandle -> String -> IO ()) -> FedHandlers t ()
onDiscoverObjectInstance discoverObjectInstance = do
    fedAmb <- ask
    liftIO (set_discoverObjectInstance fedAmb discoverObjectInstance)

set_discoverObjectInstance :: HsFederateAmbassador t -> (ObjectHandle -> ObjectClassHandle -> String -> IO ()) -> IO ()
set_discoverObjectInstance fedAmb discoverObjectInstance = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_OC_P_V $ \theObject theObjectHandle theName -> do
            theName <- peekCString theName
            evaluate (length theName)
            discoverObjectInstance theObject theObjectHandle theName
        hsfa_set_discoverObjectInstance fedAmb funPtr

onReflectAttributeValues :: FedTimeImpl t => (ObjectHandle -> AttributeHandleValuePairSet -> String -> Maybe (FedTimeRepr t, EventRetractionHandle) -> IO ()) -> FedHandlers t ()
onReflectAttributeValues reflectAttributeValues = do
    fedAmb <- ask
    liftIO (set_reflectAttributeValues fedAmb reflectAttributeValues)

set_reflectAttributeValues :: FedTimeImpl t => HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleValuePairSet -> String -> Maybe (FedTimeRepr t, EventRetractionHandle) -> IO ()) -> IO ()
set_reflectAttributeValues fedAmb reflectAttributeValues =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P3_U_F_V $ \theObject theAttrs theTime theTag theHandleSerial theHandleFed -> do
            theTag <- peekCString theTag
            theAttrs <- fmap AttributeHandleValuePairSet (newForeignPtr_ theAttrs)
            
            if theTime == nullPtr
                then reflectAttributeValues theObject theAttrs theTag Nothing
                else do
                    theTime <- importFedTime theTime
                    reflectAttributeValues theObject theAttrs theTag (Just (theTime, EventRetractionHandle theHandleSerial theHandleFed))
        hsfa_set_reflectAttributeValues fedAmb funPtr

onReceiveInteraction :: FedTimeImpl t => (InteractionClassHandle -> ParameterHandleValuePairSet -> String -> Maybe (FedTimeRepr t, EventRetractionHandle) -> IO ()) -> FedHandlers t ()
onReceiveInteraction receiveInteraction = do
    fedAmb <- ask
    liftIO (set_receiveInteraction fedAmb receiveInteraction)

set_receiveInteraction :: FedTimeImpl t => HsFederateAmbassador t -> (InteractionClassHandle -> ParameterHandleValuePairSet -> String -> Maybe (FedTimeRepr t, EventRetractionHandle) -> IO ()) -> IO ()
set_receiveInteraction fedAmb receiveInteraction =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_IC_P3_U_F_V $ \theInteraction theParameters theTime theTag theHandleSerial theHandleFed -> do
            theTag <- peekCString theTag
            theParameters <- fmap ParameterHandleValuePairSet (newForeignPtr_ theParameters)
            
            if theTime == nullPtr
                then receiveInteraction theInteraction theParameters theTag Nothing
                else do
                    theTime <- importFedTime theTime
                    receiveInteraction theInteraction theParameters theTag $ Just (theTime, EventRetractionHandle theHandleSerial theHandleFed)
        hsfa_set_receiveInteraction fedAmb funPtr

onRemoveObjectInstance :: FedTimeImpl t => (ObjectHandle -> String -> Maybe (FedTimeRepr t, EventRetractionHandle) -> IO ()) -> FedHandlers t ()
onRemoveObjectInstance removeObjectInstance = do
    fedAmb <- ask
    liftIO (set_removeObjectInstance fedAmb removeObjectInstance)

set_removeObjectInstance :: FedTimeImpl t => HsFederateAmbassador t -> (ObjectHandle -> String -> Maybe (FedTimeRepr t, EventRetractionHandle) -> IO ()) -> IO ()
set_removeObjectInstance fedAmb removeObjectInstance = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P2_U_F_V $ \theObject theTime theTag theHandleSerial theHandleFederate -> do
            theTag <- peekCString theTag
            if theTime == nullPtr
                then removeObjectInstance theObject theTag Nothing
                else do
                    theTime <- importFedTime theTime
                    removeObjectInstance theObject theTag (Just (theTime, EventRetractionHandle theHandleSerial theHandleFederate))
        
        hsfa_set_removeObjectInstance fedAmb funPtr

onAttributesInScope :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onAttributesInScope attributesInScope = do
    fedAmb <- ask
    liftIO (set_attributesInScope fedAmb attributesInScope)

set_attributesInScope :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_attributesInScope fedAmb attributesInScope = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            attributesInScope theObject (AttributeHandleSet theAttrs)
        hsfa_set_attributesInScope fedAmb funPtr

onAttributesOutOfScope :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onAttributesOutOfScope attributesOutOfScope = do
    fedAmb <- ask
    liftIO (set_attributesOutOfScope fedAmb attributesOutOfScope)

set_attributesOutOfScope :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_attributesOutOfScope fedAmb attributesOutOfScope = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            attributesOutOfScope theObject (AttributeHandleSet theAttrs)
        hsfa_set_attributesOutOfScope fedAmb funPtr


onProvideAttributeValueUpdate :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onProvideAttributeValueUpdate provideAttributeValueUpdate = do
    fedAmb <- ask
    liftIO (set_provideAttributeValueUpdate fedAmb provideAttributeValueUpdate)

set_provideAttributeValueUpdate :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_provideAttributeValueUpdate fedAmb provideAttributeValueUpdate = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            provideAttributeValueUpdate theObject (AttributeHandleSet theAttrs)
        hsfa_set_provideAttributeValueUpdate fedAmb funPtr

onTurnUpdatesOnForObjectInstance :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onTurnUpdatesOnForObjectInstance turnUpdatesOnForObjectInstance = do
    fedAmb <- ask
    liftIO (set_turnUpdatesOnForObjectInstance fedAmb turnUpdatesOnForObjectInstance)

set_turnUpdatesOnForObjectInstance :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_turnUpdatesOnForObjectInstance fedAmb turnUpdatesOnForObjectInstance = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            turnUpdatesOnForObjectInstance theObject (AttributeHandleSet theAttrs)
        hsfa_set_turnUpdatesOnForObjectInstance fedAmb funPtr

onTurnUpdatesOffForObjectInstance :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onTurnUpdatesOffForObjectInstance turnUpdatesOffForObjectInstance = do
    fedAmb <- ask
    liftIO (set_turnUpdatesOffForObjectInstance fedAmb turnUpdatesOffForObjectInstance)

set_turnUpdatesOffForObjectInstance :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_turnUpdatesOffForObjectInstance fedAmb turnUpdatesOffForObjectInstance = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            turnUpdatesOffForObjectInstance theObject (AttributeHandleSet theAttrs)
        hsfa_set_turnUpdatesOffForObjectInstance fedAmb funPtr


-----------------------------------
-- Ownership Management Services --
-----------------------------------

onRequestAttributeOwnershipAssumption :: (ObjectHandle -> AttributeHandleSet -> String -> IO ()) -> FedHandlers t ()
onRequestAttributeOwnershipAssumption requestAttributeOwnershipAssumption = do
    fedAmb <- ask
    liftIO (set_requestAttributeOwnershipAssumption fedAmb requestAttributeOwnershipAssumption)

set_requestAttributeOwnershipAssumption :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> String -> IO ()) -> IO ()
set_requestAttributeOwnershipAssumption fedAmb requestAttributeOwnershipAssumption = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P2_V $ \theObject theAttrs theTag -> do
            theAttrs <- newForeignPtr_ theAttrs
            theTag <- peekCString theTag
            requestAttributeOwnershipAssumption theObject (AttributeHandleSet theAttrs) theTag
        hsfa_set_requestAttributeOwnershipAssumption fedAmb funPtr

onAttributeOwnershipDivestitureNotification :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onAttributeOwnershipDivestitureNotification attributeOwnershipDivestitureNotification = do
    fedAmb <- ask
    liftIO (set_attributeOwnershipDivestitureNotification fedAmb attributeOwnershipDivestitureNotification)

set_attributeOwnershipDivestitureNotification :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_attributeOwnershipDivestitureNotification fedAmb attributeOwnershipDivestitureNotification = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            attributeOwnershipDivestitureNotification theObject (AttributeHandleSet theAttrs)
        hsfa_set_attributeOwnershipDivestitureNotification fedAmb funPtr

onAttributeOwnershipAcquisitionNotification :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onAttributeOwnershipAcquisitionNotification attributeOwnershipAcquisitionNotification = do
    fedAmb <- ask
    liftIO (set_attributeOwnershipAcquisitionNotification fedAmb attributeOwnershipAcquisitionNotification)

set_attributeOwnershipAcquisitionNotification :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_attributeOwnershipAcquisitionNotification fedAmb attributeOwnershipAcquisitionNotification = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            attributeOwnershipAcquisitionNotification theObject (AttributeHandleSet theAttrs)
        hsfa_set_attributeOwnershipAcquisitionNotification fedAmb funPtr

onAttributeOwnershipUnavailable :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onAttributeOwnershipUnavailable attributeOwnershipUnavailable = do
    fedAmb <- ask
    liftIO (set_attributeOwnershipUnavailable fedAmb attributeOwnershipUnavailable)

set_attributeOwnershipUnavailable :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_attributeOwnershipUnavailable fedAmb attributeOwnershipUnavailable = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            attributeOwnershipUnavailable theObject (AttributeHandleSet theAttrs)
        hsfa_set_attributeOwnershipUnavailable fedAmb funPtr

onRequestAttributeOwnershipRelease :: (ObjectHandle -> AttributeHandleSet -> String -> IO ()) -> FedHandlers t ()
onRequestAttributeOwnershipRelease requestAttributeOwnershipRelease = do
    fedAmb <- ask
    liftIO (set_requestAttributeOwnershipRelease fedAmb requestAttributeOwnershipRelease)

set_requestAttributeOwnershipRelease :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> String -> IO ()) -> IO ()
set_requestAttributeOwnershipRelease fedAmb requestAttributeOwnershipRelease = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P2_V $ \theObject theAttrs theTag -> do
            theAttrs <- newForeignPtr_ theAttrs
            theTag <- peekCString theTag
            requestAttributeOwnershipRelease theObject (AttributeHandleSet theAttrs) theTag
        hsfa_set_requestAttributeOwnershipRelease fedAmb funPtr


onConfirmAttributeOwnershipAcquisitionCancellation :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onConfirmAttributeOwnershipAcquisitionCancellation confirmAttributeOwnershipAcquisitionCancellation = do
    fedAmb <- ask
    liftIO (set_confirmAttributeOwnershipAcquisitionCancellation fedAmb confirmAttributeOwnershipAcquisitionCancellation)

set_confirmAttributeOwnershipAcquisitionCancellation :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_confirmAttributeOwnershipAcquisitionCancellation fedAmb confirmAttributeOwnershipAcquisitionCancellation = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            confirmAttributeOwnershipAcquisitionCancellation theObject (AttributeHandleSet theAttrs)
        hsfa_set_confirmAttributeOwnershipAcquisitionCancellation fedAmb funPtr

onInformAttributeOwnership :: (ObjectHandle -> AttributeHandle -> FederateHandle -> IO ()) -> FedHandlers t ()
onInformAttributeOwnership informAttributeOwnership = do
    fedAmb <- ask
    liftIO (set_informAttributeOwnership fedAmb informAttributeOwnership)

set_informAttributeOwnership :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandle -> FederateHandle -> IO ()) -> IO ()
set_informAttributeOwnership fedAmb informAttributeOwnership = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_A_F_V informAttributeOwnership
        hsfa_set_informAttributeOwnership fedAmb funPtr

onAttributeIsNotOwned :: (ObjectHandle -> AttributeHandle -> IO ()) -> FedHandlers t ()
onAttributeIsNotOwned attributeIsNotOwned = do
    fedAmb <- ask
    liftIO (set_attributeIsNotOwned fedAmb attributeIsNotOwned)

set_attributeIsNotOwned :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandle -> IO ()) -> IO ()
set_attributeIsNotOwned fedAmb attributeIsNotOwned = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_A_V attributeIsNotOwned
        hsfa_set_attributeIsNotOwned fedAmb funPtr

onAttributeOwnedByRTI :: (ObjectHandle -> AttributeHandle -> IO ()) -> FedHandlers t ()
onAttributeOwnedByRTI attributeOwnedByRTI = do
    fedAmb <- ask
    liftIO (set_attributeOwnedByRTI fedAmb attributeOwnedByRTI)

set_attributeOwnedByRTI :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandle -> IO ()) -> IO ()
set_attributeOwnedByRTI fedAmb attributeOwnedByRTI = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_A_V attributeOwnedByRTI
        hsfa_set_attributeOwnedByRTI fedAmb funPtr


---------------------
-- Time Management --
---------------------

onTimeRegulationEnabled :: FedTimeImpl t => (FedTimeRepr t -> IO ()) -> FedHandlers t ()
onTimeRegulationEnabled timeRegulationEnabled = do
    fedAmb <- ask
    liftIO (set_timeRegulationEnabled fedAmb timeRegulationEnabled)

set_timeRegulationEnabled :: FedTimeImpl t => HsFederateAmbassador t -> (FedTimeRepr t -> IO ()) -> IO ()
set_timeRegulationEnabled fedAmb timeRegulationEnabled = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \ftPtr -> do
            someFedTime <- importFedTime ftPtr
            timeRegulationEnabled someFedTime
        hsfa_set_timeRegulationEnabled fedAmb funPtr

onTimeConstrainedEnabled :: FedTimeImpl t => (FedTimeRepr t -> IO ()) -> FedHandlers t ()
onTimeConstrainedEnabled timeConstrainedEnabled = do
    fedAmb <- ask
    liftIO (set_timeConstrainedEnabled fedAmb timeConstrainedEnabled)

set_timeConstrainedEnabled :: FedTimeImpl t => HsFederateAmbassador t -> (FedTimeRepr t -> IO ()) -> IO ()
set_timeConstrainedEnabled fedAmb timeConstrainedEnabled = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \ftPtr -> do
            someFedTime <- importFedTime ftPtr
            timeConstrainedEnabled someFedTime
        hsfa_set_timeConstrainedEnabled fedAmb funPtr

onTimeAdvanceGrant :: FedTimeImpl t => (FedTimeRepr t -> IO ()) -> FedHandlers t ()
onTimeAdvanceGrant timeAdvanceGrant = do
    fedAmb <- ask
    liftIO (set_timeAdvanceGrant fedAmb timeAdvanceGrant)

set_timeAdvanceGrant :: FedTimeImpl t => HsFederateAmbassador t -> (FedTimeRepr t -> IO ()) -> IO ()
set_timeAdvanceGrant fedAmb timeAdvanceGrant = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \ftPtr -> do
            someFedTime <- importFedTime ftPtr
            timeAdvanceGrant someFedTime
        hsfa_set_timeAdvanceGrant fedAmb funPtr

onRequestRetraction :: (EventRetractionHandle -> IO ()) -> FedHandlers t ()
onRequestRetraction requestRetraction = do
    fedAmb <- ask
    liftIO (set_requestRetraction fedAmb requestRetraction)

set_requestRetraction :: HsFederateAmbassador t -> (EventRetractionHandle -> IO ()) -> IO ()
set_requestRetraction fedAmb requestRetraction =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_U_F_V $ \serial federate -> do
            requestRetraction (EventRetractionHandle serial federate)
        hsfa_set_requestRetraction fedAmb funPtr