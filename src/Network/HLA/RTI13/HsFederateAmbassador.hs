{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Network.HLA.RTI13.HsFederateAmbassador
    ( HsFederateAmbassador
    , FedHandlers, setHandlers
    , module Network.HLA.RTI13.HsFederateAmbassador
    ) where

import Foreign hiding (newForeignPtr)
import Foreign.Concurrent
import Control.Monad.Reader
import Data.ByteString (ByteString, packCString)

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

newFedAmbWithHandlers :: FedHandlers t () -> IO (HsFederateAmbassador t)
newFedAmbWithHandlers handlers = do
    fedAmb <- newHsFederateAmbassador
    setHandlers fedAmb handlers
    return fedAmb

-------------------------------------
-- * Federation Management Services
-------------------------------------

onSynchronizationPointRegistrationSucceeded :: (ByteString -> IO ()) -> FedHandlers t ()
onSynchronizationPointRegistrationSucceeded synchronizationPointRegistrationSucceeded = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \cstr -> do
            str <- packCString cstr
            synchronizationPointRegistrationSucceeded str
        hsfa_set_synchronizationPointRegistrationSucceeded fedAmb funPtr

onSynchronizationPointRegistrationFailed :: (ByteString -> IO ()) -> FedHandlers t ()
onSynchronizationPointRegistrationFailed synchronizationPointRegistrationFailed = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \cstr -> do
            str <- packCString cstr
            synchronizationPointRegistrationFailed str
        hsfa_set_synchronizationPointRegistrationFailed fedAmb funPtr

onAnnounceSynchronizationPoint :: (ByteString -> ByteString -> IO ()) -> FedHandlers t ()
onAnnounceSynchronizationPoint announceSynchronizationPoint = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P2_V $ \cstr1 cstr2 -> do
            str1 <- packCString cstr1
            str2 <- packCString cstr2
            announceSynchronizationPoint str1 str2
        hsfa_set_announceSynchronizationPoint fedAmb funPtr

onFederationSynchronized :: (ByteString -> IO ()) -> FedHandlers t ()
onFederationSynchronized federationSynchronized = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \cstr -> do
            str <- packCString cstr
            federationSynchronized str
        hsfa_set_federationSynchronized fedAmb funPtr

onInitiateFederateSave :: (ByteString -> IO ()) -> FedHandlers t ()
onInitiateFederateSave initiateFederateSave = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \cstr -> do
            str <- packCString cstr
            initiateFederateSave str
        hsfa_set_initiateFederateSave fedAmb funPtr


onFederationSaved :: IO () -> FedHandlers t ()
onFederationSaved federationSaved = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_V federationSaved
        hsfa_set_federationSaved fedAmb funPtr

onFederationNotSaved :: IO () -> FedHandlers t ()
onFederationNotSaved federationNotSaved = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_V federationNotSaved
        hsfa_set_federationNotSaved fedAmb funPtr

onRequestFederationRestoreSucceeded :: (ByteString -> IO ()) -> FedHandlers t ()
onRequestFederationRestoreSucceeded requestFederationRestoreSucceeded = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \cstr -> do
            str <- packCString cstr
            requestFederationRestoreSucceeded str
        hsfa_set_requestFederationRestoreSucceeded fedAmb funPtr


onRequestFederationRestoreFailed :: (ByteString -> ByteString -> IO ()) -> FedHandlers t ()
onRequestFederationRestoreFailed requestFederationRestoreFailed = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P2_V $ \cstr1 cstr2 -> do
            str1 <- packCString cstr1
            str2 <- packCString cstr2
            requestFederationRestoreFailed str1 str2
        hsfa_set_requestFederationRestoreFailed fedAmb funPtr


onFederationRestoreBegun :: IO () -> FedHandlers t ()
onFederationRestoreBegun federationRestoreBegun = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_V federationRestoreBegun
        hsfa_set_federationRestoreBegun fedAmb funPtr

onInitiateFederateRestore :: (ByteString -> FederateHandle -> IO ()) -> FedHandlers t ()
onInitiateFederateRestore initiateFederateRestore = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_F_V $ \cstr fh -> do
            str <- packCString cstr
            initiateFederateRestore str fh
        hsfa_set_initiateFederateRestore fedAmb funPtr


onFederationRestored :: IO () -> FedHandlers t ()
onFederationRestored federationRestored = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_V federationRestored
        hsfa_set_federationRestored fedAmb funPtr

onFederationNotRestored :: IO () -> FedHandlers t ()
onFederationNotRestored federationNotRestored = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_V federationNotRestored
        hsfa_set_federationNotRestored fedAmb funPtr


-----------------------------
-- * Declaration Management
-----------------------------

onTurnInteractionsOn :: (InteractionClassHandle -> IO ()) -> FedHandlers t ()
onTurnInteractionsOn turnInteractionsOn = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_IC_V turnInteractionsOn
        hsfa_set_turnInteractionsOn fedAmb funPtr

onTurnInteractionsOff :: (InteractionClassHandle -> IO ()) -> FedHandlers t ()
onTurnInteractionsOff turnInteractionsOff = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_IC_V turnInteractionsOff
        hsfa_set_turnInteractionsOff fedAmb funPtr

------------------------
-- * Object Management
------------------------

onStartRegistrationForObjectClass :: (ObjectClassHandle -> IO ()) -> FedHandlers t ()
onStartRegistrationForObjectClass startRegistrationForObjectClass = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_OC_V startRegistrationForObjectClass
        hsfa_set_startRegistrationForObjectClass fedAmb funPtr


onStopRegistrationForObjectClass :: (ObjectClassHandle -> IO ()) -> FedHandlers t ()
onStopRegistrationForObjectClass stopRegistrationForObjectClass = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_OC_V stopRegistrationForObjectClass
        hsfa_set_stopRegistrationForObjectClass fedAmb funPtr


onDiscoverObjectInstance :: (ObjectHandle -> ObjectClassHandle -> ByteString -> IO ()) -> FedHandlers t ()
onDiscoverObjectInstance discoverObjectInstance = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_OC_P_V $ \theObject theObjectHandle theName -> do
            theName <- packCString theName
            discoverObjectInstance theObject theObjectHandle theName
        hsfa_set_discoverObjectInstance fedAmb funPtr

onReflectAttributeValues :: FedTimeImpl t => (ObjectHandle -> AttributeHandleValuePairSet -> ByteString -> Maybe (FedTimeRepr t, EventRetractionHandle) -> IO ()) -> FedHandlers t ()
onReflectAttributeValues reflectAttributeValues = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P3_U_F_V $ \theObject theAttrs theTime theTag theHandleSerial theHandleFed -> do
            theTag <- packCString theTag
            theAttrs <- fmap AttributeHandleValuePairSet (newForeignPtr_ theAttrs)
            
            if theTime == nullPtr
                then reflectAttributeValues theObject theAttrs theTag Nothing
                else do
                    theTime <- importFedTime theTime
                    reflectAttributeValues theObject theAttrs theTag (Just (theTime, EventRetractionHandle theHandleSerial theHandleFed))
        hsfa_set_reflectAttributeValues fedAmb funPtr

onReceiveInteraction :: FedTimeImpl t => (InteractionClassHandle -> ParameterHandleValuePairSet -> ByteString -> Maybe (FedTimeRepr t, EventRetractionHandle) -> IO ()) -> FedHandlers t ()
onReceiveInteraction receiveInteraction = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_IC_P3_U_F_V $ \theInteraction theParameters theTime theTag theHandleSerial theHandleFed -> do
            theTag <- packCString theTag
            theParameters <- fmap ParameterHandleValuePairSet (newForeignPtr_ theParameters)
            
            if theTime == nullPtr
                then receiveInteraction theInteraction theParameters theTag Nothing
                else do
                    theTime <- importFedTime theTime
                    receiveInteraction theInteraction theParameters theTag $ Just (theTime, EventRetractionHandle theHandleSerial theHandleFed)
        hsfa_set_receiveInteraction fedAmb funPtr

onRemoveObjectInstance :: FedTimeImpl t => (ObjectHandle -> ByteString -> Maybe (FedTimeRepr t, EventRetractionHandle) -> IO ()) -> FedHandlers t ()
onRemoveObjectInstance removeObjectInstance = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P2_U_F_V $ \theObject theTime theTag theHandleSerial theHandleFederate -> do
            theTag <- packCString theTag
            if theTime == nullPtr
                then removeObjectInstance theObject theTag Nothing
                else do
                    theTime <- importFedTime theTime
                    removeObjectInstance theObject theTag (Just (theTime, EventRetractionHandle theHandleSerial theHandleFederate))
        
        hsfa_set_removeObjectInstance fedAmb funPtr

onAttributesInScope :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onAttributesInScope attributesInScope = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            attributesInScope theObject (AttributeHandleSet theAttrs)
        hsfa_set_attributesInScope fedAmb funPtr

onAttributesOutOfScope :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onAttributesOutOfScope attributesOutOfScope = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            attributesOutOfScope theObject (AttributeHandleSet theAttrs)
        hsfa_set_attributesOutOfScope fedAmb funPtr


onProvideAttributeValueUpdate :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onProvideAttributeValueUpdate provideAttributeValueUpdate = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            provideAttributeValueUpdate theObject (AttributeHandleSet theAttrs)
        hsfa_set_provideAttributeValueUpdate fedAmb funPtr

onTurnUpdatesOnForObjectInstance :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onTurnUpdatesOnForObjectInstance turnUpdatesOnForObjectInstance = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            turnUpdatesOnForObjectInstance theObject (AttributeHandleSet theAttrs)
        hsfa_set_turnUpdatesOnForObjectInstance fedAmb funPtr

onTurnUpdatesOffForObjectInstance :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onTurnUpdatesOffForObjectInstance turnUpdatesOffForObjectInstance = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            turnUpdatesOffForObjectInstance theObject (AttributeHandleSet theAttrs)
        hsfa_set_turnUpdatesOffForObjectInstance fedAmb funPtr


------------------------------------
-- * Ownership Management Services
------------------------------------

onRequestAttributeOwnershipAssumption :: (ObjectHandle -> AttributeHandleSet -> ByteString -> IO ()) -> FedHandlers t ()
onRequestAttributeOwnershipAssumption requestAttributeOwnershipAssumption = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P2_V $ \theObject theAttrs theTag -> do
            theAttrs <- newForeignPtr_ theAttrs
            theTag <- packCString theTag
            requestAttributeOwnershipAssumption theObject (AttributeHandleSet theAttrs) theTag
        hsfa_set_requestAttributeOwnershipAssumption fedAmb funPtr

onAttributeOwnershipDivestitureNotification :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onAttributeOwnershipDivestitureNotification attributeOwnershipDivestitureNotification = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            attributeOwnershipDivestitureNotification theObject (AttributeHandleSet theAttrs)
        hsfa_set_attributeOwnershipDivestitureNotification fedAmb funPtr

onAttributeOwnershipAcquisitionNotification :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onAttributeOwnershipAcquisitionNotification attributeOwnershipAcquisitionNotification = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            attributeOwnershipAcquisitionNotification theObject (AttributeHandleSet theAttrs)
        hsfa_set_attributeOwnershipAcquisitionNotification fedAmb funPtr

onAttributeOwnershipUnavailable :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onAttributeOwnershipUnavailable attributeOwnershipUnavailable = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            attributeOwnershipUnavailable theObject (AttributeHandleSet theAttrs)
        hsfa_set_attributeOwnershipUnavailable fedAmb funPtr

onRequestAttributeOwnershipRelease :: (ObjectHandle -> AttributeHandleSet -> ByteString -> IO ()) -> FedHandlers t ()
onRequestAttributeOwnershipRelease requestAttributeOwnershipRelease = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P2_V $ \theObject theAttrs theTag -> do
            theAttrs <- newForeignPtr_ theAttrs
            theTag <- packCString theTag
            requestAttributeOwnershipRelease theObject (AttributeHandleSet theAttrs) theTag
        hsfa_set_requestAttributeOwnershipRelease fedAmb funPtr


onConfirmAttributeOwnershipAcquisitionCancellation :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onConfirmAttributeOwnershipAcquisitionCancellation confirmAttributeOwnershipAcquisitionCancellation = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            confirmAttributeOwnershipAcquisitionCancellation theObject (AttributeHandleSet theAttrs)
        hsfa_set_confirmAttributeOwnershipAcquisitionCancellation fedAmb funPtr

onInformAttributeOwnership :: (ObjectHandle -> AttributeHandle -> FederateHandle -> IO ()) -> FedHandlers t ()
onInformAttributeOwnership informAttributeOwnership = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_A_F_V informAttributeOwnership
        hsfa_set_informAttributeOwnership fedAmb funPtr

onAttributeIsNotOwned :: (ObjectHandle -> AttributeHandle -> IO ()) -> FedHandlers t ()
onAttributeIsNotOwned attributeIsNotOwned = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_A_V attributeIsNotOwned
        hsfa_set_attributeIsNotOwned fedAmb funPtr

onAttributeOwnedByRTI :: (ObjectHandle -> AttributeHandle -> IO ()) -> FedHandlers t ()
onAttributeOwnedByRTI attributeOwnedByRTI = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_A_V attributeOwnedByRTI
        hsfa_set_attributeOwnedByRTI fedAmb funPtr


----------------------
-- * Time Management
----------------------

onTimeRegulationEnabled :: FedTimeImpl t => (FedTimeRepr t -> IO ()) -> FedHandlers t ()
onTimeRegulationEnabled timeRegulationEnabled = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \ftPtr -> do
            someFedTime <- importFedTime ftPtr
            timeRegulationEnabled someFedTime
        hsfa_set_timeRegulationEnabled fedAmb funPtr

onTimeConstrainedEnabled :: FedTimeImpl t => (FedTimeRepr t -> IO ()) -> FedHandlers t ()
onTimeConstrainedEnabled timeConstrainedEnabled = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \ftPtr -> do
            someFedTime <- importFedTime ftPtr
            timeConstrainedEnabled someFedTime
        hsfa_set_timeConstrainedEnabled fedAmb funPtr

onTimeAdvanceGrant :: FedTimeImpl t => (FedTimeRepr t -> IO ()) -> FedHandlers t ()
onTimeAdvanceGrant timeAdvanceGrant = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \ftPtr -> do
            someFedTime <- importFedTime ftPtr
            timeAdvanceGrant someFedTime
        hsfa_set_timeAdvanceGrant fedAmb funPtr

onRequestRetraction :: (EventRetractionHandle -> IO ()) -> FedHandlers t ()
onRequestRetraction requestRetraction = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_U_F_V $ \serial federate -> do
            requestRetraction (EventRetractionHandle serial federate)
        hsfa_set_requestRetraction fedAmb funPtr