{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Network.HLA.RTI13.HsFederateAmbassador
    ( HsFederateAmbassador
    , FedHandlers, setHandlers
    , module Network.HLA.RTI13.HsFederateAmbassador
    ) where

import qualified Network.HLA.RTI13.HsFederateAmbassador.FFI as FFI
import Network.HLA.RTI13.HsFederateAmbassador.FunPtrWrappers
import Network.HLA.RTI13.HsFederateAmbassador.Types

import Network.HLA.RTI13.RTITypes

import Control.Monad.Reader
import Data.ByteString (ByteString, packCString)
import qualified Data.Map as M (Map)
import qualified Data.Set as S (Set)
import Foreign hiding (newForeignPtr)
import Foreign.Concurrent

newHsFederateAmbassador :: IO (HsFederateAmbassador t)
newHsFederateAmbassador = do
    fedAmb <- FFI.new_HsFederateAmbassador
    fedAmb <- newForeignPtr fedAmb (FFI.delete_HsFederateAmbassador fedAmb)
    return (HsFederateAmbassador fedAmb)

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
        FFI.set_synchronizationPointRegistrationSucceeded fedAmb funPtr

onSynchronizationPointRegistrationFailed :: (ByteString -> IO ()) -> FedHandlers t ()
onSynchronizationPointRegistrationFailed synchronizationPointRegistrationFailed = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \cstr -> do
            str <- packCString cstr
            synchronizationPointRegistrationFailed str
        FFI.set_synchronizationPointRegistrationFailed fedAmb funPtr

onAnnounceSynchronizationPoint :: (ByteString -> ByteString -> IO ()) -> FedHandlers t ()
onAnnounceSynchronizationPoint announceSynchronizationPoint = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P2_V $ \cstr1 cstr2 -> do
            str1 <- packCString cstr1
            str2 <- packCString cstr2
            announceSynchronizationPoint str1 str2
        FFI.set_announceSynchronizationPoint fedAmb funPtr

onFederationSynchronized :: (ByteString -> IO ()) -> FedHandlers t ()
onFederationSynchronized federationSynchronized = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \cstr -> do
            str <- packCString cstr
            federationSynchronized str
        FFI.set_federationSynchronized fedAmb funPtr

onInitiateFederateSave :: (ByteString -> IO ()) -> FedHandlers t ()
onInitiateFederateSave initiateFederateSave = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \cstr -> do
            str <- packCString cstr
            initiateFederateSave str
        FFI.set_initiateFederateSave fedAmb funPtr


onFederationSaved :: IO () -> FedHandlers t ()
onFederationSaved federationSaved = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_V federationSaved
        FFI.set_federationSaved fedAmb funPtr

onFederationNotSaved :: IO () -> FedHandlers t ()
onFederationNotSaved federationNotSaved = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_V federationNotSaved
        FFI.set_federationNotSaved fedAmb funPtr

onRequestFederationRestoreSucceeded :: (ByteString -> IO ()) -> FedHandlers t ()
onRequestFederationRestoreSucceeded requestFederationRestoreSucceeded = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \cstr -> do
            str <- packCString cstr
            requestFederationRestoreSucceeded str
        FFI.set_requestFederationRestoreSucceeded fedAmb funPtr


onRequestFederationRestoreFailed :: (ByteString -> ByteString -> IO ()) -> FedHandlers t ()
onRequestFederationRestoreFailed requestFederationRestoreFailed = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P2_V $ \cstr1 cstr2 -> do
            str1 <- packCString cstr1
            str2 <- packCString cstr2
            requestFederationRestoreFailed str1 str2
        FFI.set_requestFederationRestoreFailed fedAmb funPtr


onFederationRestoreBegun :: IO () -> FedHandlers t ()
onFederationRestoreBegun federationRestoreBegun = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_V federationRestoreBegun
        FFI.set_federationRestoreBegun fedAmb funPtr

onInitiateFederateRestore :: (ByteString -> FederateHandle -> IO ()) -> FedHandlers t ()
onInitiateFederateRestore initiateFederateRestore = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_F_V $ \cstr fh -> do
            str <- packCString cstr
            initiateFederateRestore str fh
        FFI.set_initiateFederateRestore fedAmb funPtr


onFederationRestored :: IO () -> FedHandlers t ()
onFederationRestored federationRestored = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_V federationRestored
        FFI.set_federationRestored fedAmb funPtr

onFederationNotRestored :: IO () -> FedHandlers t ()
onFederationNotRestored federationNotRestored = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_V federationNotRestored
        FFI.set_federationNotRestored fedAmb funPtr


-----------------------------
-- * Declaration Management
-----------------------------

onTurnInteractionsOn :: (InteractionClassHandle -> IO ()) -> FedHandlers t ()
onTurnInteractionsOn turnInteractionsOn = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_IC_V turnInteractionsOn
        FFI.set_turnInteractionsOn fedAmb funPtr

onTurnInteractionsOff :: (InteractionClassHandle -> IO ()) -> FedHandlers t ()
onTurnInteractionsOff turnInteractionsOff = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_IC_V turnInteractionsOff
        FFI.set_turnInteractionsOff fedAmb funPtr

------------------------
-- * Object Management
------------------------

onStartRegistrationForObjectClass :: (ObjectClassHandle -> IO ()) -> FedHandlers t ()
onStartRegistrationForObjectClass startRegistrationForObjectClass = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_OC_V startRegistrationForObjectClass
        FFI.set_startRegistrationForObjectClass fedAmb funPtr


onStopRegistrationForObjectClass :: (ObjectClassHandle -> IO ()) -> FedHandlers t ()
onStopRegistrationForObjectClass stopRegistrationForObjectClass = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_OC_V stopRegistrationForObjectClass
        FFI.set_stopRegistrationForObjectClass fedAmb funPtr


onDiscoverObjectInstance :: (ObjectHandle -> ObjectClassHandle -> ByteString -> IO ()) -> FedHandlers t ()
onDiscoverObjectInstance discoverObjectInstance = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_OC_P_V $ \theObject theObjectHandle theName -> do
            theName <- packCString theName
            discoverObjectInstance theObject theObjectHandle theName
        FFI.set_discoverObjectInstance fedAmb funPtr

onReflectAttributeValues :: FedTimeImpl t => (ObjectHandle -> M.Map AttributeHandle ByteString -> ByteString -> Maybe (FedTime t, EventRetractionHandle) -> IO ()) -> FedHandlers t ()
onReflectAttributeValues reflectAttributeValues = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P3_U_F_V $ \theObject theAttrs theTime theTag theHandleSerial theHandleFed -> do
            theTag <- packCString theTag
            theAttrs <- importAttributeHandleValuePairSet theAttrs
            
            if theTime == nullPtr
                then reflectAttributeValues theObject theAttrs theTag Nothing
                else do
                    theTime <- importFedTime theTime
                    reflectAttributeValues theObject theAttrs theTag (Just (theTime, EventRetractionHandle theHandleSerial theHandleFed))
        FFI.set_reflectAttributeValues fedAmb funPtr

onReceiveInteraction :: FedTimeImpl t => (InteractionClassHandle -> M.Map ParameterHandle ByteString -> ByteString -> Maybe (FedTime t, EventRetractionHandle) -> IO ()) -> FedHandlers t ()
onReceiveInteraction receiveInteraction = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_IC_P3_U_F_V $ \theInteraction theParameters theTime theTag theHandleSerial theHandleFed -> do
            theTag <- packCString theTag
            theParameters <- importParameterHandleValuePairSet theParameters
            
            if theTime == nullPtr
                then receiveInteraction theInteraction theParameters theTag Nothing
                else do
                    theTime <- importFedTime theTime
                    receiveInteraction theInteraction theParameters theTag $ Just (theTime, EventRetractionHandle theHandleSerial theHandleFed)
        FFI.set_receiveInteraction fedAmb funPtr

onRemoveObjectInstance :: FedTimeImpl t => (ObjectHandle -> ByteString -> Maybe (FedTime t, EventRetractionHandle) -> IO ()) -> FedHandlers t ()
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
        
        FFI.set_removeObjectInstance fedAmb funPtr

onAttributesInScope :: (ObjectHandle -> S.Set AttributeHandle -> IO ()) -> FedHandlers t ()
onAttributesInScope attributesInScope = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- importAttributeHandleSet theAttrs
            attributesInScope theObject theAttrs
        FFI.set_attributesInScope fedAmb funPtr

onAttributesOutOfScope :: (ObjectHandle -> S.Set AttributeHandle -> IO ()) -> FedHandlers t ()
onAttributesOutOfScope attributesOutOfScope = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- importAttributeHandleSet theAttrs
            attributesOutOfScope theObject theAttrs
        FFI.set_attributesOutOfScope fedAmb funPtr


onProvideAttributeValueUpdate :: (ObjectHandle -> S.Set AttributeHandle -> IO ()) -> FedHandlers t ()
onProvideAttributeValueUpdate provideAttributeValueUpdate = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- importAttributeHandleSet theAttrs
            provideAttributeValueUpdate theObject theAttrs
        FFI.set_provideAttributeValueUpdate fedAmb funPtr

onTurnUpdatesOnForObjectInstance :: (ObjectHandle -> S.Set AttributeHandle -> IO ()) -> FedHandlers t ()
onTurnUpdatesOnForObjectInstance turnUpdatesOnForObjectInstance = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- importAttributeHandleSet theAttrs
            turnUpdatesOnForObjectInstance theObject theAttrs
        FFI.set_turnUpdatesOnForObjectInstance fedAmb funPtr

onTurnUpdatesOffForObjectInstance :: (ObjectHandle -> S.Set AttributeHandle -> IO ()) -> FedHandlers t ()
onTurnUpdatesOffForObjectInstance turnUpdatesOffForObjectInstance = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- importAttributeHandleSet theAttrs
            turnUpdatesOffForObjectInstance theObject theAttrs
        FFI.set_turnUpdatesOffForObjectInstance fedAmb funPtr


------------------------------------
-- * Ownership Management Services
------------------------------------

onRequestAttributeOwnershipAssumption :: (ObjectHandle -> S.Set AttributeHandle -> ByteString -> IO ()) -> FedHandlers t ()
onRequestAttributeOwnershipAssumption requestAttributeOwnershipAssumption = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P2_V $ \theObject theAttrs theTag -> do
            theAttrs <- importAttributeHandleSet theAttrs
            theTag <- packCString theTag
            requestAttributeOwnershipAssumption theObject  theAttrs theTag
        FFI.set_requestAttributeOwnershipAssumption fedAmb funPtr

onAttributeOwnershipDivestitureNotification :: (ObjectHandle -> S.Set AttributeHandle -> IO ()) -> FedHandlers t ()
onAttributeOwnershipDivestitureNotification attributeOwnershipDivestitureNotification = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- importAttributeHandleSet theAttrs
            attributeOwnershipDivestitureNotification theObject theAttrs
        FFI.set_attributeOwnershipDivestitureNotification fedAmb funPtr

onAttributeOwnershipAcquisitionNotification :: (ObjectHandle -> S.Set AttributeHandle -> IO ()) -> FedHandlers t ()
onAttributeOwnershipAcquisitionNotification attributeOwnershipAcquisitionNotification = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- importAttributeHandleSet theAttrs
            attributeOwnershipAcquisitionNotification theObject theAttrs
        FFI.set_attributeOwnershipAcquisitionNotification fedAmb funPtr

onAttributeOwnershipUnavailable :: (ObjectHandle -> S.Set AttributeHandle -> IO ()) -> FedHandlers t ()
onAttributeOwnershipUnavailable attributeOwnershipUnavailable = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- importAttributeHandleSet theAttrs
            attributeOwnershipUnavailable theObject theAttrs
        FFI.set_attributeOwnershipUnavailable fedAmb funPtr

onRequestAttributeOwnershipRelease :: (ObjectHandle -> S.Set AttributeHandle -> ByteString -> IO ()) -> FedHandlers t ()
onRequestAttributeOwnershipRelease requestAttributeOwnershipRelease = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P2_V $ \theObject theAttrs theTag -> do
            theAttrs <- importAttributeHandleSet theAttrs
            theTag <- packCString theTag
            requestAttributeOwnershipRelease theObject theAttrs theTag
        FFI.set_requestAttributeOwnershipRelease fedAmb funPtr


onConfirmAttributeOwnershipAcquisitionCancellation :: (ObjectHandle -> S.Set AttributeHandle -> IO ()) -> FedHandlers t ()
onConfirmAttributeOwnershipAcquisitionCancellation confirmAttributeOwnershipAcquisitionCancellation = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_P_V $ \theObject theAttrs -> do
            theAttrs <- importAttributeHandleSet theAttrs
            confirmAttributeOwnershipAcquisitionCancellation theObject theAttrs
        FFI.set_confirmAttributeOwnershipAcquisitionCancellation fedAmb funPtr

onInformAttributeOwnership :: (ObjectHandle -> AttributeHandle -> FederateHandle -> IO ()) -> FedHandlers t ()
onInformAttributeOwnership informAttributeOwnership = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_A_F_V informAttributeOwnership
        FFI.set_informAttributeOwnership fedAmb funPtr

onAttributeIsNotOwned :: (ObjectHandle -> AttributeHandle -> IO ()) -> FedHandlers t ()
onAttributeIsNotOwned attributeIsNotOwned = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_A_V attributeIsNotOwned
        FFI.set_attributeIsNotOwned fedAmb funPtr

onAttributeOwnedByRTI :: (ObjectHandle -> AttributeHandle -> IO ()) -> FedHandlers t ()
onAttributeOwnedByRTI attributeOwnedByRTI = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_O_A_V attributeOwnedByRTI
        FFI.set_attributeOwnedByRTI fedAmb funPtr


----------------------
-- * Time Management
----------------------

onTimeRegulationEnabled :: FedTimeImpl t => (FedTime t -> IO ()) -> FedHandlers t ()
onTimeRegulationEnabled timeRegulationEnabled = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \ftPtr -> do
            someFedTime <- importFedTime ftPtr
            timeRegulationEnabled someFedTime
        FFI.set_timeRegulationEnabled fedAmb funPtr

onTimeConstrainedEnabled :: FedTimeImpl t => (FedTime t -> IO ()) -> FedHandlers t ()
onTimeConstrainedEnabled timeConstrainedEnabled = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \ftPtr -> do
            someFedTime <- importFedTime ftPtr
            timeConstrainedEnabled someFedTime
        FFI.set_timeConstrainedEnabled fedAmb funPtr

onTimeAdvanceGrant :: FedTimeImpl t => (FedTime t -> IO ()) -> FedHandlers t ()
onTimeAdvanceGrant timeAdvanceGrant = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_P_V $ \ftPtr -> do
            someFedTime <- importFedTime ftPtr
            timeAdvanceGrant someFedTime
        FFI.set_timeAdvanceGrant fedAmb funPtr

onRequestRetraction :: (EventRetractionHandle -> IO ()) -> FedHandlers t ()
onRequestRetraction requestRetraction = do
    fedAmb <- ask
    liftIO $ withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_U_F_V $ \serial federate -> do
            requestRetraction (EventRetractionHandle serial federate)
        FFI.set_requestRetraction fedAmb funPtr
