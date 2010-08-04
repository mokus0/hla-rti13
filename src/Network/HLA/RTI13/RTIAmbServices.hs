module Network.HLA.RTI13.RTIAmbServices
    ( RTIAmbassador
    , module Network.HLA.RTI13.RTIAmbServices
    ) where

import Network.HLA.RTI13.BaseTypes
import Network.HLA.RTI13.RTITypes
import Network.HLA.RTI13.RTIAmbServices.FFI as FFI
import Network.HLA.RTI13.RTIException
import Foreign hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C.String
import Control.Exception (bracket_)
import Data.StateRef
import System.Mem
import Data.List

getRTIAmbassador :: IO (RTIAmbassador fedAmb)
getRTIAmbassador = do
    rtiAmb <- new_RTIambassador
    rtiAmb <- newForeignPtr rtiAmb (delete_RTIambassador rtiAmb)
    fedAmb <- newReference Nothing
    return (RTIAmbassador rtiAmb fedAmb)

------------------------------------
-- Federation Management Services --
------------------------------------

createFederationExecution :: RTIAmbassador fedAmb -> String -> String -> IO ()
createFederationExecution rtiAmb executionName fed = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withCString executionName $ \executionName ->
            withCString fed $ \fed ->
                wrapExceptions (wrap_createFederationExecution rtiAmb executionName fed)

destroyFederationExecution :: RTIAmbassador fedAmb -> String -> IO ()
destroyFederationExecution rtiAmb executionName = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withCString executionName $ \executionName ->
            wrapExceptions (wrap_destroyFederationExecution rtiAmb executionName)

joinFederationExecution :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> String -> String -> fedAmb -> IO FederateHandle
joinFederationExecution rtiAmb yourName executionName fedAmb = do
    fedHandle <- withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withCString yourName $ \yourName ->
            withCString executionName $ \executionName ->
                withFederateAmbassador fedAmb $ \fedAmb ->
                    wrapExceptions (wrap_joinFederationExecution rtiAmb yourName executionName fedAmb)
    writeReference (rtiFedAmb rtiAmb) (Just fedAmb)
    return fedHandle

resignFederationExecution :: RTIAmbassador fedAmb -> ResignAction -> IO ()
resignFederationExecution rtiAmb resignAction = do
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        wrapExceptions (wrap_resignFederationExecution rtiAmb (fromIntegral (fromEnum resignAction)))
    writeReference (rtiFedAmb rtiAmb) Nothing
    performGC

registerFederationSynchronizationPoint :: RTIAmbassador fedAmb -> String -> String -> Maybe FederateHandleSet -> IO ()
registerFederationSynchronizationPoint rtiAmb label theTag mbSyncSet = do
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withCString label $ \label ->
            withCString theTag $ \theTag -> case mbSyncSet of
                    Nothing ->
                        wrapExceptions (wrap_registerFederationSynchronizationPoint rtiAmb label theTag)
                    Just syncSet -> withFederateHandleSet syncSet $ \syncSet ->
                        wrapExceptions (wrap_registerFederationSynchronizationPoint_with_syncSet rtiAmb label theTag syncSet)

synchronizationPointAchieved :: RTIAmbassador fedAmb -> String -> IO ()
synchronizationPointAchieved rtiAmb label =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withCString label $ \label ->
            wrapExceptions (wrap_synchronizationPointAchieved rtiAmb label)

requestFederationSave :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> String -> Maybe (FedTime fedAmb) -> IO ()
requestFederationSave rtiAmb label mbTime =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withCString label $ \label -> case mbTime of
            Nothing ->
                wrapExceptions (wrap_requestFederationSave rtiAmb label)
            Just theTime -> 
                withFedTime_ theTime $ \theTime ->
                    wrapExceptions (wrap_requestFederationSaveAtTime rtiAmb label theTime)

federateSaveBegun :: RTIAmbassador fedAmb -> IO ()
federateSaveBegun rtiAmb = withRTIAmbassador rtiAmb
    (wrapExceptions . wrap_federateSaveBegun)

federateSaveComplete :: RTIAmbassador fedAmb -> IO ()
federateSaveComplete rtiAmb = withRTIAmbassador rtiAmb
    (wrapExceptions . wrap_federateSaveComplete)

federateSaveNotComplete :: RTIAmbassador fedAmb -> IO ()
federateSaveNotComplete rtiAmb = withRTIAmbassador rtiAmb
    (wrapExceptions . wrap_federateSaveNotComplete)

requestFederationRestore :: RTIAmbassador fedAmb -> String -> IO ()
requestFederationRestore rtiAmb label =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withCString label $ \label ->
            wrapExceptions (wrap_requestFederationRestore rtiAmb label)

federateRestoreComplete :: RTIAmbassador fedAmb -> IO ()
federateRestoreComplete rtiAmb = withRTIAmbassador rtiAmb
    (wrapExceptions . wrap_federateRestoreComplete)

federateRestoreNotComplete :: RTIAmbassador fedAmb -> IO ()
federateRestoreNotComplete rtiAmb = withRTIAmbassador rtiAmb
    (wrapExceptions . wrap_federateRestoreNotComplete)

-------------------------------------
-- Declaration Management Services --
-------------------------------------

publishObjectClass :: RTIAmbassador fedAmb -> ObjectClassHandle -> AttributeHandleSet -> IO ()
publishObjectClass rtiAmb theClass attributeList =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet attributeList $ \attributeList ->
            wrapExceptions (wrap_publishObjectClass rtiAmb theClass attributeList)

unpublishObjectClass :: RTIAmbassador fedAmb -> ObjectClassHandle -> IO ()
unpublishObjectClass rtiAmb theClass =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (wrap_unpublishObjectClass rtiAmb theClass)

publishInteractionClass :: RTIAmbassador fedAmb -> InteractionClassHandle -> IO ()
publishInteractionClass rtiAmb theInteraction =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (wrap_publishInteractionClass rtiAmb theInteraction)

unpublishInteractionClass :: RTIAmbassador fedAmb -> InteractionClassHandle -> IO ()
unpublishInteractionClass rtiAmb theInteraction =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (wrap_unpublishInteractionClass rtiAmb theInteraction)

subscribeObjectClassAttributes :: RTIAmbassador fedAmb -> ObjectClassHandle -> AttributeHandleSet -> IO ()
subscribeObjectClassAttributes rtiAmb theClass attributeList =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet attributeList $ \attributeList ->
            wrapExceptions (wrap_subscribeObjectClassAttributes rtiAmb theClass attributeList)

unsubscribeObjectClass :: RTIAmbassador fedAmb -> ObjectClassHandle -> IO ()
unsubscribeObjectClass rtiAmb theClass =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (wrap_unsubscribeObjectClass rtiAmb theClass)

subscribeInteractionClass :: RTIAmbassador fedAmb -> InteractionClassHandle -> Bool -> IO ()
subscribeInteractionClass rtiAmb theClass active =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (wrap_subscribeInteractionClass rtiAmb theClass active)

unsubscribeInteractionClass :: RTIAmbassador fedAmb -> InteractionClassHandle -> IO ()
unsubscribeInteractionClass rtiAmb theClass =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (wrap_unsubscribeInteractionClass rtiAmb theClass)

--------------------------------
-- Object Management Services --
--------------------------------

registerObjectInstance :: RTIAmbassador fedAmb -> ObjectClassHandle -> Maybe String -> IO ObjectHandle
registerObjectInstance rtiAmb theClass mbObject =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        case mbObject of
            Just theObject  ->
                withCString theObject $ \theObject ->
                    wrapExceptions (wrap_registerObjectInstance_withName rtiAmb theClass theObject)
            Nothing         ->
                wrapExceptions (wrap_registerObjectInstance rtiAmb theClass)


updateAttributeValuesAtTime :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandleValuePairSet -> FedTime fedAmb -> String -> IO EventRetractionHandle
updateAttributeValuesAtTime rtiAmb theObject theAttributes theTime theTag =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withAttributeHandleValuePairSet theAttributes $ \theAttributes ->
            withFedTime_ theTime $ \theTime ->
                withCString theTag $ \theTag ->
                    withEventRetractionHandleReturn $ \u fh ->
                        wrapExceptions (wrap_updateAttributeValuesAtTime rtiAmb theObject theAttributes theTime theTag u fh)

updateAttributeValues :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandleValuePairSet -> String -> IO ()
updateAttributeValues rtiAmb theObject theAttributes theTag =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withAttributeHandleValuePairSet theAttributes $ \theAttributes ->
            withCString theTag $ \theTag ->
                wrapExceptions (wrap_updateAttributeValues rtiAmb theObject theAttributes theTag)

sendInteractionAtTime :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> InteractionClassHandle -> ParameterHandleValuePairSet -> FedTime fedAmb -> String -> IO EventRetractionHandle
sendInteractionAtTime rtiAmb theInteraction theParameters theTime theTag = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withParameterHandleValuePairSet theParameters $ \theParameters ->
            withFedTime_ theTime $ \ theTime ->
                withCString theTag $ \theTag -> 
                    withEventRetractionHandleReturn $ \u fh ->
                        wrapExceptions (wrap_sendInteractionAtTime rtiAmb theInteraction theParameters theTime theTag u fh)

sendInteraction :: RTIAmbassador fedAmb -> InteractionClassHandle -> ParameterHandleValuePairSet -> String -> IO ()
sendInteraction rtiAmb theInteraction theParameters theTag = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withParameterHandleValuePairSet theParameters $ \theParameters ->
            withCString theTag $ \theTag -> 
                wrapExceptions (wrap_sendInteraction rtiAmb theInteraction theParameters theTag)

deleteObjectInstanceAtTime :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> ObjectHandle -> FedTime fedAmb -> String -> IO EventRetractionHandle
deleteObjectInstanceAtTime rtiAmb theObject theTime theTag = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withFedTime_ theTime $ \ theTime ->
            withCString theTag $ \theTag -> 
                withEventRetractionHandleReturn $ \u fh ->
                    wrapExceptions (wrap_deleteObjectInstanceAtTime rtiAmb theObject theTime theTag u fh)

deleteObjectInstance :: RTIAmbassador fedAmb -> ObjectHandle -> String -> IO ()
deleteObjectInstance rtiAmb theObject theTag = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withCString theTag $ \theTag -> 
            wrapExceptions (wrap_deleteObjectInstance rtiAmb theObject theTag)

localDeleteObjectInstance :: RTIAmbassador fedAmb -> ObjectHandle -> IO ()
localDeleteObjectInstance rtiAmb theObject = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (wrap_localDeleteObjectInstance rtiAmb theObject)

changeAttributeTransportationType :: RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandleSet -> TransportationHandle -> IO ()
changeAttributeTransportationType rtiAmb theObject theAttributes theType =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (wrap_changeAttributeTransportationType rtiAmb theObject theAttributes theType)

changeInteractionTransportationType :: RTIAmbassador fedAmb -> InteractionClassHandle -> TransportationHandle -> IO ()
changeInteractionTransportationType rtiAmb theClass theType =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (wrap_changeInteractionTransportationType rtiAmb theClass theType)

requestObjectAttributeValueUpdate :: RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandleSet -> IO ()
requestObjectAttributeValueUpdate rtiAmb theObject theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (wrap_requestObjectAttributeValueUpdate rtiAmb theObject theAttributes)

requestClassAttributeValueUpdate :: RTIAmbassador fedAmb -> ObjectClassHandle -> AttributeHandleSet -> IO ()
requestClassAttributeValueUpdate rtiAmb theClass theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (wrap_requestClassAttributeValueUpdate rtiAmb theClass theAttributes)

-----------------------------------
-- Ownership Management Services --
-----------------------------------

unconditionalAttributeOwnershipDivestiture :: RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandleSet -> IO ()
unconditionalAttributeOwnershipDivestiture rtiAmb theObject theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (wrap_unconditionalAttributeOwnershipDivestiture rtiAmb theObject theAttributes)

    -- // 7.3
    -- void negotiatedAttributeOwnershipDivestiture (
    --         ObjectHandle                  theObject,     // supplied C1
    --   const AttributeHandleSet&           theAttributes, // supplied C4
    --   const char                         *theTag)        // supplied C4
    -- throw (
    --   ObjectNotKnown,
    --   AttributeNotDefined,
    --   AttributeNotOwned,
    --   AttributeAlreadyBeingDivested,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 7.7
    -- void attributeOwnershipAcquisition (
    --         ObjectHandle        theObject,         // supplied C1
    --   const AttributeHandleSet& desiredAttributes, // supplied C4
    --   const char               *theTag)            // supplied C4
    -- throw (
    --   ObjectNotKnown,
    --   ObjectClassNotPublished,
    --   AttributeNotDefined,
    --   AttributeNotPublished,
    --   FederateOwnsAttributes,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 7.8
    -- void attributeOwnershipAcquisitionIfAvailable (
    --         ObjectHandle        theObject,         // supplied C1
    --   const AttributeHandleSet& desiredAttributes) // supplied C4
    -- throw (
    --   ObjectNotKnown,
    --   ObjectClassNotPublished,
    --   AttributeNotDefined,
    --   AttributeNotPublished,
    --   FederateOwnsAttributes,
    --   AttributeAlreadyBeingAcquired,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 7.11
    -- AttributeHandleSet*                        // returned C6
    -- attributeOwnershipReleaseResponse (
    --         ObjectHandle        theObject,     // supplied C1
    --   const AttributeHandleSet& theAttributes) // supplied C4
    -- throw (
    --   ObjectNotKnown,
    --   AttributeNotDefined,
    --   AttributeNotOwned,
    --   FederateWasNotAskedToReleaseAttribute,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 7.12
    -- void cancelNegotiatedAttributeOwnershipDivestiture (
    --         ObjectHandle        theObject,     // supplied C1
    --   const AttributeHandleSet& theAttributes) // supplied C4
    -- throw (
    --   ObjectNotKnown,
    --   AttributeNotDefined,
    --   AttributeNotOwned,
    --   AttributeDivestitureWasNotRequested,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 7.13
    -- void cancelAttributeOwnershipAcquisition (
    --         ObjectHandle        theObject,     // supplied C1
    --   const AttributeHandleSet& theAttributes) // supplied C4
    -- throw (
    --   ObjectNotKnown,
    --   AttributeNotDefined,
    --   AttributeAlreadyOwned,
    --   AttributeAcquisitionWasNotRequested,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 7.15
    -- void queryAttributeOwnership (
    --   ObjectHandle    theObject,    // supplied C1
    --   AttributeHandle theAttribute) // supplied C1
    -- throw (
    --   ObjectNotKnown,
    --   AttributeNotDefined,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 7.17
    -- Boolean                          // returned C3
    -- isAttributeOwnedByFederate (
    --   ObjectHandle    theObject,     // supplied C1
    --   AttributeHandle theAttribute)  // supplied C1
    -- throw (
    --   ObjectNotKnown,
    --   AttributeNotDefined,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);

------------------------------
-- Time Management Services --
------------------------------

withTimeRegulation :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> FedTime fedAmb -> FedTime fedAmb -> IO a -> IO a
withTimeRegulation rtiAmb theFederateTime theLookahead regulatedAction =
    withFedTime_ theFederateTime $ \theFederateTime -> do
        withFedTime_ theLookahead $ \theLookahead -> do
            bracket_
                (enableTimeRegulation  rtiAmb theFederateTime theLookahead)
                (disableTimeRegulation rtiAmb)
                regulatedAction

    where
        enableTimeRegulation rtiAmb theFederateTime theLookahead =
            withRTIAmbassador rtiAmb $ \rtiAmb ->
                wrapExceptions (wrap_enableTimeRegulation rtiAmb theFederateTime theLookahead)

        disableTimeRegulation rtiAmb =
            withRTIAmbassador rtiAmb $ \rtiAmb ->
                wrapExceptions (wrap_disableTimeRegulation rtiAmb)


enableTimeConstrained :: RTIAmbassador fedAmb -> IO ()
enableTimeConstrained rtiAmb =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (wrap_enableTimeConstrained rtiAmb)

disableTimeConstrained :: RTIAmbassador fedAmb -> IO ()
disableTimeConstrained rtiAmb =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (wrap_disableTimeConstrained rtiAmb)

timeAdvanceRequest :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> FedTime fedAmb -> IO ()
timeAdvanceRequest rtiAmb theTime = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withFedTime_ theTime $ \theTime -> 
            wrapExceptions (wrap_timeAdvanceRequest rtiAmb theTime)


    -- void timeAdvanceRequestAvailable (
    -- const FedTime& theTime) // supplied C4
    --   throw (
    --     InvalidFederationTime,
    --     FederationTimeAlreadyPassed,
    --     TimeAdvanceAlreadyInProgress,
    --     EnableTimeRegulationPending,
    --     EnableTimeConstrainedPending,
    --     FederateNotExecutionMember,
    --     ConcurrentAccessAttempted,
    --     SaveInProgress,
    --     RestoreInProgress,
    --     RTIinternalError);
    -- 
    -- // 8.10
    -- void nextEventRequest (
    --   const FedTime& theTime) // supplied C4
    -- throw (
    --   InvalidFederationTime,
    --   FederationTimeAlreadyPassed,
    --   TimeAdvanceAlreadyInProgress,
    --   EnableTimeRegulationPending,
    --   EnableTimeConstrainedPending,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 8.11
    -- void nextEventRequestAvailable (
    --   const FedTime& theTime) // supplied C4
    -- throw (
    --   InvalidFederationTime,
    --   FederationTimeAlreadyPassed,
    --   TimeAdvanceAlreadyInProgress,
    --   EnableTimeRegulationPending,
    --   EnableTimeConstrainedPending,  
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 8.12
    -- void flushQueueRequest (
    --   const FedTime& theTime) // supplied C4
    -- throw (
    --   InvalidFederationTime,
    --   FederationTimeAlreadyPassed,
    --   TimeAdvanceAlreadyInProgress,
    --   EnableTimeRegulationPending,
    --   EnableTimeConstrainedPending,  
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 8.14
    -- void enableAsynchronousDelivery()
    --   throw (
    --     AsynchronousDeliveryAlreadyEnabled,
    --     FederateNotExecutionMember,
    --     ConcurrentAccessAttempted,
    --     SaveInProgress,
    --     RestoreInProgress,
    --     RTIinternalError);
    -- 
    -- // 8.15
    -- void disableAsynchronousDelivery()
    --   throw (
    --     AsynchronousDeliveryAlreadyDisabled,
    --     FederateNotExecutionMember,
    --     ConcurrentAccessAttempted,
    --     SaveInProgress,
    --     RestoreInProgress,
    --     RTIinternalError);
    -- 
    -- // 8.16
    -- void queryLBTS (
    --   FedTime& theTime) // returned C5
    -- throw (
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 8.17
    -- void queryFederateTime (
    --   FedTime& theTime) // returned C5
    -- throw (
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 8.18
    -- void queryMinNextEventTime (
    --   FedTime& theTime) // returned C5
    -- throw (
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 8.19
    -- void modifyLookahead (
    --   const FedTime& theLookahead) // supplied C4
    -- throw (
    --   InvalidLookahead,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
queryLookahead :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> IO (FedTime fedAmb)
queryLookahead rtiAmb =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withArbitraryFedTime $ \fedTime -> 
            wrapExceptions (wrap_queryLookahead rtiAmb fedTime)
    
    where 
        wrap_queryLookahead :: Ptr (RTIAmbassador fedAmb) -> Ptr (FedAmbTime fedAmb) -> Ptr (Ptr RTIException) -> IO ()
        wrap_queryLookahead = error "wrap_queryLookahead"

    -- // 8.21
    -- void retract (
    --   EventRetractionHandle theHandle) // supplied C1
    -- throw (
    --   InvalidRetractionHandle,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 8.23
    -- void changeAttributeOrderType (
    --         ObjectHandle        theObject,     // supplied C1
    --   const AttributeHandleSet& theAttributes, // supplied C4
    --         OrderingHandle      theType)       // supplied C1
    -- throw (
    --   ObjectNotKnown,
    --   AttributeNotDefined,
    --   AttributeNotOwned,
    --   InvalidOrderingHandle,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 8.24
    -- void changeInteractionOrderType (
    --   InteractionClassHandle theClass, // supplied C1
    --   OrderingHandle         theType)  // supplied C1
    -- throw (
    --   InteractionClassNotDefined,
    --   InteractionClassNotPublished,
    --   InvalidOrderingHandle,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);

----------------------------------
-- Data Distribution Management --
----------------------------------

-- |TODO - think about this... can it ever call the finalizer for RTIAmbassador
-- before the one for Region?  Probably.  Is that a problem?  Probably...
createRegion :: RTIAmbassador fedAmb -> SpaceHandle -> ULong -> IO Region
createRegion rtiAmb theSpace numberOfExtents =
    withRTIAmbassador rtiAmb $ \rtiAmb -> do
        r <- wrapExceptions (wrap_createRegion rtiAmb theSpace numberOfExtents)
        r <- newForeignPtr r (deleteRegion r)
        return (Region r)
    where 
        deleteRegion theRegion =
            withRTIAmbassador rtiAmb $ \rtiAmb ->
                wrapExceptions (wrap_deleteRegion rtiAmb theRegion)

notifyAboutRegionModification :: RTIAmbassador fedAmb -> Region -> IO ()
notifyAboutRegionModification rtiAmb theRegion =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            wrapExceptions (wrap_notifyAboutRegionModification rtiAmb theRegion)

-- |Should not normally be run by user; region will be properly deleted
-- when garbage collected (?)
deleteRegion :: RTIAmbassador fedAmb -> Region -> IO ()
deleteRegion rtiAmb (Region theRegion) = finalizeForeignPtr theRegion


registerObjectInstanceWithRegion :: RTIAmbassador t -> ObjectClassHandle -> String -> [(AttributeHandle, Region)] -> IO ObjectHandle
registerObjectInstanceWithRegion rtiAmb theClass theObject theHandles = do
    let (theAttributes, theRegions) = unzip theHandles
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withCString theObject $ \theObject -> 
            withMany withRegion theRegions $ \theRegions ->
                withArray theRegions $ \theRegions -> 
                    withArrayLen theAttributes $ \theNumberOfHandles theAttributes ->
                        wrapExceptions (wrap_registerObjectInstanceWithRegion rtiAmb theClass theObject theAttributes theRegions (fromIntegral theNumberOfHandles))

    -- ObjectHandle                                  // returned C3
    -- registerObjectInstanceWithRegion (
    --         ObjectClassHandle theClass,           // supplied C1
    --   const char             *theObject,          // supplied C4
    --         AttributeHandle   theAttributes[],    // supplied C4
    --         Region           *theRegions[],       // supplied C4
    --         ULong             theNumberOfHandles) // supplied C1
    -- throw (
    --   ObjectClassNotDefined,
    --   ObjectClassNotPublished,
    --   AttributeNotDefined,
    --   AttributeNotPublished,
    --   RegionNotKnown,
    --   InvalidRegionContext,
    --   ObjectAlreadyRegistered,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- ObjectHandle                              // returned C3
    -- registerObjectInstanceWithRegion (
    --   ObjectClassHandle theClass,             // supplied C1
    --   AttributeHandle   theAttributes[],      // supplied C4
    --   Region           *theRegions[],         // supplied C4
    --   ULong             theNumberOfHandles)   // supplied C1
    -- throw (
    --   ObjectClassNotDefined,
    --   ObjectClassNotPublished,
    --   AttributeNotDefined,
    --   AttributeNotPublished,
    --   RegionNotKnown,
    --   InvalidRegionContext,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 9.6
    -- void associateRegionForUpdates (
    --         Region             &theRegion,     // supplied C4
    --         ObjectHandle        theObject,     // supplied C1
    --   const AttributeHandleSet &theAttributes) // supplied C4
    -- throw (
    --   ObjectNotKnown,
    --   AttributeNotDefined,
    --   InvalidRegionContext,
    --   RegionNotKnown,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 9.7
    -- void unassociateRegionForUpdates (
    --   Region       &theRegion,     // supplied C4
    --   ObjectHandle  theObject)     // supplied C1
    -- throw (
    --   ObjectNotKnown,
    --   InvalidRegionContext,
    --   RegionNotKnown,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 9.8
    -- void subscribeObjectClassAttributesWithRegion (
    --         ObjectClassHandle   theClass,      // supplied C1
    --         Region             &theRegion,     // supplied C4
    --   const AttributeHandleSet &attributeList, // supplied C4
    --         Boolean        active = RTI_TRUE)
    -- throw (
    --   ObjectClassNotDefined,
    --   AttributeNotDefined,
    --   RegionNotKnown,
    --   InvalidRegionContext,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 9.9
    -- void unsubscribeObjectClassWithRegion (
    --   ObjectClassHandle theClass,          // supplied C1
    --   Region           &theRegion)         // supplied C4
    -- throw (
    --   ObjectClassNotDefined,
    --   RegionNotKnown,
    --   ObjectClassNotSubscribed,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
subscribeInteractionClassWithRegion :: RTIAmbassador fedAmb -> InteractionClassHandle -> Region -> Bool -> IO ()
subscribeInteractionClassWithRegion rtiAmb theClass theRegion active = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            wrapExceptions (wrap_subscribeInteractionClassWithRegion rtiAmb theClass theRegion active)


unsubscribeInteractionClassWithRegion :: RTIAmbassador fedAmb -> InteractionClassHandle -> Region -> IO ()
unsubscribeInteractionClassWithRegion rtiAmb theClass theRegion = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            wrapExceptions (wrap_unsubscribeInteractionClassWithRegion rtiAmb theClass theRegion)


    -- // 9.12
    -- EventRetractionHandle                                // returned C3
    -- sendInteractionWithRegion (
    --         InteractionClassHandle       theInteraction, // supplied C1
    --   const ParameterHandleValuePairSet &theParameters,  // supplied C4
    --   const FedTime&                     theTime,        // supplied C4
    --   const char                        *theTag,         // supplied C4
    --   const Region                      &theRegion)      // supplied C4
    -- throw (
    --   InteractionClassNotDefined,
    --   InteractionClassNotPublished,
    --   InteractionParameterNotDefined,
    --   InvalidFederationTime,
    --   RegionNotKnown,
    --   InvalidRegionContext,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- void sendInteractionWithRegion (
    --         InteractionClassHandle       theInteraction, // supplied C1
    --   const ParameterHandleValuePairSet &theParameters,  // supplied C4
    --   const char                        *theTag,         // supplied C4
    --   const Region                      &theRegion)      // supplied C4
    -- throw (
    --   InteractionClassNotDefined,
    --   InteractionClassNotPublished,
    --   InteractionParameterNotDefined,
    --   RegionNotKnown,
    --   InvalidRegionContext,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 9.13
    -- void requestClassAttributeValueUpdateWithRegion (
    --         ObjectClassHandle   theClass,      // supplied C1
    --   const AttributeHandleSet &theAttributes, // supplied C4
    --   const Region             &theRegion)     // supplied C4
    -- throw (
    --   ObjectClassNotDefined, 
    --   AttributeNotDefined,
    --   RegionNotKnown,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);

--------------------------
-- RTI Support Services --
--------------------------

getObjectClassHandle :: RTIAmbassador fedAmb -> String -> IO ObjectClassHandle
getObjectClassHandle rtiAmb theName = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withCString theName $ \theName ->
            wrapExceptions (wrap_getObjectClassHandle rtiAmb theName)
    -- 
    -- // 10.3
    -- char *                         // returned C6    
    -- getObjectClassName (
    --   ObjectClassHandle theHandle) // supplied C1
    -- throw (
    --   ObjectClassNotDefined,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   RTIinternalError);

getAttributeHandle :: RTIAmbassador fedAmb -> String -> ObjectClassHandle -> IO AttributeHandle
getAttributeHandle rtiAmb theName whichClass = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withCString theName $ \theName -> 
            wrapExceptions (wrap_getAttributeHandle rtiAmb theName whichClass)

    -- // 10.5
    -- char *                          // returned C6 
    -- getAttributeName (
    --   AttributeHandle   theHandle,  // supplied C1
    --   ObjectClassHandle whichClass) // supplied C1
    -- throw (
    --   ObjectClassNotDefined,
    --   AttributeNotDefined,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   RTIinternalError);

getInteractionClassHandle :: RTIAmbassador fedAmb -> String -> IO InteractionClassHandle
getInteractionClassHandle rtiAmb theName = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withCString theName $ \theName ->
            wrapExceptions (wrap_getInteractionClassHandle rtiAmb theName)

    -- // 10.7
    -- char *                              // returned C6 
    -- getInteractionClassName (
    --   InteractionClassHandle theHandle) // supplied C1
    -- throw (
    --   InteractionClassNotDefined,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   RTIinternalError);

getParameterHandle :: RTIAmbassador fedAmb -> String -> InteractionClassHandle -> IO ParameterHandle
getParameterHandle rtiAmb theName whichClass =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withCString theName $ \theName ->
            wrapExceptions (wrap_getParameterHandle rtiAmb theName whichClass)

    -- // 10.9
    -- char *                               // returned C6
    -- getParameterName (
    --   ParameterHandle        theHandle,  // supplied C1
    --   InteractionClassHandle whichClass) // supplied C1
    -- throw (
    --   InteractionClassNotDefined,
    --   InteractionParameterNotDefined,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   RTIinternalError);
    -- 
    -- // 10.10
    -- ObjectHandle                 // returned C3
    -- getObjectInstanceHandle (
    --   const char *theName)       // supplied C4
    -- throw (
    --     ObjectNotKnown,
    --     FederateNotExecutionMember,
    --     ConcurrentAccessAttempted,
    --     RTIinternalError);
    -- 
    -- // 10.11
    -- char *                     // returned C6  
    -- getObjectInstanceName (
    --   ObjectHandle theHandle)  // supplied C1
    -- throw (
    --   ObjectNotKnown,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   RTIinternalError);
    -- 
getRoutingSpaceHandle :: RTIAmbassador fedAmb -> String -> IO SpaceHandle
getRoutingSpaceHandle rtiAmb theName =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withCString theName $ \theName ->
            wrapExceptions (wrap_getRoutingSpaceHandle rtiAmb theName)

    -- // 10.13
    -- char *                         // returned C6
    -- getRoutingSpaceName (
    --    //
    --    // This const was removed for the RTI 1.3 NG to work around a limitation of
    --    // the Sun 4.2 C++ compiler regarding template instantiation.  The const
    --    // is unnecessary.
    --    //
    --    /* const */ SpaceHandle theHandle) // supplied C4
    -- throw (
    --   SpaceNotDefined,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   RTIinternalError);

getDimensionHandle :: RTIAmbassador fedAmb -> String -> SpaceHandle -> IO DimensionHandle
getDimensionHandle rtiAmb theName whichSpace =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withCString theName $ \theName ->
            wrapExceptions (wrap_getDimensionHandle rtiAmb theName whichSpace)

    -- // 10.15
    -- char *                        // returned C6
    -- getDimensionName (
    --   DimensionHandle theHandle,  // supplied C1
    --   SpaceHandle     whichSpace) // supplied C1
    -- throw (
    --   SpaceNotDefined,
    --   DimensionNotDefined,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   RTIinternalError);
    -- 
    -- // 10.16
    -- SpaceHandle                      // returned C3
    -- getAttributeRoutingSpaceHandle (
    --   AttributeHandle   theHandle,   // supplied C1
    --   ObjectClassHandle whichClass)  // supplied C1
    -- throw (
    --   ObjectClassNotDefined,
    --   AttributeNotDefined,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   RTIinternalError);
    -- 
    -- // 10.17
    -- ObjectClassHandle            // returned C3
    -- getObjectClass (
    --   ObjectHandle theObject)    // supplied C1
    -- throw (
    --   ObjectNotKnown,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   RTIinternalError);
    -- 
    -- // 10.18
    -- SpaceHandle                             // returned C3
    -- getInteractionRoutingSpaceHandle (
    --   InteractionClassHandle   theHandle)   // supplied C1
    -- throw (
    --   InteractionClassNotDefined,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   RTIinternalError);
    -- 
    -- // 10.19
    -- TransportationHandle      // returned C3
    -- getTransportationHandle (
    --   const char *theName)    // supplied C4
    -- throw (
    --   NameNotFound,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   RTIinternalError);
    -- 
    -- // 10.20
    -- char *                            // returned C6 
    -- getTransportationName (
    --   TransportationHandle theHandle) // supplied C1
    -- throw (
    --   InvalidTransportationHandle,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   RTIinternalError);
    -- 
    -- // 10.21
    -- OrderingHandle         // returned C3
    -- getOrderingHandle (
    --   const char *theName) // supplied C4
    -- throw (
    --   NameNotFound,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   RTIinternalError);
    -- 
    -- // 10.22
    -- char *                      // returned C6 
    -- getOrderingName (
    --   OrderingHandle theHandle) // supplied C1
    -- throw (
    --   InvalidOrderingHandle,
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   RTIinternalError);
    -- 
    -- // 10.23
    -- void enableClassRelevanceAdvisorySwitch()
    -- throw(
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 10.24
    -- void disableClassRelevanceAdvisorySwitch()
    -- throw(
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);

enableAttributeRelevanceAdvisorySwitch :: RTIAmbassador fedAmb -> IO ()
enableAttributeRelevanceAdvisorySwitch rtiAmb = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (wrap_enableAttributeRelevanceAdvisorySwitch rtiAmb)
    
    -- 
    -- // 10.26
    -- void disableAttributeRelevanceAdvisorySwitch()
    -- throw(
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 10.27
    -- void enableAttributeScopeAdvisorySwitch()
    -- throw(
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 10.28
    -- void disableAttributeScopeAdvisorySwitch()
    -- throw(
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 10.29
    -- void enableInteractionRelevanceAdvisorySwitch()
    -- throw(
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);
    -- 
    -- // 10.30
    -- void disableInteractionRelevanceAdvisorySwitch()
    -- throw(
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   SaveInProgress,
    --   RestoreInProgress,
    --   RTIinternalError);

    -- Boolean // returned C3
-- tick :: 
    -- tick ()
    -- throw (
    --   SpecifiedSaveLabelDoesNotExist,
    --   ConcurrentAccessAttempted,
    --   RTIinternalError);

tick_minimum_maximum :: RTIAmbassador fedAmb -> TickTime -> TickTime -> IO Bool
tick_minimum_maximum rtiAmb min max = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (wrap_tick_minimum_maximum rtiAmb min max)

    -- RTIambassador()
    -- throw (
    --   MemoryExhausted,
    --   RTIinternalError);
    -- 
    -- ~RTIambassador()
    -- throw (RTIinternalError);
    -- 
    -- RegionToken
    -- getRegionToken(
    --   Region *)
    -- throw(
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   RegionNotKnown,
    --   RTIinternalError);
    -- 
    -- Region *
    -- getRegion(
    --   RegionToken)
    -- throw(
    --   FederateNotExecutionMember,
    --   ConcurrentAccessAttempted,
    --   RegionNotKnown,
    --   RTIinternalError);



