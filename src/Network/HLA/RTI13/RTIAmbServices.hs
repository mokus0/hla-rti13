module Network.HLA.RTI13.RTIAmbServices
    ( RTIAmbassador
    , module Network.HLA.RTI13.RTIAmbServices
    ) where

import Network.HLA.RTI13.RTIAmbServices.FFI (RTIAmbassador(..), withRTIAmbassador)
import qualified Network.HLA.RTI13.RTIAmbServices.FFI as FFI

import Network.HLA.RTI13.BaseTypes
import Network.HLA.RTI13.OddsAndEnds
import Network.HLA.RTI13.RTITypes
import Network.HLA.RTI13.RTIException

import Control.Exception (bracket_)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.List
import Data.StateRef
import Foreign hiding (newForeignPtr)
import Foreign.Concurrent
import System.Mem

-- |Create a new 'RTIAmbassador', which is the object that manages the 
-- connection to the RTI.  There should be exactly one 'RTIAmbassador'
-- and one 'FederateAmbassador' per federate.  Some RTIs only support one
-- federate per application instance.
newRTIAmbassador :: IO (RTIAmbassador fedAmb)
newRTIAmbassador = do
    rtiAmb <- FFI.new_RTIambassador
    rtiAmb <- newForeignPtr rtiAmb (FFI.delete_RTIambassador rtiAmb)
    fedAmb <- newReference Nothing
    return (RTIAmbassador rtiAmb fedAmb)

--------------------------------------
-- * Federation Management Services
--------------------------------------

-- |Attempt to create a federation on the RTI.  If the federation already exists,
-- a 'FederationExecutionAlreadyExists' exception will be thrown.
-- 
-- Usage:
-- 
-- > createFederationExecution fedAmb executionName fedFile
-- 
createFederationExecution :: RTIAmbassador fedAmb -> ByteString -> ByteString -> IO ()
createFederationExecution rtiAmb executionName fed = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        unsafeUseAsCString executionName $ \executionName ->
            unsafeUseAsCString fed $ \fed ->
                wrapExceptions (FFI.createFederationExecution rtiAmb executionName fed)

-- |Attempt to halt a federation on the RTI.  If the federation does not exist,
-- a 'FederationExecutionDoesNotExist' exception will be thrown.
-- 
-- Usage:
-- 
-- > destroyFederationExecution fedAmb executionName
-- 
destroyFederationExecution :: RTIAmbassador fedAmb -> ByteString -> IO ()
destroyFederationExecution rtiAmb executionName = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        unsafeUseAsCString executionName $ \executionName ->
            wrapExceptions (FFI.destroyFederationExecution rtiAmb executionName)

-- |Attempt to join a federation execution.
-- 
-- Usage:
-- 
-- > joinFederationExecution rtiAmb federateName executionName fedAmb
joinFederationExecution :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> ByteString -> ByteString -> fedAmb -> IO FederateHandle
joinFederationExecution rtiAmb yourName executionName fedAmb = do
    fedHandle <- withRTIAmbassador rtiAmb $ \rtiAmb -> 
        unsafeUseAsCString yourName $ \yourName ->
            unsafeUseAsCString executionName $ \executionName ->
                withFederateAmbassador fedAmb $ \fedAmb ->
                    wrapExceptions (FFI.joinFederationExecution rtiAmb yourName executionName fedAmb)
    writeReference (rtiFedAmb rtiAmb) (Just fedAmb)
    return fedHandle

-- |Attempt to resign from a federation execution.  The 'ResignAction' 
-- parameter describes how attributes owned by the federate will be disposed of.
resignFederationExecution :: RTIAmbassador fedAmb -> ResignAction -> IO ()
resignFederationExecution rtiAmb resignAction = do
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        wrapExceptions (FFI.resignFederationExecution rtiAmb (fromIntegral (fromEnum resignAction)))
    writeReference (rtiFedAmb rtiAmb) Nothing
    performGC

registerFederationSynchronizationPoint :: RTIAmbassador fedAmb -> ByteString -> ByteString -> Maybe FederateHandleSet -> IO ()
registerFederationSynchronizationPoint rtiAmb label theTag mbSyncSet = do
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        unsafeUseAsCString label $ \label ->
            unsafeUseAsCString theTag $ \theTag -> case mbSyncSet of
                    Nothing ->
                        wrapExceptions (FFI.registerFederationSynchronizationPoint rtiAmb label theTag)
                    Just syncSet -> withFederateHandleSet syncSet $ \syncSet ->
                        wrapExceptions (FFI.registerFederationSynchronizationPoint_with_syncSet rtiAmb label theTag syncSet)

synchronizationPointAchieved :: RTIAmbassador fedAmb -> ByteString -> IO ()
synchronizationPointAchieved rtiAmb label =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        unsafeUseAsCString label $ \label ->
            wrapExceptions (FFI.synchronizationPointAchieved rtiAmb label)

requestFederationSave :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> ByteString -> Maybe (FedTime fedAmb) -> IO ()
requestFederationSave rtiAmb label mbTime =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        unsafeUseAsCString label $ \label -> case mbTime of
            Nothing ->
                wrapExceptions (FFI.requestFederationSave rtiAmb label)
            Just theTime -> 
                withFedTime_ theTime $ \theTime ->
                    wrapExceptions (FFI.requestFederationSaveAtTime rtiAmb label theTime)

federateSaveBegun :: RTIAmbassador fedAmb -> IO ()
federateSaveBegun rtiAmb = withRTIAmbassador rtiAmb
    (wrapExceptions . FFI.federateSaveBegun)

federateSaveComplete :: RTIAmbassador fedAmb -> IO ()
federateSaveComplete rtiAmb = withRTIAmbassador rtiAmb
    (wrapExceptions . FFI.federateSaveComplete)

federateSaveNotComplete :: RTIAmbassador fedAmb -> IO ()
federateSaveNotComplete rtiAmb = withRTIAmbassador rtiAmb
    (wrapExceptions . FFI.federateSaveNotComplete)

requestFederationRestore :: RTIAmbassador fedAmb -> ByteString -> IO ()
requestFederationRestore rtiAmb label =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        unsafeUseAsCString label $ \label ->
            wrapExceptions (FFI.requestFederationRestore rtiAmb label)

federateRestoreComplete :: RTIAmbassador fedAmb -> IO ()
federateRestoreComplete rtiAmb = withRTIAmbassador rtiAmb
    (wrapExceptions . FFI.federateRestoreComplete)

federateRestoreNotComplete :: RTIAmbassador fedAmb -> IO ()
federateRestoreNotComplete rtiAmb = withRTIAmbassador rtiAmb
    (wrapExceptions . FFI.federateRestoreNotComplete)

--------------------------------------
-- * Declaration Management Services
--------------------------------------

publishObjectClass :: RTIAmbassador fedAmb -> ObjectClassHandle -> AttributeHandleSet -> IO ()
publishObjectClass rtiAmb theClass attributeList =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet attributeList $ \attributeList ->
            wrapExceptions (FFI.publishObjectClass rtiAmb theClass attributeList)

unpublishObjectClass :: RTIAmbassador fedAmb -> ObjectClassHandle -> IO ()
unpublishObjectClass rtiAmb theClass =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.unpublishObjectClass rtiAmb theClass)

publishInteractionClass :: RTIAmbassador fedAmb -> InteractionClassHandle -> IO ()
publishInteractionClass rtiAmb theInteraction =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.publishInteractionClass rtiAmb theInteraction)

unpublishInteractionClass :: RTIAmbassador fedAmb -> InteractionClassHandle -> IO ()
unpublishInteractionClass rtiAmb theInteraction =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.unpublishInteractionClass rtiAmb theInteraction)

subscribeObjectClassAttributes :: RTIAmbassador fedAmb -> ObjectClassHandle -> AttributeHandleSet -> IO ()
subscribeObjectClassAttributes rtiAmb theClass attributeList =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet attributeList $ \attributeList ->
            wrapExceptions (FFI.subscribeObjectClassAttributes rtiAmb theClass attributeList)

unsubscribeObjectClass :: RTIAmbassador fedAmb -> ObjectClassHandle -> IO ()
unsubscribeObjectClass rtiAmb theClass =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.unsubscribeObjectClass rtiAmb theClass)

subscribeInteractionClass :: RTIAmbassador fedAmb -> InteractionClassHandle -> Bool -> IO ()
subscribeInteractionClass rtiAmb theClass active =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.subscribeInteractionClass rtiAmb theClass active)

unsubscribeInteractionClass :: RTIAmbassador fedAmb -> InteractionClassHandle -> IO ()
unsubscribeInteractionClass rtiAmb theClass =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.unsubscribeInteractionClass rtiAmb theClass)

---------------------------------
-- * Object Management Services
---------------------------------

registerObjectInstance :: RTIAmbassador fedAmb -> ObjectClassHandle -> Maybe ByteString -> IO ObjectHandle
registerObjectInstance rtiAmb theClass mbObject =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        case mbObject of
            Just theObject  ->
                unsafeUseAsCString theObject $ \theObject ->
                    wrapExceptions (FFI.registerObjectInstance_withName rtiAmb theClass theObject)
            Nothing         ->
                wrapExceptions (FFI.registerObjectInstance rtiAmb theClass)


updateAttributeValuesAtTime :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandleValuePairSet -> FedTime fedAmb -> ByteString -> IO EventRetractionHandle
updateAttributeValuesAtTime rtiAmb theObject theAttributes theTime theTag =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withAttributeHandleValuePairSet theAttributes $ \theAttributes ->
            withFedTime_ theTime $ \theTime ->
                unsafeUseAsCString theTag $ \theTag ->
                    withEventRetractionHandleReturn $ \u fh ->
                        wrapExceptions (FFI.updateAttributeValuesAtTime rtiAmb theObject theAttributes theTime theTag u fh)

updateAttributeValues :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandleValuePairSet -> ByteString -> IO ()
updateAttributeValues rtiAmb theObject theAttributes theTag =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withAttributeHandleValuePairSet theAttributes $ \theAttributes ->
            unsafeUseAsCString theTag $ \theTag ->
                wrapExceptions (FFI.updateAttributeValues rtiAmb theObject theAttributes theTag)

sendInteractionAtTime :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> InteractionClassHandle -> ParameterHandleValuePairSet -> FedTime fedAmb -> ByteString -> IO EventRetractionHandle
sendInteractionAtTime rtiAmb theInteraction theParameters theTime theTag = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withParameterHandleValuePairSet theParameters $ \theParameters ->
            withFedTime_ theTime $ \ theTime ->
                unsafeUseAsCString theTag $ \theTag -> 
                    withEventRetractionHandleReturn $ \u fh ->
                        wrapExceptions (FFI.sendInteractionAtTime rtiAmb theInteraction theParameters theTime theTag u fh)

sendInteraction :: RTIAmbassador fedAmb -> InteractionClassHandle -> ParameterHandleValuePairSet -> ByteString -> IO ()
sendInteraction rtiAmb theInteraction theParameters theTag = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withParameterHandleValuePairSet theParameters $ \theParameters ->
            unsafeUseAsCString theTag $ \theTag -> 
                wrapExceptions (FFI.sendInteraction rtiAmb theInteraction theParameters theTag)

deleteObjectInstanceAtTime :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> ObjectHandle -> FedTime fedAmb -> ByteString -> IO EventRetractionHandle
deleteObjectInstanceAtTime rtiAmb theObject theTime theTag = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withFedTime_ theTime $ \ theTime ->
            unsafeUseAsCString theTag $ \theTag -> 
                withEventRetractionHandleReturn $ \u fh ->
                    wrapExceptions (FFI.deleteObjectInstanceAtTime rtiAmb theObject theTime theTag u fh)

deleteObjectInstance :: RTIAmbassador fedAmb -> ObjectHandle -> ByteString -> IO ()
deleteObjectInstance rtiAmb theObject theTag = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        unsafeUseAsCString theTag $ \theTag -> 
            wrapExceptions (FFI.deleteObjectInstance rtiAmb theObject theTag)

localDeleteObjectInstance :: RTIAmbassador fedAmb -> ObjectHandle -> IO ()
localDeleteObjectInstance rtiAmb theObject = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.localDeleteObjectInstance rtiAmb theObject)

changeAttributeTransportationType :: RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandleSet -> TransportationHandle -> IO ()
changeAttributeTransportationType rtiAmb theObject theAttributes theType =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.changeAttributeTransportationType rtiAmb theObject theAttributes theType)

changeInteractionTransportationType :: RTIAmbassador fedAmb -> InteractionClassHandle -> TransportationHandle -> IO ()
changeInteractionTransportationType rtiAmb theClass theType =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.changeInteractionTransportationType rtiAmb theClass theType)

requestObjectAttributeValueUpdate :: RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandleSet -> IO ()
requestObjectAttributeValueUpdate rtiAmb theObject theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.requestObjectAttributeValueUpdate rtiAmb theObject theAttributes)

requestClassAttributeValueUpdate :: RTIAmbassador fedAmb -> ObjectClassHandle -> AttributeHandleSet -> IO ()
requestClassAttributeValueUpdate rtiAmb theClass theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.requestClassAttributeValueUpdate rtiAmb theClass theAttributes)

------------------------------------
-- * Ownership Management Services
------------------------------------

unconditionalAttributeOwnershipDivestiture :: RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandleSet -> IO ()
unconditionalAttributeOwnershipDivestiture rtiAmb theObject theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.unconditionalAttributeOwnershipDivestiture rtiAmb theObject theAttributes)

negotiatedAttributeOwnershipDivestiture :: RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandleSet -> ByteString -> IO ()
negotiatedAttributeOwnershipDivestiture rtiAmb theObject theAttributes theTag =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            unsafeUseAsCString theTag $ \theTag ->
                wrapExceptions (FFI.negotiatedAttributeOwnershipDivestiture rtiAmb theObject theAttributes theTag)

attributeOwnershipAcquisition :: RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandleSet -> ByteString -> IO ()
attributeOwnershipAcquisition rtiAmb theObject theAttributes theTag =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            unsafeUseAsCString theTag $ \theTag ->
                wrapExceptions (FFI.attributeOwnershipAcquisition rtiAmb theObject theAttributes theTag)

attributeOwnershipAcquisitionIfAvailable :: RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandleSet -> IO ()
attributeOwnershipAcquisitionIfAvailable rtiAmb theObject theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.attributeOwnershipAcquisitionIfAvailable rtiAmb theObject theAttributes)

attributeOwnershipReleaseResponse :: RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandleSet -> IO AttributeHandleSet
attributeOwnershipReleaseResponse rtiAmb theObject theAttributes = do
    response <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.attributeOwnershipReleaseResponse rtiAmb theObject theAttributes)
    importAttributeHandleSet response

cancelNegotiatedAttributeOwnershipDivestiture :: RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandleSet -> IO ()
cancelNegotiatedAttributeOwnershipDivestiture rtiAmb theObject theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.cancelNegotiatedAttributeOwnershipDivestiture rtiAmb theObject theAttributes)

cancelAttributeOwnershipAcquisition :: RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandleSet -> IO ()
cancelAttributeOwnershipAcquisition rtiAmb theObject theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.cancelAttributeOwnershipAcquisition rtiAmb theObject theAttributes)

queryAttributeOwnership :: RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandle -> IO ()
queryAttributeOwnership rtiAmb theObject theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.queryAttributeOwnership rtiAmb theObject theAttributes)

isAttributeOwnedByFederate :: RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandle -> IO Bool
isAttributeOwnedByFederate rtiAmb theObject theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.isAttributeOwnedByFederate rtiAmb theObject theAttributes)

-------------------------------
-- * Time Management Services
-------------------------------

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
                wrapExceptions (FFI.enableTimeRegulation rtiAmb theFederateTime theLookahead)

        disableTimeRegulation rtiAmb =
            withRTIAmbassador rtiAmb $ \rtiAmb ->
                wrapExceptions (FFI.disableTimeRegulation rtiAmb)


enableTimeConstrained :: RTIAmbassador fedAmb -> IO ()
enableTimeConstrained rtiAmb =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.enableTimeConstrained rtiAmb)

disableTimeConstrained :: RTIAmbassador fedAmb -> IO ()
disableTimeConstrained rtiAmb =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.disableTimeConstrained rtiAmb)

timeAdvanceRequest :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> FedTime fedAmb -> IO ()
timeAdvanceRequest rtiAmb theTime = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withFedTime_ theTime $ \theTime -> 
            wrapExceptions (FFI.timeAdvanceRequest rtiAmb theTime)

timeAdvanceRequestAvailable :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> FedTime fedAmb -> IO ()
timeAdvanceRequestAvailable rtiAmb theTime =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withFedTime_ theTime $ \theTime -> 
            wrapExceptions (FFI.timeAdvanceRequestAvailable rtiAmb theTime)

nextEventRequest :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> FedTime fedAmb -> IO ()
nextEventRequest rtiAmb theTime =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withFedTime_ theTime $ \theTime -> 
            wrapExceptions (FFI.nextEventRequest rtiAmb theTime)

nextEventRequestAvailable :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> FedTime fedAmb -> IO ()
nextEventRequestAvailable rtiAmb theTime =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withFedTime_ theTime $ \theTime -> 
            wrapExceptions (FFI.nextEventRequestAvailable rtiAmb theTime)

flushQueueRequest :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> FedTime fedAmb -> IO ()
flushQueueRequest rtiAmb theTime =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withFedTime_ theTime $ \theTime -> 
            wrapExceptions (FFI.flushQueueRequest rtiAmb theTime)

enableAsynchronousDelivery :: RTIAmbassador fedAmb -> IO ()
enableAsynchronousDelivery rtiAmb = 
    withRTIAmbassador rtiAmb
        (wrapExceptions . FFI.enableAsynchronousDelivery)

disableAsynchronousDelivery :: RTIAmbassador fedAmb -> IO ()
disableAsynchronousDelivery rtiAmb = 
    withRTIAmbassador rtiAmb
        (wrapExceptions . FFI.disableAsynchronousDelivery)

queryLBTS :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> IO (FedTime fedAmb)
queryLBTS rtiAmb =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withArbitraryFedTime $ \fedTime -> 
            wrapExceptions (FFI.queryLBTS rtiAmb fedTime)

queryFederateTime :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> IO (FedTime fedAmb)
queryFederateTime rtiAmb =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withArbitraryFedTime $ \fedTime -> 
            wrapExceptions (FFI.queryFederateTime rtiAmb fedTime)

queryMinNextEventTime :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> IO (FedTime fedAmb)
queryMinNextEventTime rtiAmb =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withArbitraryFedTime $ \fedTime -> 
            wrapExceptions (FFI.queryMinNextEventTime rtiAmb fedTime)

modifyLookahead :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> FedTime fedAmb -> IO ()
modifyLookahead rtiAmb theTime =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withFedTime_ theTime $ \theTime -> 
            wrapExceptions (FFI.modifyLookahead rtiAmb theTime)

queryLookahead :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> IO (FedTime fedAmb)
queryLookahead rtiAmb =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withArbitraryFedTime $ \fedTime -> 
            wrapExceptions (FFI.queryLookahead rtiAmb fedTime)

retract :: RTIAmbassador fedAmb -> EventRetractionHandle -> IO ()
retract rtiAmb (EventRetractionHandle u fh) =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.retract rtiAmb u fh)

changeAttributeOrderType :: RTIAmbassador fedAmb -> ObjectHandle -> AttributeHandleSet -> OrderingHandle -> IO ()
changeAttributeOrderType rtiAmb theObject theAttributes theType =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.changeAttributeOrderType rtiAmb theObject theAttributes theType)

changeInteractionOrderType :: RTIAmbassador fedAmb -> InteractionClassHandle -> OrderingHandle -> IO ()
changeInteractionOrderType rtiAmb theClass theType =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.changeInteractionOrderType rtiAmb theClass theType)

-----------------------------------
-- * Data Distribution Management
-----------------------------------

-- |TODO - think about this... can it ever call the finalizer for RTIAmbassador
-- before the one for Region?  Probably.  Is that a problem?  Probably...
createRegion :: RTIAmbassador fedAmb -> SpaceHandle -> ULong -> IO Region
createRegion rtiAmb theSpace numberOfExtents =
    withRTIAmbassador rtiAmb $ \rtiAmb -> do
        r <- wrapExceptions (FFI.createRegion rtiAmb theSpace numberOfExtents)
        r <- newForeignPtr r (deleteRegion r)
        return (Region r)
    where 
        deleteRegion theRegion =
            withRTIAmbassador rtiAmb $ \rtiAmb ->
                wrapExceptions (FFI.deleteRegion rtiAmb theRegion)

notifyAboutRegionModification :: RTIAmbassador fedAmb -> Region -> IO ()
notifyAboutRegionModification rtiAmb theRegion =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            wrapExceptions (FFI.notifyAboutRegionModification rtiAmb theRegion)

-- |Should not normally be run by user; region will be properly deleted
-- when garbage collected (?)
deleteRegion :: RTIAmbassador fedAmb -> Region -> IO ()
deleteRegion rtiAmb (Region theRegion) = finalizeForeignPtr theRegion


registerObjectInstanceWithRegion :: RTIAmbassador t -> ObjectClassHandle -> Maybe ByteString -> [(AttributeHandle, Region)] -> IO ObjectHandle
registerObjectInstanceWithRegion rtiAmb theClass mbObject theHandles = do
    let (theAttributes, theRegions) = unzip theHandles
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withMany withRegion theRegions $ \theRegions ->
            withArray theRegions $ \theRegions -> 
                withArrayLen theAttributes $ \theNumberOfHandles theAttributes ->
                    case mbObject of
                        Nothing ->
                            wrapExceptions (FFI.registerObjectInstanceWithRegion          rtiAmb theClass           theAttributes theRegions (fromIntegral theNumberOfHandles))
                        Just theObject -> unsafeUseAsCString theObject $ \theObject ->
                            wrapExceptions (FFI.registerObjectInstanceWithRegion_withName rtiAmb theClass theObject theAttributes theRegions (fromIntegral theNumberOfHandles))

associateRegionForUpdates :: RTIAmbassador fedAmb -> Region -> ObjectHandle -> IO ()
associateRegionForUpdates rtiAmb theRegion theObject =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            wrapExceptions (FFI.associateRegionForUpdates rtiAmb theRegion theObject)

unassociateRegionForUpdates :: RTIAmbassador fedAmb -> Region -> ObjectHandle -> IO ()
unassociateRegionForUpdates rtiAmb theRegion theObject =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            wrapExceptions (FFI.unassociateRegionForUpdates rtiAmb theRegion theObject)

subscribeObjectClassAttributesWithRegion :: RTIAmbassador fedAmb -> ObjectClassHandle -> Region -> AttributeHandleSet -> Bool -> IO ()
subscribeObjectClassAttributesWithRegion rtiAmb theClass theRegion attributeList active =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            withAttributeHandleSet attributeList $ \attributeList ->
                wrapExceptions (FFI.subscribeObjectClassAttributesWithRegion rtiAmb theClass theRegion attributeList active)

unsubscribeObjectClassWithRegion :: RTIAmbassador fedAmb -> ObjectClassHandle -> Region -> IO ()
unsubscribeObjectClassWithRegion rtiAmb theClass theRegion =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            wrapExceptions (FFI.unsubscribeObjectClassWithRegion rtiAmb theClass theRegion)

subscribeInteractionClassWithRegion :: RTIAmbassador fedAmb -> InteractionClassHandle -> Region -> Bool -> IO ()
subscribeInteractionClassWithRegion rtiAmb theClass theRegion active = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            wrapExceptions (FFI.subscribeInteractionClassWithRegion rtiAmb theClass theRegion active)


unsubscribeInteractionClassWithRegion :: RTIAmbassador fedAmb -> InteractionClassHandle -> Region -> IO ()
unsubscribeInteractionClassWithRegion rtiAmb theClass theRegion = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            wrapExceptions (FFI.unsubscribeInteractionClassWithRegion rtiAmb theClass theRegion)

sendInteractionWithRegionAtTime :: FederateAmbassador fedAmb => RTIAmbassador fedAmb -> InteractionClassHandle -> ParameterHandleValuePairSet -> FedTime fedAmb -> ByteString -> Region -> IO EventRetractionHandle
sendInteractionWithRegionAtTime rtiAmb theInteraction theParameters theTime theTag theRegion =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withParameterHandleValuePairSet theParameters $ \theParameters ->
            withFedTime_ theTime $ \theTime ->
                unsafeUseAsCString theTag $ \theTag ->
                    withRegion theRegion $ \theRegion ->
                        withEventRetractionHandleReturn $ \u fh ->
                            wrapExceptions (FFI.sendInteractionWithRegionAtTime rtiAmb theInteraction theParameters theTime theTag theRegion u fh)

sendInteractionWithRegion :: RTIAmbassador fedAmb -> InteractionClassHandle -> ParameterHandleValuePairSet -> ByteString -> Region -> IO ()
sendInteractionWithRegion rtiAmb theInteraction theParameters theTag theRegion =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withParameterHandleValuePairSet theParameters $ \theParameters ->
            unsafeUseAsCString theTag $ \theTag ->
                withRegion theRegion $ \theRegion ->
                    wrapExceptions (FFI.sendInteractionWithRegion rtiAmb theInteraction theParameters theTag theRegion)

requestClassAttributeValueUpdateWithRegion :: RTIAmbassador fedAmb -> ObjectClassHandle -> AttributeHandleSet -> Region -> IO ()
requestClassAttributeValueUpdateWithRegion rtiAmb theClass theAttributes theRegion =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            withRegion theRegion $ \theRegion ->
                wrapExceptions (FFI.requestClassAttributeValueUpdateWithRegion rtiAmb theClass theAttributes theRegion)

---------------------------
-- * RTI Support Services
---------------------------

getObjectClassHandle :: RTIAmbassador fedAmb -> ByteString -> IO ObjectClassHandle
getObjectClassHandle rtiAmb theName = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        unsafeUseAsCString theName $ \theName ->
            wrapExceptions (FFI.getObjectClassHandle rtiAmb theName)

getObjectClassName :: RTIAmbassador fedAmb -> ObjectClassHandle -> IO ByteString
getObjectClassName rtiAmb theHandle = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getObjectClassName rtiAmb theHandle)
    unsafePackNewCString cStr
    
getAttributeHandle :: RTIAmbassador fedAmb -> ByteString -> ObjectClassHandle -> IO AttributeHandle
getAttributeHandle rtiAmb theName whichClass = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        unsafeUseAsCString theName $ \theName -> 
            wrapExceptions (FFI.getAttributeHandle rtiAmb theName whichClass)

getAttributeName :: RTIAmbassador fedAmb -> AttributeHandle -> ObjectClassHandle -> IO ByteString
getAttributeName rtiAmb theHandle whichClass = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getAttributeName rtiAmb theHandle whichClass)
    unsafePackNewCString cStr

getInteractionClassHandle :: RTIAmbassador fedAmb -> ByteString -> IO InteractionClassHandle
getInteractionClassHandle rtiAmb theName = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        unsafeUseAsCString theName $ \theName ->
            wrapExceptions (FFI.getInteractionClassHandle rtiAmb theName)

getInteractionClassName :: RTIAmbassador fedAmb -> InteractionClassHandle -> IO ByteString
getInteractionClassName rtiAmb theHandle = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getInteractionClassName rtiAmb theHandle)
    unsafePackNewCString cStr

getParameterHandle :: RTIAmbassador fedAmb -> ByteString -> InteractionClassHandle -> IO ParameterHandle
getParameterHandle rtiAmb theName whichClass =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        unsafeUseAsCString theName $ \theName ->
            wrapExceptions (FFI.getParameterHandle rtiAmb theName whichClass)

getParameterName :: RTIAmbassador fedAmb -> ParameterHandle -> InteractionClassHandle -> IO ByteString
getParameterName rtiAmb theHandle whichClass = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getParameterName rtiAmb theHandle whichClass)
    unsafePackNewCString cStr

getObjectInstanceHandle :: RTIAmbassador fedAmb -> ByteString -> IO ObjectHandle
getObjectInstanceHandle rtiAmb theName =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        unsafeUseAsCString theName $ \theName ->
            wrapExceptions (FFI.getObjectInstanceHandle rtiAmb theName)

getObjectInstanceName :: RTIAmbassador fedAmb -> ObjectHandle -> IO ByteString
getObjectInstanceName rtiAmb theHandle = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getObjectInstanceName rtiAmb theHandle)
    unsafePackNewCString cStr

getRoutingSpaceHandle :: RTIAmbassador fedAmb -> ByteString -> IO SpaceHandle
getRoutingSpaceHandle rtiAmb theName =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        unsafeUseAsCString theName $ \theName ->
            wrapExceptions (FFI.getRoutingSpaceHandle rtiAmb theName)

getRoutingSpaceName :: RTIAmbassador fedAmb -> SpaceHandle -> IO ByteString
getRoutingSpaceName rtiAmb theHandle = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getRoutingSpaceName rtiAmb theHandle)
    unsafePackNewCString cStr

getDimensionHandle :: RTIAmbassador fedAmb -> ByteString -> SpaceHandle -> IO DimensionHandle
getDimensionHandle rtiAmb theName whichSpace =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        unsafeUseAsCString theName $ \theName ->
            wrapExceptions (FFI.getDimensionHandle rtiAmb theName whichSpace)

getDimensionName :: RTIAmbassador fedAmb -> DimensionHandle -> SpaceHandle -> IO ByteString
getDimensionName rtiAmb theHandle whichSpace = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getDimensionName rtiAmb theHandle whichSpace)
    unsafePackNewCString cStr

getAttributeRoutingSpaceHandle :: RTIAmbassador fedAmb -> AttributeHandle -> ObjectClassHandle -> IO SpaceHandle
getAttributeRoutingSpaceHandle rtiAmb theHandle whichClass =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        wrapExceptions (FFI.getAttributeRoutingSpaceHandle rtiAmb theHandle whichClass)

getObjectClass :: RTIAmbassador fedAmb -> ObjectHandle -> IO ObjectClassHandle
getObjectClass rtiAmb theObject =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        wrapExceptions (FFI.getObjectClass rtiAmb theObject)

getInteractionRoutingSpaceHandle :: RTIAmbassador fedAmb -> InteractionClassHandle -> IO SpaceHandle
getInteractionRoutingSpaceHandle rtiAmb theHandle =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        wrapExceptions (FFI.getInteractionRoutingSpaceHandle rtiAmb theHandle)

getTransportationHandle :: RTIAmbassador fedAmb -> ByteString -> IO TransportationHandle
getTransportationHandle rtiAmb theName =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        unsafeUseAsCString theName $ \theName ->
            wrapExceptions (FFI.getTransportationHandle rtiAmb theName)

getTransportationName :: RTIAmbassador fedAmb -> TransportationHandle -> IO ByteString
getTransportationName rtiAmb theHandle = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getTransportationName rtiAmb theHandle)
    unsafePackNewCString cStr

getOrderingHandle :: RTIAmbassador fedAmb -> ByteString -> IO OrderingHandle
getOrderingHandle rtiAmb theName =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        unsafeUseAsCString theName $ \theName ->
            wrapExceptions (FFI.getOrderingHandle rtiAmb theName)

getOrderingName :: RTIAmbassador fedAmb -> OrderingHandle -> IO ByteString
getOrderingName rtiAmb theHandle = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getOrderingName rtiAmb theHandle)
    unsafePackNewCString cStr

enableClassRelevanceAdvisorySwitch :: RTIAmbassador fedAmb -> IO ()
enableClassRelevanceAdvisorySwitch rtiAmb = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.enableClassRelevanceAdvisorySwitch rtiAmb)
    
disableClassRelevanceAdvisorySwitch :: RTIAmbassador fedAmb -> IO ()
disableClassRelevanceAdvisorySwitch rtiAmb = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.disableClassRelevanceAdvisorySwitch rtiAmb)

enableAttributeRelevanceAdvisorySwitch :: RTIAmbassador fedAmb -> IO ()
enableAttributeRelevanceAdvisorySwitch rtiAmb = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.enableAttributeRelevanceAdvisorySwitch rtiAmb)
    
disableAttributeRelevanceAdvisorySwitch :: RTIAmbassador fedAmb -> IO ()
disableAttributeRelevanceAdvisorySwitch rtiAmb = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.disableAttributeRelevanceAdvisorySwitch rtiAmb)

enableAttributeScopeAdvisorySwitch :: RTIAmbassador fedAmb -> IO ()
enableAttributeScopeAdvisorySwitch rtiAmb = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.enableAttributeScopeAdvisorySwitch rtiAmb)
    
disableAttributeScopeAdvisorySwitch :: RTIAmbassador fedAmb -> IO ()
disableAttributeScopeAdvisorySwitch rtiAmb = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.disableAttributeScopeAdvisorySwitch rtiAmb)

enableInteractionRelevanceAdvisorySwitch :: RTIAmbassador fedAmb -> IO ()
enableInteractionRelevanceAdvisorySwitch rtiAmb = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.enableInteractionRelevanceAdvisorySwitch rtiAmb)
    
disableInteractionRelevanceAdvisorySwitch :: RTIAmbassador fedAmb -> IO ()
disableInteractionRelevanceAdvisorySwitch rtiAmb = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.disableInteractionRelevanceAdvisorySwitch rtiAmb)

tick :: RTIAmbassador fedAmb -> IO Bool
tick rtiAmb =
    withRTIAmbassador rtiAmb
        (wrapExceptions . FFI.tick)

tick_minimum_maximum :: RTIAmbassador fedAmb -> TickTime -> TickTime -> IO Bool
tick_minimum_maximum rtiAmb min max = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.tick_minimum_maximum rtiAmb min max)

getRegionToken :: RTIAmbassador fedAmb -> Region -> IO RegionToken
getRegionToken rtiAmb theRegion =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            wrapExceptions (FFI.getRegionToken rtiAmb theRegion)

-- |WARNING: the header did not say anything about the return convention of 
-- this function, so I don't really know how long these Region objects live.
-- My guess would be that they live about as long as the RTIAmbassador, but
-- I'm not sure.
getRegion :: RTIAmbassador fedAmb -> RegionToken -> IO Region
getRegion rtiAmb theRegion = do
    regionPtr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getRegion rtiAmb theRegion)
    fPtr <- newForeignPtr_ regionPtr
    return (Region fPtr)
    
