module Network.HLA.RTI13.RTIAmbServices
    ( RTIAmbassador
    , module Network.HLA.RTI13.RTIAmbServices
    ) where

import Network.HLA.RTI13.RTIAmbServices.FFI (RTIAmbassador(..), withRTIAmbassador)
import qualified Network.HLA.RTI13.RTIAmbServices.FFI as FFI
import qualified Network.HLA.RTI13.RTITypes.FFI as FFI

import Network.HLA.RTI13.BaseTypes
import Network.HLA.RTI13.OddsAndEnds
import Network.HLA.RTI13.RTITypes
import Network.HLA.RTI13.RTIException

import Control.Exception (bracket_)
import Data.ByteString (ByteString, useAsCString)
import Data.IORef
import qualified Data.Map as M (Map)
import qualified Data.Set as S (Set)
import Foreign hiding (newForeignPtr)
import Foreign.Concurrent
import System.Mem

-- |Create a new 'RTIAmbassador', which is the object that manages the 
-- connection to the RTI.  There should be exactly one 'RTIAmbassador'
-- and one 'FederateAmbassador' per federate.  Some RTIs only support one
-- federate per application instance.
newRTIAmbassador :: IO (RTIAmbassador t)
newRTIAmbassador = do
    rtiAmb <- FFI.new_RTIambassador
    rtiAmb <- newForeignPtr rtiAmb (FFI.delete_RTIambassador rtiAmb)
    fedAmb <- newIORef Nothing
    return (RTIAmbassador rtiAmb fedAmb)

--------------------------------------
-- * Federation Management Services
--------------------------------------

-- |Attempt to create a federation on the RTI.
-- 
-- Preconditions:
-- 
-- * No federation execution exists with that name.  If the federation already
-- exists, a 'FederationExecutionAlreadyExists' exception will be thrown.
-- 
-- Other possible exceptions include 'CouldNotOpenFED', 'ErrorReadingFED', 
-- 'ConcurrentAccessAttempted' or 'RTIinternalError'.
-- 
-- Usage:
-- 
-- > createFederationExecution fedAmb executionName fedFile
-- 
createFederationExecution :: RTIAmbassador t -> ByteString -> ByteString -> IO ()
createFederationExecution rtiAmb executionName fed = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        useAsCString executionName $ \executionName ->
            useAsCString fed $ \fed ->
                wrapExceptions (FFI.createFederationExecution rtiAmb executionName fed)

-- |Attempt to halt a federation on the RTI.
-- 
-- Preconditions:
-- 
-- * A federation with that name exists.  If the federation does not exist,
-- a 'FederationExecutionDoesNotExist' exception will be thrown.
-- 
-- * No federates are joined to that federation.  If there are federates
-- participating in the federation, a 'FederatesCurrentlyJoined' exception
-- will be thrown.
-- 
-- Other possible exceptions include 'ConcurrentAccessAttempted' or 'RTIinternalError'.
-- 
-- Usage:
-- 
-- > destroyFederationExecution fedAmb executionName
-- 
destroyFederationExecution :: RTIAmbassador t -> ByteString -> IO ()
destroyFederationExecution rtiAmb executionName = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        useAsCString executionName $ \executionName ->
            wrapExceptions (FFI.destroyFederationExecution rtiAmb executionName)

-- |Attempt to join a federation execution.
-- 
-- Preconditions:
-- 
-- * A federation with the specified name exists.  If there is no such federation,
-- a 'FederationExecutionDoesNotExist' exception will be thrown.
-- 
-- * The federation does not already have a federate with the specified name.
-- If there is already a federate with the requested name, a
-- 'FederateAlreadyExecutionMember' exception will be thrown.
-- 
-- Other possible exceptions include 'CouldNotOpenFED', 'ErrorReadingFED', 
-- 'SaveInProgress', 'RestoreInProgress', 'ConcurrentAccessAttempted', 
-- or 'RTIinternalError'.
-- 
-- Usage:
-- 
-- > joinFederationExecution rtiAmb federateName executionName fedAmb
joinFederationExecution :: FederateAmbassador fedAmb => RTIAmbassador (FedAmbTime fedAmb) -> ByteString -> ByteString -> fedAmb -> IO FederateHandle
joinFederationExecution rtiAmb yourName executionName fedAmb = do
    fedHandle <- withRTIAmbassador rtiAmb $ \rtiAmb -> 
        useAsCString yourName $ \yourName ->
            useAsCString executionName $ \executionName ->
                withFederateAmbassador fedAmb $ \fedAmb ->
                    wrapExceptions (FFI.joinFederationExecution rtiAmb yourName executionName fedAmb)
    writeIORef (rtiFedAmb rtiAmb) (Just (FFI.SomeFedAmb fedAmb))
    return fedHandle

-- |Attempt to resign from a federation execution.  The 'ResignAction' 
-- parameter describes how attributes owned by the federate will be disposed of.
-- 
-- Possible exceptions are 'FederateOwnsAttributes', 'FederateNotExecutionMember',
-- 'InvalidResignAction', 'ConcurrentAccessAttempted' and 'RTIinternalError'.
resignFederationExecution :: RTIAmbassador t -> ResignAction -> IO ()
resignFederationExecution rtiAmb resignAction = do
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        wrapExceptions (FFI.resignFederationExecution rtiAmb (fromIntegral (fromEnum resignAction)))
    writeIORef (rtiFedAmb rtiAmb) Nothing
    performGC

registerFederationSynchronizationPoint :: RTIAmbassador t -> ByteString -> ByteString -> Maybe (S.Set FederateHandle) -> IO ()
registerFederationSynchronizationPoint rtiAmb label theTag mbSyncSet = do
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        useAsCString label $ \label ->
            useAsCString theTag $ \theTag -> case mbSyncSet of
                    Nothing ->
                        wrapExceptions (FFI.registerFederationSynchronizationPoint rtiAmb label theTag)
                    Just syncSet -> withFederateHandleSet syncSet $ \syncSet ->
                        wrapExceptions (FFI.registerFederationSynchronizationPoint_with_syncSet rtiAmb label theTag syncSet)

synchronizationPointAchieved :: RTIAmbassador t -> ByteString -> IO ()
synchronizationPointAchieved rtiAmb label =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        useAsCString label $ \label ->
            wrapExceptions (FFI.synchronizationPointAchieved rtiAmb label)

requestFederationSave :: FedTimeImpl t => RTIAmbassador t -> ByteString -> Maybe (FedTime t) -> IO ()
requestFederationSave rtiAmb label mbTime =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        useAsCString label $ \label -> case mbTime of
            Nothing ->
                wrapExceptions (FFI.requestFederationSave rtiAmb label)
            Just theTime -> 
                withFedTimeIn theTime $ \theTime ->
                    wrapExceptions (FFI.requestFederationSaveAtTime rtiAmb label theTime)

federateSaveBegun :: RTIAmbassador t -> IO ()
federateSaveBegun rtiAmb = withRTIAmbassador rtiAmb
    (wrapExceptions . FFI.federateSaveBegun)

federateSaveComplete :: RTIAmbassador t -> IO ()
federateSaveComplete rtiAmb = withRTIAmbassador rtiAmb
    (wrapExceptions . FFI.federateSaveComplete)

federateSaveNotComplete :: RTIAmbassador t -> IO ()
federateSaveNotComplete rtiAmb = withRTIAmbassador rtiAmb
    (wrapExceptions . FFI.federateSaveNotComplete)

requestFederationRestore :: RTIAmbassador t -> ByteString -> IO ()
requestFederationRestore rtiAmb label =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        useAsCString label $ \label ->
            wrapExceptions (FFI.requestFederationRestore rtiAmb label)

federateRestoreComplete :: RTIAmbassador t -> IO ()
federateRestoreComplete rtiAmb = withRTIAmbassador rtiAmb
    (wrapExceptions . FFI.federateRestoreComplete)

federateRestoreNotComplete :: RTIAmbassador t -> IO ()
federateRestoreNotComplete rtiAmb = withRTIAmbassador rtiAmb
    (wrapExceptions . FFI.federateRestoreNotComplete)

--------------------------------------
-- * Declaration Management Services
--------------------------------------

-- |Indicates to the RTI that this federate is capable of sending updates for
-- the specified set of attributes of the specified object class.
publishObjectClass :: RTIAmbassador t -> ObjectClassHandle -> S.Set AttributeHandle -> IO ()
publishObjectClass rtiAmb theClass attributeList =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet attributeList $ \attributeList ->
            wrapExceptions (FFI.publishObjectClass rtiAmb theClass attributeList)

-- |Indicates to the RTI that this federate is no longer capable of sending
-- updates for the specified set of attributes of the specified object class.
unpublishObjectClass :: RTIAmbassador t -> ObjectClassHandle -> IO ()
unpublishObjectClass rtiAmb theClass =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.unpublishObjectClass rtiAmb theClass)

-- |Indicates to the RTI that this federate is capable of sending interactions
-- of the specified interaction class.
publishInteractionClass :: RTIAmbassador t -> InteractionClassHandle -> IO ()
publishInteractionClass rtiAmb theInteraction =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.publishInteractionClass rtiAmb theInteraction)

-- |Indicates to the RTI that this federate is no longer capable of sending
-- interactions of the specified interaction class.
unpublishInteractionClass :: RTIAmbassador t -> InteractionClassHandle -> IO ()
unpublishInteractionClass rtiAmb theInteraction =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.unpublishInteractionClass rtiAmb theInteraction)

-- |Indicates to the RTI that this federate is interested in the specified set
-- of attributes for all objects of the specified class.
subscribeObjectClassAttributes :: RTIAmbassador t -> ObjectClassHandle -> S.Set AttributeHandle -> IO ()
subscribeObjectClassAttributes rtiAmb theClass attributeList =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet attributeList $ \attributeList ->
            wrapExceptions (FFI.subscribeObjectClassAttributes rtiAmb theClass attributeList)

-- |Indicates to the RTI that this federate is no longer interested in updates
-- for any attributes of any objects of the specified class.
unsubscribeObjectClass :: RTIAmbassador t -> ObjectClassHandle -> IO ()
unsubscribeObjectClass rtiAmb theClass =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.unsubscribeObjectClass rtiAmb theClass)

-- |Indicates to the RTI that this federate is interested in any occurences of
-- interactions of the specified class.  The 'Bool' flag indicates whether this
-- interest is \"active\", which means something to someone, probably.  The
-- default value for the \"active\" parameter in C++ is 'True'.
subscribeInteractionClass :: RTIAmbassador t -> InteractionClassHandle -> Bool -> IO ()
subscribeInteractionClass rtiAmb theClass active =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.subscribeInteractionClass rtiAmb theClass active)

-- |Indicates to the RTI that this federate is no longer interested in 
-- interactions of the specified class.
unsubscribeInteractionClass :: RTIAmbassador t -> InteractionClassHandle -> IO ()
unsubscribeInteractionClass rtiAmb theClass =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.unsubscribeInteractionClass rtiAmb theClass)

---------------------------------
-- * Object Management Services
---------------------------------

-- |Report to the federation the existence of an object of the specified class.  
-- 
-- This should be a class previously \"published\" via 'publishObjectClass', 
-- and if \"class relevance advisories\" are enabled  
-- (see 'enableClassRelevanceAdvisorySwitch') then this only needs to be
-- sent if the federate has received a \"start registration\" message for
-- the class of the object.
-- 
-- In the case where registration is off for a class, all object instances
-- /should/ be registered upon later receipt of a \"start registration\" 
-- message for that class.
registerObjectInstance :: RTIAmbassador t -> ObjectClassHandle -> Maybe ByteString -> IO ObjectHandle
registerObjectInstance rtiAmb theClass mbObject =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        case mbObject of
            Just theObject  ->
                useAsCString theObject $ \theObject ->
                    wrapExceptions (FFI.registerObjectInstance_withName rtiAmb theClass theObject)
            Nothing         ->
                wrapExceptions (FFI.registerObjectInstance rtiAmb theClass)


-- |Sends an attribute value update to the federation for the specified object,
-- along with a time stamp at which the attributes values take effect.
--
-- Returns an 'EventRetractionHandle' which can be used to cancel the change
-- by calling 'retract'.
updateAttributeValuesAtTime :: FedTimeImpl t => RTIAmbassador t -> ObjectHandle -> M.Map AttributeHandle ByteString -> FedTime t -> ByteString -> IO EventRetractionHandle
updateAttributeValuesAtTime rtiAmb theObject theAttributes theTime theTag =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withAttributeHandleValuePairSet theAttributes $ \theAttributes ->
            withFedTimeIn theTime $ \theTime ->
                useAsCString theTag $ \theTag ->
                    withEventRetractionHandleReturn $ \u fh ->
                        wrapExceptions (FFI.updateAttributeValuesAtTime rtiAmb theObject theAttributes theTime theTag u fh)

-- |Sends an attribute value update to the federation for the specified object.
-- The new attribute values take effect immediately.
updateAttributeValues :: RTIAmbassador t -> ObjectHandle -> M.Map AttributeHandle ByteString -> ByteString -> IO ()
updateAttributeValues rtiAmb theObject theAttributes theTag =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withAttributeHandleValuePairSet theAttributes $ \theAttributes ->
            useAsCString theTag $ \theTag ->
                wrapExceptions (FFI.updateAttributeValues rtiAmb theObject theAttributes theTag)

-- |Send an interaction to the federation which is considered to \"occur\" at
-- a given time.
-- 
-- Returns an 'EventRetractionHandle' which can be passed to 'retract' in 
-- order to cancel the event, if retracted soon enough.
sendInteractionAtTime :: FedTimeImpl t => RTIAmbassador t -> InteractionClassHandle -> M.Map ParameterHandle ByteString -> FedTime t -> ByteString -> IO EventRetractionHandle
sendInteractionAtTime rtiAmb theInteraction theParameters theTime theTag = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withParameterHandleValuePairSet theParameters $ \theParameters ->
            withFedTimeIn theTime $ \ theTime ->
                useAsCString theTag $ \theTag -> 
                    withEventRetractionHandleReturn $ \u fh ->
                        wrapExceptions (FFI.sendInteractionAtTime rtiAmb theInteraction theParameters theTime theTag u fh)

-- |Send an interaction to the federation which is considered to \"occur\"
-- immediately.
sendInteraction :: RTIAmbassador t -> InteractionClassHandle -> M.Map ParameterHandle ByteString -> ByteString -> IO ()
sendInteraction rtiAmb theInteraction theParameters theTag = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withParameterHandleValuePairSet theParameters $ \theParameters ->
            useAsCString theTag $ \theTag -> 
                wrapExceptions (FFI.sendInteraction rtiAmb theInteraction theParameters theTag)

deleteObjectInstanceAtTime :: FedTimeImpl t => RTIAmbassador t -> ObjectHandle -> FedTime t -> ByteString -> IO EventRetractionHandle
deleteObjectInstanceAtTime rtiAmb theObject theTime theTag = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withFedTimeIn theTime $ \ theTime ->
            useAsCString theTag $ \theTag -> 
                withEventRetractionHandleReturn $ \u fh ->
                    wrapExceptions (FFI.deleteObjectInstanceAtTime rtiAmb theObject theTime theTag u fh)

deleteObjectInstance :: RTIAmbassador t -> ObjectHandle -> ByteString -> IO ()
deleteObjectInstance rtiAmb theObject theTag = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        useAsCString theTag $ \theTag -> 
            wrapExceptions (FFI.deleteObjectInstance rtiAmb theObject theTag)

localDeleteObjectInstance :: RTIAmbassador t -> ObjectHandle -> IO ()
localDeleteObjectInstance rtiAmb theObject = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.localDeleteObjectInstance rtiAmb theObject)

changeAttributeTransportationType :: RTIAmbassador t -> ObjectHandle -> S.Set AttributeHandle -> TransportationHandle -> IO ()
changeAttributeTransportationType rtiAmb theObject theAttributes theType =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.changeAttributeTransportationType rtiAmb theObject theAttributes theType)

changeInteractionTransportationType :: RTIAmbassador t -> InteractionClassHandle -> TransportationHandle -> IO ()
changeInteractionTransportationType rtiAmb theClass theType =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.changeInteractionTransportationType rtiAmb theClass theType)

requestObjectAttributeValueUpdate :: RTIAmbassador t -> ObjectHandle -> S.Set AttributeHandle -> IO ()
requestObjectAttributeValueUpdate rtiAmb theObject theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.requestObjectAttributeValueUpdate rtiAmb theObject theAttributes)

requestClassAttributeValueUpdate :: RTIAmbassador t -> ObjectClassHandle -> S.Set AttributeHandle -> IO ()
requestClassAttributeValueUpdate rtiAmb theClass theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.requestClassAttributeValueUpdate rtiAmb theClass theAttributes)

------------------------------------
-- * Ownership Management Services
------------------------------------

unconditionalAttributeOwnershipDivestiture :: RTIAmbassador t -> ObjectHandle -> S.Set AttributeHandle -> IO ()
unconditionalAttributeOwnershipDivestiture rtiAmb theObject theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.unconditionalAttributeOwnershipDivestiture rtiAmb theObject theAttributes)

negotiatedAttributeOwnershipDivestiture :: RTIAmbassador t -> ObjectHandle -> S.Set AttributeHandle -> ByteString -> IO ()
negotiatedAttributeOwnershipDivestiture rtiAmb theObject theAttributes theTag =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            useAsCString theTag $ \theTag ->
                wrapExceptions (FFI.negotiatedAttributeOwnershipDivestiture rtiAmb theObject theAttributes theTag)

attributeOwnershipAcquisition :: RTIAmbassador t -> ObjectHandle -> S.Set AttributeHandle -> ByteString -> IO ()
attributeOwnershipAcquisition rtiAmb theObject theAttributes theTag =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            useAsCString theTag $ \theTag ->
                wrapExceptions (FFI.attributeOwnershipAcquisition rtiAmb theObject theAttributes theTag)

attributeOwnershipAcquisitionIfAvailable :: RTIAmbassador t -> ObjectHandle -> S.Set AttributeHandle -> IO ()
attributeOwnershipAcquisitionIfAvailable rtiAmb theObject theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.attributeOwnershipAcquisitionIfAvailable rtiAmb theObject theAttributes)

attributeOwnershipReleaseResponse :: RTIAmbassador t -> ObjectHandle -> S.Set AttributeHandle -> IO (S.Set AttributeHandle)
attributeOwnershipReleaseResponse rtiAmb theObject theAttributes = do
    ahSet <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.attributeOwnershipReleaseResponse rtiAmb theObject theAttributes)
    theAttrs <- importAttributeHandleSet ahSet
    FFI.delete_AttributeHandleSet ahSet
    return theAttrs

cancelNegotiatedAttributeOwnershipDivestiture :: RTIAmbassador t -> ObjectHandle -> S.Set AttributeHandle -> IO ()
cancelNegotiatedAttributeOwnershipDivestiture rtiAmb theObject theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.cancelNegotiatedAttributeOwnershipDivestiture rtiAmb theObject theAttributes)

cancelAttributeOwnershipAcquisition :: RTIAmbassador t -> ObjectHandle -> S.Set AttributeHandle -> IO ()
cancelAttributeOwnershipAcquisition rtiAmb theObject theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.cancelAttributeOwnershipAcquisition rtiAmb theObject theAttributes)

queryAttributeOwnership :: RTIAmbassador t -> ObjectHandle -> AttributeHandle -> IO ()
queryAttributeOwnership rtiAmb theObject theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.queryAttributeOwnership rtiAmb theObject theAttributes)

isAttributeOwnedByFederate :: RTIAmbassador t -> ObjectHandle -> AttributeHandle -> IO Bool
isAttributeOwnedByFederate rtiAmb theObject theAttributes =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.isAttributeOwnedByFederate rtiAmb theObject theAttributes)

-------------------------------
-- * Time Management Services
-------------------------------

withTimeRegulation :: FedTimeImpl t => RTIAmbassador t -> FedTime t -> FedTime t -> IO a -> IO a
withTimeRegulation rtiAmb theFederateTime theLookahead regulatedAction =
    withFedTimeIn theFederateTime $ \theFederateTime -> do
        withFedTimeIn theLookahead $ \theLookahead -> do
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


enableTimeConstrained :: RTIAmbassador t -> IO ()
enableTimeConstrained rtiAmb =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.enableTimeConstrained rtiAmb)

disableTimeConstrained :: RTIAmbassador t -> IO ()
disableTimeConstrained rtiAmb =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.disableTimeConstrained rtiAmb)

-- |Request that the federate's local \"logical time\" (the time returned by
-- 'queryFederateTime') be advanced to the specified time.  If the federate
-- is not time-constrained, this request will be granted unconditionally.  If
-- the federate is time-constrained, the time may be advanced less than 
-- requested.
-- 
-- In either case, the time is not actually advanced until the federate is
-- sent the \"Time Advance Grant\" message, and the granted time advance
-- will be no farther into the future than requested.
timeAdvanceRequest :: FedTimeImpl t => RTIAmbassador t -> FedTime t -> IO ()
timeAdvanceRequest rtiAmb theTime = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withFedTimeIn theTime $ \theTime -> 
            wrapExceptions (FFI.timeAdvanceRequest rtiAmb theTime)

timeAdvanceRequestAvailable :: FedTimeImpl t => RTIAmbassador t -> FedTime t -> IO ()
timeAdvanceRequestAvailable rtiAmb theTime =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withFedTimeIn theTime $ \theTime -> 
            wrapExceptions (FFI.timeAdvanceRequestAvailable rtiAmb theTime)

-- |Request that the federate's local \"logical time\" (the time returned by
-- 'queryFederateTime') be advanced to the time of the next incoming simulation
-- event, or to the time specified if no events are received before then.
-- 
-- The time is not actually advanced until the federate is sent the \"Time
-- Advance Grant\" message.
nextEventRequest :: FedTimeImpl t => RTIAmbassador t -> FedTime t -> IO ()
nextEventRequest rtiAmb theTime =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withFedTimeIn theTime $ \theTime -> 
            wrapExceptions (FFI.nextEventRequest rtiAmb theTime)

nextEventRequestAvailable :: FedTimeImpl t => RTIAmbassador t -> FedTime t -> IO ()
nextEventRequestAvailable rtiAmb theTime =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withFedTimeIn theTime $ \theTime -> 
            wrapExceptions (FFI.nextEventRequestAvailable rtiAmb theTime)

flushQueueRequest :: FedTimeImpl t => RTIAmbassador t -> FedTime t -> IO ()
flushQueueRequest rtiAmb theTime =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withFedTimeIn theTime $ \theTime -> 
            wrapExceptions (FFI.flushQueueRequest rtiAmb theTime)

-- |Requests that Receive-Ordered events be delivered during any 'tick' call
-- rather than only during time advances.
enableAsynchronousDelivery :: RTIAmbassador t -> IO ()
enableAsynchronousDelivery rtiAmb = 
    withRTIAmbassador rtiAmb
        (wrapExceptions . FFI.enableAsynchronousDelivery)

disableAsynchronousDelivery :: RTIAmbassador t -> IO ()
disableAsynchronousDelivery rtiAmb = 
    withRTIAmbassador rtiAmb
        (wrapExceptions . FFI.disableAsynchronousDelivery)

-- |For a time-constrained federate, the LBTS (\"Lower Bound Time Stamp\") is
-- the latest time to which it is currently allowed to advance.  It is (by
-- definition) the earliest possible time stamp of all simulation events that
-- have not yet been delivered.
queryLBTS :: FedTimeImpl t => RTIAmbassador t -> IO (FedTime t)
queryLBTS rtiAmb =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withFedTimeOut $ \fedTime -> 
            wrapExceptions (FFI.queryLBTS rtiAmb fedTime)

-- |Look up the current \"Logical Time\", or local time, of the federate.
queryFederateTime :: FedTimeImpl t => RTIAmbassador t -> IO (FedTime t)
queryFederateTime rtiAmb =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withFedTimeOut $ \fedTime -> 
            wrapExceptions (FFI.queryFederateTime rtiAmb fedTime)

-- |For a time-regulating federate, this call returns the earliest time at 
-- which a new event may be scheduled.  This is the same as the federate's
-- current logical time plus the lookahead interval.  This is also known as
-- the federate's \"Effective Time\".
queryMinNextEventTime :: FedTimeImpl t => RTIAmbassador t -> IO (FedTime t)
queryMinNextEventTime rtiAmb =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withFedTimeOut $ \fedTime -> 
            wrapExceptions (FFI.queryMinNextEventTime rtiAmb fedTime)

modifyLookahead :: FedTimeImpl t => RTIAmbassador t -> FedTime t -> IO ()
modifyLookahead rtiAmb theTime =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withFedTimeIn theTime $ \theTime -> 
            wrapExceptions (FFI.modifyLookahead rtiAmb theTime)

queryLookahead :: FedTimeImpl t => RTIAmbassador t -> IO (FedTime t)
queryLookahead rtiAmb =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        withFedTimeOut $ \fedTime -> 
            wrapExceptions (FFI.queryLookahead rtiAmb fedTime)

-- |Retract an event previously scheduled for future delivery.  If possible, the
-- RTI will refrain from delivering the event to other federates.  If it is too
-- late to prevent delivery, all federates that received the event will be
-- sent a \"Request Retraction\" message.
retract :: RTIAmbassador t -> EventRetractionHandle -> IO ()
retract rtiAmb (EventRetractionHandle u fh) =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.retract rtiAmb u fh)

changeAttributeOrderType :: RTIAmbassador t -> ObjectHandle -> S.Set AttributeHandle -> OrderingHandle -> IO ()
changeAttributeOrderType rtiAmb theObject theAttributes theType =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            wrapExceptions (FFI.changeAttributeOrderType rtiAmb theObject theAttributes theType)

changeInteractionOrderType :: RTIAmbassador t -> InteractionClassHandle -> OrderingHandle -> IO ()
changeInteractionOrderType rtiAmb theClass theType =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.changeInteractionOrderType rtiAmb theClass theType)

-----------------------------------
-- * Data Distribution Management
-----------------------------------

-- |Create a region which can then be associated with subscriptions and/or
-- publications of objects, attributes and interactions.  Note that when the
-- returned 'Region' is garbage-collected the corresponding region in the RTI
-- will be deleted, and as a consequence will no longer be associated with any
-- subscriptions or publications.
createRegion :: RTIAmbassador t -> SpaceHandle -> ULong -> IO Region
createRegion rtiAmb theSpace numberOfExtents =
    withRTIAmbassador rtiAmb $ \rtiAmb -> do
        r <- wrapExceptions (FFI.createRegion rtiAmb theSpace numberOfExtents)
        r <- newForeignPtr r (deleteRegion r)
        return (Region r)
    where 
        -- TODO!  Figure out some way to enforce that the RTIAmbassador _not_
        -- be deleted before the Regions it created (but still allow them
        -- to be collected before the RTIAmbassador)
        
        -- One idea:
        -- RTIAmbassador gets a new field of type MVar (S.Set (Ptr Region))
        -- containing pointers to all existing regions.  Region gets a hidden 
        -- field referencing the RTIAmbassador (if the reference from the
        -- finalizer isn't enough).  When a Region is finalized, it
        -- takes the RTIAmbassador's MVar, deletes itself if it was in the set,
        -- and puts the MVar back with itself removed from the Set.
        -- When the RTIAmbassador is deleted (which can only occur when 
        -- it _AND_ all Regions become unreachable), it takes the MVar,
        -- deletes _ALL_ regions, and puts the MVar back as S.empty.  Then
        -- it deletes itself.
        deleteRegion theRegion =
            withRTIAmbassador rtiAmb $ \rtiAmb ->
                wrapExceptions (FFI.deleteRegion rtiAmb theRegion)

-- |This must be called if an existing region is modified in any way, in order
-- to inform the RTI of the changes.
notifyAboutRegionModification :: RTIAmbassador t -> Region -> IO ()
notifyAboutRegionModification rtiAmb theRegion =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            wrapExceptions (FFI.notifyAboutRegionModification rtiAmb theRegion)

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
                        Just theObject -> useAsCString theObject $ \theObject ->
                            wrapExceptions (FFI.registerObjectInstanceWithRegion_withName rtiAmb theClass theObject theAttributes theRegions (fromIntegral theNumberOfHandles))

associateRegionForUpdates :: RTIAmbassador t -> Region -> ObjectHandle -> IO ()
associateRegionForUpdates rtiAmb theRegion theObject =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            wrapExceptions (FFI.associateRegionForUpdates rtiAmb theRegion theObject)

unassociateRegionForUpdates :: RTIAmbassador t -> Region -> ObjectHandle -> IO ()
unassociateRegionForUpdates rtiAmb theRegion theObject =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            wrapExceptions (FFI.unassociateRegionForUpdates rtiAmb theRegion theObject)

subscribeObjectClassAttributesWithRegion :: RTIAmbassador t -> ObjectClassHandle -> Region -> S.Set AttributeHandle -> Bool -> IO ()
subscribeObjectClassAttributesWithRegion rtiAmb theClass theRegion attributeList active =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            withAttributeHandleSet attributeList $ \attributeList ->
                wrapExceptions (FFI.subscribeObjectClassAttributesWithRegion rtiAmb theClass theRegion attributeList active)

unsubscribeObjectClassWithRegion :: RTIAmbassador t -> ObjectClassHandle -> Region -> IO ()
unsubscribeObjectClassWithRegion rtiAmb theClass theRegion =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            wrapExceptions (FFI.unsubscribeObjectClassWithRegion rtiAmb theClass theRegion)

subscribeInteractionClassWithRegion :: RTIAmbassador t -> InteractionClassHandle -> Region -> Bool -> IO ()
subscribeInteractionClassWithRegion rtiAmb theClass theRegion active = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            wrapExceptions (FFI.subscribeInteractionClassWithRegion rtiAmb theClass theRegion active)


unsubscribeInteractionClassWithRegion :: RTIAmbassador t -> InteractionClassHandle -> Region -> IO ()
unsubscribeInteractionClassWithRegion rtiAmb theClass theRegion = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            wrapExceptions (FFI.unsubscribeInteractionClassWithRegion rtiAmb theClass theRegion)

sendInteractionWithRegionAtTime :: FedTimeImpl t => RTIAmbassador t -> InteractionClassHandle -> M.Map ParameterHandle ByteString -> FedTime t -> ByteString -> Region -> IO EventRetractionHandle
sendInteractionWithRegionAtTime rtiAmb theInteraction theParameters theTime theTag theRegion =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withParameterHandleValuePairSet theParameters $ \theParameters ->
            withFedTimeIn theTime $ \theTime ->
                useAsCString theTag $ \theTag ->
                    withRegion theRegion $ \theRegion ->
                        withEventRetractionHandleReturn $ \u fh ->
                            wrapExceptions (FFI.sendInteractionWithRegionAtTime rtiAmb theInteraction theParameters theTime theTag theRegion u fh)

sendInteractionWithRegion :: RTIAmbassador t -> InteractionClassHandle -> M.Map ParameterHandle ByteString -> ByteString -> Region -> IO ()
sendInteractionWithRegion rtiAmb theInteraction theParameters theTag theRegion =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withParameterHandleValuePairSet theParameters $ \theParameters ->
            useAsCString theTag $ \theTag ->
                withRegion theRegion $ \theRegion ->
                    wrapExceptions (FFI.sendInteractionWithRegion rtiAmb theInteraction theParameters theTag theRegion)

requestClassAttributeValueUpdateWithRegion :: RTIAmbassador t -> ObjectClassHandle -> S.Set AttributeHandle -> Region -> IO ()
requestClassAttributeValueUpdateWithRegion rtiAmb theClass theAttributes theRegion =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withAttributeHandleSet theAttributes $ \theAttributes ->
            withRegion theRegion $ \theRegion ->
                wrapExceptions (FFI.requestClassAttributeValueUpdateWithRegion rtiAmb theClass theAttributes theRegion)

---------------------------
-- * RTI Support Services
---------------------------

getObjectClassHandle :: RTIAmbassador t -> ByteString -> IO ObjectClassHandle
getObjectClassHandle rtiAmb theName = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        useAsCString theName $ \theName ->
            wrapExceptions (FFI.getObjectClassHandle rtiAmb theName)

getObjectClassName :: RTIAmbassador t -> ObjectClassHandle -> IO ByteString
getObjectClassName rtiAmb theHandle = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getObjectClassName rtiAmb theHandle)
    unsafePackNewCString cStr
    
getAttributeHandle :: RTIAmbassador t -> ByteString -> ObjectClassHandle -> IO AttributeHandle
getAttributeHandle rtiAmb theName whichClass = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        useAsCString theName $ \theName -> 
            wrapExceptions (FFI.getAttributeHandle rtiAmb theName whichClass)

getAttributeName :: RTIAmbassador t -> AttributeHandle -> ObjectClassHandle -> IO ByteString
getAttributeName rtiAmb theHandle whichClass = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getAttributeName rtiAmb theHandle whichClass)
    unsafePackNewCString cStr

getInteractionClassHandle :: RTIAmbassador t -> ByteString -> IO InteractionClassHandle
getInteractionClassHandle rtiAmb theName = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        useAsCString theName $ \theName ->
            wrapExceptions (FFI.getInteractionClassHandle rtiAmb theName)

getInteractionClassName :: RTIAmbassador t -> InteractionClassHandle -> IO ByteString
getInteractionClassName rtiAmb theHandle = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getInteractionClassName rtiAmb theHandle)
    unsafePackNewCString cStr

getParameterHandle :: RTIAmbassador t -> ByteString -> InteractionClassHandle -> IO ParameterHandle
getParameterHandle rtiAmb theName whichClass =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        useAsCString theName $ \theName ->
            wrapExceptions (FFI.getParameterHandle rtiAmb theName whichClass)

getParameterName :: RTIAmbassador t -> ParameterHandle -> InteractionClassHandle -> IO ByteString
getParameterName rtiAmb theHandle whichClass = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getParameterName rtiAmb theHandle whichClass)
    unsafePackNewCString cStr

getObjectInstanceHandle :: RTIAmbassador t -> ByteString -> IO ObjectHandle
getObjectInstanceHandle rtiAmb theName =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        useAsCString theName $ \theName ->
            wrapExceptions (FFI.getObjectInstanceHandle rtiAmb theName)

getObjectInstanceName :: RTIAmbassador t -> ObjectHandle -> IO ByteString
getObjectInstanceName rtiAmb theHandle = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getObjectInstanceName rtiAmb theHandle)
    unsafePackNewCString cStr

getRoutingSpaceHandle :: RTIAmbassador t -> ByteString -> IO SpaceHandle
getRoutingSpaceHandle rtiAmb theName =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        useAsCString theName $ \theName ->
            wrapExceptions (FFI.getRoutingSpaceHandle rtiAmb theName)

getRoutingSpaceName :: RTIAmbassador t -> SpaceHandle -> IO ByteString
getRoutingSpaceName rtiAmb theHandle = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getRoutingSpaceName rtiAmb theHandle)
    unsafePackNewCString cStr

getDimensionHandle :: RTIAmbassador t -> ByteString -> SpaceHandle -> IO DimensionHandle
getDimensionHandle rtiAmb theName whichSpace =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        useAsCString theName $ \theName ->
            wrapExceptions (FFI.getDimensionHandle rtiAmb theName whichSpace)

getDimensionName :: RTIAmbassador t -> DimensionHandle -> SpaceHandle -> IO ByteString
getDimensionName rtiAmb theHandle whichSpace = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getDimensionName rtiAmb theHandle whichSpace)
    unsafePackNewCString cStr

getAttributeRoutingSpaceHandle :: RTIAmbassador t -> AttributeHandle -> ObjectClassHandle -> IO SpaceHandle
getAttributeRoutingSpaceHandle rtiAmb theHandle whichClass =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        wrapExceptions (FFI.getAttributeRoutingSpaceHandle rtiAmb theHandle whichClass)

getObjectClass :: RTIAmbassador t -> ObjectHandle -> IO ObjectClassHandle
getObjectClass rtiAmb theObject =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        wrapExceptions (FFI.getObjectClass rtiAmb theObject)

getInteractionRoutingSpaceHandle :: RTIAmbassador t -> InteractionClassHandle -> IO SpaceHandle
getInteractionRoutingSpaceHandle rtiAmb theHandle =
    withRTIAmbassador rtiAmb $ \rtiAmb -> 
        wrapExceptions (FFI.getInteractionRoutingSpaceHandle rtiAmb theHandle)

getTransportationHandle :: RTIAmbassador t -> ByteString -> IO TransportationHandle
getTransportationHandle rtiAmb theName =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        useAsCString theName $ \theName ->
            wrapExceptions (FFI.getTransportationHandle rtiAmb theName)

getTransportationName :: RTIAmbassador t -> TransportationHandle -> IO ByteString
getTransportationName rtiAmb theHandle = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getTransportationName rtiAmb theHandle)
    unsafePackNewCString cStr

getOrderingHandle :: RTIAmbassador t -> ByteString -> IO OrderingHandle
getOrderingHandle rtiAmb theName =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        useAsCString theName $ \theName ->
            wrapExceptions (FFI.getOrderingHandle rtiAmb theName)

getOrderingName :: RTIAmbassador t -> OrderingHandle -> IO ByteString
getOrderingName rtiAmb theHandle = do
    cStr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getOrderingName rtiAmb theHandle)
    unsafePackNewCString cStr

-- |Enable the sending of \"Start/Stop Object Registration\" messages to the
-- federate.  These messages indicate, for each object class, whether any
-- other federate cares about the existence of objects of that class.
enableClassRelevanceAdvisorySwitch :: RTIAmbassador t -> IO ()
enableClassRelevanceAdvisorySwitch rtiAmb = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.enableClassRelevanceAdvisorySwitch rtiAmb)
    
-- |Disable the sending of \"Start/Stop Object Registration\" messages to the
-- federate (see 'enableClassRelevanceAdvisorySwitch').
disableClassRelevanceAdvisorySwitch :: RTIAmbassador t -> IO ()
disableClassRelevanceAdvisorySwitch rtiAmb = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.disableClassRelevanceAdvisorySwitch rtiAmb)

-- |Enables the sending of \"Turn updates on/off\" messages to this federate
-- which indicate whether or not there are federates interested in each 
-- attribute's values.
enableAttributeRelevanceAdvisorySwitch :: RTIAmbassador t -> IO ()
enableAttributeRelevanceAdvisorySwitch rtiAmb = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.enableAttributeRelevanceAdvisorySwitch rtiAmb)
    
-- |Disables the sending of \"Turn updates on/off\" messages to this federate
-- which indicate whether or not there are federates interested in each 
-- attribute's values.
disableAttributeRelevanceAdvisorySwitch :: RTIAmbassador t -> IO ()
disableAttributeRelevanceAdvisorySwitch rtiAmb = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.disableAttributeRelevanceAdvisorySwitch rtiAmb)

-- |Enables the sending of \"Attributes In/Out of Scope\" messages to this 
-- federate, which indicate the presence or absence (respectively) of other
-- federates capable of setting attributes this federate is subscribed to.
enableAttributeScopeAdvisorySwitch :: RTIAmbassador t -> IO ()
enableAttributeScopeAdvisorySwitch rtiAmb = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.enableAttributeScopeAdvisorySwitch rtiAmb)
    
-- |Disables the sending of \"Attributes In/Out of Scope\" messages to this 
-- federate, which indicate the presence or absence (respectively) of other
-- federates capable of setting attributes this federate is subscribed to.
disableAttributeScopeAdvisorySwitch :: RTIAmbassador t -> IO ()
disableAttributeScopeAdvisorySwitch rtiAmb = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.disableAttributeScopeAdvisorySwitch rtiAmb)

-- |Enables the sending of \"Turn Interactions On/Off\" messages to this
-- federate, which indicate that there are or are not (respectively) one or
-- more remote federates interested in interactions of any given class.
enableInteractionRelevanceAdvisorySwitch :: RTIAmbassador t -> IO ()
enableInteractionRelevanceAdvisorySwitch rtiAmb = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.enableInteractionRelevanceAdvisorySwitch rtiAmb)
    
-- |Disables the sending of \"Turn Interactions On/Off\" messages to this
-- federate, which indicate that there are or are not (respectively) one or
-- more remote federates interested in interactions of any given class.
disableInteractionRelevanceAdvisorySwitch :: RTIAmbassador t -> IO ()
disableInteractionRelevanceAdvisorySwitch rtiAmb = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.disableInteractionRelevanceAdvisorySwitch rtiAmb)

tick :: RTIAmbassador t -> IO Bool
tick rtiAmb =
    withRTIAmbassador rtiAmb
        (wrapExceptions . FFI.tick)

tick_minimum_maximum :: RTIAmbassador t -> TickTime -> TickTime -> IO Bool
tick_minimum_maximum rtiAmb min max = 
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.tick_minimum_maximum rtiAmb min max)

getRegionToken :: RTIAmbassador t -> Region -> IO RegionToken
getRegionToken rtiAmb theRegion =
    withRTIAmbassador rtiAmb $ \rtiAmb ->
        withRegion theRegion $ \theRegion ->
            wrapExceptions (FFI.getRegionToken rtiAmb theRegion)

-- |WARNING: the header did not say anything about the return convention of 
-- this function, so I don't really know how long these Region objects live.
-- My guess would be that they live about as long as the RTIAmbassador, but
-- I'm not sure.
getRegion :: RTIAmbassador t -> RegionToken -> IO Region
getRegion rtiAmb theRegion = do
    regionPtr <- withRTIAmbassador rtiAmb $ \rtiAmb ->
        wrapExceptions (FFI.getRegion rtiAmb theRegion)
    fPtr <- newForeignPtr_ regionPtr
    return (Region fPtr)
    
