{-# LANGUAGE
        DeriveDataTypeable,
        ForeignFunctionInterface
  #-}
module Network.HLA.RTI13.RTIException where

import Network.HLA.RTI13.BaseTypes
import System.IO.Unsafe
import qualified Data.IntMap as IM
import Data.StateRef
import Foreign.C.String
import Foreign.Ptr
import Foreign
import Unsafe.Coerce
import Data.Generics
import Control.Exception

data RTIException = RTIException
    { exceptionName   :: !RTIExceptionName
    , exceptionSerial :: !ULong
    , exceptionReason :: !String
    } deriving (Eq, Ord, Data, Typeable)

instance Show RTIException where
    showsPrec p (RTIException name serial reason) = 
        showParen (p > 10)
            ( showsPrec 11 name 
            . showParen True (shows serial)
            . showString ": "
            . shows reason
            )
instance Exception RTIException

{-# INLINE wrapExceptions #-}
wrapExceptions :: (Ptr (Ptr RTIException) -> IO a) -> IO a
wrapExceptions f = alloca $ \p -> do
    poke p nullPtr
    x <- f p
    exc <- peek p
    case ptrToIntPtr exc of
        0   -> return x
        -1  -> do
            fail "unknown exception"
        _   -> do
            throwRTIException exc

foreign import ccall unsafe "wrap/baseTypes.h delete_Exception" 
    delete_Exception :: Ptr RTIException -> IO ()

foreign import ccall "wrap/baseTypes.h dissect_Exception"
    dissect_Exception :: Ptr RTIException -> Ptr ULong -> Ptr CString -> Ptr CString -> IO ()

importException :: Ptr RTIException -> IO RTIException
importException pExc = 
    alloca $ \pSerial -> 
        alloca $ \pReason ->
            alloca $ \pName -> do
                dissect_Exception pExc pSerial pReason pName
                
                serial <- peek pSerial
                reason <- peekCString =<< peek pReason
                name   <- lookupRTIExceptionName =<< peek pName
                
                return (RTIException name serial reason)

throwRTIException :: Ptr RTIException ->  IO a
throwRTIException e = do
    hs_exception <- importException e
    evaluate hs_exception
    delete_Exception e
    
    throw hs_exception


-- table mapping "_ex" pointer value to "RTIExceptionName"s
{-# NOINLINE exceptionNameTable #-}
exceptionNameTable :: TVar (IM.IntMap RTIExceptionName)
exceptionNameTable = unsafePerformIO (newReference IM.empty)

readRTIExceptionName :: CString -> IO RTIExceptionName
readRTIExceptionName name = do
    name <- peekCString name
    evaluate (length name)  -- is this necessary? (is peekCString strict or lazy?)
    
    return $ case reads name of
        [(excName, "")] -> excName
        other           -> UnknownRTIException name

lookupRTIExceptionName :: CString -> IO RTIExceptionName
lookupRTIExceptionName name = do
    let ptr :: Int
        ptr = unsafeCoerce (ptrToIntPtr name)
    
    readName <- unsafeInterleaveIO (readRTIExceptionName name)
    
    atomically $ do
        entry <- readsRef exceptionNameTable (IM.lookup ptr)
        case entry of 
            Nothing -> readName `seq` do
                modifyReference exceptionNameTable (IM.insert ptr readName)
                return readName
            Just name -> return name

data RTIExceptionName
    = ArrayIndexOutOfBounds
    | AsynchronousDeliveryAlreadyDisabled
    | AsynchronousDeliveryAlreadyEnabled  
    | AttributeAcquisitionWasNotRequested
    | AttributeAcquisitionWasNotCanceled  
    | AttributeAlreadyBeingAcquired  
    | AttributeAlreadyBeingDivested  
    | AttributeAlreadyOwned
    | AttributeDivestitureWasNotRequested  
    | AttributeNotDefined
    | AttributeNotKnown
    | AttributeNotOwned
    | AttributeNotPublished
    | ConcurrentAccessAttempted
    | CouldNotDiscover
    | CouldNotOpenFED
    | CouldNotRestore
    | DeletePrivilegeNotHeld
    | DimensionNotDefined
    | EnableTimeConstrainedPending
    | EnableTimeConstrainedWasNotPending
    | EnableTimeRegulationPending
    | EnableTimeRegulationWasNotPending  
    | ErrorReadingFED
    | EventNotKnown
    | FederateAlreadyExecutionMember
    | FederateInternalError
    | FederateLoggingServiceCalls
    | FederateNotExecutionMember
    | FederateOwnsAttributes
    | FederateWasNotAskedToReleaseAttribute  
    | FederatesCurrentlyJoined
    | FederationExecutionAlreadyExists
    | FederationExecutionDoesNotExist
    | FederationTimeAlreadyPassed
    | HandleValuePairMaximumExceeded
    | InteractionClassNotDefined
    | InteractionClassNotKnown
    | InteractionClassNotPublished
    | InteractionClassNotSubscribed  
    | InteractionParameterNotDefined
    | InteractionParameterNotKnown
    | InvalidExtents
    | InvalidFederationTime
    | InvalidHandleValuePairSetContext
    | InvalidLookahead
    | InvalidOrderingHandle
    | InvalidRegionContext  
    | InvalidResignAction
    | InvalidRetractionHandle
    | InvalidTransportationHandle
    | MemoryExhausted
    | NameNotFound
    | ObjectClassNotDefined
    | ObjectClassNotKnown
    | ObjectClassNotPublished
    | ObjectClassNotSubscribed
    | ObjectNotKnown
    | ObjectAlreadyRegistered
    | OwnershipAcquisitionPending
    | RegionInUse
    | RegionNotKnown
    | RestoreInProgress  
    | RestoreNotRequested
    | RTIinternalError
    | SpaceNotDefined
    | SaveInProgress
    | SaveNotInitiated
    | SpecifiedSaveLabelDoesNotExist
    | SynchronizationPointLabelWasNotAnnounced  
    | TimeAdvanceAlreadyInProgress
    | TimeAdvanceWasNotInProgress
    | TimeConstrainedAlreadyEnabled
    | TimeConstrainedWasNotEnabled
    | TimeRegulationAlreadyEnabled
    | TimeRegulationWasNotEnabled
    | UnableToPerformSave
    | ValueCountExceeded
    | ValueLengthExceeded
    | UnknownRTIException !String
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- selectors for catchJust
rtiException :: RTIException -> Maybe RTIException
rtiException = Just

arrayIndexOutOfBounds e 
    | exceptionName e == ArrayIndexOutOfBounds = Just e
    | otherwise = Nothing 
    
asynchronousDeliveryAlreadyDisabled e 
    | exceptionName e == AsynchronousDeliveryAlreadyDisabled = Just e
    | otherwise = Nothing 
    
asynchronousDeliveryAlreadyEnabled e 
    | exceptionName e == AsynchronousDeliveryAlreadyEnabled = Just e
    | otherwise = Nothing 
    
attributeAcquisitionWasNotRequested e 
    | exceptionName e == AttributeAcquisitionWasNotRequested = Just e
    | otherwise = Nothing 
    
attributeAcquisitionWasNotCanceled e 
    | exceptionName e == AttributeAcquisitionWasNotCanceled = Just e
    | otherwise = Nothing 
    
attributeAlreadyBeingAcquired e 
    | exceptionName e == AttributeAlreadyBeingAcquired = Just e
    | otherwise = Nothing 
    
attributeAlreadyBeingDivested e 
    | exceptionName e == AttributeAlreadyBeingDivested = Just e
    | otherwise = Nothing 
    
attributeAlreadyOwned e 
    | exceptionName e == AttributeAlreadyOwned = Just e
    | otherwise = Nothing 
    
attributeDivestitureWasNotRequested e 
    | exceptionName e == AttributeDivestitureWasNotRequested = Just e
    | otherwise = Nothing 
    
attributeNotDefined e 
    | exceptionName e == AttributeNotDefined = Just e
    | otherwise = Nothing 
    
attributeNotKnown e 
    | exceptionName e == AttributeNotKnown = Just e
    | otherwise = Nothing 
    
attributeNotOwned e 
    | exceptionName e == AttributeNotOwned = Just e
    | otherwise = Nothing 
    
attributeNotPublished e 
    | exceptionName e == AttributeNotPublished = Just e
    | otherwise = Nothing 
    
concurrentAccessAttempted e 
    | exceptionName e == ConcurrentAccessAttempted = Just e
    | otherwise = Nothing 
    
couldNotDiscover e 
    | exceptionName e == CouldNotDiscover = Just e
    | otherwise = Nothing 
    
couldNotOpenFED e 
    | exceptionName e == CouldNotOpenFED = Just e
    | otherwise = Nothing 
    
couldNotRestore e 
    | exceptionName e == CouldNotRestore = Just e
    | otherwise = Nothing 
    
deletePrivilegeNotHeld e 
    | exceptionName e == DeletePrivilegeNotHeld = Just e
    | otherwise = Nothing 
    
dimensionNotDefined e 
    | exceptionName e == DimensionNotDefined = Just e
    | otherwise = Nothing 
    
enableTimeConstrainedPending e 
    | exceptionName e == EnableTimeConstrainedPending = Just e
    | otherwise = Nothing 
    
enableTimeConstrainedWasNotPending e 
    | exceptionName e == EnableTimeConstrainedWasNotPending = Just e
    | otherwise = Nothing 
    
enableTimeRegulationPending e 
    | exceptionName e == EnableTimeRegulationPending = Just e
    | otherwise = Nothing 
    
enableTimeRegulationWasNotPending e 
    | exceptionName e == EnableTimeRegulationWasNotPending = Just e
    | otherwise = Nothing 
    
errorReadingFED e 
    | exceptionName e == ErrorReadingFED = Just e
    | otherwise = Nothing 
    
eventNotKnown e 
    | exceptionName e == EventNotKnown = Just e
    | otherwise = Nothing 
    
federateAlreadyExecutionMember e 
    | exceptionName e == FederateAlreadyExecutionMember = Just e
    | otherwise = Nothing 
    
federateInternalError e 
    | exceptionName e == FederateInternalError = Just e
    | otherwise = Nothing 
    
federateLoggingServiceCalls e 
    | exceptionName e == FederateLoggingServiceCalls = Just e
    | otherwise = Nothing 
    
federateNotExecutionMember e 
    | exceptionName e == FederateNotExecutionMember = Just e
    | otherwise = Nothing 
    
federateOwnsAttributes e 
    | exceptionName e == FederateOwnsAttributes = Just e
    | otherwise = Nothing 
    
federateWasNotAskedToReleaseAttribute e 
    | exceptionName e == FederateWasNotAskedToReleaseAttribute = Just e
    | otherwise = Nothing 
    
federatesCurrentlyJoined e 
    | exceptionName e == FederatesCurrentlyJoined = Just e
    | otherwise = Nothing 
    
federationExecutionAlreadyExists e 
    | exceptionName e == FederationExecutionAlreadyExists = Just e
    | otherwise = Nothing 
    
federationExecutionDoesNotExist e 
    | exceptionName e == FederationExecutionDoesNotExist = Just e
    | otherwise = Nothing 
    
federationTimeAlreadyPassed e 
    | exceptionName e == FederationTimeAlreadyPassed = Just e
    | otherwise = Nothing 
    
handleValuePairMaximumExceeded e 
    | exceptionName e == HandleValuePairMaximumExceeded = Just e
    | otherwise = Nothing 
    
interactionClassNotDefined e 
    | exceptionName e == InteractionClassNotDefined = Just e
    | otherwise = Nothing 
    
interactionClassNotKnown e 
    | exceptionName e == InteractionClassNotKnown = Just e
    | otherwise = Nothing 
    
interactionClassNotPublished e 
    | exceptionName e == InteractionClassNotPublished = Just e
    | otherwise = Nothing 
    
interactionClassNotSubscribed e 
    | exceptionName e == InteractionClassNotSubscribed = Just e
    | otherwise = Nothing 
    
interactionParameterNotDefined e 
    | exceptionName e == InteractionParameterNotDefined = Just e
    | otherwise = Nothing 
    
interactionParameterNotKnown e 
    | exceptionName e == InteractionParameterNotKnown = Just e
    | otherwise = Nothing 
    
invalidExtents e 
    | exceptionName e == InvalidExtents = Just e
    | otherwise = Nothing 
    
invalidFederationTime e 
    | exceptionName e == InvalidFederationTime = Just e
    | otherwise = Nothing 
    
invalidHandleValuePairSetContext e 
    | exceptionName e == InvalidHandleValuePairSetContext = Just e
    | otherwise = Nothing 
    
invalidLookahead e 
    | exceptionName e == InvalidLookahead = Just e
    | otherwise = Nothing 
    
invalidOrderingHandle e 
    | exceptionName e == InvalidOrderingHandle = Just e
    | otherwise = Nothing 
    
invalidRegionContext e 
    | exceptionName e == InvalidRegionContext = Just e
    | otherwise = Nothing 
    
invalidResignAction e 
    | exceptionName e == InvalidResignAction = Just e
    | otherwise = Nothing 
    
invalidRetractionHandle e 
    | exceptionName e == InvalidRetractionHandle = Just e
    | otherwise = Nothing 
    
invalidTransportationHandle e 
    | exceptionName e == InvalidTransportationHandle = Just e
    | otherwise = Nothing 
    
memoryExhausted e 
    | exceptionName e == MemoryExhausted = Just e
    | otherwise = Nothing 
    
nameNotFound e 
    | exceptionName e == NameNotFound = Just e
    | otherwise = Nothing 
    
objectClassNotDefined e 
    | exceptionName e == ObjectClassNotDefined = Just e
    | otherwise = Nothing 
    
objectClassNotKnown e 
    | exceptionName e == ObjectClassNotKnown = Just e
    | otherwise = Nothing 
    
objectClassNotPublished e 
    | exceptionName e == ObjectClassNotPublished = Just e
    | otherwise = Nothing 
    
objectClassNotSubscribed e 
    | exceptionName e == ObjectClassNotSubscribed = Just e
    | otherwise = Nothing 
    
objectNotKnown e 
    | exceptionName e == ObjectNotKnown = Just e
    | otherwise = Nothing 
    
objectAlreadyRegistered e 
    | exceptionName e == ObjectAlreadyRegistered = Just e
    | otherwise = Nothing 
    
ownershipAcquisitionPending e 
    | exceptionName e == OwnershipAcquisitionPending = Just e
    | otherwise = Nothing 
    
regionInUse e 
    | exceptionName e == RegionInUse = Just e
    | otherwise = Nothing 
    
regionNotKnown e 
    | exceptionName e == RegionNotKnown = Just e
    | otherwise = Nothing 
    
restoreInProgress e 
    | exceptionName e == RestoreInProgress = Just e
    | otherwise = Nothing 
    
restoreNotRequested e 
    | exceptionName e == RestoreNotRequested = Just e
    | otherwise = Nothing 
    
rTIinternalError e 
    | exceptionName e == RTIinternalError = Just e
    | otherwise = Nothing 
    
spaceNotDefined e 
    | exceptionName e == SpaceNotDefined = Just e
    | otherwise = Nothing 
    
saveInProgress e 
    | exceptionName e == SaveInProgress = Just e
    | otherwise = Nothing 
    
saveNotInitiated e 
    | exceptionName e == SaveNotInitiated = Just e
    | otherwise = Nothing 
    
specifiedSaveLabelDoesNotExist e 
    | exceptionName e == SpecifiedSaveLabelDoesNotExist = Just e
    | otherwise = Nothing 
    
synchronizationPointLabelWasNotAnnounced e 
    | exceptionName e == SynchronizationPointLabelWasNotAnnounced  = Just e
    | otherwise = Nothing 
    
timeAdvanceAlreadyInProgress e 
    | exceptionName e == TimeAdvanceAlreadyInProgress = Just e
    | otherwise = Nothing 
    
timeAdvanceWasNotInProgress e 
    | exceptionName e == TimeAdvanceWasNotInProgress = Just e
    | otherwise = Nothing 
    
timeConstrainedAlreadyEnabled e 
    | exceptionName e == TimeConstrainedAlreadyEnabled = Just e
    | otherwise = Nothing 
    
timeConstrainedWasNotEnabled e 
    | exceptionName e == TimeConstrainedWasNotEnabled = Just e
    | otherwise = Nothing 
    
timeRegulationAlreadyEnabled e 
    | exceptionName e == TimeRegulationAlreadyEnabled = Just e
    | otherwise = Nothing 
    
timeRegulationWasNotEnabled e 
    | exceptionName e == TimeRegulationWasNotEnabled = Just e
    | otherwise = Nothing 
    
unableToPerformSave e 
    | exceptionName e == UnableToPerformSave = Just e
    | otherwise = Nothing 
    
valueCountExceeded e 
    | exceptionName e == ValueCountExceeded = Just e
    | otherwise = Nothing 
    
valueLengthExceeded e 
    | exceptionName e == ValueLengthExceeded = Just e
    | otherwise = Nothing 
    
unknownRTIException e@UnknownRTIException{} = Just e 
unknownRTIException _ = Nothing
