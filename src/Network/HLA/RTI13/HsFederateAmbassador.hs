{-# LANGUAGE
        ForeignFunctionInterface, GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts, UndecidableInstances
  #-}
module Network.HLA.RTI13.HsFederateAmbassador where

import Foreign hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C.String
import System.IO.Unsafe
import Control.Monad.Reader
import Control.Exception

import Network.HLA.RTI13.BaseTypes
import Network.HLA.RTI13.RTITypes
import Network.HLA.RTI13.RTIException

-- |Parameterized over type of FedTime
newtype HsFederateAmbassador t = HsFederateAmbassador (ForeignPtr (HsFederateAmbassador t))
instance FedTimeImpl t => FederateAmbassador (HsFederateAmbassador t) where
    type FedAmbTime (HsFederateAmbassador t) = t
    withFederateAmbassador fedAmb action = 
        withHsFederateAmbassador fedAmb (action . castPtr)

withHsFederateAmbassador (HsFederateAmbassador fedAmb) = withForeignPtr fedAmb

newtype FedHandlers t a = FedHandlers (ReaderT (HsFederateAmbassador t) IO a)
    deriving (Functor, Monad, MonadIO, MonadReader (HsFederateAmbassador t))

setHandlers :: HsFederateAmbassador t -> FedHandlers t () -> IO ()
setHandlers fedAmb (FedHandlers handlers) = runReaderT handlers fedAmb

foreign import ccall unsafe "hsFederateAmb.h wrap_new_HsFederateAmbassador"
    wrap_new_HsFederateAmbassador :: FunPtr (FunPtr a -> IO ()) -> Ptr (Ptr RTIException) -> IO (Ptr (HsFederateAmbassador t))

foreign import ccall "hsFederateAmb.h wrap_delete_HsFederateAmbassador"
    wrap_delete_HsFederateAmbassador :: Ptr (HsFederateAmbassador t) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrapper" mkFreeFunPtr :: (FunPtr a                -> IO ()) -> IO (FunPtr (FunPtr a               -> IO ()))
foreign import ccall "wrapper" mkPtrFunPtr  :: (Ptr a                   -> IO ()) -> IO (FunPtr (Ptr a                  -> IO ()))
foreign import ccall "wrapper" mkICHFunPtr  :: (InteractionClassHandle  -> IO ()) -> IO (FunPtr (InteractionClassHandle -> IO ()))

foreign import ccall "wrapper" mkObjectInstanceFunPtr :: 
    (ObjectHandle -> ObjectClassHandle -> CString -> IO ())
    -> IO (FunPtr (ObjectHandle -> ObjectClassHandle -> CString -> IO ()))

foreign import ccall "wrapper" mkObjectPtrFunPtr ::
    (ObjectHandle -> Ptr a -> IO ())
    -> IO (FunPtr (ObjectHandle -> Ptr a -> IO ()))

{-# NOINLINE freeHaskellFunPtrPtr #-}
freeHaskellFunPtrPtr :: FunPtr (FunPtr a -> IO ())
freeHaskellFunPtrPtr = unsafePerformIO (mkFreeFunPtr freeHaskellFunPtr)

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

foreign import ccall "hsFederateAmb.h hsfa_set_synchronizationPointRegistrationSucceeded"
    hsfa_set_synchronizationPointRegistrationSucceeded :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> IO ()) -> IO ()

onSynchronizationPointRegistrationSucceeded :: (String -> IO ()) -> FedHandlers t ()
onSynchronizationPointRegistrationSucceeded synchronizationPointRegistrationSucceeded = do
    fedAmb <- ask
    liftIO (set_synchronizationPointRegistrationSucceeded fedAmb synchronizationPointRegistrationSucceeded)

set_synchronizationPointRegistrationSucceeded :: HsFederateAmbassador t -> (String -> IO ()) -> IO ()
set_synchronizationPointRegistrationSucceeded fedAmb synchronizationPointRegistrationSucceeded =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkPtrFunPtr $ \cstr -> do
            str <- peekCString cstr
            synchronizationPointRegistrationSucceeded str
        hsfa_set_synchronizationPointRegistrationSucceeded fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_synchronizationPointRegistrationFailed"
    hsfa_set_synchronizationPointRegistrationFailed :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> IO ()) -> IO ()

onSynchronizationPointRegistrationFailed :: (String -> IO ()) -> FedHandlers t ()
onSynchronizationPointRegistrationFailed synchronizationPointRegistrationFailed = do
    fedAmb <- ask
    liftIO (set_synchronizationPointRegistrationFailed fedAmb synchronizationPointRegistrationFailed)

set_synchronizationPointRegistrationFailed :: HsFederateAmbassador t -> (String -> IO ()) -> IO ()
set_synchronizationPointRegistrationFailed fedAmb synchronizationPointRegistrationFailed =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkPtrFunPtr $ \cstr -> do
            str <- peekCString cstr
            synchronizationPointRegistrationFailed str
        hsfa_set_synchronizationPointRegistrationFailed fedAmb funPtr


----------------------------
-- Declaration Management --
----------------------------

foreign import ccall "hsFederateAmb.h hsfa_set_turnInteractionsOn"
    hsfa_set_turnInteractionsOn :: Ptr (HsFederateAmbassador t) -> FunPtr (InteractionClassHandle -> IO ()) -> IO ()

onTurnInteractionsOn :: (InteractionClassHandle -> IO ()) -> FedHandlers t ()
onTurnInteractionsOn turnInteractionsOn = do
    fedAmb <- ask
    liftIO (set_turnInteractionsOn fedAmb turnInteractionsOn)

set_turnInteractionsOn :: HsFederateAmbassador t -> (InteractionClassHandle -> IO ()) -> IO ()
set_turnInteractionsOn fedAmb turnInteractionsOn = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkICHFunPtr turnInteractionsOn
        hsfa_set_turnInteractionsOn fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_turnInteractionsOff"
    hsfa_set_turnInteractionsOff :: Ptr (HsFederateAmbassador t) -> FunPtr (InteractionClassHandle -> IO ()) -> IO ()

onTurnInteractionsOff :: (InteractionClassHandle -> IO ()) -> FedHandlers t ()
onTurnInteractionsOff turnInteractionsOff = do
    fedAmb <- ask
    liftIO (set_turnInteractionsOff fedAmb turnInteractionsOff)

set_turnInteractionsOff :: HsFederateAmbassador t -> (InteractionClassHandle -> IO ()) -> IO ()
set_turnInteractionsOff fedAmb turnInteractionsOff = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkICHFunPtr turnInteractionsOff
        hsfa_set_turnInteractionsOff fedAmb funPtr

-----------------------
-- Object Management --
-----------------------

foreign import ccall "hsFederateAmb.h hsfa_set_discoverObjectInstance"
    hsfa_set_discoverObjectInstance :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> ObjectClassHandle -> CString -> IO ()) -> IO ()

onDiscoverObjectInstance :: (ObjectHandle -> ObjectClassHandle -> String -> IO ()) -> FedHandlers t ()
onDiscoverObjectInstance discoverObjectInstance = do
    fedAmb <- ask
    liftIO (set_discoverObjectInstance fedAmb discoverObjectInstance)

set_discoverObjectInstance :: HsFederateAmbassador t -> (ObjectHandle -> ObjectClassHandle -> String -> IO ()) -> IO ()
set_discoverObjectInstance fedAmb discoverObjectInstance = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkObjectInstanceFunPtr $ \theObject theObjectHandle theName -> do
            theName <- peekCString theName
            evaluate (length theName)
            discoverObjectInstance theObject theObjectHandle theName
        hsfa_set_discoverObjectInstance fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_reflectAttributeValues"
    hsfa_set_reflectAttributeValues :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleValuePairSet -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ()) -> IO ()

foreign import ccall "wrapper"
    mkReflectAttributeValuesFunPtr :: (ObjectHandle -> Ptr AttributeHandleValuePairSet -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ())
        -> IO (FunPtr (ObjectHandle -> Ptr AttributeHandleValuePairSet -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ()))

onReflectAttributeValues :: FedTimeImpl t => (ObjectHandle -> AttributeHandleValuePairSet -> String -> Maybe (FedTimeRepr t, EventRetractionHandle) -> IO ()) -> FedHandlers t ()
onReflectAttributeValues reflectAttributeValues = do
    fedAmb <- ask
    liftIO (set_reflectAttributeValues fedAmb reflectAttributeValues)

set_reflectAttributeValues :: FedTimeImpl t => HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleValuePairSet -> String -> Maybe (FedTimeRepr t, EventRetractionHandle) -> IO ()) -> IO ()
set_reflectAttributeValues fedAmb reflectAttributeValues =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkReflectAttributeValuesFunPtr $ \theObject theAttrs theTime theTag theHandleSerial theHandleFed -> do
            theTag <- peekCString theTag
            theAttrs <- fmap AttributeHandleValuePairSet (newForeignPtr_ theAttrs)
            
            if theTime == nullPtr
                then reflectAttributeValues theObject theAttrs theTag Nothing
                else do
                    theTime <- importFedTime theTime
                    reflectAttributeValues theObject theAttrs theTag (Just (theTime, EventRetractionHandle theHandleSerial theHandleFed))
        hsfa_set_reflectAttributeValues fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_receiveInteraction"
    hsfa_set_receiveInteraction :: Ptr (HsFederateAmbassador t) -> FunPtr (InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ()) -> IO ()

foreign import ccall "wrapper"
    mkReceiveInteractionFunPtr :: (InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ())
        -> IO (FunPtr (InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ()))

onReceiveInteraction :: FedTimeImpl t => (InteractionClassHandle -> ParameterHandleValuePairSet -> String -> Maybe (FedTimeRepr t, EventRetractionHandle) -> IO ()) -> FedHandlers t ()
onReceiveInteraction receiveInteraction = do
    fedAmb <- ask
    liftIO (set_receiveInteraction fedAmb receiveInteraction)

set_receiveInteraction :: FedTimeImpl t => HsFederateAmbassador t -> (InteractionClassHandle -> ParameterHandleValuePairSet -> String -> Maybe (FedTimeRepr t, EventRetractionHandle) -> IO ()) -> IO ()
set_receiveInteraction fedAmb receiveInteraction =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkReceiveInteractionFunPtr $ \theInteraction theParameters theTime theTag theHandleSerial theHandleFed -> do
            theTag <- peekCString theTag
            theParameters <- fmap ParameterHandleValuePairSet (newForeignPtr_ theParameters)
            
            if theTime == nullPtr
                then receiveInteraction theInteraction theParameters theTag Nothing
                else do
                    theTime <- importFedTime theTime
                    receiveInteraction theInteraction theParameters theTag $ Just (theTime, EventRetractionHandle theHandleSerial theHandleFed)
        hsfa_set_receiveInteraction fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_removeObjectInstance"
    hsfa_set_removeObjectInstance :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ()) -> IO ()

onRemoveObjectInstance :: FedTimeImpl t => (ObjectHandle -> String -> Maybe (FedTimeRepr t, EventRetractionHandle) -> IO ()) -> FedHandlers t ()
onRemoveObjectInstance removeObjectInstance = do
    fedAmb <- ask
    liftIO (set_removeObjectInstance fedAmb removeObjectInstance)

foreign import ccall "wrapper" 
    mkRemoveObjectInstanceFunPtr
        :: (ObjectHandle -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ())
        -> IO (FunPtr (ObjectHandle -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ()))
set_removeObjectInstance :: FedTimeImpl t => HsFederateAmbassador t -> (ObjectHandle -> String -> Maybe (FedTimeRepr t, EventRetractionHandle) -> IO ()) -> IO ()
set_removeObjectInstance fedAmb removeObjectInstance = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkRemoveObjectInstanceFunPtr $ \theObject theTime theTag theHandleSerial theHandleFederate -> do
            theTag <- peekCString theTag
            if theTime == nullPtr
                then removeObjectInstance theObject theTag Nothing
                else do
                    theTime <- importFedTime theTime
                    removeObjectInstance theObject theTag (Just (theTime, EventRetractionHandle theHandleSerial theHandleFederate))
        
        hsfa_set_removeObjectInstance fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_provideAttributeValueUpdate"
    hsfa_set_provideAttributeValueUpdate :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

foreign import ccall "wrapper" 
    mkProvideAttributeValueFunPtr :: (ObjectHandle -> Ptr AttributeHandleSet -> IO ())
        -> IO (FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()))

onProvideAttributeValueUpdate :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onProvideAttributeValueUpdate provideAttributeValueUpdate = do
    fedAmb <- ask
    liftIO (set_provideAttributeValueUpdate fedAmb provideAttributeValueUpdate)

set_provideAttributeValueUpdate :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_provideAttributeValueUpdate fedAmb provideAttributeValueUpdate = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkProvideAttributeValueFunPtr $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            provideAttributeValueUpdate theObject (AttributeHandleSet theAttrs)
        hsfa_set_provideAttributeValueUpdate fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_turnUpdatesOnForObjectInstance"
    hsfa_set_turnUpdatesOnForObjectInstance :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

onTurnUpdatesOnForObjectInstance :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onTurnUpdatesOnForObjectInstance turnUpdatesOnForObjectInstance = do
    fedAmb <- ask
    liftIO (set_turnUpdatesOnForObjectInstance fedAmb turnUpdatesOnForObjectInstance)

set_turnUpdatesOnForObjectInstance :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_turnUpdatesOnForObjectInstance fedAmb turnUpdatesOnForObjectInstance = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkObjectPtrFunPtr $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            turnUpdatesOnForObjectInstance theObject (AttributeHandleSet theAttrs)
        hsfa_set_turnUpdatesOnForObjectInstance fedAmb funPtr


-----------------------------------
-- Ownership Management Services --
-----------------------------------


---------------------
-- Time Management --
---------------------

foreign import ccall "hsFederateAmb.h hsfa_set_timeRegulationEnabled"
    hsfa_set_timeRegulationEnabled :: Ptr (HsFederateAmbassador t) -> FunPtr (Ptr t -> IO ()) -> IO ()

onTimeRegulationEnabled :: FedTimeImpl t => (FedTimeRepr t -> IO ()) -> FedHandlers t ()
onTimeRegulationEnabled timeRegulationEnabled = do
    fedAmb <- ask
    liftIO (set_timeRegulationEnabled fedAmb timeRegulationEnabled)

set_timeRegulationEnabled :: FedTimeImpl t => HsFederateAmbassador t -> (FedTimeRepr t -> IO ()) -> IO ()
set_timeRegulationEnabled fedAmb timeRegulationEnabled = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkPtrFunPtr $ \ftPtr -> do
            someFedTime <- importFedTime ftPtr
            timeRegulationEnabled someFedTime
        hsfa_set_timeRegulationEnabled fedAmb funPtr

