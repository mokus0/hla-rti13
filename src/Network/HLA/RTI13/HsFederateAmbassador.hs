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

foreign import ccall "wrapper" mkFreeFunPtr  :: (FunPtr a                -> IO ()) -> IO (FunPtr (FunPtr a               -> IO ()))
foreign import ccall "wrapper" mkVoidFunPtr  :: (                           IO ()) -> IO (FunPtr (                          IO ()))
foreign import ccall "wrapper" mkPtrFunPtr   :: (Ptr a                   -> IO ()) -> IO (FunPtr (Ptr a                  -> IO ()))
foreign import ccall "wrapper" mkPtrX2FunPtr :: (Ptr a -> Ptr a          -> IO ()) -> IO (FunPtr (Ptr a -> Ptr a         -> IO ()))
foreign import ccall "wrapper" mkICHFunPtr   :: (InteractionClassHandle  -> IO ()) -> IO (FunPtr (InteractionClassHandle -> IO ()))

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

foreign import ccall "hsFederateAmb.h hsfa_set_announceSynchronizationPoint"
    hsfa_set_announceSynchronizationPoint :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> CString -> IO ()) -> IO ()

onAnnounceSynchronizationPoint :: (String -> String -> IO ()) -> FedHandlers t ()
onAnnounceSynchronizationPoint announceSynchronizationPoint = do
    fedAmb <- ask
    liftIO (set_announceSynchronizationPoint fedAmb announceSynchronizationPoint)

set_announceSynchronizationPoint :: HsFederateAmbassador t -> (String -> String -> IO ()) -> IO ()
set_announceSynchronizationPoint fedAmb announceSynchronizationPoint =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkPtrX2FunPtr $ \cstr1 cstr2 -> do
            str1 <- peekCString cstr1
            str2 <- peekCString cstr2
            announceSynchronizationPoint str1 str2
        hsfa_set_announceSynchronizationPoint fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_federationSynchronized"
    hsfa_set_federationSynchronized :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> IO ()) -> IO ()

onFederationSynchronized :: (String -> IO ()) -> FedHandlers t ()
onFederationSynchronized federationSynchronized = do
    fedAmb <- ask
    liftIO (set_federationSynchronized fedAmb federationSynchronized)

set_federationSynchronized :: HsFederateAmbassador t -> (String -> IO ()) -> IO ()
set_federationSynchronized fedAmb federationSynchronized =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkPtrFunPtr $ \cstr -> do
            str <- peekCString cstr
            federationSynchronized str
        hsfa_set_federationSynchronized fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_initiateFederateSave"
    hsfa_set_initiateFederateSave :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> IO ()) -> IO ()

onInitiateFederateSave :: (String -> IO ()) -> FedHandlers t ()
onInitiateFederateSave initiateFederateSave = do
    fedAmb <- ask
    liftIO (set_initiateFederateSave fedAmb initiateFederateSave)

set_initiateFederateSave :: HsFederateAmbassador t -> (String -> IO ()) -> IO ()
set_initiateFederateSave fedAmb initiateFederateSave =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkPtrFunPtr $ \cstr -> do
            str <- peekCString cstr
            initiateFederateSave str
        hsfa_set_initiateFederateSave fedAmb funPtr


foreign import ccall "hsFederateAmb.h hsfa_set_federationSaved"
    hsfa_set_federationSaved :: Ptr (HsFederateAmbassador t) -> FunPtr (IO ()) -> IO ()

onFederationSaved :: IO () -> FedHandlers t ()
onFederationSaved federationSaved = do
    fedAmb <- ask
    liftIO (set_federationSaved fedAmb federationSaved)

set_federationSaved :: HsFederateAmbassador t -> IO () -> IO ()
set_federationSaved fedAmb federationSaved =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkVoidFunPtr federationSaved
        hsfa_set_federationSaved fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_federationNotSaved"
    hsfa_set_federationNotSaved :: Ptr (HsFederateAmbassador t) -> FunPtr (IO ()) -> IO ()

onFederationNotSaved :: IO () -> FedHandlers t ()
onFederationNotSaved federationNotSaved = do
    fedAmb <- ask
    liftIO (set_federationSaved fedAmb federationNotSaved)

set_federationNotSaved :: HsFederateAmbassador t -> IO () -> IO ()
set_federationNotSaved fedAmb federationNotSaved =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkVoidFunPtr federationNotSaved
        hsfa_set_federationNotSaved fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_requestFederationRestoreSucceeded"
    hsfa_set_requestFederationRestoreSucceeded :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> IO ()) -> IO ()

onRequestFederationRestoreSucceeded :: (String -> IO ()) -> FedHandlers t ()
onRequestFederationRestoreSucceeded requestFederationRestoreSucceeded = do
    fedAmb <- ask
    liftIO (set_requestFederationRestoreSucceeded fedAmb requestFederationRestoreSucceeded)

set_requestFederationRestoreSucceeded :: HsFederateAmbassador t -> (String -> IO ()) -> IO ()
set_requestFederationRestoreSucceeded fedAmb requestFederationRestoreSucceeded =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkPtrFunPtr $ \cstr -> do
            str <- peekCString cstr
            requestFederationRestoreSucceeded str
        hsfa_set_requestFederationRestoreSucceeded fedAmb funPtr


foreign import ccall "hsFederateAmb.h hsfa_set_requestFederationRestoreFailed"
    hsfa_set_requestFederationRestoreFailed :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> CString -> IO ()) -> IO ()

onRequestFederationRestoreFailed :: (String -> String -> IO ()) -> FedHandlers t ()
onRequestFederationRestoreFailed requestFederationRestoreFailed = do
    fedAmb <- ask
    liftIO (set_requestFederationRestoreFailed fedAmb requestFederationRestoreFailed)

set_requestFederationRestoreFailed :: HsFederateAmbassador t -> (String -> String -> IO ()) -> IO ()
set_requestFederationRestoreFailed fedAmb requestFederationRestoreFailed =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkPtrX2FunPtr $ \cstr1 cstr2 -> do
            str1 <- peekCString cstr1
            str2 <- peekCString cstr2
            requestFederationRestoreFailed str1 str2
        hsfa_set_requestFederationRestoreFailed fedAmb funPtr


foreign import ccall "hsFederateAmb.h hsfa_set_federationRestoreBegun"
    hsfa_set_federationRestoreBegun :: Ptr (HsFederateAmbassador t) -> FunPtr (IO ()) -> IO ()

onFederationRestoreBegun :: IO () -> FedHandlers t ()
onFederationRestoreBegun federationRestoreBegun = do
    fedAmb <- ask
    liftIO (set_federationRestoreBegun fedAmb federationRestoreBegun)

set_federationRestoreBegun :: HsFederateAmbassador t -> IO () -> IO ()
set_federationRestoreBegun fedAmb federationRestoreBegun =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkVoidFunPtr federationRestoreBegun
        hsfa_set_federationRestoreBegun fedAmb funPtr

foreign import ccall "wrapper"
    mkInitiateFederateRestoreFunPtr :: (CString -> FederateHandle -> IO ()) -> IO (FunPtr (CString -> FederateHandle -> IO ()))

foreign import ccall "hsFederateAmb.h hsfa_set_initiateFederateRestore"
    hsfa_set_initiateFederateRestore :: Ptr (HsFederateAmbassador t) -> FunPtr (CString -> FederateHandle -> IO ()) -> IO ()

onInitiateFederateRestore :: (String -> FederateHandle -> IO ()) -> FedHandlers t ()
onInitiateFederateRestore initiateFederateRestore = do
    fedAmb <- ask
    liftIO (set_initiateFederateRestore fedAmb initiateFederateRestore)

set_initiateFederateRestore :: HsFederateAmbassador t -> (String -> FederateHandle -> IO ()) -> IO ()
set_initiateFederateRestore fedAmb initiateFederateRestore =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkInitiateFederateRestoreFunPtr $ \cstr fh -> do
            str <- peekCString cstr
            initiateFederateRestore str fh
        hsfa_set_initiateFederateRestore fedAmb funPtr


foreign import ccall "hsFederateAmb.h hsfa_set_federationRestored"
    hsfa_set_federationRestored :: Ptr (HsFederateAmbassador t) -> FunPtr (IO ()) -> IO ()

onFederationRestored :: IO () -> FedHandlers t ()
onFederationRestored federationRestored = do
    fedAmb <- ask
    liftIO (set_federationRestored fedAmb federationRestored)

set_federationRestored :: HsFederateAmbassador t -> IO () -> IO ()
set_federationRestored fedAmb federationRestored =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkVoidFunPtr federationRestored
        hsfa_set_federationRestored fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_federationNotRestored"
    hsfa_set_federationNotRestored :: Ptr (HsFederateAmbassador t) -> FunPtr (IO ()) -> IO ()

onFederationNotRestored :: IO () -> FedHandlers t ()
onFederationNotRestored federationNotRestored = do
    fedAmb <- ask
    liftIO (set_federationNotRestored fedAmb federationNotRestored)

set_federationNotRestored :: HsFederateAmbassador t -> IO () -> IO ()
set_federationNotRestored fedAmb federationNotRestored =
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkVoidFunPtr federationNotRestored
        hsfa_set_federationNotRestored fedAmb funPtr


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

-- hsfa_set_startRegistrationForObjectClass
foreign import ccall "wrapper" mkObjectClassFunPtr :: (ObjectClassHandle -> IO ()) -> IO (FunPtr (ObjectClassHandle -> IO ()))

foreign import ccall "hsFederateAmb.h hsfa_set_startRegistrationForObjectClass"
    hsfa_set_startRegistrationForObjectClass :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectClassHandle -> IO ()) -> IO ()

onStartRegistrationForObjectClass :: (ObjectClassHandle -> IO ()) -> FedHandlers t ()
onStartRegistrationForObjectClass startRegistrationForObjectClass = do
    fedAmb <- ask
    liftIO (set_startRegistrationForObjectClass fedAmb startRegistrationForObjectClass)

set_startRegistrationForObjectClass :: HsFederateAmbassador t -> (ObjectClassHandle -> IO ()) -> IO ()
set_startRegistrationForObjectClass fedAmb startRegistrationForObjectClass = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkObjectClassFunPtr startRegistrationForObjectClass
        hsfa_set_startRegistrationForObjectClass fedAmb funPtr


foreign import ccall "hsFederateAmb.h hsfa_set_stopRegistrationForObjectClass"
    hsfa_set_stopRegistrationForObjectClass :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectClassHandle -> IO ()) -> IO ()

onStopRegistrationForObjectClass :: (ObjectClassHandle -> IO ()) -> FedHandlers t ()
onStopRegistrationForObjectClass stopRegistrationForObjectClass = do
    fedAmb <- ask
    liftIO (set_stopRegistrationForObjectClass fedAmb stopRegistrationForObjectClass)

set_stopRegistrationForObjectClass :: HsFederateAmbassador t -> (ObjectClassHandle -> IO ()) -> IO ()
set_stopRegistrationForObjectClass fedAmb stopRegistrationForObjectClass = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkObjectClassFunPtr stopRegistrationForObjectClass
        hsfa_set_stopRegistrationForObjectClass fedAmb funPtr


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

foreign import ccall "hsFederateAmb.h hsfa_set_attributesInScope"
    hsfa_set_attributesInScope :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

onAttributesInScope :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onAttributesInScope attributesInScope = do
    fedAmb <- ask
    liftIO (set_attributesInScope fedAmb attributesInScope)

set_attributesInScope :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_attributesInScope fedAmb attributesInScope = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkProvideAttributeValueFunPtr $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            attributesInScope theObject (AttributeHandleSet theAttrs)
        hsfa_set_attributesInScope fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_attributesOutOfScope"
    hsfa_set_attributesOutOfScope :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

onAttributesOutOfScope :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onAttributesOutOfScope attributesOutOfScope = do
    fedAmb <- ask
    liftIO (set_attributesOutOfScope fedAmb attributesOutOfScope)

set_attributesOutOfScope :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_attributesOutOfScope fedAmb attributesOutOfScope = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkProvideAttributeValueFunPtr $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            attributesOutOfScope theObject (AttributeHandleSet theAttrs)
        hsfa_set_attributesOutOfScope fedAmb funPtr


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

foreign import ccall "hsFederateAmb.h hsfa_set_turnUpdatesOffForObjectInstance"
    hsfa_set_turnUpdatesOffForObjectInstance :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

onTurnUpdatesOffForObjectInstance :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onTurnUpdatesOffForObjectInstance turnUpdatesOffForObjectInstance = do
    fedAmb <- ask
    liftIO (set_turnUpdatesOffForObjectInstance fedAmb turnUpdatesOffForObjectInstance)

set_turnUpdatesOffForObjectInstance :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_turnUpdatesOffForObjectInstance fedAmb turnUpdatesOffForObjectInstance = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkObjectPtrFunPtr $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            turnUpdatesOffForObjectInstance theObject (AttributeHandleSet theAttrs)
        hsfa_set_turnUpdatesOffForObjectInstance fedAmb funPtr


-----------------------------------
-- Ownership Management Services --
-----------------------------------

foreign import ccall "hsFederateAmb.h hsfa_set_requestAttributeOwnershipAssumption"
    hsfa_set_requestAttributeOwnershipAssumption :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> CString -> IO ()) -> IO ()

onRequestAttributeOwnershipAssumption :: (ObjectHandle -> AttributeHandleSet -> String -> IO ()) -> FedHandlers t ()
onRequestAttributeOwnershipAssumption requestAttributeOwnershipAssumption = do
    fedAmb <- ask
    liftIO (set_requestAttributeOwnershipAssumption fedAmb requestAttributeOwnershipAssumption)

foreign import ccall "wrapper"
    mkFunPtr_ObjectHandle_to_ConstPtrX2_to_Void :: (ObjectHandle -> Ptr a -> Ptr b -> IO ()) -> IO (FunPtr (ObjectHandle -> Ptr a -> Ptr b -> IO ()))
set_requestAttributeOwnershipAssumption :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> String -> IO ()) -> IO ()
set_requestAttributeOwnershipAssumption fedAmb requestAttributeOwnershipAssumption = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_ObjectHandle_to_ConstPtrX2_to_Void $ \theObject theAttrs theTag -> do
            theAttrs <- newForeignPtr_ theAttrs
            theTag <- peekCString theTag
            requestAttributeOwnershipAssumption theObject (AttributeHandleSet theAttrs) theTag
        hsfa_set_requestAttributeOwnershipAssumption fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_attributeOwnershipDivestitureNotification"
    hsfa_set_attributeOwnershipDivestitureNotification :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

onAttributeOwnershipDivestitureNotification :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onAttributeOwnershipDivestitureNotification attributeOwnershipDivestitureNotification = do
    fedAmb <- ask
    liftIO (set_attributeOwnershipDivestitureNotification fedAmb attributeOwnershipDivestitureNotification)

set_attributeOwnershipDivestitureNotification :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_attributeOwnershipDivestitureNotification fedAmb attributeOwnershipDivestitureNotification = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkObjectPtrFunPtr $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            attributeOwnershipDivestitureNotification theObject (AttributeHandleSet theAttrs)
        hsfa_set_attributeOwnershipDivestitureNotification fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_attributeOwnershipAcquisitionNotification"
    hsfa_set_attributeOwnershipAcquisitionNotification :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

onAttributeOwnershipAcquisitionNotification :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onAttributeOwnershipAcquisitionNotification attributeOwnershipAcquisitionNotification = do
    fedAmb <- ask
    liftIO (set_attributeOwnershipAcquisitionNotification fedAmb attributeOwnershipAcquisitionNotification)

set_attributeOwnershipAcquisitionNotification :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_attributeOwnershipAcquisitionNotification fedAmb attributeOwnershipAcquisitionNotification = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkObjectPtrFunPtr $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            attributeOwnershipAcquisitionNotification theObject (AttributeHandleSet theAttrs)
        hsfa_set_attributeOwnershipAcquisitionNotification fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_attributeOwnershipUnavailable"
    hsfa_set_attributeOwnershipUnavailable :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

onAttributeOwnershipUnavailable :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onAttributeOwnershipUnavailable attributeOwnershipUnavailable = do
    fedAmb <- ask
    liftIO (set_attributeOwnershipUnavailable fedAmb attributeOwnershipUnavailable)

set_attributeOwnershipUnavailable :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_attributeOwnershipUnavailable fedAmb attributeOwnershipUnavailable = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkObjectPtrFunPtr $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            attributeOwnershipUnavailable theObject (AttributeHandleSet theAttrs)
        hsfa_set_attributeOwnershipUnavailable fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_requestAttributeOwnershipRelease"
    hsfa_set_requestAttributeOwnershipRelease :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> CString -> IO ()) -> IO ()

onRequestAttributeOwnershipRelease :: (ObjectHandle -> AttributeHandleSet -> String -> IO ()) -> FedHandlers t ()
onRequestAttributeOwnershipRelease requestAttributeOwnershipRelease = do
    fedAmb <- ask
    liftIO (set_requestAttributeOwnershipRelease fedAmb requestAttributeOwnershipRelease)

set_requestAttributeOwnershipRelease :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> String -> IO ()) -> IO ()
set_requestAttributeOwnershipRelease fedAmb requestAttributeOwnershipRelease = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_ObjectHandle_to_ConstPtrX2_to_Void $ \theObject theAttrs theTag -> do
            theAttrs <- newForeignPtr_ theAttrs
            theTag <- peekCString theTag
            requestAttributeOwnershipRelease theObject (AttributeHandleSet theAttrs) theTag
        hsfa_set_requestAttributeOwnershipRelease fedAmb funPtr


foreign import ccall "hsFederateAmb.h hsfa_set_confirmAttributeOwnershipAcquisitionCancellation"
    hsfa_set_confirmAttributeOwnershipAcquisitionCancellation :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()) -> IO ()

onConfirmAttributeOwnershipAcquisitionCancellation :: (ObjectHandle -> AttributeHandleSet -> IO ()) -> FedHandlers t ()
onConfirmAttributeOwnershipAcquisitionCancellation confirmAttributeOwnershipAcquisitionCancellation = do
    fedAmb <- ask
    liftIO (set_confirmAttributeOwnershipAcquisitionCancellation fedAmb confirmAttributeOwnershipAcquisitionCancellation)

set_confirmAttributeOwnershipAcquisitionCancellation :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandleSet -> IO ()) -> IO ()
set_confirmAttributeOwnershipAcquisitionCancellation fedAmb confirmAttributeOwnershipAcquisitionCancellation = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkObjectPtrFunPtr $ \theObject theAttrs -> do
            theAttrs <- newForeignPtr_ theAttrs
            confirmAttributeOwnershipAcquisitionCancellation theObject (AttributeHandleSet theAttrs)
        hsfa_set_confirmAttributeOwnershipAcquisitionCancellation fedAmb funPtr

foreign import ccall "hsFederateAmb.h hsfa_set_informAttributeOwnership"
    hsfa_set_informAttributeOwnership :: Ptr (HsFederateAmbassador t) -> FunPtr (ObjectHandle -> AttributeHandle -> FederateHandle -> IO ()) -> IO ()

onInformAttributeOwnership :: (ObjectHandle -> AttributeHandle -> FederateHandle -> IO ()) -> FedHandlers t ()
onInformAttributeOwnership informAttributeOwnership = do
    fedAmb <- ask
    liftIO (set_informAttributeOwnership fedAmb informAttributeOwnership)

foreign import ccall "wrapper" mkFunPtr_ObjectHandle_to_AttributeHandle_to_FederateHandle_to_Void :: 
    (ObjectHandle -> AttributeHandle -> FederateHandle -> IO ())
    -> IO (FunPtr (ObjectHandle -> AttributeHandle -> FederateHandle -> IO ()))

set_informAttributeOwnership :: HsFederateAmbassador t -> (ObjectHandle -> AttributeHandle -> FederateHandle -> IO ()) -> IO ()
set_informAttributeOwnership fedAmb informAttributeOwnership = 
    withHsFederateAmbassador fedAmb $ \fedAmb -> do
        funPtr <- mkFunPtr_ObjectHandle_to_AttributeHandle_to_FederateHandle_to_Void informAttributeOwnership
        hsfa_set_informAttributeOwnership fedAmb funPtr

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

