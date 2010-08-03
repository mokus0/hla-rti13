{-# LANGUAGE ForeignFunctionInterface #-}
module Network.HLA.RTI13.HsFederateAmbassador.FunPtrWrappers where

import Foreign
import Foreign.C

import Network.HLA.RTI13.RTITypes.Types
import Network.HLA.RTI13.FedTime

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

foreign import ccall "wrapper"
    mkInitiateFederateRestoreFunPtr :: (CString -> FederateHandle -> IO ()) -> IO (FunPtr (CString -> FederateHandle -> IO ()))

foreign import ccall "wrapper" mkObjectClassFunPtr :: (ObjectClassHandle -> IO ()) -> IO (FunPtr (ObjectClassHandle -> IO ()))

foreign import ccall "wrapper"
    mkReflectAttributeValuesFunPtr :: (ObjectHandle -> Ptr AttributeHandleValuePairSet -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ())
        -> IO (FunPtr (ObjectHandle -> Ptr AttributeHandleValuePairSet -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ()))

foreign import ccall "wrapper"
    mkReceiveInteractionFunPtr :: (InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ())
        -> IO (FunPtr (InteractionClassHandle -> Ptr ParameterHandleValuePairSet -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ()))

foreign import ccall "wrapper" 
    mkRemoveObjectInstanceFunPtr
        :: (ObjectHandle -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ())
        -> IO (FunPtr (ObjectHandle -> Ptr t -> CString -> UniqueID -> FederateHandle -> IO ()))

foreign import ccall "wrapper" 
    mkProvideAttributeValueFunPtr :: (ObjectHandle -> Ptr AttributeHandleSet -> IO ())
        -> IO (FunPtr (ObjectHandle -> Ptr AttributeHandleSet -> IO ()))


foreign import ccall "wrapper" mkFunPtr_ObjectHandle_to_AttributeHandle_to_FederateHandle_to_Void :: 
    (ObjectHandle -> AttributeHandle -> FederateHandle -> IO ())
    -> IO (FunPtr (ObjectHandle -> AttributeHandle -> FederateHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_ObjectHandle_to_AttributeHandle_to_Void :: 
    (ObjectHandle -> AttributeHandle -> IO ())
    -> IO (FunPtr (ObjectHandle -> AttributeHandle -> IO ()))

foreign import ccall "wrapper"
    mkFunPtr_ObjectHandle_to_ConstPtrX2_to_Void :: (ObjectHandle -> Ptr a -> Ptr b -> IO ()) -> IO (FunPtr (ObjectHandle -> Ptr a -> Ptr b -> IO ()))

foreign import ccall "wrapper"
    mkFunPtr_UniqueID_to_FederateHandle_to_Void :: (UniqueID -> FederateHandle -> IO ()) -> IO (FunPtr (UniqueID -> FederateHandle -> IO ()))

