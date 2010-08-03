{-# LANGUAGE ForeignFunctionInterface #-}
module Network.HLA.RTI13.HsFederateAmbassador.FunPtrWrappers where

import Foreign
import Foreign.C

import Network.HLA.RTI13.RTITypes.Types
import Network.HLA.RTI13.FedTime

foreign import ccall "wrapper" mkFreeFunPtr
    ::            (FunPtr a -> IO ()) 
    -> IO (FunPtr (FunPtr a -> IO ()))

foreign import ccall "wrapper" mkFunPtr_Void
    ::             IO () 
    -> IO (FunPtr (IO ()))

foreign import ccall "wrapper" mkFunPtr_Ptr_to_Void 
    ::            (Ptr a -> IO ()) 
    -> IO (FunPtr (Ptr a -> IO ()))
    
foreign import ccall "wrapper" mkFunPtr_PtrX2_to_Void
    ::            (Ptr a -> Ptr a -> IO ()) 
    -> IO (FunPtr (Ptr a -> Ptr a -> IO ()))

foreign import ccall "wrapper" mkFunPtr_InteractionClassHandle_to_Void
    ::            (InteractionClassHandle -> IO ())
    -> IO (FunPtr (InteractionClassHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_ObjectHandle_to_ObjectClassHandle_to_Ptr_to_Void
    ::            (ObjectHandle -> ObjectClassHandle -> Ptr a -> IO ())
    -> IO (FunPtr (ObjectHandle -> ObjectClassHandle -> Ptr a -> IO ()))

foreign import ccall "wrapper" mkFunPtr_ObjectHandle_to_Ptr_to_Void
    ::            (ObjectHandle -> Ptr a -> IO ())
    -> IO (FunPtr (ObjectHandle -> Ptr a -> IO ()))

foreign import ccall "wrapper" mkFunPtr_Ptr_to_FederateHandle_to_Void
    ::            (Ptr a -> FederateHandle -> IO ())
    -> IO (FunPtr (Ptr a -> FederateHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_ObjectClassHandle_to_Void
    ::            (ObjectClassHandle -> IO ())
    -> IO (FunPtr (ObjectClassHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_ObjectHandle_to_PtrX3_to_UniqueID_to_FederateHandle_to_Void
    ::            (ObjectHandle -> Ptr a -> Ptr b -> Ptr c -> UniqueID -> FederateHandle -> IO ())
    -> IO (FunPtr (ObjectHandle -> Ptr a -> Ptr b -> Ptr c -> UniqueID -> FederateHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_InteractionClassHandle_to_PtrX3_to_UniqueID_to_FederateHandle_to_Void
    ::            (InteractionClassHandle -> Ptr a -> Ptr b -> Ptr c -> UniqueID -> FederateHandle -> IO ())
    -> IO (FunPtr (InteractionClassHandle -> Ptr a -> Ptr b -> Ptr c -> UniqueID -> FederateHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_ObjectHandle_to_PtrX2_to_UniqueID_to_FederateHandle_to_Void
    ::            (ObjectHandle -> Ptr a -> Ptr b -> UniqueID -> FederateHandle -> IO ())
    -> IO (FunPtr (ObjectHandle -> Ptr a -> Ptr b -> UniqueID -> FederateHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_ObjectHandle_to_AttributeHandle_to_FederateHandle_to_Void
    ::            (ObjectHandle -> AttributeHandle -> FederateHandle -> IO ())
    -> IO (FunPtr (ObjectHandle -> AttributeHandle -> FederateHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_ObjectHandle_to_AttributeHandle_to_Void
    ::            (ObjectHandle -> AttributeHandle -> IO ())
    -> IO (FunPtr (ObjectHandle -> AttributeHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_ObjectHandle_to_ConstPtrX2_to_Void
    ::            (ObjectHandle -> Ptr a -> Ptr b -> IO ())
    -> IO (FunPtr (ObjectHandle -> Ptr a -> Ptr b -> IO ()))

foreign import ccall "wrapper" mkFunPtr_UniqueID_to_FederateHandle_to_Void
    ::            (UniqueID -> FederateHandle -> IO ()) 
    -> IO (FunPtr (UniqueID -> FederateHandle -> IO ()))
