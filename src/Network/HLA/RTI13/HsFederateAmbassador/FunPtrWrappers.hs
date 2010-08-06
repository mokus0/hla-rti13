{-# LANGUAGE ForeignFunctionInterface #-}
module Network.HLA.RTI13.HsFederateAmbassador.FunPtrWrappers where

import Foreign

import Network.HLA.RTI13.RTITypes.Types

-- naming convention:
-- mkFunPtr_{type}(_{type})*
-- where {type} is one of the following abbreviations, with a number of 
-- repetitions of parameters of that type optionally appended:
-- 
-- A    = AttributeHandle
-- IC   = InteractionClassHandle
-- O    = ObjectHandle
-- OC   = ObjectClassHandle
-- P    = Ptr t
-- U    = UniqueID
-- V    = Void

foreign import ccall "wrapper" mkFunPtr_IC_P3_U_F_V
    ::            (InteractionClassHandle -> Ptr a -> Ptr b -> Ptr c -> UniqueID -> FederateHandle -> IO ())
    -> IO (FunPtr (InteractionClassHandle -> Ptr a -> Ptr b -> Ptr c -> UniqueID -> FederateHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_IC_V
    ::            (InteractionClassHandle -> IO ())
    -> IO (FunPtr (InteractionClassHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_OC_V
    ::            (ObjectClassHandle -> IO ())
    -> IO (FunPtr (ObjectClassHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_O_A_F_V
    ::            (ObjectHandle -> AttributeHandle -> FederateHandle -> IO ())
    -> IO (FunPtr (ObjectHandle -> AttributeHandle -> FederateHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_O_A_V
    ::            (ObjectHandle -> AttributeHandle -> IO ())
    -> IO (FunPtr (ObjectHandle -> AttributeHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_O_OC_P_V
    ::            (ObjectHandle -> ObjectClassHandle -> Ptr a -> IO ())
    -> IO (FunPtr (ObjectHandle -> ObjectClassHandle -> Ptr a -> IO ()))

foreign import ccall "wrapper" mkFunPtr_O_P_V
    ::            (ObjectHandle -> Ptr a -> IO ())
    -> IO (FunPtr (ObjectHandle -> Ptr a -> IO ()))

foreign import ccall "wrapper" mkFunPtr_O_P2_U_F_V
    ::            (ObjectHandle -> Ptr a -> Ptr b -> UniqueID -> FederateHandle -> IO ())
    -> IO (FunPtr (ObjectHandle -> Ptr a -> Ptr b -> UniqueID -> FederateHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_O_P2_V
    ::            (ObjectHandle -> Ptr a -> Ptr b -> IO ())
    -> IO (FunPtr (ObjectHandle -> Ptr a -> Ptr b -> IO ()))

foreign import ccall "wrapper" mkFunPtr_O_P3_U_F_V
    ::            (ObjectHandle -> Ptr a -> Ptr b -> Ptr c -> UniqueID -> FederateHandle -> IO ())
    -> IO (FunPtr (ObjectHandle -> Ptr a -> Ptr b -> Ptr c -> UniqueID -> FederateHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_P_F_V
    ::            (Ptr a -> FederateHandle -> IO ())
    -> IO (FunPtr (Ptr a -> FederateHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_P_V 
    ::            (Ptr a -> IO ()) 
    -> IO (FunPtr (Ptr a -> IO ()))
    
foreign import ccall "wrapper" mkFunPtr_P2_V
    ::            (Ptr a -> Ptr a -> IO ()) 
    -> IO (FunPtr (Ptr a -> Ptr a -> IO ()))

foreign import ccall "wrapper" mkFunPtr_U_F_V
    ::            (UniqueID -> FederateHandle -> IO ()) 
    -> IO (FunPtr (UniqueID -> FederateHandle -> IO ()))

foreign import ccall "wrapper" mkFunPtr_V
    ::             IO () 
    -> IO (FunPtr (IO ()))
