{-# LANGUAGE 
        ForeignFunctionInterface
  #-}
module Network.HLA.RTI13.RTITypes.FFI where

import Data.ByteString (ByteString, packCString)
import qualified Data.Map as M (Map)
import qualified Data.Set as S (Set)
import Foreign
import Foreign.C
import Network.HLA.RTI13.BaseTypes
import Network.HLA.RTI13.RTIException
import Network.HLA.RTI13.RTITypes.Types

-- * Constants

foreign import ccall "wrap/RTItypes.h wrap_DEFAULT_SPACE_NAME" 
    wrap_DEFAULT_SPACE_NAME :: CString
{-# NOINLINE defaultSpaceName #-}
defaultSpaceName :: ByteString
defaultSpaceName = unsafePerformIO (packCString wrap_DEFAULT_SPACE_NAME)
foreign import ccall "wrap/RTItypes.h wrap_DEFAULT_SPACE_DIMENSION_NAME"
    wrap_DEFAULT_SPACE_DIMENSION_NAME :: CString
{-# NOINLINE defaultSpaceDimensionName #-}
defaultSpaceDimensionName :: ByteString
defaultSpaceDimensionName = unsafePerformIO (packCString wrap_DEFAULT_SPACE_DIMENSION_NAME)

foreign import ccall "wrap/RTItypes.h wrap_RTI_VERSION"
    wrap_RTI_VERSION :: CString
{-# NOINLINE rtiVersion #-}
rtiVersion :: ByteString
rtiVersion = unsafePerformIO (packCString wrap_RTI_VERSION)
foreign import ccall "wrap/RTItypes.h wrap_RTI_INTERNAL_VERSION"
    wrap_RTI_INTERNAL_VERSION :: CString
{-# NOINLINE rtiInternalVersion #-}
rtiInternalVersion :: ByteString
rtiInternalVersion = unsafePerformIO (packCString wrap_RTI_INTERNAL_VERSION)

foreign import ccall "wrap/RTItypes.h wrap_RTI_MAJOR_VERSION"
    wrap_RTI_MAJOR_VERSION :: ULong
foreign import ccall "wrap/RTItypes.h wrap_RTI_MINOR_VERSION"
    wrap_RTI_MINOR_VERSION :: ULong
foreign import ccall "wrap/RTItypes.h wrap_RTI_RELEASE"
    wrap_RTI_RELEASE :: ULong

rtiMajorVersion, rtiMinorVersion, rtiRelease :: Num a => a
rtiMajorVersion = fromIntegral wrap_RTI_MAJOR_VERSION
rtiMinorVersion = fromIntegral wrap_RTI_MINOR_VERSION
rtiRelease = fromIntegral wrap_RTI_RELEASE

foreign import ccall "wrap/RTItypes.h wrap_RTI_INTERNAL_MAJOR_VERSION"
    wrap_RTI_INTERNAL_MAJOR_VERSION :: ULong
foreign import ccall "wrap/RTItypes.h wrap_RTI_INTERNAL_MINOR_VERSION"
    wrap_RTI_INTERNAL_MINOR_VERSION :: ULong
foreign import ccall "wrap/RTItypes.h wrap_RTI_INTERNAL_RELEASE"
    wrap_RTI_INTERNAL_RELEASE :: ULong

rtiInternalMajorVersion, rtiInternalMinorVersion, rtiInternalRelease :: Num a => a
rtiInternalMajorVersion = fromIntegral wrap_RTI_INTERNAL_MAJOR_VERSION
rtiInternalMinorVersion = fromIntegral wrap_RTI_INTERNAL_MINOR_VERSION
rtiInternalRelease = fromIntegral wrap_RTI_INTERNAL_RELEASE



-- * AttributeHandleValuePairSet

delete_AttributeHandleValuePairSet ahSet
    = wrapExceptions (wrap_delete_AttributeHandleValuePairSet ahSet)
foreign import ccall "wrap/RTItypes.h wrap_delete_AttributeHandleValuePairSet"
    wrap_delete_AttributeHandleValuePairSet :: Ptr (M.Map AttributeHandle ByteString) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTItypes.h wrap_AttributeHandleValuePairSet_size"
    wrap_AttributeHandleValuePairSet_size :: Ptr (M.Map AttributeHandle ByteString) -> Ptr (Ptr RTIException) -> IO ULong

foreign import ccall "wrap/RTItypes.h wrap_AttributeHandleValuePairSet_getHandle"
    wrap_AttributeHandleValuePairSet_getHandle :: Ptr (M.Map AttributeHandle ByteString) -> ULong -> Ptr (Ptr RTIException) -> IO AttributeHandle
foreign import ccall "wrap/RTItypes.h wrap_AttributeHandleValuePairSet_getValueLength"
    wrap_AttributeHandleValuePairSet_getValueLength :: Ptr (M.Map AttributeHandle ByteString) -> ULong -> Ptr (Ptr RTIException) -> IO ULong
        
foreign import ccall "wrap/RTItypes.h wrap_AttributeHandleValuePairSet_getValue"
    wrap_AttributeHandleValuePairSet_getValue :: Ptr (M.Map AttributeHandle ByteString) -> ULong -> CString -> Ptr ULong -> Ptr (Ptr RTIException) -> IO ()
foreign import ccall "wrap/RTItypes.h wrap_AttributeHandleValuePairSet_getValuePointer"
    wrap_AttributeHandleValuePairSet_getValuePointer :: Ptr (M.Map AttributeHandle ByteString) -> ULong -> Ptr ULong -> Ptr (Ptr RTIException) -> IO CString
foreign import ccall "wrap/RTItypes.h wrap_AttributeHandleValuePairSet_getTransportType"
    wrap_AttributeHandleValuePairSet_getTransportType :: Ptr (M.Map AttributeHandle ByteString) -> ULong -> Ptr (Ptr RTIException) -> IO TransportType
foreign import ccall "wrap/RTItypes.h wrap_AttributeHandleValuePairSet_getOrderType"
    wrap_AttributeHandleValuePairSet_getOrderType :: Ptr (M.Map AttributeHandle ByteString) -> ULong -> Ptr (Ptr RTIException) -> IO OrderType
foreign import ccall "wrap/RTItypes.h wrap_AttributeHandleValuePairSet_getRegion"
    wrap_AttributeHandleValuePairSet_getRegion :: Ptr (M.Map AttributeHandle ByteString) -> ULong -> Ptr (Ptr RTIException) -> IO (Ptr Region)

foreign import ccall "wrap/RTItypes.h wrap_AttributeHandleValuePairSet_add"
    wrap_AttributeHandleValuePairSet_add :: Ptr (M.Map AttributeHandle ByteString) -> AttributeHandle -> CString -> ULong -> Ptr (Ptr RTIException) -> IO ()
foreign import ccall "wrap/RTItypes.h wrap_AttributeHandleValuePairSet_remove"
    wrap_AttributeHandleValuePairSet_remove :: Ptr (M.Map AttributeHandle ByteString) -> AttributeHandle -> Ptr (Ptr RTIException) -> IO ()
foreign import ccall "wrap/RTItypes.h wrap_AttributeHandleValuePairSet_moveFrom"
    wrap_AttributeHandleValuePairSet_moveFrom  :: Ptr (M.Map AttributeHandle ByteString) -> Ptr (M.Map AttributeHandle ByteString) -> Ptr ULong -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTItypes.h wrap_AttributeHandleValuePairSet_empty"
    wrap_AttributeHandleValuePairSet_empty :: Ptr (M.Map AttributeHandle ByteString) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTItypes.h wrap_AttributeHandleValuePairSet_start"
    wrap_AttributeHandleValuePairSet_start :: Ptr (M.Map AttributeHandle ByteString) -> Ptr (Ptr RTIException) -> IO ULong
foreign import ccall "wrap/RTItypes.h wrap_AttributeHandleValuePairSet_valid"
    wrap_AttributeHandleValuePairSet_valid :: Ptr (M.Map AttributeHandle ByteString) -> ULong -> Ptr (Ptr RTIException) -> IO ULong
foreign import ccall "wrap/RTItypes.h wrap_AttributeHandleValuePairSet_next"
    wrap_AttributeHandleValuePairSet_next :: Ptr (M.Map AttributeHandle ByteString) -> ULong -> Ptr (Ptr RTIException) -> IO ULong

-- * AttributeSetFactory
foreign import ccall "wrap/RTItypes.h wrap_AttributeSetFactory_create"
    wrap_AttributeSetFactory_create :: ULong -> Ptr (Ptr RTIException) -> IO (Ptr (M.Map AttributeHandle ByteString))

-- * AttributeHandleSet

delete_AttributeHandleSet :: Ptr (S.Set AttributeHandle) -> IO ()
delete_AttributeHandleSet ahSet = 
    wrapExceptions (wrap_delete_AttributeHandleSet ahSet)
foreign import ccall unsafe "wrap/RTItypes.h wrap_delete_AttributeHandleSet"
    wrap_delete_AttributeHandleSet :: Ptr (S.Set AttributeHandle) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTItypes.h wrap_AttributeHandleSet_size"
    wrap_AttributeHandleSet_size :: Ptr (S.Set AttributeHandle) -> Ptr (Ptr RTIException) -> IO ULong

foreign import ccall unsafe "wrap/RTItypes.h wrap_AttributeHandleSet_getHandle"
    wrap_AttributeHandleSet_getHandle :: Ptr (S.Set AttributeHandle) -> ULong -> Ptr (Ptr RTIException) -> IO AttributeHandle

foreign import ccall unsafe "wrap/RTItypes.h wrap_AttributeHandleSet_add"
    wrap_AttributeHandleSet_add :: Ptr (S.Set AttributeHandle) -> AttributeHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTItypes.h wrap_AttributeHandleSet_remove"
    wrap_AttributeHandleSet_remove :: Ptr (S.Set AttributeHandle) -> AttributeHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTItypes.h wrap_AttributeHandleSet_empty"
    wrap_AttributeHandleSet_empty :: Ptr (S.Set AttributeHandle) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTItypes.h wrap_AttributeHandleSet_isEmpty"
    wrap_AttributeHandleSet_isEmpty :: Ptr (S.Set AttributeHandle) -> Ptr (Ptr RTIException) -> IO Bool

foreign import ccall unsafe "wrap/RTItypes.h wrap_AttributeHandleSet_isMember"
    wrap_AttributeHandleSet_isMember :: Ptr (S.Set AttributeHandle) -> AttributeHandle -> Ptr (Ptr RTIException) -> IO Bool

-- * AttributeHandleSetFactory

foreign import ccall unsafe "wrap/RTItypes.h wrap_AttributeHandleSetFactory_create"
    wrap_AttributeHandleSetFactory_create :: ULong -> Ptr (Ptr RTIException) -> IO (Ptr (S.Set AttributeHandle))

-- * FederateHandleSet

delete_FederateHandleSet :: Ptr (S.Set FederateHandle) -> IO ()
delete_FederateHandleSet fhSet = 
    wrapExceptions (wrap_delete_FederateHandleSet fhSet)
foreign import ccall unsafe "wrap/RTItypes.h wrap_delete_FederateHandleSet"
    wrap_delete_FederateHandleSet :: Ptr (S.Set FederateHandle) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTItypes.h wrap_FederateHandleSet_size"
    wrap_FederateHandleSet_size :: Ptr (S.Set FederateHandle) -> Ptr (Ptr RTIException) -> IO ULong

foreign import ccall unsafe "wrap/RTItypes.h wrap_FederateHandleSet_getHandle"
    wrap_FederateHandleSet_getHandle :: Ptr (S.Set FederateHandle) -> ULong -> Ptr (Ptr RTIException) -> IO FederateHandle

foreign import ccall unsafe "wrap/RTItypes.h wrap_FederateHandleSet_add"
    wrap_FederateHandleSet_add :: Ptr (S.Set FederateHandle) -> FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTItypes.h wrap_FederateHandleSet_remove"
    wrap_FederateHandleSet_remove :: Ptr (S.Set FederateHandle) -> FederateHandle -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTItypes.h wrap_FederateHandleSet_empty"
    wrap_FederateHandleSet_empty :: Ptr (S.Set FederateHandle) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall unsafe "wrap/RTItypes.h wrap_FederateHandleSet_isMember"
    wrap_FederateHandleSet_isMember :: Ptr (S.Set FederateHandle) -> FederateHandle -> Ptr (Ptr RTIException) -> IO Bool

-- * FederateHandleSetFactory

foreign import ccall unsafe "wrap/RTItypes.h wrap_FederateHandleSetFactory_create"
    wrap_FederateHandleSetFactory_create :: ULong -> Ptr (Ptr RTIException) -> IO (Ptr (S.Set FederateHandle))

-- * ParameterHandleValuePairSet

delete_ParameterHandleValuePairSet :: Ptr (M.Map ParameterHandle ByteString) -> IO ()
delete_ParameterHandleValuePairSet pSet = 
    wrapExceptions (wrap_delete_ParameterHandleValuePairSet pSet)
foreign import ccall unsafe "wrap/RTItypes.h wrap_delete_ParameterHandleValuePairSet"
    wrap_delete_ParameterHandleValuePairSet :: Ptr (M.Map ParameterHandle ByteString) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTItypes.h wrap_ParameterHandleValuePairSet_size"
    wrap_ParameterHandleValuePairSet_size :: Ptr (M.Map ParameterHandle ByteString) -> Ptr (Ptr RTIException) -> IO ULong

foreign import ccall "wrap/RTItypes.h wrap_ParameterHandleValuePairSet_getHandle"
    wrap_ParameterHandleValuePairSet_getHandle :: Ptr (M.Map ParameterHandle ByteString) -> ULong -> Ptr (Ptr RTIException) -> IO ParameterHandle
foreign import ccall "wrap/RTItypes.h wrap_ParameterHandleValuePairSet_getValueLength"
    wrap_ParameterHandleValuePairSet_getValueLength :: Ptr (M.Map ParameterHandle ByteString) -> ULong -> Ptr (Ptr RTIException) -> IO ULong
        
foreign import ccall "wrap/RTItypes.h wrap_ParameterHandleValuePairSet_getValue"
    wrap_ParameterHandleValuePairSet_getValue :: Ptr (M.Map ParameterHandle ByteString) -> ULong -> CString -> Ptr ULong -> Ptr (Ptr RTIException) -> IO ()
foreign import ccall "wrap/RTItypes.h wrap_ParameterHandleValuePairSet_getValuePointer"
    wrap_ParameterHandleValuePairSet_getValuePointer :: Ptr (M.Map ParameterHandle ByteString) -> ULong -> Ptr ULong -> Ptr (Ptr RTIException) -> IO CString
foreign import ccall "wrap/RTItypes.h wrap_ParameterHandleValuePairSet_getTransportType"
    wrap_ParameterHandleValuePairSet_getTransportType :: Ptr (M.Map ParameterHandle ByteString) -> ULong -> Ptr (Ptr RTIException) -> IO TransportType
foreign import ccall "wrap/RTItypes.h wrap_ParameterHandleValuePairSet_getOrderType"
    wrap_ParameterHandleValuePairSet_getOrderType :: Ptr (M.Map ParameterHandle ByteString) -> ULong -> Ptr (Ptr RTIException) -> IO OrderType
foreign import ccall "wrap/RTItypes.h wrap_ParameterHandleValuePairSet_getRegion"
    wrap_ParameterHandleValuePairSet_getRegion :: Ptr (M.Map ParameterHandle ByteString) -> ULong -> Ptr (Ptr RTIException) -> IO (Ptr Region)

foreign import ccall unsafe "wrap/RTItypes.h wrap_ParameterHandleValuePairSet_add"
    wrap_ParameterHandleValuePairSet_add :: Ptr (M.Map ParameterHandle ByteString) -> ParameterHandle -> CString -> ULong -> Ptr (Ptr RTIException) -> IO ()
foreign import ccall "wrap/RTItypes.h wrap_ParameterHandleValuePairSet_remove"
    wrap_ParameterHandleValuePairSet_remove :: Ptr (M.Map ParameterHandle ByteString) -> ParameterHandle -> Ptr (Ptr RTIException) -> IO ()
foreign import ccall "wrap/RTItypes.h wrap_ParameterHandleValuePairSet_moveFrom"
    wrap_ParameterHandleValuePairSet_moveFrom  :: Ptr (M.Map ParameterHandle ByteString) -> Ptr (M.Map ParameterHandle ByteString) -> Ptr ULong -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTItypes.h wrap_ParameterHandleValuePairSet_empty"
    wrap_ParameterHandleValuePairSet_empty :: Ptr (M.Map ParameterHandle ByteString) -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTItypes.h wrap_ParameterHandleValuePairSet_start"
    wrap_ParameterHandleValuePairSet_start :: Ptr (M.Map ParameterHandle ByteString) -> Ptr (Ptr RTIException) -> IO ULong
foreign import ccall "wrap/RTItypes.h wrap_ParameterHandleValuePairSet_valid"
    wrap_ParameterHandleValuePairSet_valid :: Ptr (M.Map ParameterHandle ByteString) -> ULong -> Ptr (Ptr RTIException) -> IO ULong
foreign import ccall "wrap/RTItypes.h wrap_ParameterHandleValuePairSet_next"
    wrap_ParameterHandleValuePairSet_next :: Ptr (M.Map ParameterHandle ByteString) -> ULong -> Ptr (Ptr RTIException) -> IO ULong

-- * ParameterSetFactory
foreign import ccall unsafe "wrap/RTItypes.h wrap_ParameterSetFactory_create"
    wrap_ParameterSetFactory_create :: ULong -> Ptr (Ptr RTIException) -> IO (Ptr (M.Map ParameterHandle ByteString))

-- * Region
delete_Region region = 
    wrapExceptions (wrap_delete_Region region)
foreign import ccall "wrap/RTItypes.h wrap_delete_Region"
    wrap_delete_Region :: Ptr Region -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTItypes.h wrap_Region_getRangeLowerBound"
    wrap_Region_getRangeLowerBound :: Ptr Region -> ExtentIndex -> DimensionHandle -> Ptr (Ptr RTIException) -> IO ULong
foreign import ccall "wrap/RTItypes.h wrap_Region_getRangeUpperBound"
    wrap_Region_getRangeUpperBound :: Ptr Region -> ExtentIndex -> DimensionHandle -> Ptr (Ptr RTIException) -> IO ULong

foreign import ccall "wrap/RTItypes.h wrap_Region_setRangeLowerBound"
    wrap_Region_setRangeLowerBound :: Ptr Region -> ExtentIndex -> DimensionHandle -> ULong -> Ptr (Ptr RTIException) -> IO ()
foreign import ccall "wrap/RTItypes.h wrap_Region_setRangeUpperBound"
    wrap_Region_setRangeUpperBound :: Ptr Region -> ExtentIndex -> DimensionHandle -> ULong -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTItypes.h wrap_Region_getSpaceHandle"
    wrap_Region_getSpaceHandle :: Ptr Region -> Ptr (Ptr RTIException) -> IO SpaceHandle

foreign import ccall "wrap/RTItypes.h wrap_Region_getNumberOfExtents"
    wrap_Region_getNumberOfExtents :: Ptr Region -> Ptr (Ptr RTIException) -> IO ULong

foreign import ccall "wrap/RTItypes.h wrap_Region_getRangeLowerBoundNotificationLimit"
    wrap_Region_getRangeLowerBoundNotificationLimit :: Ptr Region -> ExtentIndex -> DimensionHandle -> Ptr (Ptr RTIException) -> IO ULong
foreign import ccall "wrap/RTItypes.h wrap_Region_getRangeUpperBoundNotificationLimit"
    wrap_Region_getRangeUpperBoundNotificationLimit :: Ptr Region -> ExtentIndex -> DimensionHandle -> Ptr (Ptr RTIException) -> IO ULong

-- * FedTime
delete_FedTime fedTime
    = wrapExceptions (wrap_delete_FedTime fedTime)
foreign import ccall "wrap/RTItypes.h wrap_delete_FedTime"
    wrap_delete_FedTime :: Ptr fedTime -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTItypes.h wrap_FedTime_setZero"
    wrap_FedTime_setZero :: Ptr fedTime -> Ptr (Ptr RTIException) -> IO ()
foreign import ccall "wrap/RTItypes.h wrap_FedTime_isZero"
    wrap_FedTime_isZero :: Ptr fedTime -> Ptr (Ptr RTIException) -> IO Bool

foreign import ccall "wrap/RTItypes.h wrap_FedTime_setEpsilon"
    wrap_FedTime_setEpsilon :: Ptr fedTime -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTItypes.h wrap_FedTime_setPositiveInfinity"
    wrap_FedTime_setPositiveInfinity :: Ptr fedTime -> Ptr (Ptr RTIException) -> IO ()
foreign import ccall "wrap/RTItypes.h wrap_FedTime_isPositiveInfinity"
    wrap_FedTime_isPositiveInfinity :: Ptr fedTime -> Ptr (Ptr RTIException) -> IO Bool

    --   virtual FedTime& operator+= (const FedTime&)
    --   virtual FedTime& operator-= (const FedTime&)
    --   virtual Boolean operator<= (const FedTime&) const
    --   virtual Boolean operator< (const FedTime&) const
    --    virtual Boolean operator>= (const FedTime&) const
    --   virtual Boolean operator> (const FedTime&) const
    --   virtual Boolean operator== (const FedTime&) const
    --   virtual FedTime& operator= (const FedTime&)

foreign import ccall "wrap/RTItypes.h wrap_FedTime_encodedLength"
    wrap_FedTime_encodedLength :: Ptr fedTime -> Ptr (Ptr RTIException) -> IO CInt
foreign import ccall "wrap/RTItypes.h wrap_FedTime_encode"
    wrap_FedTime_encode :: Ptr fedTime -> CString -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTItypes.h wrap_FedTime_getPrintableLength"
    wrap_FedTime_getPrintableLength :: Ptr fedTime -> Ptr (Ptr RTIException) -> IO CInt
foreign import ccall "wrap/RTItypes.h wrap_FedTime_getPrintableString"
    wrap_FedTime_getPrintableString :: Ptr fedTime -> CString -> Ptr (Ptr RTIException) -> IO ()

-- * FedTimeFactory
foreign import ccall "wrap/RTItypes.h wrap_FedTimeFactory_makeZero"
    wrap_FedTimeFactory_makeZero :: Ptr (Ptr RTIException) -> IO (Ptr fedTime)
-- foreign import ccall "wrap/RTItypes.h wrap_FedTimeFactory_decode"
--     wrap_FedTimeFactory_decode :: CString -> Ptr (Ptr RTIException) -> IO (Ptr fedTime)
