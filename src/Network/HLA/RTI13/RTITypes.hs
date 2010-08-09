{-# LANGUAGE
        TypeFamilies,
        MultiParamTypeClasses
  #-}
module Network.HLA.RTI13.RTITypes
    ( module Network.HLA.RTI13.RTITypes.Types
    , module Network.HLA.RTI13.RTITypes
    ) where

import Network.HLA.RTI13.RTITypes.Types
import Network.HLA.RTI13.RTITypes.FFI

import Network.HLA.RTI13.BaseTypes
import Network.HLA.RTI13.RTIException

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Foreign hiding (newForeignPtr)

-- * AttributeHandleValuePairSet

withAttributeHandleValuePairSet :: M.Map AttributeHandle BS.ByteString -> (Ptr (M.Map AttributeHandle BS.ByteString) -> IO a) -> IO a
withAttributeHandleValuePairSet theAttrs action = do
    ahvpSet <- wrapExceptions (wrap_AttributeSetFactory_create (fromIntegral (M.size theAttrs)))
    
    let insert (h, bs) =
            BS.useAsCString bs $ \buf -> 
                wrapExceptions (wrap_AttributeHandleValuePairSet_add ahvpSet h buf len)
            where len = fromIntegral (BS.length bs)
    mapM_ insert (M.toList theAttrs)
    
    result <- action ahvpSet
    
    delete_AttributeHandleValuePairSet ahvpSet
    return result

importAttributeHandleValuePairSet :: Ptr (M.Map AttributeHandle BS.ByteString) -> IO (M.Map AttributeHandle BS.ByteString)
importAttributeHandleValuePairSet ahvpSet = do
    n <- wrapExceptions (wrap_AttributeHandleValuePairSet_size ahvpSet)
    let getKV i = do
            k <- attributeHandleValuePairSet_getHandle ahvpSet i
            v <- attributeHandleValuePairSet_getValue  ahvpSet i
            return (k,v)
    
    theAttrs <- mapM getKV [0 .. n-1]
    return (M.fromList theAttrs)

attributeHandleValuePairSet_getHandle :: Ptr (M.Map AttributeHandle BS.ByteString) -> ULong -> IO AttributeHandle
attributeHandleValuePairSet_getHandle ahSet i =
    wrapExceptions (wrap_AttributeHandleValuePairSet_getHandle ahSet i)

attributeHandleValuePairSet_getValue :: Ptr (M.Map AttributeHandle BS.ByteString) -> ULong -> IO BS.ByteString
attributeHandleValuePairSet_getValue ahSet i =
    alloca $ \lenCell -> do
        buf <- wrapExceptions (wrap_AttributeHandleValuePairSet_getValuePointer ahSet i lenCell)
        
        len <- peek lenCell
        BS.packCStringLen (buf, fromIntegral len)

-- * AttributeHandleSet

withAttributeHandleSet :: S.Set AttributeHandle -> (Ptr (S.Set AttributeHandle) -> IO a) -> IO a
withAttributeHandleSet theAttrs action = do
    ahSet <- wrapExceptions (wrap_AttributeHandleSetFactory_create (fromIntegral (S.size theAttrs)))
    mapM_ (wrapExceptions . wrap_AttributeHandleSet_add ahSet) (S.toList theAttrs)
    
    result <- action ahSet
    
    delete_AttributeHandleSet ahSet
    return result
    

-- |Read an AttributeHandleSet pointer into Haskell-space (copies the data, so
-- the pointer may be subsequently deleted)
importAttributeHandleSet :: Ptr (S.Set AttributeHandle) -> IO (S.Set AttributeHandle)
importAttributeHandleSet ahSetPtr = do
    n <- wrapExceptions (wrap_AttributeHandleSet_size ahSetPtr)
    theAttrs <- mapM (wrapExceptions . wrap_AttributeHandleSet_getHandle ahSetPtr) [0 .. n - 1]
    
    return (S.fromList theAttrs)

-- * FederateHandleSet

withFederateHandleSet :: S.Set FederateHandle -> (Ptr (S.Set FederateHandle) -> IO a) -> IO a
withFederateHandleSet theHandles action = do
    fhSet <- wrapExceptions (wrap_FederateHandleSetFactory_create (fromIntegral (S.size theHandles)))
    
    mapM_ (wrapExceptions . wrap_FederateHandleSet_add fhSet) (S.toList theHandles)
    
    result <- action fhSet
    delete_FederateHandleSet fhSet
    return result

importFederateHandleSet :: Ptr (S.Set FederateHandle) -> IO (S.Set FederateHandle)
importFederateHandleSet fhSet = do
    n <- wrapExceptions (wrap_FederateHandleSet_size fhSet)
    theHandles <- mapM (wrapExceptions . wrap_FederateHandleSet_getHandle fhSet) [0..n-1]
    return (S.fromList theHandles)
    
-- * ParameterHandleValuePairSet

-- newtype ParameterHandleValuePairSet = ParameterHandleValuePairSet (ForeignPtr ParameterHandleValuePairSet)
withParameterHandleValuePairSet :: M.Map ParameterHandle BS.ByteString -> (Ptr (M.Map ParameterHandle BS.ByteString) -> IO a) -> IO a
withParameterHandleValuePairSet theParams action = do
    phvpSet <- wrapExceptions (wrap_ParameterSetFactory_create (fromIntegral (M.size theParams)))
    let insert (h, bs) =
            BS.useAsCString bs $ \buf -> 
                wrapExceptions (wrap_ParameterHandleValuePairSet_add phvpSet h buf len)
            where len = fromIntegral (BS.length bs)
    mapM_ insert (M.toList theParams)
    
    result <- action phvpSet
    delete_ParameterHandleValuePairSet phvpSet
    return result

importParameterHandleValuePairSet :: Ptr (M.Map ParameterHandle BS.ByteString) -> IO (M.Map ParameterHandle BS.ByteString)
importParameterHandleValuePairSet phvpSet = do
    n <- wrapExceptions (wrap_ParameterHandleValuePairSet_size phvpSet)
    let getKV i = do
            k <- parameterHandleValuePairSet_getHandle phvpSet i
            v <- parameterHandleValuePairSet_getValue  phvpSet i
            return (k,v)
    
    theParameters <- mapM getKV [0 .. n-1]
    return (M.fromList theParameters)

parameterHandleValuePairSet_getHandle :: Ptr (M.Map ParameterHandle BS.ByteString) -> ULong -> IO ParameterHandle
parameterHandleValuePairSet_getHandle phSet i =
    wrapExceptions (wrap_ParameterHandleValuePairSet_getHandle phSet i)

parameterHandleValuePairSet_getValue :: Ptr (M.Map ParameterHandle BS.ByteString) -> ULong -> IO BS.ByteString
parameterHandleValuePairSet_getValue phSet i = do
    alloca $ \lenCell -> do
        buf <- wrapExceptions (wrap_ParameterHandleValuePairSet_getValuePointer phSet i lenCell)
        
        len <- peek lenCell
        BS.packCStringLen (buf, fromIntegral len)

-- * Region

getRangeLowerBound :: Region -> ExtentIndex -> DimensionHandle -> IO ULong
getRangeLowerBound theRegion theExtent theDimension =
    withRegion theRegion $ \theRegion -> 
        wrapExceptions (wrap_Region_getRangeLowerBound theRegion theExtent theDimension)

getRangeUpperBound :: Region -> ExtentIndex -> DimensionHandle -> IO ULong
getRangeUpperBound theRegion theExtent theDimension =
    withRegion theRegion $ \theRegion -> 
        wrapExceptions (wrap_Region_getRangeUpperBound theRegion theExtent theDimension)

setRangeLowerBound :: Region -> ExtentIndex -> DimensionHandle -> ULong -> IO ()
setRangeLowerBound theRegion theExtent theDimension theLowerBound = 
    withRegion theRegion $ \theRegion -> 
        wrapExceptions (wrap_Region_setRangeLowerBound theRegion theExtent theDimension theLowerBound)

setRangeUpperBound :: Region -> ExtentIndex -> DimensionHandle -> ULong -> IO ()
setRangeUpperBound theRegion theExtent theDimension theUpperBound = 
    withRegion theRegion $ \theRegion -> 
        wrapExceptions (wrap_Region_setRangeUpperBound theRegion theExtent theDimension theUpperBound)

getSpaceHandle :: Region -> IO SpaceHandle
getSpaceHandle theRegion = withRegion theRegion
    (wrapExceptions . wrap_Region_getSpaceHandle)
    
getNumberOfExtents :: Region -> IO ULong
getNumberOfExtents theRegion = withRegion theRegion
    (wrapExceptions . wrap_Region_getNumberOfExtents)

getRangeLowerBoundNotificationLimit :: Region -> ExtentIndex -> DimensionHandle -> IO ULong
getRangeLowerBoundNotificationLimit theRegion theExtent theDimension =
    withRegion theRegion $ \theRegion -> 
        wrapExceptions (wrap_Region_getRangeLowerBoundNotificationLimit theRegion theExtent theDimension)

getRangeUpperBoundNotificationLimit :: Region -> ExtentIndex -> DimensionHandle -> IO ULong
getRangeUpperBoundNotificationLimit theRegion theExtent theDimension =
    withRegion theRegion $ \theRegion -> 
        wrapExceptions (wrap_Region_getRangeUpperBoundNotificationLimit theRegion theExtent theDimension)

-- * FedTime
    -- public:
    --   virtual ~FedTime();
    -- 
    --   virtual void setZero() = 0;
    -- 
    --   virtual Boolean isZero() = 0;
    --   
    --   virtual void setEpsilon() = 0;
    -- 
    --   virtual void setPositiveInfinity() = 0;
    -- 
    --   virtual Boolean isPositiveInfinity() = 0;
    --   
    --   virtual FedTime& operator+= (const FedTime&)
    --     throw (
    --       InvalidFederationTime) = 0;
    -- 
    --   virtual FedTime& operator-= (const FedTime&)
    --     throw (
    --       InvalidFederationTime) = 0;
    --   
    --   virtual Boolean operator<= (const FedTime&) const
    --     throw (
    --       InvalidFederationTime) = 0;
    -- 
    --   virtual Boolean operator< (const FedTime&) const
    --     throw (
    --       InvalidFederationTime) = 0;
    -- 
    --    virtual Boolean operator>= (const FedTime&) const
    --     throw (
    --       InvalidFederationTime) = 0;
    --   
    --   virtual Boolean operator> (const FedTime&) const
    --     throw (
    --       InvalidFederationTime) = 0;
    -- 
    --   virtual Boolean operator== (const FedTime&) const
    --     throw (
    --       InvalidFederationTime) = 0;
    --   
    --   virtual FedTime& operator= (const FedTime&)
    --     throw (
    --       InvalidFederationTime) = 0;
    -- 
    --   //return bytes needed to encode
    --   virtual int encodedLength() const = 0;
    --   
    --   //encode into suppled buffer
    --   virtual void encode(char *buff) const = 0;
    --   
    --   virtual int getPrintableLength() const = 0;
    -- 
    --   virtual void getPrintableString(char*) const = 0;
    --   
    -- };

-- * FedTimeFactory
    -- public:
    --   static FedTime* makeZero()
    --     throw (
    --       MemoryExhausted);
    -- 
    --   static FedTime* decode(const char *buf)
    --     throw (
    --       MemoryExhausted);
    -- };

