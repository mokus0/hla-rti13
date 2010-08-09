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
import Data.Container.Mutable
import Data.List (genericLength)
import qualified Data.Map as M
import qualified Data.Set as S
import Foreign hiding (newForeignPtr)
import Foreign.Concurrent

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
instance Container ParameterHandleValuePairSet where
    type Elem ParameterHandleValuePairSet = (ParameterHandle, BS.ByteString)

instance KVContainer ParameterHandleValuePairSet where
    type Key ParameterHandleValuePairSet = ParameterHandle
    type Value ParameterHandleValuePairSet = BS.ByteString
    key = fst
    value = snd

instance HasCount IO ParameterHandleValuePairSet where
    count phvps = 
        withParameterHandleValuePairSet phvps $ \phvps ->
            fmap fromIntegral $
                wrapExceptions (wrap_ParameterHandleValuePairSet_size phvps)

parameterHandleValuePairSet_getHandle :: ParameterHandleValuePairSet -> ULong -> IO ParameterHandle
parameterHandleValuePairSet_getHandle phSet i =
    withParameterHandleValuePairSet phSet $ \phSet ->
        wrapExceptions (wrap_ParameterHandleValuePairSet_getHandle phSet i)

parameterHandleValuePairSet_getValueLength :: ParameterHandleValuePairSet -> ULong -> IO ULong
parameterHandleValuePairSet_getValueLength phSet i =
    withParameterHandleValuePairSet phSet $ \phSet ->
        wrapExceptions (wrap_ParameterHandleValuePairSet_getValueLength phSet i)

parameterHandleValuePairSet_getValue :: ParameterHandleValuePairSet -> ULong -> IO BS.ByteString
parameterHandleValuePairSet_getValue phSet i = do
    withParameterHandleValuePairSet phSet $ \phSet ->
        alloca $ \lenCell -> do
            buf <- wrapExceptions (wrap_ParameterHandleValuePairSet_getValuePointer phSet i lenCell)
            
            len <- peek lenCell
            BS.packCStringLen (buf, fromIntegral len)

parameterHandleValuePairSet_getTransportType :: ParameterHandleValuePairSet -> ULong -> IO TransportType
parameterHandleValuePairSet_getTransportType phSet i = 
    withParameterHandleValuePairSet phSet $ \phSet ->
        wrapExceptions (wrap_ParameterHandleValuePairSet_getTransportType phSet i)

parameterHandleValuePairSet_getOrderType :: ParameterHandleValuePairSet -> ULong -> IO OrderType
parameterHandleValuePairSet_getOrderType phSet i = 
    withParameterHandleValuePairSet phSet $ \phSet ->
        wrapExceptions (wrap_ParameterHandleValuePairSet_getOrderType phSet i)

parameterHandleValuePairSet_getRegion :: ParameterHandleValuePairSet -> ULong -> IO Region
parameterHandleValuePairSet_getRegion phSet i =
    withParameterHandleValuePairSet phSet $ \phSet -> do
        region <- wrapExceptions (wrap_ParameterHandleValuePairSet_getRegion phSet i)
        regionPtr <- newForeignPtr_ region
        return (Region regionPtr)

instance Insert IO ParameterHandleValuePairSet where
    insert pSet (h,buff) = withParameterHandleValuePairSet pSet $ \pSet ->
        BS.useAsCString buff $ \buff ->
            wrapExceptions (wrap_ParameterHandleValuePairSet_add pSet h buff valueLength)
        where 
            valueLength = fromIntegral (BS.length buff)

instance RemoveKey IO ParameterHandleValuePairSet where
    removeKey pSet h = withParameterHandleValuePairSet pSet $ \pSet ->
        wrapExceptions (wrap_ParameterHandleValuePairSet_remove pSet h)
        
parameterHandleValuePairSet_moveFrom :: ParameterHandleValuePairSet -> ParameterHandleValuePairSet -> ULong -> IO ULong
parameterHandleValuePairSet_moveFrom phSet from i =
    withParameterHandleValuePairSet phSet $ \phSet ->
        withParameterHandleValuePairSet from $ \from ->
            alloca $ \iCell -> do
                poke iCell i
                wrapExceptions (wrap_ParameterHandleValuePairSet_moveFrom phSet from iCell)
                peek iCell

instance Empty IO ParameterHandleValuePairSet where
    empty phSet =
        withParameterHandleValuePairSet phSet $ \phSet ->
            wrapExceptions (wrap_ParameterHandleValuePairSet_empty phSet)

instance ToList IO ParameterHandleValuePairSet where
    toList phSet = 
        let go xs i = do
                valid <- parameterHandleValuePairSet_valid phSet i
                if valid
                    then do
                        hndl <- parameterHandleValuePairSet_getHandle phSet i
                        val  <- parameterHandleValuePairSet_getValue  phSet i
                        
                        next <- parameterHandleValuePairSet_start phSet
                        go (((hndl,val) :) . xs) next
                    else return (xs [])
         in parameterHandleValuePairSet_start phSet >>= go id

parameterHandleValuePairSet_start :: ParameterHandleValuePairSet -> IO ULong
parameterHandleValuePairSet_start phSet =
    withParameterHandleValuePairSet phSet $ \phSet ->
        wrapExceptions (wrap_ParameterHandleValuePairSet_start phSet)

parameterHandleValuePairSet_valid :: ParameterHandleValuePairSet -> ULong -> IO Bool
parameterHandleValuePairSet_valid phSet i =
    withParameterHandleValuePairSet phSet $ \phSet ->
        fmap (/= 0) $
            wrapExceptions (wrap_ParameterHandleValuePairSet_valid phSet i)

parameterHandleValuePairSet_next :: ParameterHandleValuePairSet -> ULong -> IO ULong
parameterHandleValuePairSet_next phSet i =
    withParameterHandleValuePairSet phSet $ \phSet ->
        wrapExceptions (wrap_ParameterHandleValuePairSet_next phSet i)

-- * ParameterSetFactory

instance NewContainer IO ParameterHandleValuePairSet where
    newContainer n = parameterSetFactory_create (maybe 0 fromIntegral n)
    fromList xs = do
        phvps <- parameterSetFactory_create (genericLength xs)
        mapM_ (insert phvps) xs
        return phvps

parameterSetFactory_create :: ULong -> IO ParameterHandleValuePairSet
parameterSetFactory_create count = do
    pSet <- wrapExceptions (wrap_ParameterSetFactory_create count)
    pSet <- newForeignPtr pSet (delete_ParameterHandleValuePairSet pSet)
    return (ParameterHandleValuePairSet pSet)

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

