{-# LANGUAGE
        TypeFamilies,
        MultiParamTypeClasses
  #-}
module Network.HLA.RTI13.RTITypes
    ( module Network.HLA.RTI13.RTITypes.Types
    , module Network.HLA.RTI13.RTITypes
    ) where

import Network.HLA.RTI13.BaseTypes
import Network.HLA.RTI13.RTIException
import Network.HLA.RTI13.RTITypes.Types
import Network.HLA.RTI13.RTITypes.FFI
import Foreign hiding (newForeignPtr)
import Foreign.Concurrent
import Data.Container.Mutable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.List (genericLength)

-- AttributeHandleValuePairSet:

instance Container AttributeHandleValuePairSet where
    type Elem AttributeHandleValuePairSet = (AttributeHandle, BS.ByteString)

instance KVContainer AttributeHandleValuePairSet where
    type Key AttributeHandleValuePairSet = AttributeHandle
    type Value AttributeHandleValuePairSet = BS.ByteString
    key = fst
    value = snd

instance HasCount IO AttributeHandleValuePairSet where
    count ahSet =  fmap fromIntegral $
        withAttributeHandleValuePairSet ahSet $ \ahSet ->
            wrapExceptions (wrap_AttributeHandleValuePairSet_size ahSet)

attributeHandleValuePairSet_getHandle :: AttributeHandleValuePairSet -> ULong -> IO AttributeHandle
attributeHandleValuePairSet_getHandle ahSet i =
    withAttributeHandleValuePairSet ahSet $ \ahSet ->
        wrapExceptions (wrap_AttributeHandleValuePairSet_getHandle ahSet i)

attributeHandleValuePairSet_getValueLength :: AttributeHandleValuePairSet -> ULong -> IO ULong
attributeHandleValuePairSet_getValueLength ahSet i =
    withAttributeHandleValuePairSet ahSet $ \ahSet ->
        wrapExceptions (wrap_AttributeHandleValuePairSet_getValueLength ahSet i)

attributeHandleValuePairSet_getValue :: AttributeHandleValuePairSet -> ULong -> IO BS.ByteString
attributeHandleValuePairSet_getValue ahSet i =
    withAttributeHandleValuePairSet ahSet $ \ahSet ->
        alloca $ \lenCell -> do
            buf <- wrapExceptions (wrap_AttributeHandleValuePairSet_getValuePointer ahSet i lenCell)
            
            len <- peek lenCell
            BS.packCStringLen (buf, fromIntegral len)

attributeHandleValuePairSet_getTransportType :: AttributeHandleValuePairSet -> ULong -> IO TransportType
attributeHandleValuePairSet_getTransportType ahSet i = 
    withAttributeHandleValuePairSet ahSet $ \ahSet ->
        wrapExceptions (wrap_AttributeHandleValuePairSet_getTransportType ahSet i)

attributeHandleValuePairSet_getOrderType :: AttributeHandleValuePairSet -> ULong -> IO OrderType
attributeHandleValuePairSet_getOrderType ahSet i = 
    withAttributeHandleValuePairSet ahSet $ \ahSet ->
        wrapExceptions (wrap_AttributeHandleValuePairSet_getOrderType ahSet i)

attributeHandleValuePairSet_getRegion :: AttributeHandleValuePairSet -> ULong -> IO Region
attributeHandleValuePairSet_getRegion ahSet i =
    withAttributeHandleValuePairSet ahSet $ \ahSet -> do
        region <- wrapExceptions (wrap_AttributeHandleValuePairSet_getRegion ahSet i)
        regionPtr <- newForeignPtr_ region
        return (Region regionPtr)

instance Insert IO AttributeHandleValuePairSet where
    insert ahSet (h, bs) =
        withAttributeHandleValuePairSet ahSet $ \ahSet ->
            BS.unsafeUseAsCString bs $ \buf -> 
                wrapExceptions (wrap_AttributeHandleValuePairSet_add ahSet h buf len)
        where len = fromIntegral (BS.length bs)

instance RemoveKey IO AttributeHandleValuePairSet where
    removeKey ahSet h =
        withAttributeHandleValuePairSet ahSet $ \ahSet ->
            wrapExceptions (wrap_AttributeHandleValuePairSet_remove ahSet h)

attributeHandleValuePairSet_moveFrom :: AttributeHandleValuePairSet -> AttributeHandleValuePairSet -> ULong -> IO ULong
attributeHandleValuePairSet_moveFrom ahSet from i =
    withAttributeHandleValuePairSet ahSet $ \ahSet ->
        withAttributeHandleValuePairSet from $ \from ->
            alloca $ \iCell -> do
                poke iCell i
                wrapExceptions (wrap_AttributeHandleValuePairSet_moveFrom ahSet from iCell)
                peek iCell

instance Empty IO AttributeHandleValuePairSet where
    empty ahSet =
        withAttributeHandleValuePairSet ahSet $ \ahSet ->
            wrapExceptions (wrap_AttributeHandleValuePairSet_empty ahSet)

instance ToList IO AttributeHandleValuePairSet where
    toList ahSet = 
        let go xs i = do
                valid <- attributeHandleValuePairSet_valid ahSet i
                if valid
                    then do
                        hndl <- attributeHandleValuePairSet_getHandle ahSet i
                        val  <- attributeHandleValuePairSet_getValue  ahSet i
                        
                        next <- attributeHandleValuePairSet_start ahSet
                        go (((hndl,val) :) . xs) next
                    else return (xs [])
         in attributeHandleValuePairSet_start ahSet >>= go id

attributeHandleValuePairSet_start :: AttributeHandleValuePairSet -> IO ULong
attributeHandleValuePairSet_start ahSet =
    withAttributeHandleValuePairSet ahSet $ \ahSet ->
        wrapExceptions (wrap_AttributeHandleValuePairSet_start ahSet)

attributeHandleValuePairSet_valid :: AttributeHandleValuePairSet -> ULong -> IO Bool
attributeHandleValuePairSet_valid ahSet i =
    withAttributeHandleValuePairSet ahSet $ \ahSet ->
        fmap (/= 0) $
            wrapExceptions (wrap_AttributeHandleValuePairSet_valid ahSet i)

attributeHandleValuePairSet_next :: AttributeHandleValuePairSet -> ULong -> IO ULong
attributeHandleValuePairSet_next ahSet i =
    withAttributeHandleValuePairSet ahSet $ \ahSet ->
        wrapExceptions (wrap_AttributeHandleValuePairSet_next ahSet i)

-- AttributeSetFactory:

instance NewContainer IO AttributeHandleValuePairSet where
    newContainer n = attributeSetFactory_create (maybe 0 fromIntegral n)
    fromList xs = do
        ahvps <- attributeSetFactory_create (genericLength xs)
        mapM_ (insert ahvps) xs
        return ahvps
        
    
attributeSetFactory_create :: ULong -> IO AttributeHandleValuePairSet
attributeSetFactory_create n = do
    ahvps <- wrapExceptions (wrap_AttributeSetFactory_create n)
    ahvps <- newForeignPtr ahvps (delete_AttributeHandleValuePairSet ahvps)
    return (AttributeHandleValuePairSet ahvps)

-- data AttributeHandleSet

-- |Import an AttributeHandleSet pointer, accepting responsibility to delete
-- it when the object goes out of scope.
importAttributeHandleSet :: Ptr AttributeHandleSet -> IO AttributeHandleSet
importAttributeHandleSet ahSetPtr = do
    fPtr <- newForeignPtr ahSetPtr (delete_AttributeHandleSet ahSetPtr)
    return (AttributeHandleSet fPtr)
    

instance Container AttributeHandleSet where
    type Elem AttributeHandleSet = AttributeHandle

instance HasCount IO AttributeHandleSet where
    count ahSet =
        withAttributeHandleSet ahSet $ \ahSet ->
            fmap fromIntegral $
                wrapExceptions (wrap_AttributeHandleSet_size ahSet)

attributeHandleSet_getHandle ahSet i =
    withAttributeHandleSet ahSet $ \ahSet -> 
        wrapExceptions (wrap_AttributeHandleSet_getHandle ahSet i)

instance Insert IO AttributeHandleSet where
    insert ahSet ah = 
        withAttributeHandleSet ahSet $ \ahSet ->
            wrapExceptions (wrap_AttributeHandleSet_add ahSet ah)

instance Remove IO AttributeHandleSet where
    remove ahSet ah = 
        withAttributeHandleSet ahSet $ \ahSet ->
            wrapExceptions (wrap_AttributeHandleSet_remove ahSet ah)

instance Empty IO AttributeHandleSet where
    empty ahSet =
        withAttributeHandleSet ahSet $ \ahSet ->
            wrapExceptions (wrap_AttributeHandleSet_empty ahSet)

instance IsEmpty IO AttributeHandleSet where
    isEmpty ahSet =
        withAttributeHandleSet ahSet $ \ahSet ->
            wrapExceptions (wrap_AttributeHandleSet_isEmpty ahSet)

instance Contains IO AttributeHandleSet where
    ahSet `contains` h =
        withAttributeHandleSet ahSet $ \ahSet ->
            wrapExceptions (wrap_AttributeHandleSet_isMember ahSet h)


-- AttributeHandleSetFactory:

instance NewContainer IO AttributeHandleSet where
    newContainer n = attributeHandleSetFactory_create (maybe 0 fromIntegral n)
    fromList xs = do
        ahSet <- attributeHandleSetFactory_create (genericLength xs)
        mapM_ (insert ahSet) xs
        return ahSet

attributeHandleSetFactory_create :: ULong -> IO AttributeHandleSet
attributeHandleSetFactory_create n = do
    ahSet <- wrapExceptions (wrap_AttributeHandleSetFactory_create n)
    ahSet <- newForeignPtr ahSet (delete_AttributeHandleSet ahSet)
    return (AttributeHandleSet ahSet)

-- FederateHandleSet:

instance Container FederateHandleSet where
    type Elem FederateHandleSet = FederateHandle

instance HasCount IO FederateHandleSet where
    count fhSet =
        withFederateHandleSet fhSet $ \fhSet ->
            fmap fromIntegral $
                wrapExceptions (wrap_FederateHandleSet_size fhSet)

federateHandleSet_getHandle fhSet i =
    withFederateHandleSet fhSet $ \fhSet -> 
        wrapExceptions (wrap_FederateHandleSet_getHandle fhSet i)

instance Insert IO FederateHandleSet where
    insert fhSet fh = 
        withFederateHandleSet fhSet $ \fhSet ->
            wrapExceptions (wrap_FederateHandleSet_add fhSet fh)

instance Remove IO FederateHandleSet where
    remove fhSet fh = 
        withFederateHandleSet fhSet $ \fhSet ->
            wrapExceptions (wrap_FederateHandleSet_remove fhSet fh)

instance Empty IO FederateHandleSet where
    empty fhSet =
        withFederateHandleSet fhSet $ \fhSet ->
            wrapExceptions (wrap_FederateHandleSet_empty fhSet)

instance Contains IO FederateHandleSet where
    fhSet `contains` h =
        withFederateHandleSet fhSet $ \fhSet ->
            wrapExceptions (wrap_FederateHandleSet_isMember fhSet h)

-- data FederateHandleSetFactory

instance NewContainer IO FederateHandleSet where
    newContainer n = federateHandleSetFactory_create (maybe 0 fromIntegral n)
    fromList xs = do
        fhSet <- federateHandleSetFactory_create (genericLength xs)
        mapM_ (insert fhSet) xs
        return fhSet

federateHandleSetFactory_create :: ULong -> IO FederateHandleSet
federateHandleSetFactory_create n = do
    fhSet <- wrapExceptions (wrap_FederateHandleSetFactory_create n)
    fhSet <- newForeignPtr fhSet (delete_FederateHandleSet fhSet)
    return (FederateHandleSet fhSet)

-- data ParameterHandleValuePairSet
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
        BS.unsafeUseAsCString buff $ \buff ->
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

-- data ParameterSetFactory

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

-- Region:

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

-- data FedTime
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

--data FedTimeFactory
    -- public:
    --   static FedTime* makeZero()
    --     throw (
    --       MemoryExhausted);
    -- 
    --   static FedTime* decode(const char *buf)
    --     throw (
    --       MemoryExhausted);
    -- };

