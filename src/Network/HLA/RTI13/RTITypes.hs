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
import Foreign.C

-- AttributeHandleValuePairSet:

attributeHandleValuePairSet_size :: AttributeHandleValuePairSet -> IO ULong
attributeHandleValuePairSet_size ahSet =
    withAttributeHandleValuePairSet ahSet $ \ahSet ->
        wrapExceptions (wrap_AttributeHandleValuePairSet_size ahSet)

attributeHandleValuePairSet_getHandle :: AttributeHandleValuePairSet -> ULong -> IO Handle
attributeHandleValuePairSet_getHandle ahSet i =
    withAttributeHandleValuePairSet ahSet $ \ahSet ->
        wrapExceptions (wrap_AttributeHandleValuePairSet_getHandle ahSet i)

attributeHandleValuePairSet_getValueLength :: AttributeHandleValuePairSet -> ULong -> IO ULong
attributeHandleValuePairSet_getValueLength ahSet i =
    withAttributeHandleValuePairSet ahSet $ \ahSet ->
        wrapExceptions (wrap_AttributeHandleValuePairSet_getValueLength ahSet i)

attributeHandleValuePairSet_getValue :: AttributeHandleValuePairSet -> ULong -> IO BS.ByteString
attributeHandleValuePairSet_getValue ahSet i = do
    len <- attributeHandleValuePairSet_getValueLength ahSet i
    
    withAttributeHandleValuePairSet ahSet $ \ahSet ->
        alloca $ \lenCell -> do
            poke lenCell len
            buf <- mallocBytes (fromIntegral len + 1)
            wrapExceptions (wrap_AttributeHandleValuePairSet_getValue ahSet i buf lenCell)
            
            len <- peek lenCell
            buf <- reallocBytes buf (fromIntegral len)
            BS.unsafePackMallocCString buf

    --   virtual char *getValuePointer(
    --     ULong i,
    --     ULong&     valueLength) const
    --     throw (
    --       ArrayIndexOutOfBounds) = 0;

attributeHandleValuePairSet_getTransportType :: AttributeHandleValuePairSet -> ULong -> IO TransportType
attributeHandleValuePairSet_getTransportType ahSet i = 
    withAttributeHandleValuePairSet ahSet $ \ahSet ->
        wrapExceptions (wrap_AttributeHandleValuePairSet_getTransportType ahSet i)

attributeHandleValuePairSet_getOrderType :: AttributeHandleValuePairSet -> ULong -> IO OrderType
attributeHandleValuePairSet_getOrderType ahSet i = 
    withAttributeHandleValuePairSet ahSet $ \ahSet ->
        wrapExceptions (wrap_AttributeHandleValuePairSet_getOrderType ahSet i)

    --   virtual Region *getRegion(
    --     ULong i) const
    --     throw (
    --       ArrayIndexOutOfBounds,
    --       InvalidHandleValuePairSetContext) = 0;

instance Container AttributeHandleValuePairSet where
    type Elem AttributeHandleValuePairSet = (Handle, BS.ByteString)

instance Insert IO AttributeHandleValuePairSet where
    insert ahSet (h, bs) =
        withAttributeHandleValuePairSet ahSet $ \ahSet ->
            BS.unsafeUseAsCString bs $ \buf -> 
                wrapExceptions (wrap_AttributeHandleValuePairSet_add ahSet h buf len)
        where len = fromIntegral (BS.length bs)

    --   virtual void remove(		// not guaranteed safe while iterating
    --     Handle      h)
    --     throw (
    --       ArrayIndexOutOfBounds) = 0;
    --   
    --   virtual void moveFrom(
    --     const AttributeHandleValuePairSet& ahvps,
    --     ULong&               i)
    --     throw (
    --       ValueCountExceeded,
    --       ArrayIndexOutOfBounds) = 0;

attributeHandleValuePairSet_empty :: AttributeHandleValuePairSet -> IO ()
attributeHandleValuePairSet_empty ahSet =
    withAttributeHandleValuePairSet ahSet $ \ahSet ->
        wrapExceptions (wrap_AttributeHandleValuePairSet_empty ahSet)

    --   virtual ULong start() const = 0;
    --   virtual ULong valid(ULong i) const = 0;
    --   virtual ULong next(ULong i) const = 0;
    -- };


--data AttributeSetFactory
    -- public:
    --   static AttributeHandleValuePairSet* create(
    --     ULong count)
    --     throw (
    --       MemoryExhausted,
    --       ValueCountExceeded,
    --       HandleValuePairMaximumExceeded);
    -- };

-- data AttributeHandleSet
instance Container AttributeHandleSet where
    type Elem AttributeHandleSet = AttributeHandle

    --   virtual ULong size() const = 0;
    -- 
    --   virtual AttributeHandle getHandle(ULong i) const
    --     throw (
    --       ArrayIndexOutOfBounds) = 0;
    --   


instance Insert IO AttributeHandleSet where
    insert ahSet ah = 
        withAttributeHandleSet ahSet $ \ahSet ->
            wrapExceptions (wrap_AttributeHandleSet_add ahSet ah)
    --   
    --   virtual void remove(AttributeHandle h)
    --     throw (			// not guaranteed safe while iterating
    --       AttributeNotDefined) = 0;
    --   
    --   virtual void empty() = 0; // Empty the Set
    -- 
    --   virtual Boolean isEmpty() const = 0;  //is set empty?
    --   virtual Boolean isMember(AttributeHandle h) const = 0;
    -- };


-- data AttributeHandleSetFactory

attributeHandleSetWithCapacity :: ULong -> IO AttributeHandleSet
attributeHandleSetWithCapacity n = do
    ahSet <- attributeHandleSetFactory_create n
    ahSet <- newForeignPtr ahSet (delete_AttributeHandleSet ahSet)
    return (AttributeHandleSet ahSet)

-- data FederateHandleSet
    -- public:
    -- 
    --   virtual ~FederateHandleSet() {}
    -- 
    --   virtual ULong size() const = 0;
    -- 
    --   virtual FederateHandle getHandle(ULong i) const
    --     throw (
    --       ArrayIndexOutOfBounds) = 0;
    --   
    --   virtual void add(FederateHandle h)
    --     throw (
    --       ValueCountExceeded) = 0;
    --   
    --   virtual void remove(FederateHandle h)
    --     throw (
    --       ArrayIndexOutOfBounds) = 0;
    --   
    --   virtual void empty() = 0; // Empty the set without deallocating space.
    -- 
    --   virtual Boolean isMember(FederateHandle h) const = 0;
    -- };

-- data FederateHandleSetFactory
    -- public:
    --   static FederateHandleSet* create(ULong count)
    --     throw (
    --       MemoryExhausted,
    --       ValueCountExceeded);
    -- };
    -- 

-- data ParameterHandleValuePairSet
instance Container ParameterHandleValuePairSet where
    type Elem ParameterHandleValuePairSet = (ParameterHandle, BS.ByteString)
    -- public:
    --   virtual ~ParameterHandleValuePairSet() { ; }
    -- 
    --   virtual ULong size() const = 0;
    --   
    --   virtual Handle getHandle(
    --     ULong i) const
    --     throw (
    --       ArrayIndexOutOfBounds) = 0;
    --   
    --   virtual ULong getValueLength(
    --     ULong i) const
    --     throw (
    --       ArrayIndexOutOfBounds) = 0;
    --   
    --   virtual void getValue(
    --     ULong i,
    --     char*      buff,
    --     ULong&     valueLength) const
    --     throw (
    --       ArrayIndexOutOfBounds) = 0;
    --   
    --   virtual char *getValuePointer(
    --     ULong i,
    --     ULong&     valueLength) const
    --     throw (
    --       ArrayIndexOutOfBounds) = 0;
    --   
    --   virtual TransportType getTransportType(void) const
    --     throw ( InvalidHandleValuePairSetContext) = 0;
    --   
    --   virtual OrderType getOrderType(void) const
    --     throw ( InvalidHandleValuePairSetContext) = 0;
    --   
    --   virtual Region *getRegion(void) const
    --     throw ( InvalidHandleValuePairSetContext) = 0;
    --   
instance Insert IO ParameterHandleValuePairSet where
    insert pSet (h,buff) = withParameterHandleValuePairSet pSet $ \pSet ->
        BS.unsafeUseAsCString buff $ \buff ->
            wrapExceptions (wrap_ParameterHandleValuePairSet_add pSet h buff valueLength)
        where 
            valueLength = fromIntegral (BS.length buff)

    --   virtual void remove(		// not guaranteed safe while iterating
    --     Handle      h)
    --     throw (
    --       ArrayIndexOutOfBounds) = 0;
    --   
    --   virtual void moveFrom(
    --     const ParameterHandleValuePairSet& phvps,
    --     ULong&               i)
    --     throw (
    --       ValueCountExceeded,
    --       ArrayIndexOutOfBounds) = 0;
    --   
    --   virtual void empty() = 0; // Empty the Set without deallocating space.
    -- 
    --   virtual ULong start() const = 0;
    --   virtual ULong valid(ULong i) const = 0;
    --   virtual ULong next(ULong i) const = 0;
    -- };

-- data ParameterSetFactory
parameterSetWithCapacity :: ULong -> IO ParameterHandleValuePairSet
parameterSetWithCapacity count = do
    pSet <- parameterSetFactory_create count
    pSet <- newForeignPtr pSet (delete_ParameterHandleValuePairSet pSet)
    return (ParameterHandleValuePairSet pSet)

-- data Region
    -- public:
    -- 
    --   virtual ULong getRangeLowerBound(
    --     ExtentIndex     theExtent,
    --     DimensionHandle theDimension) const
    --     throw (
    --       ArrayIndexOutOfBounds) = 0;
    -- 
    --   virtual ULong getRangeUpperBound(
    --     ExtentIndex     theExtent,
    --     DimensionHandle theDimension) const
    --     throw (
    --       ArrayIndexOutOfBounds) = 0;

setRangeLowerBound :: Region -> ExtentIndex -> DimensionHandle -> ULong -> IO ()
setRangeLowerBound theRegion theExtent theDimension theLowerBound = 
    withRegion theRegion $ \theRegion -> 
        wrapExceptions (wrap_Region_setRangeLowerBound theRegion theExtent theDimension theLowerBound)

setRangeUpperBound :: Region -> ExtentIndex -> DimensionHandle -> ULong -> IO ()
setRangeUpperBound theRegion theExtent theDimension theUpperBound = 
    withRegion theRegion $ \theRegion -> 
        wrapExceptions (wrap_Region_setRangeUpperBound theRegion theExtent theDimension theUpperBound)

    --   virtual SpaceHandle getSpaceHandle() const
    --     throw (
    --       ) = 0;
    -- 
    --   virtual ULong getNumberOfExtents() const
    --     throw (
    --       ) = 0;
    -- 
    --   virtual ULong getRangeLowerBoundNotificationLimit(
    --     ExtentIndex     theExtent,
    --     DimensionHandle theDimension) const
    --     throw (
    --       ArrayIndexOutOfBounds) = 0;
    -- 
    --   virtual ULong getRangeUpperBoundNotificationLimit(
    --     ExtentIndex     theExtent,
    --     DimensionHandle theDimension) const
    --     throw (
    --       ArrayIndexOutOfBounds) = 0;
    --       
    --   static ULong getMaxExtent() 
    --     throw ();
    --   
    --   static ULong getMinExtent() 
    --     throw ();
    -- 
    -- };

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

