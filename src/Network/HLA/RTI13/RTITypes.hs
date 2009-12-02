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

--data AttributeHandleValuePairSet
    -- public:
    --   virtual ~AttributeHandleValuePairSet() { ; }
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
    --   virtual TransportType getTransportType( ULong i) const
    --     throw (
    --       ArrayIndexOutOfBounds,
    --       InvalidHandleValuePairSetContext) = 0;
    --   
    --   virtual OrderType getOrderType( ULong i) const
    --     throw (
    --       ArrayIndexOutOfBounds,
    --       InvalidHandleValuePairSetContext) = 0;
    --   
    --   virtual Region *getRegion(
    --     ULong i) const
    --     throw (
    --       ArrayIndexOutOfBounds,
    --       InvalidHandleValuePairSetContext) = 0;
    --   
    --   virtual void add(
    --     Handle      h,
    --     const char* buff,
    --     ULong       valueLength)
    --     throw (
    --       ValueLengthExceeded,
    --       ValueCountExceeded) = 0;
    --   
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
    --   
    --   virtual void empty() = 0; // Empty the Set without deallocating space.
    -- 
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
            wrapExceptions (wrap_attributeHandleSet_add ahSet ah)
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
            wrapExceptions (wrap_parameterHandleValuePairSet_add pSet h buff valueLength)
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
        wrapExceptions (wrap_setRangeLowerBound theRegion theExtent theDimension theLowerBound)

setRangeUpperBound :: Region -> ExtentIndex -> DimensionHandle -> ULong -> IO ()
setRangeUpperBound theRegion theExtent theDimension theUpperBound = 
    withRegion theRegion $ \theRegion -> 
        wrapExceptions (wrap_setRangeUpperBound theRegion theExtent theDimension theUpperBound)

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

