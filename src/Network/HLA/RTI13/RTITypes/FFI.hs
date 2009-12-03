{-# LANGUAGE 
        ForeignFunctionInterface
  #-}
module Network.HLA.RTI13.RTITypes.FFI where

import Foreign
import Foreign.C
import Network.HLA.RTI13.BaseTypes
import Network.HLA.RTI13.RTIException
import Network.HLA.RTI13.RTITypes.Types
import System.IO.Unsafe

foreign import ccall "wrap/RTItypes.h wrap_DEFAULT_SPACE_NAME" 
    wrap_DEFAULT_SPACE_NAME :: CString
{-# NOINLINE defaultSpaceName #-}
defaultSpaceName = unsafePerformIO (peekCString wrap_DEFAULT_SPACE_NAME)
foreign import ccall "wrap/RTItypes.h wrap_DEFAULT_SPACE_DIMENSION_NAME"
    wrap_DEFAULT_SPACE_DIMENSION_NAME :: CString
{-# NOINLINE defaultSpaceDimensionName #-}
defaultSpaceDimensionName = unsafePerformIO (peekCString wrap_DEFAULT_SPACE_DIMENSION_NAME)

foreign import ccall "wrap/RTItypes.h wrap_RTI_VERSION"
    wrap_RTI_VERSION :: CString
{-# NOINLINE rtiVersion #-}
rtiVersion = unsafePerformIO (peekCString wrap_RTI_VERSION)
foreign import ccall "wrap/RTItypes.h wrap_RTI_INTERNAL_VERSION"
    wrap_RTI_INTERNAL_VERSION :: CString
{-# NOINLINE rtiInternalVersion #-}
rtiInternalVersion = unsafePerformIO (peekCString wrap_RTI_INTERNAL_VERSION)

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

delete_AttributeHandleSet :: Ptr AttributeHandleSet -> IO ()
delete_AttributeHandleSet ahSet = 
    wrapExceptions (wrap_delete_AttributeHandleSet ahSet)
foreign import ccall unsafe "wrap/RTItypes.h wrap_delete_AttributeHandleSet"
    wrap_delete_AttributeHandleSet :: Ptr AttributeHandleSet -> Ptr (Ptr RTIException) -> IO ()


    --   virtual ULong size() const = 0;
    -- 
    --   virtual AttributeHandle getHandle(ULong i) const
    --     throw (
    --       ArrayIndexOutOfBounds) = 0;
    --   
foreign import ccall unsafe "wrap/RTItypes.h wrap_AttributeHandleSet_add"
    wrap_AttributeHandleSet_add :: Ptr AttributeHandleSet -> AttributeHandle -> Ptr (Ptr RTIException) -> IO ()

    --   virtual void add(AttributeHandle h)
    --     throw (
    --       ArrayIndexOutOfBounds,
    --       AttributeNotDefined) = 0;
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

attributeHandleSetFactory_create :: ULong -> IO (Ptr AttributeHandleSet)
attributeHandleSetFactory_create n = 
    wrapExceptions (wrap_AttributeHandleSetFactory_create n)
foreign import ccall unsafe "wrap/RTItypes.h wrap_AttributeHandleSetFactory_create"
    wrap_AttributeHandleSetFactory_create :: ULong -> Ptr (Ptr RTIException) -> IO (Ptr AttributeHandleSet)

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
    -- public:
delete_ParameterHandleValuePairSet :: Ptr ParameterHandleValuePairSet -> IO ()
delete_ParameterHandleValuePairSet pSet = 
    wrapExceptions (wrap_delete_ParameterHandleValuePairSet pSet)
foreign import ccall unsafe "wrap/RTItypes.h wrap_delete_ParameterHandleValuePairSet"
    wrap_delete_ParameterHandleValuePairSet :: Ptr ParameterHandleValuePairSet -> Ptr (Ptr RTIException) -> IO ()

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

foreign import ccall unsafe "wrap/RTItypes.h wrap_ParameterHandleValuePairSet_add"
    wrap_ParameterHandleValuePairSet_add :: Ptr ParameterHandleValuePairSet -> ParameterHandle -> CString -> ULong -> Ptr (Ptr RTIException) -> IO ()

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
parameterSetFactory_create :: ULong -> IO (Ptr ParameterHandleValuePairSet)
parameterSetFactory_create count = 
    wrapExceptions (wrap_ParameterSetFactory_create count)
foreign import ccall unsafe "wrap/RTItypes.h wrap_ParameterSetFactory_create"
    wrap_ParameterSetFactory_create :: ULong -> Ptr (Ptr RTIException) -> IO (Ptr ParameterHandleValuePairSet)

-- data Region
    -- public:
    -- 
delete_Region region = 
    wrapExceptions (wrap_delete_Region region)
foreign import ccall "wrap/RTItypes.h wrap_delete_Region"
    wrap_delete_Region :: Ptr Region -> Ptr (Ptr RTIException) -> IO ()

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

foreign import ccall "wrap/RTItypes.h wrap_Region_setRangeLowerBound"
    wrap_Region_setRangeLowerBound :: Ptr Region -> ExtentIndex -> DimensionHandle -> ULong -> Ptr (Ptr RTIException) -> IO ()

foreign import ccall "wrap/RTItypes.h wrap_Region_setRangeUpperBound"
    wrap_Region_setRangeUpperBound :: Ptr Region -> ExtentIndex -> DimensionHandle -> ULong -> Ptr (Ptr RTIException) -> IO ()

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

