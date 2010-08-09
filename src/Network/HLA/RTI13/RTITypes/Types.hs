{-# LANGUAGE 
        GeneralizedNewtypeDeriving,
        DeriveDataTypeable,
        ForeignFunctionInterface,
        ExistentialQuantification,
        FlexibleInstances, FlexibleContexts,
        TypeFamilies
  #-}
module Network.HLA.RTI13.RTITypes.Types where

import Data.Generics
import Foreign
import Network.HLA.RTI13.BaseTypes
import Text.Printf

-- * ResignAction (An enumeration)

data ResignAction
    = ReleaseAttributes
    | DeleteObjects
    | DeleteObjectsAndReleaseAttributes
    | NoAction
    deriving (Eq, Show, Read, Ord, Bounded)

instance Enum ResignAction where
    toEnum 1 = ReleaseAttributes
    toEnum 2 = DeleteObjects
    toEnum 3 = DeleteObjectsAndReleaseAttributes
    toEnum 4 = NoAction
    toEnum x = error (show x ++ " is not a valid ResignAction value")

    fromEnum ReleaseAttributes                  = 1
    fromEnum DeleteObjects                      = 2
    fromEnum DeleteObjectsAndReleaseAttributes  = 3
    fromEnum NoAction                           = 4

-- * FedTime (an abstract representation of time)

class FedTimeImpl time where
    type FedTime time
    withFedTimeInOut  :: FedTime time -> (Ptr time -> IO a) -> IO (FedTime time, a)
    withFedTimeInOut d f = withFedTimeIn d $ \time -> do
        result  <- f time
        newTime <- importFedTime time
        return (newTime, result)
    withFedTimeIn  :: FedTime time -> (Ptr time -> IO a) -> IO a
    withFedTimeOut :: (Ptr time -> IO a) ->  IO (FedTime time)
    importFedTime :: Ptr time -> IO (FedTime time)

class FedTimeImpl (FedAmbTime fedAmb) => FederateAmbassador fedAmb where
    type FedAmbTime fedAmb
    withFederateAmbassador :: fedAmb -> (Ptr fedAmb -> IO a) -> IO a

-- * Primitive numeric types
newtype ExtentIndex = ExtentIndex ULong
    deriving (Eq, Ord, Bits, Enum, Real, Integral, Data, Typeable, Storable)
instance Show ExtentIndex            where showsPrec p (ExtentIndex            x) = showsPrec p x
instance Read ExtentIndex            where readsPrec p s = [(extentIndexCheck "readsPrec" (ExtentIndex x), rest) | (x, rest) <- readsPrec p s]
instance Bounded ExtentIndex where
    minBound = wrap_MIN_EXTENT
    maxBound = wrap_MAX_EXTENT
instance Num ExtentIndex where
    ExtentIndex a + ExtentIndex b = extentIndexCheck "+" (ExtentIndex (a + b))
    ExtentIndex a - ExtentIndex b = extentIndexCheck "-" (ExtentIndex (a - b))
    ExtentIndex a * ExtentIndex b = extentIndexCheck "*" (ExtentIndex (a * b))
    abs = id
    signum (ExtentIndex x) = extentIndexCheck "signum" (ExtentIndex (signum x))
    fromInteger x = extentIndexCheck "fromInteger" (ExtentIndex (fromInteger x))

extentIndexCheck cxt c
    | c < wrap_MIN_EXTENT   = error (cxt ++ ": ExtentIndex underflow")
    | c > wrap_MAX_EXTENT   = error (cxt ++ ": ExtentIndex overflow")
    | otherwise             = c
foreign import ccall "wrap/RTItypes.h wrap_MIN_EXTENT" wrap_MIN_EXTENT :: ExtentIndex
foreign import ccall "wrap/RTItypes.h wrap_MAX_EXTENT" wrap_MAX_EXTENT :: ExtentIndex

newtype TickTime = TickTime Double
    deriving (Eq, Ord, Enum, Num, Real, Fractional, Floating, RealFrac, RealFloat, Data, Typeable, Storable, PrintfArg)
instance Show TickTime               where showsPrec p (TickTime               x) = showsPrec p x
instance Read TickTime               where readsPrec p s = [(TickTime               x, rest) | (x, rest) <- readsPrec p s]

-- * Primitive non-numeric types (handles, etc.)
newtype FederateID             = FederateID             ULong                   deriving (Eq, Ord, Data, Typeable, Storable, PrintfArg)
newtype UniqueID               = UniqueID               ULong                   deriving (Eq, Ord, Data, Typeable, Storable, PrintfArg)
newtype RegionToken            = RegionToken            ULong                   deriving (Eq, Ord, Data, Typeable, Storable, PrintfArg)
newtype Handle                 = Handle                 ULong                   deriving (Eq, Ord, Data, Typeable, Storable, PrintfArg)
newtype FederateHandle         = FederateHandle         ULong                   deriving (Eq, Ord, Data, Typeable, Storable, PrintfArg)
newtype SpaceHandle            = SpaceHandle            Long                    deriving (Eq, Ord, Data, Typeable, Storable, PrintfArg)
newtype ObjectClassHandle      = ObjectClassHandle      ULong                   deriving (Eq, Ord, Data, Typeable, Storable, PrintfArg)
newtype InteractionClassHandle = InteractionClassHandle ULong                   deriving (Eq, Ord, Data, Typeable, Storable, PrintfArg)
newtype AttributeHandle        = AttributeHandle        Handle                  deriving (Eq, Ord, Data, Typeable, Storable, PrintfArg)
newtype ParameterHandle        = ParameterHandle        Handle                  deriving (Eq, Ord, Data, Typeable, Storable, PrintfArg)
newtype ObjectHandle           = ObjectHandle           Handle                  deriving (Eq, Ord, Data, Typeable, Storable, PrintfArg)
newtype DimensionHandle        = DimensionHandle        Handle                  deriving (Eq, Ord, Data, Typeable, Storable, PrintfArg)
newtype TransportationHandle   = TransportationHandle   Handle                  deriving (Eq, Ord, Data, Typeable, Storable, PrintfArg)
newtype TransportType          = TransportType          TransportationHandle    deriving (Eq, Ord, Data, Typeable, Storable, PrintfArg)
newtype OrderingHandle         = OrderingHandle         Handle                  deriving (Eq, Ord, Data, Typeable, Storable, PrintfArg)
newtype OrderType              = OrderType              OrderingHandle          deriving (Eq, Ord, Data, Typeable, Storable, PrintfArg)
instance Show FederateID             where showsPrec p (FederateID             x) = showsPrec p x
instance Show UniqueID               where showsPrec p (UniqueID               x) = showsPrec p x
instance Show RegionToken            where showsPrec p (RegionToken            x) = showsPrec p x
instance Read FederateID             where readsPrec p s = [(FederateID             x, rest) | (x, rest) <- readsPrec p s]
instance Read UniqueID               where readsPrec p s = [(UniqueID               x, rest) | (x, rest) <- readsPrec p s]
instance Read RegionToken            where readsPrec p s = [(RegionToken            x, rest) | (x, rest) <- readsPrec p s]
instance Show Handle                 where showsPrec p (Handle                 x) = showsPrec p x
instance Show FederateHandle         where showsPrec p (FederateHandle         x) = showsPrec p x
instance Show SpaceHandle            where showsPrec p (SpaceHandle            x) = showsPrec p x
instance Show ObjectClassHandle      where showsPrec p (ObjectClassHandle      x) = showsPrec p x
instance Show InteractionClassHandle where showsPrec p (InteractionClassHandle x) = showsPrec p x
instance Show AttributeHandle        where showsPrec p (AttributeHandle        x) = showsPrec p x
instance Show ParameterHandle        where showsPrec p (ParameterHandle        x) = showsPrec p x
instance Show ObjectHandle           where showsPrec p (ObjectHandle           x) = showsPrec p x
instance Show DimensionHandle        where showsPrec p (DimensionHandle        x) = showsPrec p x
instance Show TransportationHandle   where showsPrec p (TransportationHandle   x) = showsPrec p x
instance Show TransportType          where showsPrec p (TransportType          x) = showsPrec p x
instance Show OrderingHandle         where showsPrec p (OrderingHandle         x) = showsPrec p x
instance Show OrderType              where showsPrec p (OrderType              x) = showsPrec p x


-- * Data structures

newtype Region = Region (ForeignPtr Region) deriving (Eq, Ord)
instance Show Region where showsPrec p (Region r) = showsPrec p r
withRegion (Region r) = withForeignPtr r

data EventRetractionHandle = EventRetractionHandle
    { erhSerial             :: UniqueID
    , erhSendingFederate    :: FederateHandle
    }
withEventRetractionHandleReturn :: (Ptr UniqueID -> Ptr FederateHandle -> IO ()) -> IO EventRetractionHandle
withEventRetractionHandleReturn action = 
    alloca $ \uniqId ->
        alloca $ \fedHandle -> do
            action uniqId fedHandle
            uniqId    <- peek uniqId
            fedHandle <- peek fedHandle
            return (EventRetractionHandle uniqId fedHandle)
