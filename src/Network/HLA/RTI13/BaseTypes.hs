{-# LANGUAGE 
        GeneralizedNewtypeDeriving, 
        DeriveDataTypeable
  #-}
module Network.HLA.RTI13.BaseTypes where

import Data.Int
import Data.Word
import Data.Bits
import Data.Generics
import Foreign.Storable
import Text.Printf

-- * Primitive numeric types with specified sizes

-- |'Short' is a 16-bit signed integer type
newtype Short   = Short  Int16      deriving (Eq, Ord, Bits, Enum, Bounded, Num, Real, Integral, Data, Typeable, Storable, PrintfArg)
-- |'UShort' is a 16-bit unsigned integer type
newtype UShort  = UShort Word16     deriving (Eq, Ord, Bits, Enum, Bounded, Num, Real, Integral, Data, Typeable, Storable, PrintfArg)

-- |'Long' is a 32-bit signed integer type
newtype Long    = Long   Int32      deriving (Eq, Ord, Bits, Enum, Bounded, Num, Real, Integral, Data, Typeable, Storable, PrintfArg)
-- |'ULong' is a 32-bit unsigned integer type, and is by far the most widely-used numeric type in the RTI interface.
newtype ULong   = ULong  Word32     deriving (Eq, Ord, Bits, Enum, Bounded, Num, Real, Integral, Data, Typeable, Storable, PrintfArg)

instance Show Short  where showsPrec p (Short  x) = showsPrec p x
instance Show UShort where showsPrec p (UShort x) = showsPrec p x
instance Show Long   where showsPrec p (Long   x) = showsPrec p x
instance Show ULong  where showsPrec p (ULong  x) = showsPrec p x

instance Read Short  where readsPrec p s = [(Short  x, rest) | (x, rest) <- readsPrec p s]
instance Read UShort where readsPrec p s = [(UShort x, rest) | (x, rest) <- readsPrec p s]
instance Read Long   where readsPrec p s = [(Long   x, rest) | (x, rest) <- readsPrec p s]
instance Read ULong  where readsPrec p s = [(ULong  x, rest) | (x, rest) <- readsPrec p s]
