{-# LANGUAGE 
        GeneralizedNewtypeDeriving, 
        DeriveDataTypeable
  #-}
module Network.HLA.RTI13.BaseTypes where

import Data.Int
import Data.Word
import Data.Bits
import Data.Generics
import Control.Exception
import System.IO
import Foreign.Storable
import Text.Printf

newtype Short   = Short  Int16      deriving (Eq, Ord, Bits, Enum, Bounded, Num, Real, Integral, Data, Typeable, Storable, PrintfArg)
newtype UShort  = UShort Word16     deriving (Eq, Ord, Bits, Enum, Bounded, Num, Real, Integral, Data, Typeable, Storable, PrintfArg)
newtype Long    = Long   Int32      deriving (Eq, Ord, Bits, Enum, Bounded, Num, Real, Integral, Data, Typeable, Storable, PrintfArg)
newtype ULong   = ULong  Word32     deriving (Eq, Ord, Bits, Enum, Bounded, Num, Real, Integral, Data, Typeable, Storable, PrintfArg)

instance Show Short  where showsPrec p (Short  x) = showsPrec p x
instance Show UShort where showsPrec p (UShort x) = showsPrec p x
instance Show Long   where showsPrec p (Long   x) = showsPrec p x
instance Show ULong  where showsPrec p (ULong  x) = showsPrec p x

instance Read Short  where readsPrec p s = [(Short  x, rest) | (x, rest) <- readsPrec p s]
instance Read UShort where readsPrec p s = [(UShort x, rest) | (x, rest) <- readsPrec p s]
instance Read Long   where readsPrec p s = [(Long   x, rest) | (x, rest) <- readsPrec p s]
instance Read ULong  where readsPrec p s = [(ULong  x, rest) | (x, rest) <- readsPrec p s]
