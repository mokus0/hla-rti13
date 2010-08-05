module Network.HLA.RTI13.FedTime.Types where

import Foreign (ForeignPtr)

-- |'RTIFedTime' is the default double-based implementation of the 'FedTimeImpl'
-- interface.  In C++ it is called \"RTI::FedTime\" or \"rti13::FedTime\".
newtype RTIFedTime = RTIFedTime (ForeignPtr RTIFedTime)
    deriving (Eq, Ord, Show)
