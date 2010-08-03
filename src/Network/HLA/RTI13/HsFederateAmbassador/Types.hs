{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
module Network.HLA.RTI13.HsFederateAmbassador.Types where

import Network.HLA.RTI13.FedTime
import Network.HLA.RTI13.RTITypes

import Control.Monad.Reader
import Foreign

-- |Parameterized over type of FedTime
newtype HsFederateAmbassador t = HsFederateAmbassador (ForeignPtr (HsFederateAmbassador t))
instance FedTimeImpl t => FederateAmbassador (HsFederateAmbassador t) where
    type FedAmbTime (HsFederateAmbassador t) = t
    withFederateAmbassador fedAmb action = 
        withHsFederateAmbassador fedAmb (action . castPtr)

withHsFederateAmbassador (HsFederateAmbassador fedAmb) = withForeignPtr fedAmb

newtype FedHandlers t a = FedHandlers (ReaderT (HsFederateAmbassador t) IO a)
    deriving (Functor, Monad, MonadIO, MonadReader (HsFederateAmbassador t))

setHandlers :: HsFederateAmbassador t -> FedHandlers t () -> IO ()
setHandlers fedAmb (FedHandlers handlers) = runReaderT handlers fedAmb

