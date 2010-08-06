{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleContexts #-}
module Network.HLA.RTI13.HsFederateAmbassador.Types where

import Network.HLA.RTI13.RTITypes

import Control.Monad.Reader
import Foreign

-- |'HsFederateAmbassador' implements a very simple interface for creating
-- federate ambassadors from Haskell code.  It is based on a C++ class of the
-- same name with one installable callback for each method implementation.
-- 
-- From the haskell side, there is a function to install each callback.
-- For convenience, these functions can be chained together in a do-block 
-- defining multiple handlers, which can be installed en masse by the 
-- 'setHandlers' function.  
newtype HsFederateAmbassador t = HsFederateAmbassador (ForeignPtr (HsFederateAmbassador t))
instance FedTimeImpl t => FederateAmbassador (HsFederateAmbassador t) where
    type FedAmbTime (HsFederateAmbassador t) = t
    withFederateAmbassador fedAmb action = 
        withHsFederateAmbassador fedAmb (action . castPtr)

withHsFederateAmbassador :: HsFederateAmbassador fedTime -> (Ptr (HsFederateAmbassador fedTime) -> IO a) -> IO a
withHsFederateAmbassador (HsFederateAmbassador fedAmb) = withForeignPtr fedAmb

type FedHandlers t a = forall m. (MonadReader (HsFederateAmbassador t) m, MonadIO m) => m a

-- |Installs the handlers listed in the 'FedHandlers' argument.  For example:
-- 
-- > setHandlers fedAmb $ do
-- >     onInitiateFederateRestore $ \label handle -> do
-- >         stuff; more stuff; etc
-- >     onFederationRestored $ do
-- >         other stuff
-- >     onFederationNotRestored $ do
-- >         stuff you wish you didn't have to do
-- 
-- 'setHandlers' does not alter any handlers except the ones mentioned.
setHandlers :: HsFederateAmbassador t -> FedHandlers t () -> IO ()
setHandlers fedAmb handlers = runReaderT handlers fedAmb

