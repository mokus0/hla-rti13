module Main where

import Data.ByteString.Char8 (unpack)
import Network.HLA.RTI13
import Network.HLA.RTI13.RTITypes.FFI

-- simple test to ensure dynamic library gets loaded and linked in properly
-- build with "ghc --make", the hla-rti13 package's package description
-- should include info about where to find the libraries needed.
main = do
    putStrLn ("RTI version string: " ++ show (unpack rtiVersion))
    rti_ambassador <- newRTIAmbassador :: IO (RTIAmbassador ())
    putStrLn ("got rti_ambassador: " ++ show rti_ambassador)
