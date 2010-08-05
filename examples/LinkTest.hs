module Main where

import Network.HLA.RTI13
import Network.HLA.RTI13.RTITypes.FFI

-- simple test to ensure dynamic library gets loaded and linked in properly
-- build with "ghc --make", the hla-rti13 package's package description
-- should include info about where to find the libraries needed.
main = do
    putStrLn ("testing constants: " ++ show [rtiVersion, defaultSpaceName, show wrap_RTI_MAJOR_VERSION])
    rti_ambassador <- getRTIAmbassador :: IO (RTIAmbassador ())
    putStrLn ("got rti_ambassador: " ++ show rti_ambassador)
