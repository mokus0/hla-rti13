{-# LANGUAGE NamedFieldPuns #-}
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Verbosity
import System.Environment
import System.FilePath

main = do
    hooks <- hook_RTI_HOME simpleUserHooks
    defaultMainWithHooks hooks

hook_RTI_HOME hooks@UserHooks{preConf, confHook, postConf} = do
    env <- getEnvironment
    let rtiRoot = do
            rti_home <- lookup "RTI_HOME"       env
            rti_type <- lookup "RTI_BUILD_TYPE" env
            return (rti_home </> rti_type)
        
        rtiLibDir = fmap (</> "lib")     rtiRoot
        rtiIncDir = fmap (</> "include") rtiRoot
        
        addLib f x flags@ConfigFlags{configExtraLibDirs, configExtraIncludeDirs} = f x flags
            { configExtraLibDirs     = maybe id (:) rtiLibDir configExtraLibDirs 
            , configExtraIncludeDirs = maybe id (:) rtiIncDir configExtraIncludeDirs 
            }
    
    return hooks
        { preConf  = addLib preConf
        , confHook = addLib confHook
        , postConf = addLib postConf
        }
    
