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
    
--             = hooks           {preConf = newPreConf}
--     where
--         newPreConf args flags@ConfigFlags{configExtraLibDirs = ldirs} = do
--             let dprint level = case configVerbosity flags of
--                     Flag v | v >= level     -> putStrLn
--                     _                       -> const (return ())
--             
--             dprint verbose "In hook_RTI_HOME"
--             
--             
--             dprint verbose ("rtiRoot   = " ++ show rtiRoot)
--             dprint verbose ("rtiLibDir = " ++ show rtiLibDir)
--             
--             let hookedFlags = flags{configExtraLibDirs = maybe ldirs (:ldirs) rtiLibDir}
--             
--             dprint deafening (show hookedFlags)
--             result <- oldPreConf args hookedFlags
--             
--             dprint verbose "oldPreConf exited"
--             dprint deafening ("oldPreConf returned: " ++ show result)
--             return result
-- 