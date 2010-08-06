{-# LANGUAGE ImplicitParams, NoMonomorphismRestriction, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module FoodFight where

import Prelude hiding (catch)
import Control.Exception

import Network.HLA.RTI13

import Control.Monad
import Control.Monad.Loops
import Data.Binary.Put
import Data.Binary.IEEE754
import Data.ByteString.Class
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Container.Mutable
import qualified Data.Map as M
import Data.Random
import Data.Random.Distribution.Categorical
import Data.Random.Source.DevRandom
import Data.StateRef
import System.Posix.Process (getProcessID)
import Text.Printf

fedFile = pack "FoodFight.fed"

data FoodFightOptions = FoodFightOptions
    { regulating    :: Bool
    , constrained   :: Bool
    , channel       :: Int
    , maxJoinTries  :: Int
    }

defaultOptions = FoodFightOptions
    { regulating   = False  -- not supported in MATREX RTI, it seems
    , constrained  = False  -- not supported in MATREX RTI, it seems
    , channel      = 5
    , maxJoinTries = 10
    }

data InteractionHandles
    = SplatHandle
    | CommunicationHandle
    deriving (Eq, Ord, Enum, Show)

data ParameterHandles
    = TargetHandle
    | EnsuingMessHandle
    | MessageHandle
    deriving (Eq, Ord, Enum, Show)

data ObjectClassHandles
    = StudentHandle
    deriving (Eq, Ord, Enum, Show)

data AttributeHandles
    = LunchMoneyHandle
    | AmmoAmountHandle
    | CleanlinessHandle
    | PrivilegeToDeleteHandle
    deriving (Eq, Ord, Enum, Show)

data Handles = Handles
    { interactionHandles    :: IORef (M.Map InteractionHandles  InteractionClassHandle)
    , parameterHandles      :: IORef (M.Map ParameterHandles    ParameterHandle)
    , objectClassHandles    :: IORef (M.Map ObjectClassHandles  ObjectClassHandle)
    , attributeHandles      :: IORef (M.Map AttributeHandles    AttributeHandle)
    }
emptyHandles = do
    interactionHandles    <- newReference M.empty
    parameterHandles      <- newReference M.empty
    objectClassHandles    <- newReference M.empty
    attributeHandles      <- newReference M.empty
    return Handles
        { interactionHandles    = interactionHandles
        , parameterHandles      = parameterHandles
        , objectClassHandles    = objectClassHandles
        , attributeHandles      = attributeHandles
        }

setHandle handles k v = modifyReference (handles ?handles) (M.insert k v)

getHandle handles k = fmap (M.findWithDefault onErr k) (readReference (handles ?handles))
    where onErr = error ("getHandle: failed to find " ++ show k)

main :: IO ()
main = do
    -- init globals
    -- p_current_time              <- newRef 0
    -- p_lookahead                 <- newRef 0
    time_advance_outstanding    <- newRef False
    
    rti_ambassador              <- newRTIAmbassador
    students                    <- newRef M.empty
    handles                     <- emptyHandles
    splatUpdate                 <- newRef False
    communicationUpdate         <- newRef False
    currentTime                 <- newRef 0
    
    let ?federation_name            = pack "FoodFight"
        ?federate_name              = pack "HaskellFoodFighter"
        ?time_advance_outstanding   = time_advance_outstanding
        ?options                    = defaultOptions
        ?students                   = students
        ?handles                    = handles
        ?splatUpdate                = splatUpdate
        ?communicationUpdate        = communicationUpdate
        ?current_time               = currentTime
    
    fedAmb <- newFedAmbWithHandlers federateAmbassadorHandlers
    let ?rti_ambassador = rti_ambassador :: RTIAmbassador (FedAmbTime (HsFederateAmbassador RTIFedTime))
        ?fedAmb         = fedAmb         :: HsFederateAmbassador RTIFedTime

    withFederation primarySimulation

doTick = do
    putStrLn "ticking"
    tick_minimum_maximum ?rti_ambassador 1 1
    return ()

primarySimulation federate_handle = do
    withTimeManagement $ do
        putStrLn "in primarySimulation"
        doTick
        
        publishAndSubscribe
        doTick
        
        createObjects
        doTick
        
        radioRegion <- createRadioRegion
        let ?radioRegion = radioRegion
        doTick
        
        communicationHandle <- publishAndSubscribeDDM
        let ?communicationHandle = communicationHandle
        doTick
        
        sequence_
            [ do
                putStr $ unlines
                    [ "----------------------"
                    , "--- Loop: " ++ show i
                    , "----------------------"
                    ]
                
                updateObjects
        --         processOwnership defector i
        --         sendCommunication
                advanceTime
                doTick
                
                putStr "\n\n"
            | i <- [1..30]
            ]
        
        deleteObjects
        doTick
        
        unPublishAndSubscribe
        doTick
        
        unPublishAndSubscribeDDM
        doTick
        
        deleteRadioRegion
        doTick

withFederation = bracket
    (       createAndJoinFederation    `also` doTick)
    (\fh -> resignAndDestroyFederation `also` doTick)
    where
        a `also` b = do
            resultA <- a
            b >> return resultA

createAndJoinFederation = do
    putStrLn ("Creating the federation " ++ unpack ?federation_name ++ "\n")
    tryJust federationExecutionAlreadyExists $
        (createFederationExecution ?rti_ambassador ?federation_name fedFile)
    
    putStrLn ("createAndJoinFederation: " ++ unpack ?federate_name ++ " joining " ++ unpack ?federation_name ++ "\n")
    joinFederationExecution ?rti_ambassador ?federate_name ?federation_name ?fedAmb
    -- TODO: handle exceptions

resignAndDestroyFederation = do
    putStrLn "resigning federation"
    resignFederationExecution ?rti_ambassador DeleteObjectsAndReleaseAttributes
    
    putStrLn "destroying federation"
    catchJust federatesCurrentlyJoined
        (destroyFederationExecution ?rti_ambassador ?federation_name)
        (\e -> putStrLn "Destroy failed: other federates still using federation")

withTimeManagement action = do
    let FoodFightOptions{..} = ?options
        
        withTimeReg :: IO a -> IO a
        withTimeReg
            | regulating    = bracket_
                (putStrLn "Enabling time regulation")
                (putStrLn "Disabled time regulation")
                . withTimeRegulation ?rti_ambassador 0 1
            | otherwise     = id
        
        withTimeConstrained constrainedAction
            | constrained   = bracket_
                (enableTimeConstrained  ?rti_ambassador)
                (disableTimeConstrained ?rti_ambassador)
                constrainedAction
            | otherwise     = constrainedAction
    
    withTimeReg (withTimeConstrained action)


publishAndSubscribe = do
    putStrLn "in publishAndSubscribe"
    tryJust rtiException $ do
        enableAttributeRelevanceAdvisorySwitch ?rti_ambassador
        putStrLn "enabled attribute advisories"
    
    -- Student class
    studentHandle           <- getObjectClassHandle ?rti_ambassador (pack "ObjectRoot.Student")
    setHandle objectClassHandles StudentHandle studentHandle
    lunchMoneyHandle        <- getAttributeHandle   ?rti_ambassador (pack "LunchMoney")        studentHandle
    setHandle attributeHandles LunchMoneyHandle lunchMoneyHandle
    cleanlinessHandle       <- getAttributeHandle   ?rti_ambassador (pack "Cleanliness")       studentHandle
    setHandle attributeHandles CleanlinessHandle cleanlinessHandle
    ammoAmountHandle        <- getAttributeHandle   ?rti_ambassador (pack "AmmoAmount")        studentHandle
    setHandle attributeHandles AmmoAmountHandle ammoAmountHandle
    privilegeToDeleteHandle <- getAttributeHandle   ?rti_ambassador (pack "privilegeToDelete") studentHandle
    setHandle attributeHandles PrivilegeToDeleteHandle privilegeToDeleteHandle

    putStr $ unlines
        [ "Acquired Class Attribute Handles:"
        , "  studentHandle: " ++ show studentHandle
        , "    lunchMoneyHandle:        " ++ show lunchMoneyHandle
        , "    cleanlinessHandle:       " ++ show cleanlinessHandle
        , "    ammoAmountHandle:        " ++ show ammoAmountHandle
        , "    privilegeToDeleteHandle: " ++ show privilegeToDeleteHandle
        ]
    
    -- define attribute handle set
    attrs <- newContainer (Just 4)
    putStrLn "Created an AttributeHandleSet"
    insert attrs lunchMoneyHandle
    insert attrs cleanlinessHandle
    insert attrs ammoAmountHandle
    insert attrs privilegeToDeleteHandle
    
    putStrLn "Publishing student class"
    publishObjectClass                  ?rti_ambassador studentHandle attrs
    putStrLn "subscribing to student class"
    subscribeObjectClassAttributes      ?rti_ambassador studentHandle attrs
    putStrLn "requesting immediate updates for student class"
    requestClassAttributeValueUpdate    ?rti_ambassador studentHandle attrs
    
    -- Splat interaction
    splatHandle         <- getInteractionClassHandle ?rti_ambassador (pack "Splat")
    setHandle interactionHandles SplatHandle splatHandle
    ensuingMessHandle   <- getParameterHandle        ?rti_ambassador (pack "EnsuingMess")  splatHandle
    setHandle parameterHandles EnsuingMessHandle ensuingMessHandle
    targetHandle        <- getParameterHandle        ?rti_ambassador (pack "Target")       splatHandle
    setHandle parameterHandles TargetHandle targetHandle
    
    putStr $ unlines
        [ "Acquired Interaction Parameter Handles:"
        , "  splatHandle: " ++ show splatHandle
        , "    ensuingMessHandle:   " ++ show ensuingMessHandle
        , "    targetHandle:        " ++ show targetHandle
        ]
    
    -- Publish & subscribe Splat
    publishInteractionClass   ?rti_ambassador splatHandle
    subscribeInteractionClass ?rti_ambassador splatHandle True
    
    return (studentHandle, splatHandle)

unPublishAndSubscribe = do
    putStrLn "in unPublishAndSubscribe"
    
    studentHandle <- readsRef (objectClassHandles ?handles) (M.lookup StudentHandle)
    case studentHandle of
        Nothing -> return ()
        Just studentHandle -> do
            putStrLn "unpublishing student class"
            unpublishObjectClass        ?rti_ambassador studentHandle
    
            putStrLn "unsubscribing student class"
            unsubscribeObjectClass      ?rti_ambassador studentHandle
    
    splatHandle <- readsRef (interactionHandles ?handles) (M.lookup SplatHandle)
    case splatHandle of
        Nothing -> return ()
        Just splatHandle -> do
            putStrLn "unpublishing splat interaction"
            unpublishInteractionClass   ?rti_ambassador splatHandle
    
            putStrLn "unsubscribing splat interaction"
            unsubscribeInteractionClass ?rti_ambassador splatHandle
    
    return ()

data StudentAttr a = StudentAttr
    { attrOwned     :: Bool -- ownership flag
    , attrTransfer  :: Bool -- transfer ownership flag
    , attrUpdate    :: Bool -- update flag
    , attrProvide   :: Bool -- immediate update flag
    , attrValue     :: a
    } deriving (Eq, Ord, Show)
newStudentAttr val = StudentAttr
    { attrOwned     = True
    , attrTransfer  = False
    , attrUpdate    = False
    , attrProvide   = True
    , attrValue     = val
    }
newRemoteAttr val = StudentAttr
    { attrOwned     = False
    , attrTransfer  = False
    , attrUpdate    = False
    , attrProvide   = False
    , attrValue     = val
    }
data StudentState = StudentState
    { studentObjHandle          :: ObjectHandle
    , studentName               :: ByteString
    , studentEnsuingMess        :: TVar Double
    , studentLunchMoney         :: TVar (StudentAttr Double)
    , studentAmmoAmount         :: TVar (StudentAttr ULong)
    , studentCleanliness        :: TVar (StudentAttr Double)
    , studentPrivelegeToDelete  :: TVar (StudentAttr ())
    }
newLocalStudent handle name = do
    ensuingMess         <- newReference 0
    
    lunchMoney          <- newReference (newStudentAttr 20    )
    ammoAmount          <- newReference (newStudentAttr 20    )
    cleanliness         <- newReference (newStudentAttr 100   )
    privelegeToDelete   <- newReference (newStudentAttr ()    )
    
    return StudentState
            { studentObjHandle          = handle
            , studentName               = name
            , studentEnsuingMess        = ensuingMess
            , studentLunchMoney         = lunchMoney
            , studentAmmoAmount         = ammoAmount
            , studentCleanliness        = cleanliness
            , studentPrivelegeToDelete  = privelegeToDelete
            }
newRemoteStudent handle name = do
    ensuingMess         <- newReference 0
    
    lunchMoney          <- newReference (newStudentAttr 20    )
    ammoAmount          <- newReference (newStudentAttr 0     )
    cleanliness         <- newReference (newStudentAttr 100   )
    privelegeToDelete   <- newReference (newStudentAttr ()    )
    
    return StudentState
            { studentObjHandle          = handle
            , studentName               = name
            , studentEnsuingMess        = ensuingMess
            , studentLunchMoney         = lunchMoney
            , studentAmmoAmount         = ammoAmount
            , studentCleanliness        = cleanliness
            , studentPrivelegeToDelete  = privelegeToDelete
            }


numStudents = 1
maxStudents = 10 

createObjects = do
    studentHandle <- getHandle objectClassHandles StudentHandle
    
    pid <- getProcessID
    let pidString = show pid
    
    putStrLn "in createObjects"
    
    sequence_ 
        [ do
            nStudents <- readsRef ?students M.size
            
            if nStudents >= maxStudents
                then putStrLn "students list full"
                else do
                    putStrLn ("creating student " ++ show i)
                    let studentName = pack ("Student_" ++ show i ++ "_" ++ pidString)
                    handle <- registerObjectInstance ?rti_ambassador studentHandle (Just studentName)
                    student <- newLocalStudent handle studentName
                    
                    modifyRef ?students (M.insert handle student)
            
        | i <- [1 .. numStudents]
        ]

deleteObjects = do
    putStrLn "deleteObjects: write me!"
    return ()

maxChannels = 10

createRadioRegion = do
    let FoodFightOptions{..} = ?options
    putStrLn "in createRadioRegion"
    
    radioHandle     <- getRoutingSpaceHandle ?rti_ambassador (pack "FoodFightRadioSpace")
    channelHandle   <- getDimensionHandle    ?rti_ambassador (pack "Channel") radioHandle
    
    putStr $ unlines
        [ "Acquired Space Dimension Handles: "
        , "  radioHandle:       " ++ show radioHandle
        , "    channelHandle:   " ++ show channelHandle
        ]
    
    putStrLn "Creating RadioRegion"
    radioRegion <- createRegion ?rti_ambassador radioHandle 1
    let maxExtent = realToFrac (maxBound :: ExtentIndex)
        minExtent = realToFrac (minBound :: ExtentIndex)
        extentRange = maxExtent - minExtent
        channelFrac = realToFrac channel / realToFrac maxChannels
        channelRange = floor (channelFrac * extentRange + minExtent)
    
    putStrLn ("Set RadioRegion bounds " ++ show (channel, channel))
    setRangeLowerBound radioRegion 0 channelHandle channelRange
    setRangeUpperBound radioRegion 0 channelHandle channelRange
    
    putStrLn ("Notify RTI Ambassador of region changes")
    notifyAboutRegionModification ?rti_ambassador radioRegion
    
    return radioRegion

deleteRadioRegion = do
    deleteRegion ?rti_ambassador ?radioRegion
    putStrLn "Deleted radioRegion"

publishAndSubscribeDDM = do
    communicationHandle <- getInteractionClassHandle ?rti_ambassador (pack "Communication")
    setHandle interactionHandles CommunicationHandle communicationHandle
    messageHandle       <- getParameterHandle        ?rti_ambassador (pack "Message") communicationHandle
    setHandle parameterHandles MessageHandle messageHandle
    
    putStrLn $ unlines
        [ "Acquired Interaction Parameter Handles: "
        , "  communicationHandle:   " ++  show communicationHandle
        , "    messageHandle:       " ++ show messageHandle
        ]
    
    publishInteractionClass ?rti_ambassador communicationHandle
    putStrLn "Published Interaction Class \"Communication\""
    
    subscribeInteractionClassWithRegion ?rti_ambassador communicationHandle ?radioRegion True
    putStrLn "Subscribed interaction class \"Communication\""
    
    return communicationHandle

unPublishAndSubscribeDDM = do
    unpublishInteractionClass ?rti_ambassador ?communicationHandle
    putStrLn "unpublished Communication interaction"
    
    unsubscribeInteractionClassWithRegion ?rti_ambassador ?communicationHandle ?radioRegion
    putStrLn "unsubscribed Communication interaction"

updateObjects = do
    frozenStudents <- readsRef ?students M.elems
    sequence_ 
        [ do
            ownStudent <- ownAllStudentAttrs student
            if ownStudent
                then updateStudent student
                else putStrLn ("Skipping non-local student " ++ unpack (studentName student))
            
        | student <- frozenStudents
        ]

readAttr student attr = fmap attrValue (readReference (attr student))
modifyAttr student attr f = modifyReference (attr student) $ \attr -> attr {attrProvide = True,  attrValue = f (attrValue attr)}

updateStudent student = do
    lm   <- readAttr student studentLunchMoney
    ammo <- readAttr student studentAmmoAmount

    printf "update student %s (ammo: %d, lunch money: %.02f)\n" (unpack (studentName student)) ammo lm
    join $ sampleFrom DevURandom $ categorical
        [ (lm, do
            putStrLn ("student " ++ unpack (studentName student) ++ " buying ammo")
            modifyAttr student studentLunchMoney (subtract 1)
            modifyAttr student studentAmmoAmount (+1)
          )
        , (realToFrac ammo, do
            ammo <- readAttr student studentAmmoAmount
            when (ammo > 0) $ do
                putStrLn ("student " ++ unpack (studentName student) ++ " throwing")
                throwFood student
                return ()
          )
        , (0.001, putStrLn ("student " ++ unpack (studentName student) ++ " doing nothing"))
        ]
    
    mess <- readReference (studentEnsuingMess student)
    when (mess > 0) $ do
        modifyAttr student studentCleanliness (subtract mess)
        writeReference (studentEnsuingMess student) 0
    
    putStrLn "TODO: send updates to federation"

throwFood student = do
    students <- readsRef ?students M.elems
    targets <- filterM ownAnyStudentAttrs students
    if null targets
        then do
            putStrLn "no-one to throw food at!"
            return False
        else do
            target <- sampleFrom DevURandom $ randomElement targets
            ensuingMess <- sampleFrom DevURandom (normal 10 2) :: IO Double
            printf "student %s throwing food at student %s (ensuingMess = %.3f)\n" (unpack (studentName student)) (unpack (studentName target)) ensuingMess
            sendSplatInteraction (studentObjHandle target) ensuingMess
            
            modifyAttr student studentAmmoAmount (subtract 1)
            return True

sendSplatInteraction targetObjHandle ensuingMess = do
    let ObjectHandle (Handle (ULong target)) = targetObjHandle
        FoodFightOptions{..} = ?options
    
    splatHandle         <- getHandle interactionHandles SplatHandle
    targetHandle        <- getHandle parameterHandles   TargetHandle
    ensuingMessHandle   <- getHandle parameterHandles EnsuingMessHandle
    params <- newContainer (Just 2)
    insert params (targetHandle,        toStrictByteString $ runPut $ putWord32be target)
    insert params (ensuingMessHandle,   toStrictByteString $ runPut $ putFloat64be ensuingMess          )
    
    if regulating
        then do
            lookahead   <- queryLookahead ?rti_ambassador
            now         <- readRef ?current_time
            
            sendInteractionAtTime ?rti_ambassador splatHandle params (now + lookahead) (pack "SPLAT!!!!!!!!!!!")
            return ()
        else do
            sendInteraction ?rti_ambassador splatHandle params (pack "SPLAT!!!!!!!!!!!")

advanceTime = do
    let FoodFightOptions{..} = ?options
    
    when (regulating || constrained) $ do
        tao <- readRef ?time_advance_outstanding
        when (not tao) $ do
            return ()
            now <- readRef ?current_time
            let newNow = now + 5
            
            putStrLn ("Advancing time from " ++ show now ++ " to " ++ show newNow)
            timeAdvanceRequest ?rti_ambassador newNow
            
            writeRef ?time_advance_outstanding True
            
            whileM_ (readRef ?time_advance_outstanding) $ do
                tick_minimum_maximum ?rti_ambassador 0 1
         
ownAllStudentAttrs StudentState{..} = andM
    [ readsRef studentLunchMoney attrOwned
    , readsRef studentAmmoAmount attrOwned
    , readsRef studentCleanliness attrOwned
    , readsRef studentPrivelegeToDelete attrOwned
    ]

ownAnyStudentAttrs StudentState{..} = orM
    [ readsRef studentLunchMoney attrOwned
    , readsRef studentAmmoAmount attrOwned
    , readsRef studentCleanliness attrOwned
    , readsRef studentPrivelegeToDelete attrOwned
    ]


federateAmbassadorHandlers =  do
    ----------------------------
    -- Declaration Management --
    ----------------------------
    onTurnInteractionsOn $ \theHandle -> do
        splatHandle <- getHandle interactionHandles SplatHandle
        when (theHandle == splatHandle) $ do
            putStrLn "turnInteractionsOn callback for Splat"
            writeRef ?splatUpdate True
        
        communicationHandle <- getHandle interactionHandles CommunicationHandle
        when (theHandle == communicationHandle) $ do
            putStrLn "turnInteractionsOn callback for Communication"
            writeRef ?communicationUpdate True
    
    onTurnInteractionsOff $ \theHandle -> do
        splatHandle <- getHandle interactionHandles SplatHandle
        when (theHandle == splatHandle) $ do
            putStrLn "turnInteractionsOff callback for Splat"
            writeRef ?splatUpdate False
        
        communicationHandle <- getHandle interactionHandles CommunicationHandle
        when (theHandle == communicationHandle) $ do
            putStrLn "turnInteractionsOff callback for Communication"
            writeRef ?communicationUpdate False
    
    -----------------------
    -- Object Management --
    -----------------------
    onDiscoverObjectInstance $ \theObject theObjectClass theObjectName -> do
        studentHandle <- getHandle objectClassHandles StudentHandle
        when (theObjectClass == studentHandle) $ do
            putStrLn ("onDiscoverObjectInstance called for student " ++ unpack theObjectName ++ " with handle " ++ show theObject)
            nStudents <- readsRef ?students M.size
            
            if nStudents >= maxStudents
                then putStrLn "student ignored (student list full)"
                else do
                    student <- newRemoteStudent theObject theObjectName
                    modifyRef ?students (M.insert theObject student)
    
    onTurnUpdatesOnForObjectInstance $ \theObject theAttrs -> do
        putStrLn ("onTurnUpdatesOnForObjectInstance callback (student handle: " ++ show theObject ++ ")")
        mbStudent <- readsRef ?students (M.lookup theObject)
        case mbStudent of
            Nothing         -> putStrLn "no such student known"
            Just student    -> do
                let enableUpdate attr = modifyReference (attr student) (\attr -> attr {attrUpdate = True})
                enableUpdate studentLunchMoney
                enableUpdate studentAmmoAmount
                enableUpdate studentCleanliness
    
    onRemoveObjectInstance $ \theObject theTag mbTimeHandle -> do
        putStrLn ("onRemoveObjectInstance called (object handle: " ++ show theObject ++ ", tag: " ++ show theTag ++ ")")
        modifyRef ?students (M.delete theObject)
    
    onProvideAttributeValueUpdate $ \theObject theAttrs -> do
        putStrLn ("onProvideAttributeValueUpdate called for object " ++ show theObject)
        putStrLn "onProvideAttributeValueUpdate: write me!"
    
    onReflectAttributeValues $ \theObject theAttrs theTag mbTimeHandle -> do
        putStrLn "onReflectAttributeValues: write me!"
    
    onReceiveInteraction $ \theInteraction theParams theTag mbTimeHandle -> do
        putStrLn "onReceiveInteraction: write me!"
    
    ---------------------
    -- Time Management --
    ---------------------
    onTimeRegulationEnabled $ \newTime -> do
        putStrLn ("timeRegulationEnabled: " ++ show newTime)
