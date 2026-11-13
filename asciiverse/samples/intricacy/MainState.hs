-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

{-# LANGUAGE TupleSections #-}

module MainState where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Data.Array
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Time.Clock
import           System.FilePath

import           AsciiLock
import           Cache
import           Command
import           Database
import           Frame
import           GameStateTypes
import           Hex
import           InputMode
import           Lock
import           Maxlocksize
import           Metagame
import           Mundanities
import           Physics
import           Protocol
import           ServerAddr
import           Util

class (Applicative m, MonadIO m) => UIMonad m where
    runUI :: m a -> IO a
    initUI :: m Bool
    endUI :: m ()
    drawMainState :: MainStateT m ()
    reportAlerts :: GameState -> [Alert] -> m ()
    onPhysicsTick :: m ()
    clearMessage :: m ()
    drawMessage :: String -> m ()
    drawPrompt :: Bool -> String -> m ()
    endPrompt :: m ()
    drawError :: String -> m ()
    showHelp :: InputMode -> HelpPage -> m Bool
    getInputNoBlock :: InputMode -> m [ Command ]
    getInput :: InputMode -> m [ Command ]
    getChRaw :: m ( Maybe Char )
    unblockInput :: m (IO ())
    setUIBinding :: InputMode -> Command -> Char -> m ()
    getUIBinding :: InputMode -> Command -> m String
    impatience :: Int -> m Bool
    toggleColourMode :: m ()
    warpPointer :: HexPos -> m ()
    getUIMousePos :: m (Maybe HexPos)
    setYNButtons :: m ()
    onNewMode :: InputMode -> m ()
    withNoBG :: m () -> m ()
    suspend,redraw :: m ()

    doUI :: m a -> IO (Maybe a)
    doUI m = runUI $ do
        ok <- initUI
        if ok then m >>= (endUI >>).return.Just else return Nothing

type TutorialLevel = Maybe Int

-- | this could be neatened using GADTs
data MainState
    = PlayState
        { psCurrentState       :: GameState
        , psFrame              :: Frame
        , psLastAlerts         :: [Alert]
        , wrenchSelected       :: Bool
        , psSolved             :: Bool
        , psGameStateMoveStack :: [(GameState, PlayerMove)]
        , psUndoneStack        :: [PlayerMove]
        , psTitle              :: Maybe String
        , psTutLevel           :: TutorialLevel
        , psIsSub              :: Bool
        , psSaved              :: Bool
        , psMarks              :: Map Char MainState
        }
    | ReplayState
        { rsCurrentState       :: GameState
        , rsLastAlerts         :: [Alert]
        , rsMoveStack          :: [PlayerMove]
        , rsGameStateMoveStack :: [(GameState, PlayerMove)]
        , rsTitle              :: Maybe String
        , rsMarks              :: Map Char MainState
        }
    | EditState
        { esGameState      :: GameState
        , esGameStateStack :: [GameState]
        , esUndoneStack    :: [GameState]
        , esFrame          :: Frame
        , esPath           :: Maybe FilePath
        , esTested         :: Maybe (GameState,Solution)
        , lastSavedState   :: Maybe (GameState, Bool)
        , selectedPiece    :: Maybe PieceIdx
        , selectedPos      :: HexPos
        , lastModPos       :: HexPos
        , esMarks          :: Map Char GameState
        }
    | InitState
        { tutProgress :: TutProgress
        , initLocks   :: InitLocks
        }
    | MetaState
        { curServer        :: ServerAddr
        , undeclareds      :: [Undeclared]
        , partialSolutions :: PartialSolutions
        , cacheOnly        :: Bool
        , curAuth          :: Maybe Auth
        , codenameStack    :: [Codename]
        , newAsync         :: TVar Bool
        , asyncCount       :: TVar Int
        , asyncError       :: TVar (Maybe String)
        , asyncInvalidate  :: TVar (Maybe Codenames)
        , randomCodenames  :: TVar [Codename]
        , userInfoTVs      :: Map Codename (TVar FetchedRecord, UTCTime)
        , indexedLocks     :: Map LockSpec (TVar FetchedRecord)
        , retiredLocks     :: Maybe [LockSpec]
        , curLockPath      :: FilePath
        , curLock          :: Maybe (Lock,Maybe Solution)
        , listOffset       :: Int
        , listOffsetMax    :: Bool
        , initiated        :: Bool
        }

type MainStateT = StateT MainState

data HelpPage = HelpPageInput | HelpPageGame | HelpPageInitiated Int | HelpPageFirstEdit
    deriving (Eq, Ord, Show)

ms2im :: MainState -> InputMode
ms2im mainSt = case mainSt of
    PlayState {}   -> IMPlay
    ReplayState {} -> IMReplay
    EditState {}   -> IMEdit
    InitState {}   -> IMInit
    MetaState {}   -> IMMeta

getTutLevel :: MainState -> TutorialLevel
getTutLevel s@(PlayState {}) = psTutLevel s
getTutLevel _                = Nothing

newPlayState (frame,st) pms title tutLev sub saved = PlayState st frame [] False False [] pms title tutLev sub saved Map.empty
newReplayState st soln title = ReplayState st [] soln [] title Map.empty
newEditState (frame,st) msoln mpath = EditState st [] [] frame mpath
    ((st,)<$>msoln) (Just (st, isJust msoln)) Nothing (PHS zero) (PHS zero) Map.empty
initInitState = uncurry InitState <$> readInitProgress
initMetaState = do
    flag <- newTVarIO False
    errtvar <- newTVarIO Nothing
    invaltvar <- newTVarIO Nothing
    rnamestvar <- newTVarIO []
    counttvar <- newTVarIO 0
    (inited, saddr', auth, path) <- confFilePath "metagame.conf" >>=
        fmap (fromMaybe (False, defaultServerAddr, Nothing, "")) . readReadFile
    let saddr = updateDefaultSAddr saddr'
    let names = maybeToList $ authUser <$> auth
    (undecls,partials) <- readServerSolns saddr
    mlock <- fullLockPath path >>= readLock
    return $ MetaState saddr undecls partials False auth names flag counttvar errtvar invaltvar rnamestvar Map.empty Map.empty Nothing path mlock 0 True inited

type PartialSolutions = Map LockSpec SavedPlayState
data SavedPlayState = SavedPlayState [PlayerMove] (Map Char [PlayerMove])
    deriving (Eq, Ord, Show, Read)

data TutProgress = TutProgress
    { tutSolved  :: Bool
    , tutLevel   :: Int
    , tutPartial :: Maybe SavedPlayState
    } deriving (Eq, Ord, Show, Read)
initTutProgress = TutProgress False 1 Nothing

wrenchOnlyTutLevel, noUndoTutLevel, keysTutLevel, conflictsTutLev :: TutorialLevel -> Bool
wrenchOnlyTutLevel = (`elem` (Just <$> [1..4]))
noUndoTutLevel = (`elem` (Just <$> [1..7]))
keysTutLevel (Just n) = n >= 5
keysTutLevel _        = False
conflictsTutLev (Just n) = n == 11
conflictsTutLev _        = False

data InitLock = InitLock
    { initLockName    :: String
    , initLockDesc    :: String
    , initLockLock    :: Lock
    , initLockSolved  :: Bool
    , initLockPartial :: Maybe SavedPlayState
    } deriving (Eq, Ord, Show, Read)
type InitLocks = Map HexVec InitLock

accessibleInitLocks :: Bool -> InitLocks -> InitLocks
accessibleInitLocks isTutSolved locks =
    Map.filterWithKey (\v _ -> initLockAccessible v) locks
    where
    initLockAccessible :: HexVec -> Bool
    initLockAccessible v = or
        [ (v' == zero && isTutSolved) ||
            (Just True == (initLockSolved <$> Map.lookup v' locks))
        | v' <- (v +^) <$> hexDirs ]

isLastInitLock :: InitLock -> Bool
isLastInitLock = (== "END") . initLockName

savePlayState :: MainState -> SavedPlayState
savePlayState ps = SavedPlayState (getMoves ps) $ Map.map getMoves $ psMarks ps
    where getMoves = reverse . map snd . psGameStateMoveStack

restorePlayState :: SavedPlayState -> Lock -> [PlayerMove] -> Maybe String -> Maybe Int -> Bool -> Bool -> MainState
restorePlayState (SavedPlayState pms markPMs) (frame,st) redoPms title tutLev sub saved =
    (stateAfterMoves pms) { psMarks = Map.map stateAfterMoves markPMs }
    where
        stateAfterMoves pms' = let (stack,st') = applyMoves st pms'
            in (newPlayState (frame, st') redoPms title tutLev sub saved) { psGameStateMoveStack = stack }
        applyMoves st' = foldl tick ([],st')
        tick :: ([(GameState,PlayerMove)],GameState) -> PlayerMove -> ([(GameState,PlayerMove)],GameState)
        tick (stack,st') pm = ((st',pm):stack,fst . runWriter $ physicsTick pm st')

readServerSolns :: ServerAddr -> IO ([Undeclared],PartialSolutions)
readServerSolns saddr = if nullSaddr saddr then return ([],Map.empty) else do
    undecls <- confFilePath ("undeclared" ++ [pathSeparator] ++ saddrPath saddr) >>=
        fmap (fromMaybe []) . readReadFile
    partials <- confFilePath ("partialSolutions" ++ [pathSeparator] ++ saddrPath saddr) >>=
        fmap (fromMaybe Map.empty) . readReadFile
    return (undecls,partials)

readInitProgress :: IO (TutProgress,InitLocks)
readInitProgress = do
    initConfDir <- confFilePath "initiation"
    initDataDir <- getDataPath "initiation"
    tut <- fromMaybe initTutProgress <$> readReadFile (initConfDir </> "tutProgress")
    locknames <- fromMaybe [] <$> readReadFile (initDataDir </> "initiation.map")
    let namesMap :: Map HexVec Codename
        namesMap = Map.fromList $
            [ (rotate (-j) (neg hw) +^ i *^ hu, name)
            | (j,line) <- zip [0..] locknames
            , (i,name) <- zip [0..] line ]
        readInitLock :: String -> IO (Maybe InitLock)
        readInitLock name = runMaybeT $ do
            desc <- MaybeT $ listToMaybe <$> readStrings (initDataDir </> name ++ ".text")
            lock <- (fst <$>) . MaybeT $ readLock (initDataDir </> name ++ ".lock")
            solved <- lift . (fromMaybe False <$>) . readReadFile $ initConfDir </> name ++ ".solved"
            partial <- lift . (fromMaybe Nothing <$>) . readReadFile $ initConfDir </> name ++ ".partial"
            return $ InitLock name desc lock solved partial
    (tut,) . Map.mapMaybe id <$> mapM readInitLock namesMap

writeServerSolns :: ServerAddr -> MainState -> IO ()
writeServerSolns saddr MetaState { undeclareds=undecls,
        partialSolutions=partials } = unless (nullSaddr saddr) $ do
    confFilePath ("undeclared" ++ [pathSeparator] ++ saddrPath saddr) >>= flip writeReadFile undecls
    confFilePath ("partialSolutions" ++ [pathSeparator] ++ saddrPath saddr) >>= flip writeReadFile partials
writeServerSolns _ _ = error "writeServerSolns: tried to write non-meta state"

readLock :: FilePath -> IO (Maybe (Lock, Maybe Solution))
readLock path = runMaybeT $ msum
        [ (,Nothing) <$> MaybeT (readReadFile path)
        , do
            (mlock,msoln) <- lift $ readAsciiLockFile path
            lock <- liftMaybe mlock
            return (lock,msoln) ]
-- writeLock :: FilePath -> Lock -> IO ()
-- writeLock path lock = fullLockPath path >>= flip writeReadFile lock

writeInitState :: MainState -> IO ()
writeInitState InitState { tutProgress = tut, initLocks = locks } = do
    initConfDir <- confFilePath "initiation"
    writeReadFile (initConfDir </> "tutProgress") tut
    let writeInitLockInfo :: InitLock -> IO ()
        writeInitLockInfo (InitLock name _ _ solved partial) = do
            writeReadFile (initConfDir </> name ++ ".solved") solved
            writeReadFile (initConfDir </> name ++ ".partial") partial
    mapM_ writeInitLockInfo locks
writeInitState _ = return ()

writeMetaState :: MainState -> IO ()
writeMetaState ms@MetaState { curServer=saddr, curAuth=auth, curLockPath=path, initiated=inited } = do
    confFilePath "metagame.conf" >>= flip writeReadFile (inited, saddr, auth, path)
    writeServerSolns saddr ms
writeMetaState _ = return ()

getTitle :: UIMonad uiM => MainStateT uiM (Maybe (String, Int))
getTitle = get >>= title . ms2im
    where
    title IMEdit = do
        mpath <- gets esPath
        unsaved <- editStateUnsaved
        isTested <- isJust <$> getCurTestSoln
        height <- gets $ aboveFrame . esFrame
        return $ Just ("Editing " ++ fromMaybe "[unnamed lock]" mpath ++
                (if isTested then " (Tested)" else "") ++
                (if unsaved then " [+]" else "    "),
            height
            )
    title IMPlay = do
        height <- gets $ aboveFrame . psFrame
        gets $ ((, height) <$>) . psTitle
    title IMReplay = gets $ ((, maxHeight) <$>) . rsTitle
    title _ = return Nothing
    aboveFrame frame = min maxHeight $ 2 + frameSize frame
    maxHeight = maxlocksize + 1

editStateUnsaved :: UIMonad uiM => MainStateT uiM Bool
editStateUnsaved = (isNothing <$>) $ runMaybeT $ do
    (sst,tested) <- MaybeT $ gets lastSavedState
    st <- gets esGameState
    guard $ sst == st
    nowTested <- isJust <$> lift getCurTestSoln
    guard $ tested == nowTested

getCurTestSoln :: UIMonad uiM => MainStateT uiM (Maybe Solution)
getCurTestSoln = runMaybeT $ do
    (st',soln) <- MaybeT $ gets esTested
    st <- gets esGameState
    guard $ st == st'
    return soln

mgetOurName :: (UIMonad uiM) => MaybeT (MainStateT uiM) Codename
mgetOurName = MaybeT $ gets ((authUser <$>) . curAuth)
mgetCurName :: (UIMonad uiM) => MaybeT (MainStateT uiM) Codename
mgetCurName = MaybeT $ gets (listToMaybe . codenameStack)

getUInfoFetched :: UIMonad uiM => Integer -> Codename -> MainStateT uiM FetchedRecord
getUInfoFetched staleTime name = do
    uinfott <- gets (Map.lookup name . userInfoTVs)
    ($ uinfott) $ maybe set $ \(tvar,time) -> do
        now <- liftIO getCurrentTime
        if floor (diffUTCTime now time) > staleTime
            then set
            else liftIO $ readTVarIO tvar
    where
        set = do
            now <- liftIO getCurrentTime
            tvar <- getRecordCachedFromCur True $ RecUserInfo name
            modify $ \ms -> ms {userInfoTVs = Map.insert name (tvar, now) $ userInfoTVs ms}
            liftIO $ readTVarIO tvar

mgetUInfo :: UIMonad uiM => Codename -> MaybeT (MainStateT uiM) UserInfo
mgetUInfo name = do
    RCUserInfo (_,uinfo) <- MaybeT $ (fetchedRC <$>) $ getUInfoFetched defaultStaleTime name
    return uinfo
    where defaultStaleTime = 300


invalidateUInfo :: UIMonad uiM => Codename -> MainStateT uiM ()
invalidateUInfo name =
    modify $ \ms -> ms {userInfoTVs = Map.delete name $ userInfoTVs ms}

invalidateAllUInfo :: UIMonad uiM => MainStateT uiM ()
invalidateAllUInfo =
    modify $ \ms -> ms {userInfoTVs = Map.empty}

data Codenames = AllCodenames | SomeCodenames [Codename]

invalidateUInfos :: UIMonad uiM => Codenames -> MainStateT uiM ()
invalidateUInfos AllCodenames          = invalidateAllUInfo
invalidateUInfos (SomeCodenames names) = mapM_ invalidateUInfo names


mgetLock :: UIMonad uiM => LockSpec -> MaybeT (MainStateT uiM) Lock
mgetLock ls = do
    tvar <- msum [ MaybeT $ gets (Map.lookup ls . indexedLocks)
        , lift $ do
            tvar <- getRecordCachedFromCur True $ RecLock ls
            modify $ \ms -> ms { indexedLocks = Map.insert ls tvar $ indexedLocks ms }
            return tvar ]
    RCLock lock <- MaybeT $ (fetchedRC<$>) $ liftIO $ readTVarIO tvar
    return $ reframe lock

invalidateAllIndexedLocks :: UIMonad uiM => MainStateT uiM ()
invalidateAllIndexedLocks =
    modify $ \ms -> ms { indexedLocks = Map.empty }

refreshUInfoUI :: (UIMonad uiM) => MainStateT uiM ()
refreshUInfoUI = void.runMaybeT $ do
    modify $ \ms -> ms { listOffset = 0 }
    mourNameSelected >>? getRandomNames
    lift $ modify $ \ms -> ms {retiredLocks = Nothing}
    --lift.lift $ drawMessage ""
    where
        getRandomNames = do
            rnamestvar <- gets randomCodenames
            liftIO $ atomically $ writeTVar rnamestvar []
            flag <- gets newAsync
            saddr <- gets curServer
            void $ liftIO $ forkIO $ do
                resp <- makeRequest saddr $
                    ClientRequest protocolVersion Nothing $ GetRandomNames 19
                case resp of
                    ServedRandomNames names -> atomically $ do
                            writeTVar rnamestvar names
                            writeTVar flag True
                    _ -> return ()

mourNameSelected :: (UIMonad uiM) => MaybeT (MainStateT uiM) Bool
mourNameSelected = liftM2 (==) mgetCurName mgetOurName

purgeInvalidUndecls :: (UIMonad uiM) => MainStateT uiM ()
purgeInvalidUndecls = do
    undecls' <- gets undeclareds >>= filterM ((not<$>).invalid)
    modify $ \ms -> ms { undeclareds = undecls' }
    where
        invalid (Undeclared _ ls (ActiveLock name idx)) =
            (fromMaybe False <$>) $ runMaybeT $ do
                uinfo <- mgetUInfo name
                ourName <- mgetOurName
                (`mplus` return True) $ do
                    linfo <- liftMaybe $ userLocks uinfo ! idx
                    return $ public linfo
                        || ourName `elem` accessedBy linfo
                        || lockSpec linfo /= ls


curServerAction :: UIMonad uiM => Protocol.Action -> MainStateT uiM ServerResponse
curServerAction act = do
    saddr <- gets curServer
    auth <- gets curAuth
    cOnly <- gets cacheOnly
    if cOnly then return $ ServerError "Can't contact server in cache-only mode"
        else (fromMaybe (ServerError "Request aborted") <$>) $
            lift $ withImpatience $ makeRequest saddr $ ClientRequest protocolVersion auth act

curServerActionAsyncThenInvalidate :: UIMonad uiM => Protocol.Action -> Maybe Codenames -> MainStateT uiM ()
curServerActionAsyncThenInvalidate act names = do
    saddr <- gets curServer
    auth <- gets curAuth
    flag <- gets newAsync
    count <- gets asyncCount
    errtvar <- gets asyncError
    invaltvar <- gets asyncInvalidate
    cOnly <- gets cacheOnly
    void $ liftIO $ forkIO $ do
        atomically $ modifyTVar count (+1)
        resp <- if cOnly then return $ ServerError "Can't contact server in cache-only mode"
                else makeRequest saddr $ ClientRequest protocolVersion auth act
        case resp of
            ServerError err -> atomically $ writeTVar errtvar $ Just err
            _               -> atomically $ writeTVar invaltvar names
        atomically $ writeTVar flag True
        atomically $ modifyTVar count (+(-1))

checkAsync :: UIMonad uiM => MainStateT uiM ()
checkAsync = do
    void.runMaybeT $ do
        errtvar <- lift $ gets asyncError
        err <- MaybeT $ liftIO $ atomically $
                readTVar errtvar <* writeTVar errtvar Nothing
        lift.lift $ drawError err
    void.runMaybeT $ do
        invaltvar <- lift $ gets asyncInvalidate
        names <- MaybeT $ liftIO $ atomically $
                readTVar invaltvar <* writeTVar invaltvar Nothing
        lift $ invalidateUInfos names >> refreshUInfoUI

getRecordCachedFromCur :: UIMonad uiM => Bool -> Record -> MainStateT uiM (TVar FetchedRecord)
getRecordCachedFromCur flagIt rec = do
    saddr <- gets curServer
    auth <- gets curAuth
    cOnly <- gets cacheOnly
    flag <- gets newAsync
    liftIO $ getRecordCached saddr auth
        (if flagIt then Just flag else Nothing) cOnly rec

getFreshRecBlocking :: UIMonad uiM => Record -> MainStateT uiM (Maybe RecordContents)
getFreshRecBlocking rec = do
    tvar <- getRecordCachedFromCur False rec
    cOnly <- gets cacheOnly
    mfetched <- lift $ withImpatience $ atomically $ do
        fetched@(FetchedRecord isFresh _ _) <- readTVar tvar
        check $ isFresh || cOnly
        return fetched
    case mfetched of
        Nothing -> lift (drawError "Request aborted") >> return Nothing
        Just fetched ->
            case fetchError fetched of
                Nothing  -> return $ fetchedRC fetched
                Just err -> lift (drawError err) >> return Nothing

-- |indicate waiting for server, and allow cancellation
withImpatience :: UIMonad uiM => IO a -> uiM (Maybe a)
withImpatience m = do
    finishedTV <- liftIO $ newTVarIO Nothing
    tid <- liftIO $ forkIO $ m >>= atomically . writeTVar finishedTV . Just
    let waitImpatiently ticks = do
            finished <- liftIO $ readTVarIO finishedTV
            if isJust finished
                then return finished
                else do
                    cancel <- impatience ticks
                    if cancel
                        then liftIO $ killThread tid >> return Nothing
                        else waitImpatiently $ ticks+1
    waitImpatiently 0


getRelScore :: (UIMonad uiM) => Codename -> MainStateT uiM (Maybe Int)
getRelScore name = (fst<$>) <$> getRelScoreDetails name
getRelScoreDetails name = runMaybeT $ do
    ourName <- mgetOurName
    guard $ ourName /= name
    uinfo <- mgetUInfo name
    ourUInfo <- mgetUInfo ourName
    -- Note that this is inverted when communicated in the interface: we show
    -- the number accessed rather than the number unaccessed.
    let (posve,negve) = (countUnaccessedBy ourUInfo name, countUnaccessedBy uinfo ourName)
    return (posve-negve,(posve,negve))
    where
        countUnaccessedBy ui name' = length $ filter isNothing $ getAccessInfo ui name'

accessedAL :: (UIMonad uiM) => ActiveLock -> MainStateT uiM Bool
accessedAL (ActiveLock name idx) = (isJust <$>) $ runMaybeT $ do
    ourName <- mgetOurName
    guard $ ourName /= name
    uinfo <- mgetUInfo name
    guard $ isJust $ getAccessInfo uinfo ourName !! idx

getNotesReadOn :: UIMonad uiM => LockInfo -> MainStateT uiM [NoteInfo]
getNotesReadOn lockinfo = (fromMaybe [] <$>) $ runMaybeT $ do
    ourName <- mgetOurName
    ourUInfo <- mgetUInfo ourName
    return $ filter (\n -> isNothing (noteBehind n)
            || n `elem` notesRead ourUInfo) $ lockSolutions lockinfo

testAuth :: UIMonad uiM => MainStateT uiM ()
testAuth = isJust <$> gets curAuth >>? do
    resp <- curServerAction Authenticate
    case resp of
        ServerMessage msg -> lift $ drawMessage $ "Server: " ++ msg
        ServerError err -> do
            lift $ drawMessage err
            -- modify $ \ms -> ms {curAuth = Nothing}
        _ -> return ()

initiationHelpText :: [String]
initiationHelpText =
    [ "Suddenly surrounded by hooded figures in your locked room."
    , "Gently abducted, now wordlessly released into this dingy hole."
    , ""
    , "Some disused dungeon, a honeycomb of cells separated by sturdy gates."
    , "From the far end, light filters through the sequential barriers."
    , "Freedom?"
    , "The gate mechanisms are foolishly accessible, merely locked."
    , "Lucky that they neglected to strip you of your lockpicks."
    , "Lucky, and odd..." ]

metagameHelpText :: [String]
metagameHelpText =
    [ "By ruthlessly guarded secret arrangement, the council's agents can pick any lock in the city."
    , "A secret guild produces the necessary locks - apparently secure, but with fatal hidden flaws."
    , "A ritual game is played to determine the best designs."
    , "To master it, you must build locks which can be picked only by one who knows the secret,"
    , "and you must discover the secret flaws in the locks designed by your colleagues."
    , ""
    , "You may put forward up to three prototype locks. They will guard the secrets you discover."
    , "If you pick a colleague's lock, the rules require that a note is written on your solution."
    , "A note proves that a solution was found, while revealing no more details than necessary."
    , "To declare your solution, you must secure your note behind a lock of your own."
    , "If you are able to unlock a lock, you automatically read all the notes it secures."
    , "Reading three notes on a lock suffices to piece together the secrets of unlocking it."
    , ""
    , "The game judges players relative to each of their peers. There are no absolute rankings."
    , "You win a point of esteem against another player for one of their locks"
    , "if you have declared a solution to it, or if you have read three notes on it."
    , "You also win a point for each of their empty lock slots if you can unlock all full slots."
    , "Relative esteem is the points you win minus the points they win; +3 is best, -3 is worst."
    , ""
    , "If the secrets to one of your locks become widely disseminated, you may wish to replace it."
    , "Once replaced, a lock is \"retired\", and the notes it secured are read by everyone."
    ]

initiationCompleteText :: Int -> [String]
initiationCompleteText 1 =
    [ "Emerging from the last of the cells to what you imagined might be freedom,"
    , "you find yourself in a lamplit room with a hooded figure."
    , ""
    , "\"So. You did acquire some skills in your former life. Enough for these toys, at least."
    , "Whether you have the devious creativity to improve on their designs... remains to be seen."
    , ""
    , "\"Nonetheless, we welcome you to our number. As for what exactly it is that you are joining..."
    , "no doubt you believe you have it all worked out already. Still, allow me to explain.\""
    , ""
    , "After a pause to examine your face, and a soft chuckle, the figure continues."
    , "\"Ah, you thought this would be the end? No, no, this is very much the beginning.\""
    ]
initiationCompleteText 2 =
    [ "\"As you fatefully determined, every lock permitted in the city has a fatal hidden flaw."
    , "Those whose duties require it are entrusted with the secrets required to pick these locks."
    , "As for those who unauthorisedly discover, and even try to profit from, said secrets..."
    , "you come to us."
    , ""
    , "\"Our task is to produce the apparently secure locks necessary for this system:"
    , "locks which can in fact be picked with only minimal tools once you know the trick,"
    , "but with enough mechanical complexity to obscure this fact from the foolishly curious."
    , ""
    , "\"To push the designs to ever new extremes of intricacy, we run a ritual game."
    , "Today, we welcome you as its newest player."
    ]
initiationCompleteText 3 =
    [ "\"The idea is simple."
    , "We each design locks, and we each attempt to solve the locks designed by the others."
    , ""
    , "\"You may put forward up to three prototype locks."
    , "They will guard the secrets you discover: when you pick a colleague's lock,"
    , "you may declare the fact by placing a note on its solution behind one of your locks."
    , "As long as the owner of the lock you picked is unable to read your note,"
    , "you score a point against them. This is now your aim in life."
    , ""
    , "\"If you find a lock too difficult or trivial to pick yourself,"
    , "you may find that reading other players' notes on it will lead you to a solution."
    , ""
    , "\"The finer details of the rules can wait. Your first task is to name yourself."
    , "For reasons which should be clear, our members are known exclusively by pseudonyms;"
    , "by tradition, these codenames are triplets of letters or symbols."
    , ""
    , "\"Go, choose your codename, have it entered in the registry."
    , "Then, you should begin work on your first lock."
    , "With a new initiate always comes the hope of some genuinely new challenge..."
    , "Perhaps you already have ideas?\""
    ]
initiationCompleteText _ = []


firstEditHelpText :: [String]
firstEditHelpText =
    [ "Design a lock to protect your secrets."
    , ""
    , "It must be possible to pick your lock by pulling a sprung bolt from the hole in the top-right,"
    , "but you should place blocks, springs, pivots, and balls to make this as difficult as possible."
    , ""
    , "Place pieces with keyboard or mouse. Springs must be set next to blocks, and arms next to pivots."
    , "Repeatedly placing a piece in the same hex cycles through ways it can relate to its neighbours."
    , ""
    , "Use Test to prove that your lock is solvable, or Play to alternate between testing and editing."
    , "When you are done, Write your lock, then Quit from editing and Place your lock in a slot."
    , "You will then be able to Declare locks you solve, and others will attempt to solve your lock."
    , ""
    , "Your first lock is unlikely to stand for long against your more experienced peers;"
    , "examine their solutions to spot flaws in your design, and study their locks for ideas."
    ]
