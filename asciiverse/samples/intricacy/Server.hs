-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Network.Fancy

import           Control.Applicative
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Exception.Base     (evaluate)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Array
import           Data.Bifunctor             (bimap)
import qualified Data.Binary                as B
import qualified Data.ByteString.Char8      as CS
import qualified Data.ByteString.Lazy       as BL
import           Data.Either                (fromRight)
import           Data.Foldable              (for_)
import           Data.Function              (on)
import           Data.List
import           Data.Maybe
import qualified Data.Text                  as TS
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Short            as TSh
import           Data.Time.Clock
import           Data.Word
import           Pipes
import qualified Pipes.Prelude              as P
import           System.Directory           (renameFile)
import           System.FilePath
import           System.IO
import           System.IO.Error
import           System.Random

import           Data.Time.Format
import           Data.Time.LocalTime
import           Text.Feed.Constructor
import           Text.Feed.Export           (xmlFeed)
import           Text.Feed.Import           (parseFeedFromFile)
import qualified Text.XML                   as XML

import qualified Crypto.Argon2              as A2
import           Crypto.Hash.Algorithms     (SHA256 (..))
import           Crypto.PubKey.RSA          (generate, generateBlinder)
import           Crypto.PubKey.RSA.OAEP     (decrypt, defaultOAEPParams)
import           Crypto.PubKey.RSA.Types    (private_n)

#ifdef SENDMAIL
import           Network.Mail.Mime          (plainPart)
import qualified Network.Mail.SMTP          as SMTP
import qualified Text.Email.Validate
#endif

import           System.Console.GetOpt
import           System.Environment
import           System.Exit

import           AsciiLock
import           Database
import           Frame
import           Lock
import           Maxlocksize
import           Metagame
import           Mundanities
import           Protocol
import           Version

defaultPort = 27001 -- 27001 == ('i'<<8) + 'y'

data Opt = RequestDelay Int | Daemon | LogFile FilePath | Port Int | DBDir FilePath | ServerLockSize Int | FeedPath FilePath | Rewrite | CheckNotes | Help | Version
    deriving (Eq, Ord, Show)
options =
    [ Option ['p'] ["port"] (ReqArg (Port . read) "PORT") $ "TCP port to listen on (default: " ++ show defaultPort ++ ")"
    , Option ['P'] ["delay"] (ReqArg (RequestDelay . read) "MICROSECS") "delay before sending response (for testing) (default: 0)"
    -- , Option ['d'] ["daemon"] (NoArg Daemon) "Run as daemon"
    , Option ['l'] ["logfile"] (ReqArg LogFile "PATH") "Log to file"
    , Option ['d'] ["dir"] (ReqArg DBDir "PATH") "directory for server database [default: intricacydb]"
    , Option ['r'] ["refresh"] (NoArg Rewrite) "rewrite database (to convert from legacy formats)"
    , Option ['c'] ["check-notes"] (NoArg CheckNotes) "confirm solutions in notes (in case of rule changes)"
    , Option ['s'] ["locksize"] (ReqArg (ServerLockSize . read) "SIZE") "size of locks (only takes effect when creating a new database) [default: 8]"
    , Option ['f'] ["feed"] (ReqArg FeedPath "PATH") "write news feed to this path"
    , Option ['h'] ["help"] (NoArg Help) "show usage information"
    , Option ['v'] ["version"] (NoArg Version) "show version information"
    ]

usage :: String
usage = usageInfo header options
    where header = "Usage: intricacy-server [OPTION...]"

parseArgs :: [String] -> IO ([Opt],[String])
parseArgs argv =
    case getOpt Permute options argv of
        (o,n,[]) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: intricacy-server [OPTION...]"

main = do
    argv <- getArgs
    (opts,_) <- parseArgs argv
    {- FIXME: doesn't work
    if Daemon `elem` opts
        then void $ forkIO $ main' opts
        else main' opts
    -}
    when (Help `elem` opts) $ putStr usage >> exitSuccess
    when (Version `elem` opts) $ putStrLn version >> exitSuccess
    let delay = fromMaybe 0 $ listToMaybe [ d | RequestDelay d <- opts ]
        port = fromMaybe defaultPort $ listToMaybe [ p | Port p <- opts ]
        dbpath = fromMaybe "intricacydb" $ listToMaybe [ p | DBDir p <- opts ]
        mfeedPath = listToMaybe [ p | FeedPath p <- opts ]
        locksize = min maxlocksize $ fromMaybe 8 $ listToMaybe [ s | ServerLockSize s <- opts ]
    withDB dbpath $ setDefaultServerInfo locksize >> setKeyPair
    writeFile (lockFilePath dbpath) ""
    logh <- case listToMaybe [ f | LogFile f <- opts ] of
        Nothing   -> return stdout
        Just path -> openFile path AppendMode
    when (Rewrite `elem` opts) $ rewriteDB dbpath logh
    when (CheckNotes `elem` opts) $ checkNotes dbpath logh
    streamServer serverSpec{address = IPv4 "" port, threading=Threaded} $ handler dbpath delay logh mfeedPath
    sleepForever

setDefaultServerInfo locksize = do
    alreadySet <- recordExists RecServerInfo
    unless alreadySet $ putRecord RecServerInfo (RCServerInfo $ defaultServerInfo locksize)

setKeyPair :: DBM ()
setKeyPair = do
    alreadySet <- recordExists RecPublicKey
    unless alreadySet $ do
        (publicKey, secretKey) <- liftIO $ generate 256 65537
        putRecord RecPublicKey $ RCPublicKey publicKey
        putRecord RecSecretKey $ RCSecretKey secretKey

-- Note: switching to cryptonite's argon2 implementation would not be
-- straightforwardsly backwards-compatible, the output format is different.
argon2 :: String -> ExceptT String IO String
argon2 s = either (throwE . show) (return . TSh.unpack) $
    A2.hashEncoded hashOptions (CS.pack s) (CS.pack salt)
    where
        salt = "intricacy salt"
        -- |default argon2 hash options
        hashOptions = A2.HashOptions
                    { A2.hashIterations  = 3
                    , A2.hashMemory      = 2 ^ 12 -- 4 MiB
                    , A2.hashParallelism = 1
                    , A2.hashVariant     = A2.Argon2i
                    , A2.hashVersion     = A2.Argon2Version13
                    , A2.hashLength      = 2 ^ 5 -- 32 bytes
                    }


-- | We lock the whole database during each request, using haskell's native
-- file locking, meaning that we have at any time one writer *xor* any number
-- of readers.
withDBLock :: MonadIO m => [Char] -> IOMode -> m b -> m b
withDBLock dbpath lockMode m = do
    h <- liftIO $ getDBLock lockMode
    ret <- m
    liftIO $ hClose h
    return ret
    where
        getDBLock lockMode =
            catchIO (openFile (lockFilePath dbpath) lockMode) (\_ -> threadDelay (50*10^3) >> getDBLock lockMode)

lockFilePath dbpath = dbpath ++ [pathSeparator] ++ "lockfile"

logit h s = hPutStrLn h s >> hFlush h

handler :: FilePath -> Int -> Handle -> Maybe FilePath -> Handle -> Address -> IO ()
handler dbpath delay logh mfeedPath hdl addr = handle ((\e -> return ()) :: SomeException -> IO ()) $
    handler' hdl addr
    where handler' hdl addr = do
            response <- handle (\e -> return $ ServerError $ show (e::SomeException)) $ do
                request <- B.decode <$> BL.hGetContents hdl
                let hostname = case addr of
                        IP n _    -> n
                        IPv4 n _  -> n
                        IPv6 n _  -> n
                        Unix path -> path
                    hashedHostname = take 8 $ hash hostname
                now <- liftIO getCurrentTime
                logit logh $ show now ++ ": " ++ hashedHostname ++ " >>> " ++ showRequest request
                response <- handleRequest dbpath mfeedPath request
                when (delay > 0) $ threadDelay delay
                now' <- liftIO getCurrentTime
                logit logh $ show now' ++ ": " ++ hashedHostname ++ " <<< " ++ showResponse response
                return response
            BL.hPut hdl $ B.encode response

showRequest :: ClientRequest -> String
showRequest (ClientRequest ver mauth act) = show ver ++ " "
    ++ maybe "" (\(Auth name _) -> "Auth:" ++ name) mauth ++ " "
    ++ showAction act
showAction :: Action -> String
showAction (SetLock lock idx soln) = "SetLock " ++ show idx ++ " lock:"
    ++ (if not $ validLock $ reframe lock then " [INVALID LOCK] " else "\n" ++ unlines (lockToAscii lock))
    ++ "[SOLN]"
showAction (DeclareSolution soln ls target idx) = "DeclareSolution [SOLN] "
    ++ unwords [show ls,show target,show idx]
showAction act = show act
showResponse :: ServerResponse -> String
showResponse (ServedLock lock) = "ServedLock lock:\n" ++ unlines (lockToAscii lock)
showResponse (ServedSolution soln) = "ServedSolution [SOLN]"
showResponse resp = show resp

handleRequest :: FilePath -> Maybe FilePath -> ClientRequest -> IO ServerResponse
handleRequest dbpath mfeedPath req@(ClientRequest pv auth action) = do
    let lockMode = case action of
            Authenticate     -> ReadMode
            GetServerInfo    -> ReadMode
            GetPublicKey     -> ReadMode
            GetLock _        -> ReadMode
            GetUserInfo _ _  -> ReadMode
            GetRetired _     -> ReadMode
            GetSolution _    -> ReadMode
            GetRandomNames _ -> ReadMode
            _                -> ReadWriteMode

    -- Check solutions prior to write-locking database.
    -- Slightly awkward, because we have to drop the read lock before
    -- acquiring the write lock, so need to check preconditions again once we
    -- have the write lock.
    withDBLock dbpath ReadMode (runExceptT $ checkRequest Nothing) >>=
        either (return . ServerError) (\mCheckedLock ->
            withDBLock dbpath lockMode $
                runExceptT (checkRequest mCheckedLock >> handleRequest') >>=
                    either (return . ServerError) return)
    where
        checkRequest mCheckedLock = do
            when (pv /= protocolVersion) $ throwE "Bad protocol version"
            case action of
                DeclareSolution soln ls target idx -> do
                    info <- getUserInfoOfAuth auth
                    lock <- getLock ls
                    tinfo <- getALock target
                    when (ls /= lockSpec tinfo) $ throwE "Lock no longer in use!"
                    when (public tinfo) $ throwE "Lock solution already public knowledge!"
                    let name = codename info
                    let behind = ActiveLock name idx
                    when (name `elem` map noteAuthor (lockSolutions tinfo)) $
                        throwE "Note already taken on that lock!"
                    when (name == lockOwner target) $
                        throwE "That's your lock!"
                    behindLock <- getALock behind
                    when (public behindLock) $ throwE "Your lock is cracked!"
                    case mCheckedLock of
                        Nothing -> unless (checkSolution lock soln) $ throwE "Bad solution"
                        Just lock' -> unless (lock == lock') $ throwE "Lock changed!"
                    return $ Just lock
                SetLock lock@(frame,_) idx soln -> do
                    ServerInfo serverSize _ <- getServerInfo
                    when (frame /= BasicFrame serverSize) $ throwE $
                        "Server only accepts size "++show serverSize++" locks."
                    unless (validLock $ reframe lock) $ throwE "Invalid lock!"
                    when (checkSolved $ reframe lock) $ throwE "Lock not locked!"
                    RCLockHashes hashes <- getRecordErrored RecLockHashes
                            `catchE` const (return (RCLockHashes []))
                    let hashed = hash $ show lock
                    when (hashed `elem` hashes) $ throwE "Lock has already been used"
                    case mCheckedLock of
                        Nothing -> unless (checkSolution lock soln) $ throwE "Bad solution"
                        Just lock' -> unless (lock == lock') $ throwE "Lock changed!"
                    return $ Just lock
                RetireLock target -> do
                    tinfo <- getALock target
                    name <- codename <$> getUserInfoOfAuth auth
                    RCIsSuperuser isSuper <- lift . (fromRight (RCIsSuperuser False) <$>) . runExceptT $ getRecordErrored (RecIsSuperuser name)
                    when (not isSuper && name /= lockOwner target) $ throwE "That isn't your lock!"
                    when (public tinfo) $ throwE "Lock already public."
                    pure Nothing
                _ -> return Nothing
        handleRequest' =
            case action of
                UndefinedAction -> throwE "Request not recognised by this server"
                Authenticate -> do
                    checkAuth auth
                    return $ ServerMessage $ "Welcome, " ++ authUser (fromJust auth)
                Register -> do
                    newUser auth
                    doNews $ "New user " ++ authUser (fromJust auth) ++ " registered."
                    return ServerAck
                ResetPassword passwd -> resetPassword auth passwd >> return ServerAck
                SetEmail address -> setEmail auth address >> return ServerAck
                GetServerInfo -> ServedServerInfo <$> getServerInfo
                GetPublicKey -> ServedPublicKey <$> getPublicKey
                GetLock ls -> ServedLock <$> getLock ls
                GetRetired name -> ServedRetired <$> getRetired name
                GetUserInfo name mversion -> (do
                        RCUserInfo (curV,info) <- getRecordErrored $ RecUserInfo name
                        (fromJust<$>)$ runMaybeT $ msum [ do
                            v <- MaybeT $ return mversion
                            msum [ guard (v >= curV) >> return ServerFresh
                                , do
                                    guard (v >= curV - 10)
                                    RCUserInfoDeltas deltas <- lift $ getRecordErrored $ RecUserInfoLog name
                                    return $ ServedUserInfoDeltas $ take (curV-v) deltas
                                ]
                            , return $ ServedUserInfo (curV,info)
                            ]
                    ) `catchE` \_ -> return ServerCodenameFree
                GetSolution note -> do
                    uinfo <- getUserInfoOfAuth auth
                    let uname = codename uinfo
                    onLinfo <- getALock $ noteOn note
                    behindMLinfo <- maybe (return Nothing) ((Just<$>).getALock) $ noteBehind note
                    if uname == lockOwner (noteOn note)
                        || uname == noteAuthor note
                    then ServedSolution <$> getSolution note
                    else if case behindMLinfo of
                                Nothing -> True
                                Just behindInfo -> public behindInfo || uname `elem` accessedBy behindInfo
                            || note `elem` notesRead uinfo
                        then if public onLinfo || uname `elem` accessedBy onLinfo
                            then ServedSolution <$> getSolution note
                            else throwE "You can't wholly decipher this note - you would need more notes on the same lock."
                        else throwE "This note is secured behind a lock you have not opened."
                DeclareSolution soln ls target idx -> do
                    info <- getUserInfoOfAuth auth
                    let name = codename info
                    let behind = ActiveLock name idx
                    let note = NoteInfo name (Just behind) target
                    erroredDB $ putRecord (RecNote note) (RCSolution soln)
                    execStateT (declareNote note behind) [] >>= applyDeltasToRecords
                    doNews $ name ++ " declares solution to "
                        ++ alockStr target ++ ", securing their note behind "
                        ++ alockStr behind ++ "."
                    mailDeclaration target behind
                    return ServerAck
                SetLock lock@(frame,_) idx soln -> do
                    info <- getUserInfoOfAuth auth
                    let name = codename info
                    let al = ActiveLock name idx
                    RCLockHashes hashes <- getRecordErrored RecLockHashes
                            `catchE` const (return (RCLockHashes []))
                    let hashed = hash $ show lock
                    erroredDB $ putRecord RecLockHashes $ RCLockHashes $ hashed:hashes

                    ls <- erroredDB $ newLockRecord lock
                    let oldLockInfo = userLocks info ! idx
                    execStateT (do
                            when (isJust oldLockInfo) $
                                lift (getALock al) >>= retireLock
                            addDelta name $ PutLock ls idx
                        ) [] >>= applyDeltasToRecords

                    for_ oldLockInfo $ \oldui -> do
                        lss <- getRetired name
                        erroredDB $ putRecord (RecRetiredLocks name) $ RCLockSpecs $ lockSpec oldui:lss
                    doNews $ "New lock " ++ alockStr al ++ "."
                    return ServerAck
                GetRandomNames n -> do
                    names <- erroredDB listUsers
                    gen <- erroredIO newStdGen
                    let l = length names
                        namesArray = listArray (0,l-1) names
                        negligible name = do
                            uinfo <- getUserInfo name
                            return $ all (maybe True public . (userLocks uinfo !)) [0..2]

                    -- huzzah for pipes!
                    shuffled <- P.toListM $
                            mapM_ Pipes.yield (nub $ randomRs (0,l-1) gen)
                        >-> P.take l -- give up once we've permuted all of [0..l-1]
                        >-> P.map (namesArray !)
                        >-> P.filterM ((not <$>) . negligible) -- throw away negligibles
                        >-> P.take n -- try to take as many as we were asked for
                    liftIO newStdGen
                    return $ ServedRandomNames shuffled
                RetireLock target -> do
                    lockInfo <- getALock target
                    execStateT (publiciseLock target lockInfo) [] >>= applyDeltasToRecords
                    return ServerAck
                _ -> throwE "BUG: bad request"
        erroredIO :: IO a -> ExceptT String IO a
        erroredIO c = do
            ret <- liftIO $ catchIO (Right <$> c) (return.Left)
            case ret of
                Left e  -> throwE $ "Server IO error: " ++ show e
                Right x -> return x
        erroredDB :: DBM a -> ExceptT String IO a
        erroredDB = erroredIO . withDB dbpath
        getRecordErrored :: Record -> ExceptT String IO RecordContents
        getRecordErrored rec = do
            mrc <- lift $ withDB dbpath $ getRecord rec
            case mrc of
                Just rc -> return rc
                Nothing -> throwE $ "Bad record on server! Record was: " ++ show rec
        getLock ls = do
            RCLock lock <- getRecordErrored $ RecLock ls
            return lock
        getSolution note = do
            RCSolution soln <- getRecordErrored $ RecNote note
            return soln
        getServerInfo = do
            RCServerInfo sinfo <- getRecordErrored RecServerInfo
            return sinfo
        getPublicKey = do
            RCPublicKey publicKey <- getRecordErrored RecPublicKey
            return publicKey
        getRetired name = do
            RCLockSpecs lss <- fromMaybe (RCLockSpecs []) <$> erroredDB (getRecord $ RecRetiredLocks name)
            return lss
        getALock (ActiveLock name idx) = do
            info <- getUserInfo name
            checkValidLockIndex idx
            case ((! idx).userLocks) info of
                Nothing       -> throwE "Lock not set"
                Just lockinfo -> return lockinfo
        checkValidLockIndex idx =
            unless (0<=idx && idx < maxLocks) $ throwE "Bad lock index"
        getUserInfo name = do
            RCUserInfo (version,info) <- getRecordErrored $ RecUserInfo name
            return info
        getUserInfoOfAuth auth = do
            checkAuth auth
            let Just (Auth name _) = auth
            getUserInfo name

        decryptPassword :: String -> ExceptT String IO String
        decryptPassword pw = do
            RCSecretKey secretKey <- getRecordErrored RecSecretKey
            blinder <- liftIO . generateBlinder $ private_n secretKey
            ExceptT . return . bimap
                    (\err -> show err ++ "; try deleting ~/.intricacy/cache ?")
                    CS.unpack .
                decrypt (Just blinder) (defaultOAEPParams SHA256) secretKey . CS.pack $ pw
            -- XXX: <=intricacy-0.6.2 sends the hashed password unencrypted,
            -- but we don't support that anymore
        convertLegacyPW :: Codename -> IO ()
        convertLegacyPW name = void . runExceptT $ do
            RCPasswordLegacy legacyPw <- getRecordErrored (RecPasswordLegacy name)
            pwA2 <- argon2 legacyPw
            erroredDB $ putRecord (RecPasswordArgon2 name) (RCPasswordArgon2 pwA2)
            erroredDB $ delRecord (RecPasswordLegacy name)
        checkAuth :: Maybe Auth -> ExceptT String IO ()
        checkAuth Nothing = throwE "Authentication required"
        checkAuth (Just (Auth name pw)) = do
            exists <- checkCodeName name
            unless exists $ throwE "No such user"
            liftIO $ convertLegacyPW name
            pw' <- decryptPassword pw
            RCPasswordArgon2 correctPwA2 <- getRecordErrored (RecPasswordArgon2 name)
            pwA2 <- argon2 pw'
            when (pwA2 /= correctPwA2) $ throwE "Wrong password"
        newUser :: Maybe Auth -> ExceptT String IO ()
        newUser Nothing = throwE "Require authentication"
        newUser (Just (Auth name pw)) = do
            exists <- checkCodeName name
            when exists $ throwE "Codename taken"
            pw' <- decryptPassword pw >>= argon2
            erroredDB $ putRecord (RecPasswordArgon2 name) (RCPasswordArgon2 pw')
            erroredDB $ putRecord (RecUserInfo name) (RCUserInfo (1,initUserInfo name))
            erroredDB $ putRecord (RecUserInfoLog name) (RCUserInfoDeltas [])
        resetPassword Nothing _ = throwE "Authentication required"
        resetPassword auth@(Just (Auth name _)) newpw = do
            checkAuth auth
            newpw' <- decryptPassword newpw >>= argon2
            erroredDB $ putRecord (RecPasswordArgon2 name) (RCPasswordArgon2 newpw')
        setEmail Nothing _ = throwE "Authentication required"
        setEmail auth@(Just (Auth name _)) addressStr = do
            checkAuth auth
#ifdef SENDMAIL
            serverAddr <- erroredDB $ getRecord RecServerEmail
            when (isNothing serverAddr) $ throwE "This server is not configured to support email notifications."
            let addr = CS.pack addressStr
            unless (CS.null addr || Text.Email.Validate.isValid addr) $ throwE "Invalid email address"
            erroredDB $ putRecord (RecEmail name) (RCEmail addr)
#else
            throwE "This server is not compiled to support email notifications."
#endif
        checkCodeName :: Codename -> ExceptT String IO Bool
        checkCodeName name = do
            unless (validCodeName name) $ throwE "Invalid codename"
            liftIO $ withDB dbpath $ do
                ok <- recordExists $ RecPasswordArgon2 name
                oklegacy <- recordExists $ RecPasswordLegacy name
                return $ ok || oklegacy
        --- | TODO: journalling so we can survive death during database writes?
        applyDeltasToRecords :: [(Codename, UserInfoDelta)] -> ExceptT String IO ()
        applyDeltasToRecords nds = sequence_ $ [applyDeltasToRecord name deltas
                | group <- groupBy ((==) `on` fst) nds
                , let name = fst $ head group
                , let deltas = map snd group ]
        applyDeltasToRecord name deltas = do
            erroredDB $ modifyRecord (RecUserInfoLog name) $
                \(RCUserInfoDeltas deltas') -> RCUserInfoDeltas $ deltas ++ deltas'
            erroredDB $ modifyRecord (RecUserInfo name) $
                \(RCUserInfo (v,info)) -> RCUserInfo
                    (v+length deltas, applyDeltas info deltas)
        declareNote note@(NoteInfo _ _ target) behind@(ActiveLock name idx) = do
            accessLock name target =<< getCurrALock target
            addDelta (lockOwner target) $ LockDelta (lockIndex target) $ AddSolution note
            addDelta name $ LockDelta idx $ AddSecured note
            accessed <- accessedBy <$> getCurrALock behind
            mapM_ (addReadNote note) (name:accessed)
        addReadNote note@(NoteInfo _ _ target) name = do
            info <- getCurrUserInfo name
            tlock <- getCurrALock target
            unless (note `elem` notesRead info) $ do
                addDelta name $ AddRead note
                checkSuffReadNotes target name
        accessLock name target@(ActiveLock tname ti) tlock = do
            addDelta tname $ LockDelta ti $ AddAccessed name
            mapM_ (`addReadNote` name) $ notesSecured tlock
        publiciseLock al@(ActiveLock name idx) lock = do
            addDelta name $ LockDelta idx SetPublic
            retireLock lock
        retireLock lock = do
            mapM_ scrapNote $ lockSolutions lock
            mapM_ publiciseNote $ notesSecured lock
        scrapNote note@(NoteInfo _ (Just al@(ActiveLock name idx)) _) = do
            addDelta name $ LockDelta idx (DelSecured note)
            unreadNote note
        scrapNote _ = return ()
        unreadNote note@(NoteInfo name (Just al) _) = do
            lock <- getCurrALock al
            mapM_ (\name' -> addDelta name' (DelRead note)) $ name:accessedBy lock
        publiciseNote note@(NoteInfo _ _ al@(ActiveLock name idx)) = do
            unreadNote note
            addDelta name $ LockDelta idx $ SetPubNote note
            publified <- checkSuffPubNotes al
            unless publified $ do
                lock <- getCurrALock al
                accessorsOfNotesOnLock <- (++ map noteAuthor (lockSolutions lock)).concat
                    <$> sequence
                        [ accessedBy <$> getCurrALock behind | NoteInfo _ (Just behind) _ <- lockSolutions lock ]
                forM_ accessorsOfNotesOnLock $ checkSuffReadNotes al
        checkSuffReadNotes target name = do
            info <- getCurrUserInfo name
            tlock <- getCurrALock target
            unless (name `elem` accessedBy tlock || public tlock || name == lockOwner target) $ do
                let countRead = fromIntegral $ length $
                        filter (\n -> isNothing (noteBehind n) || n `elem` notesRead info) $ lockSolutions tlock
                when (countRead == notesNeeded) $
                    accessLock name target tlock
        checkSuffPubNotes al@(ActiveLock name idx) = do
            lock <- getCurrALock al
            let countPub = fromIntegral $ length $
                    filter (isNothing.noteBehind) $ lockSolutions lock
            if countPub == notesNeeded
                then publiciseLock al lock >> return True
                else return False
        -- | XXX we apply deltas right-to-left, so in the order of adding
        addDelta name delta = modify ((name,delta):)
        getCurrUserInfo name = do
            info <- lift $ getUserInfo name
            applyDeltas info . map snd . filter ((==name).fst) <$> get
        getCurrALock al@(ActiveLock name idx) =
             fromJust.(! idx).userLocks <$> getCurrUserInfo name
        doNews :: String -> ExceptT String IO ()
        doNews news = case mfeedPath of
            Nothing -> return ()
            Just feedPath -> lift $ void $ forkIO $ do
                let baseFeed = withFeedTitle (TS.pack "Intricacy updates") $ newFeed $ RSSKind Nothing
                feed <- fromMaybe baseFeed <$> parseFeedFromFile feedPath
                time <- formatTime defaultTimeLocale rfc822DateFormat <$> getZonedTime
                let newsText = TS.pack news
                    timeText = TS.pack time
                    item = withItemTitle newsText $ withItemDescription newsText $
                        withItemPubDate timeText $ newItem $ RSSKind Nothing
                -- TODO: purge old entries
                let Right element = XML.fromXMLElement $ xmlFeed $ withFeedLastUpdate timeText $ addItem item feed
                    document = XML.Document (XML.Prologue [] Nothing []) element []
                -- | force the feed, so feedPath is closed (lazy IO)
                feedString <- return $! TL.unpack $ XML.renderText XML.def document
                writeFile feedPath feedString
#ifdef SENDMAIL
        mailDeclaration target@(ActiveLock name _) behind@(ActiveLock solverName _) = runMaybeT $ do
            let makeAddr :: CS.ByteString -> SMTP.Address
                makeAddr bs = SMTP.Address Nothing $ TS.pack $ CS.unpack bs
            RCEmail serverAddr <-  MaybeT $ erroredDB $ getRecord RecServerEmail
            RCEmail playerAddr <- MaybeT $ erroredDB $ getRecord $ RecEmail name
            guard $ not $ CS.null playerAddr
            lift.lift $ SMTP.sendMail "localhost" $ SMTP.simpleMail (makeAddr serverAddr)
                [makeAddr playerAddr] [] []
                (TS.pack $ "[Intricacy] " ++ alockStr target ++" solved by " ++ solverName)
                [plainPart $ TL.pack $ "A solution to your lock " ++ alockStr target ++ " has been declared by " ++ solverName ++
                    " and secured behind " ++ alockStr behind ++ "." ++
                    "\n\n-----\n\nYou received this email from the game Intricacy" ++
                    "\n\thttp://mbays.sdf.org/intricacy ." ++
                    "\nYou can disable notifications in-game by pressing 'R' on your home" ++
                    "\nscreen and setting an empty address." ++
                    "\nAlternatively, just reply to this email with the phrase \"stop bugging me\"." ]
#else
        mailDeclaration _ _ = pure ()
#endif

rewriteDB :: FilePath -> Handle -> IO ()
rewriteDB dbpath logh = do
    logit logh "Rewriting DB"
    withDBLock dbpath WriteMode $ withDB dbpath $ do
        users <- listUsers
        forM_ users $ \name -> modifyRecord (RecUserInfo name) id
        forM_ users $ \name -> modifyRecord (RecUserInfoLog name) id
    logit logh "DB rewriting complete"

checkNotes :: FilePath -> Handle -> IO ()
checkNotes dbpath logh = do
    logit logh "Checking notes"
    withDBLock dbpath WriteMode $ withDB dbpath $ do
        users <- listUsers
        forM_ users $ \name -> do
            RCUserInfo (_,uinfo) <- getRecordUnsafe $ RecUserInfo name
            sequence_ [ do
                RCSolution soln <- getRecordUnsafe $ RecNote note
                RCLock lock <- getRecordUnsafe $ RecLock ls
                unless (checkSolution lock soln) $
                    liftIO . putStrLn $ "Bad note on " <> show name <> ":" <> [lockIndexChar idx]
                | idx <- [0,1,2]
                , Just lockinfo <- [userLocks uinfo ! idx]
                , let ls = lockSpec lockinfo
                , note <- lockSolutions lockinfo
                ]
    logit logh "Finished checking notes"
    where
    getRecordUnsafe :: Record -> DBM RecordContents
    getRecordUnsafe = (fromJust <$>) . getRecord
