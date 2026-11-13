-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

{-# LANGUAGE LambdaCase #-}

module Cache where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Reader
import           Data.Binary
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import           Data.Maybe
import           Network.Simple.TCP         (connect, recv, send)
import           System.Directory
import           System.FilePath

import           Database
import           Metagame
import           Mundanities
import           Protocol
import           ServerAddr

data FetchedRecord = FetchedRecord {fresh :: Bool, fetchError :: Maybe String, fetchedRC :: Maybe RecordContents}
    deriving (Eq, Show)


getRecordCached :: ServerAddr -> Maybe Auth -> Maybe (TVar Bool) -> Bool -> Record -> IO (TVar FetchedRecord)
getRecordCached saddr _ _ _ _ | nullSaddr saddr = do
    newTVarIO (FetchedRecord True (Just "No server set.") Nothing)
getRecordCached saddr auth mflag cOnly rec = do
    fromCache <- withCache saddr $ getRecord rec
    let isFresh = isJust fromCache && invariantRecord rec
    tvar <- newTVarIO (FetchedRecord isFresh Nothing fromCache)
    unless (cOnly || isFresh) $ void $ forkIO $ getRecordFromServer fromCache tvar
    return tvar
    where
        getRecordFromServer fromCache tvar = do
            let action = case rec of
                    RecUserInfo name ->
                        let curVersion = (\(RCUserInfo (v,_)) -> v) <$> fromCache
                        in GetUserInfo name curVersion
                    _ -> askForRecord rec
            resp <- makeRequest saddr (ClientRequest
                    protocolVersion (if needsAuth action then auth else Nothing) action)
            case resp of
                ServerError err -> tellRec $ FetchedRecord True (Just err) fromCache
                ServerCodenameFree -> tellRec $ FetchedRecord True Nothing Nothing
                ServerFresh -> tellRec $ FetchedRecord True Nothing fromCache
                ServedUserInfoDeltas deltas -> do
                    let Just (RCUserInfo (v,info)) = fromCache
                    let rc = RCUserInfo (v+length deltas, applyDeltas info deltas)
                    withCache saddr $ putRecord rec rc
                    tellRec $ FetchedRecord True Nothing (Just rc)
                _ -> do
                    let rc = rcOfServerResp resp
                    withCache saddr $ putRecord rec rc
                    tellRec $ FetchedRecord True Nothing (Just rc)
            where
                tellRec fr = atomically $ do
                    writeTVar tvar fr
                    case mflag of {Just flag -> writeTVar flag True; _ -> return ()}

waitFetchedFresh :: TVar FetchedRecord -> IO ()
waitFetchedFresh tvar = atomically $ readTVar tvar >>= check.fresh

makeRequest :: ServerAddr -> ClientRequest -> IO ServerResponse
makeRequest saddr _ | nullSaddr saddr =
    return $ ServerError "No server set."
makeRequest saddr@(ServerAddr host prt) request =
    handle (return . ServerError . (show::SomeException -> String)) $ do
        connect host (show prt) makeRequest'
            `catchIO` (\e -> return $ ServerError $ "Cannot connect to "++saddrStr saddr++"! "++show e)
    where
        makeRequest' (sock,_) = do
            send sock . BL.toStrict $ encode request
            decode . BL.fromStrict <$> recvAll sock
        recvAll sock =
            recv sock 4096 >>= \case
                Nothing -> return BS.empty
                Just b  -> BS.append b <$> recvAll sock


knownServers :: IO [ServerAddr]
knownServers = ignoreIOErr $ do
    cachedir <- confFilePath "cache"
    saddrstrs <- getDirectoryContents cachedir >>= filterM (\dir ->
        doesFileExist $ cachedir++[pathSeparator]++dir++[pathSeparator]++"serverInfo")
    return $ mapMaybe strToSaddr saddrstrs

withCache :: ServerAddr -> DBM a -> IO a
withCache saddr m = do
    cachedir <- (++pathSeparator : saddrPath saddr) <$> confFilePath "cache"
    runReaderT m cachedir
