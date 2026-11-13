-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Database where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.Binary                as B
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as CS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import           Data.Char                  (toUpper)
import           Data.Maybe
import           Data.Tuple                 (swap)
import           System.Directory
import           System.FilePath
import           System.IO

import           Crypto.Hash                (Digest, SHA1, hashlazy)
import           Crypto.PubKey.RSA.Types    (PrivateKey, PublicKey)
import           Data.ByteArray.Encoding    (Base (..), convertToBase)

import           Lock
import           Metagame
import           Mundanities
import           Protocol

sha1 :: CL.ByteString -> Digest SHA1
sha1 = hashlazy
hash :: String -> String
hash = CS.unpack . digestToHexByteString . sha1 . CL.pack
    where digestToHexByteString = convertToBase Base16

data Record
    = RecPasswordLegacy Codename
    | RecPasswordArgon2 Codename
    | RecEmail Codename
    | RecUserInfo Codename
    | RecUserInfoLog Codename
    | RecLock LockSpec
    | RecNote NoteInfo
    | RecLockHashes
    | RecRetiredLocks Codename
    | RecServerInfo
    | RecServerEmail
    | RecPublicKey
    | RecSecretKey
    | RecIsSuperuser Codename
    deriving (Eq, Ord, Show)
data RecordContents
    = RCPasswordLegacy Password
    | RCPasswordArgon2 String
    | RCUserInfo VersionedUInfo
    | RCUserInfoDeltas [UserInfoDelta]
    | RCLock Lock
    | RCSolution Solution
    | RCLockHashes [String]
    | RCLockSpecs [LockSpec]
    | RCServerInfo ServerInfo
    | RCEmail CS.ByteString
    | RCPublicKey PublicKey
    | RCSecretKey PrivateKey
    | RCIsSuperuser Bool
    deriving (Eq, Show)

rcOfServerResp (ServedServerInfo x) = RCServerInfo x
rcOfServerResp (ServedLock x)       = RCLock x
rcOfServerResp (ServedSolution x)   = RCSolution x
rcOfServerResp (ServedUserInfo x)   = RCUserInfo x
rcOfServerResp (ServedRetired x)    = RCLockSpecs x
rcOfServerResp (ServedPublicKey x)  = RCPublicKey x
rcOfServerResp _                    = error "no corresponding rc"

invariantRecord (RecUserInfo _)       = False
invariantRecord (RecUserInfoLog _)    = False
invariantRecord (RecPasswordLegacy _) = False
invariantRecord (RecPasswordArgon2 _) = False
invariantRecord (RecRetiredLocks _)   = False
invariantRecord (RecNote _)           = False
invariantRecord (RecEmail _)          = False
invariantRecord _                     = True

askForRecord RecServerInfo          = GetServerInfo
askForRecord (RecUserInfo name)     = GetUserInfo name Nothing
askForRecord (RecLock ls)           = GetLock ls
askForRecord (RecNote note)         = GetSolution note
askForRecord (RecRetiredLocks name) = GetRetired name
askForRecord RecPublicKey           = GetPublicKey
askForRecord _                      = error "no corresponding request"

type DBM = ReaderT FilePath IO
withDB :: FilePath -> DBM a -> IO a
withDB = flip runReaderT

recordExists :: Record -> DBM Bool
recordExists rec = recordPath rec >>= liftIO . doesFileExist

-- |try to decode binary format, falling back to legacy Read format on failure
tryGetBinary :: (B.Binary a, Read a) => Handle -> IO (Maybe a)
tryGetBinary h = tryDecode . BL.fromStrict <$> hGetContentsNoClose h
    where
    tryDecode :: (B.Binary a, Read a) => BL.ByteString -> Maybe a
    tryDecode c =
        case B.decodeOrFail c of
            Right (_,_,a) -> Just a
            Left _        -> tryRead $ CS.unpack $ BL.toStrict c

hGetStrict h = CS.unpack <$> hGetContentsNoClose h

hGetContentsNoClose :: Handle -> IO BS.ByteString
hGetContentsNoClose h = concatMWhileNonempty (repeat $ BS.hGet h 1024)
    where
    concatMWhileNonempty (m:ms) = do
        bs <- m
        if BS.null bs
            then return bs
            else (bs `BS.append`) <$> concatMWhileNonempty ms
    concatMWhileNonempty [] = pure BS.empty

getRecord :: Record -> DBM (Maybe RecordContents)
getRecord rec = do
    path <- recordPath rec
    liftIO . ignoreIOErrAlt $ do
        h <- openFile path ReadMode
        getRecordh rec h <* hClose h
getRecordh (RecPasswordLegacy _) h = (RCPasswordLegacy <$>) . tryRead <$> hGetStrict h
getRecordh (RecPasswordArgon2 _) h = (RCPasswordArgon2 <$>) . tryRead <$> hGetStrict h
getRecordh (RecEmail _) h = (RCEmail <$>) . tryRead <$> hGetStrict h
getRecordh (RecUserInfo _) h = (RCUserInfo <$>) <$> tryGetBinary h
getRecordh (RecUserInfoLog _) h = (RCUserInfoDeltas <$>) <$> tryGetBinary h
getRecordh (RecLock _) h = (RCLock <$>) . tryRead <$> hGetStrict h
getRecordh (RecNote _) h = (RCSolution <$>) . tryRead <$> hGetStrict h
getRecordh RecLockHashes h = (RCLockHashes <$>) . tryRead <$> hGetStrict h
getRecordh (RecRetiredLocks _) h = (RCLockSpecs <$>) . tryRead <$> hGetStrict h
getRecordh RecServerInfo h = (RCServerInfo <$>) . tryRead <$> hGetStrict h
getRecordh RecServerEmail h = (RCEmail <$>) . tryRead <$> hGetStrict h
getRecordh RecPublicKey h = (RCPublicKey <$>) . tryRead <$> hGetStrict h
getRecordh RecSecretKey h = (RCSecretKey <$>) . tryRead <$> hGetStrict h
getRecordh (RecIsSuperuser _) h = (RCIsSuperuser <$>) . tryRead <$> hGetStrict h

putRecord :: Record -> RecordContents -> DBM ()
putRecord rec rc = do
    path <- recordPath rec
    liftIO $ do
        mkdirhierto path
        h <- openFile path WriteMode
        putRecordh rc h
        hClose h
putRecordh (RCPasswordLegacy hpw) h    = hPutStr h $ show hpw
putRecordh (RCPasswordArgon2 hpw) h    = hPutStr h $ show hpw
putRecordh (RCEmail addr) h            = hPutStr h $ show addr
putRecordh (RCUserInfo info) h         = BL.hPut h $ B.encode info
putRecordh (RCUserInfoDeltas deltas) h = BL.hPut h $ B.encode deltas
putRecordh (RCLock lock) h             = hPutStr h $ show lock
putRecordh (RCSolution solution) h     = hPutStr h $ show solution
putRecordh (RCLockHashes hashes) h     = hPutStr h $ show hashes
putRecordh (RCLockSpecs lss) h         = hPutStr h $ show lss
putRecordh (RCServerInfo sinfo) h      = hPutStr h $ show sinfo
putRecordh (RCPublicKey publicKey) h   = hPutStr h $ show publicKey
putRecordh (RCSecretKey secretKey) h   = hPutStr h $ show secretKey
putRecordh (RCIsSuperuser super) h     = hPutStr h $ show super

modifyRecord :: Record -> (RecordContents -> RecordContents) -> DBM ()
modifyRecord rec f = do
    h <- recordPath rec >>= liftIO . flip openFile ReadWriteMode
    liftIO $ do
        Just rc <- getRecordh rec h
        hSeek h AbsoluteSeek 0
        putRecordh (f rc) h
        hTell h >>= hSetFileSize h
        hClose h

delRecord :: Record -> DBM ()
delRecord rec = recordPath rec >>= liftIO . removeFile

newLockRecord :: Lock -> DBM LockSpec
newLockRecord lock = do
    dbpath <- ask
    let path = dbpath++[pathSeparator]++"lastlock"
    h <- liftIO $ openFile path ReadWriteMode
    contents <- liftIO $ hGetStrict h
    let ls = if null contents then 0 else 1 + read contents
    liftIO $ hSeek h AbsoluteSeek 0
    liftIO $ hPutStr h $ show ls
    liftIO $ hClose h
    putRecord (RecLock ls) (RCLock lock)
    return ls

listUsers :: DBM [Codename]
listUsers = do
    dbpath <- ask
    liftIO $ (unpathifyName <$>) . filter ((==3).length) <$>
        getDirectoryContents (dbpath++[pathSeparator]++"users")

recordPath :: Record -> DBM FilePath
recordPath rec =
    (++ (pathSeparator : recordPath' rec)) <$> ask
    where
        recordPath' (RecPasswordLegacy name) = userDir name ++ "passwd"
        recordPath' (RecPasswordArgon2 name) = userDir name ++ "passwd_argon2"
        recordPath' (RecEmail name) = userDir name ++ "email"
        recordPath' (RecUserInfo name) = userDir name ++ "info"
        recordPath' (RecUserInfoLog name) = userDir name ++ "log"
        recordPath' (RecLock ls) = locksDir ++ show ls
        recordPath' (RecNote (NoteInfo name _  alock)) =
            userDir name ++ "notes" ++ [pathSeparator] ++ alockFN alock
        recordPath' (RecRetiredLocks name) = userDir name ++ "retired"
        recordPath' RecLockHashes = "lockHashes"
        recordPath' RecServerInfo = "serverInfo"
        recordPath' RecServerEmail = "serverEmail"
        recordPath' RecPublicKey = "publicKey"
        recordPath' RecSecretKey = "secretKey"
        recordPath' (RecIsSuperuser name) = userDir name ++ "superuser"

        userDir name = "users" ++ [pathSeparator] ++ pathifyName name ++ [pathSeparator]
        alockFN (ActiveLock name idx) = pathifyName name ++":"++ show idx
        locksDir = "locks"++[pathSeparator]

pathifyName = winSux . dummyPunctuation

-- | Hilariously, "CON", "PRN", "AUX", and "NUL" are reserved on DOS, and
-- Windows apparently crashes rather than write a directory with that name!
winSux name = if (toUpper <$> name) `elem` ["CON","PRN", "AUX","NUL"]
    then '_':name
    else name

-- | Dummy out characters which are disallowed on unix or dos.
-- We use lowercase characters as dummies.
-- To avoid collisions on case-insensitive filesystems, we use '_' as an
-- escape character.
dummyPunctuation = concatMap $ \c ->
    maybe [c] (('_':) . pure) (lookup c pathifyAssocs)
unpathifyName = concatMap $ \c -> case c of
        '_' -> ""
        _   -> pure $ fromMaybe c (lookup c $ swap <$> pathifyAssocs)
pathifyAssocs =
    [ ('/','s')
    , ('.','d')
    , ('\\','b')
    , ('<','l')
    , ('>','g')
    , (':','c')
    , ('|','p')
    , ('?','q')
    , ('*','a')
    , ('+','t')
    , (',','m')
    , (';','i')
    , ('=','e')
    , ('[','k')
    , (']','j')
    , ('_','u')
    ]
