-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

{-# LANGUAGE CPP #-}

module Mundanities where
import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Catch    (MonadMask, catch, handle)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BSC
import           Data.List
import           Data.Maybe
import           Paths_intricacy
import           System.Directory
import           System.Environment     (getEnv, getExecutablePath,
                                         lookupEnv)
import           System.FilePath

catchIO :: IO a -> (IOError -> IO a) -> IO a
catchIO = catch

ignoreIOErr :: (MonadIO m, MonadMask m, Monoid a) => m a -> m a
ignoreIOErr = handle ((\_ -> return mempty) :: (Monad m, Monoid a) => IOError -> m a)

ignoreIOErrAlt :: (MonadIO m, MonadMask m, Alternative f) => m (f a) -> m (f a)
ignoreIOErrAlt = handle ((\_ -> return empty) :: (Monad m, Alternative f) => IOError -> m (f a))

nothingOnIOErr :: (MonadIO m, MonadMask m) => m a -> m (Maybe a)
nothingOnIOErr = ignoreIOErrAlt . (Just <$>)

warnIOErrAlt :: (MonadIO m, MonadMask m, Alternative f) => m (f a) -> m (f a)
warnIOErrAlt = handle ((\e -> liftIO (print e) >> return empty) :: (MonadIO m, Alternative f) => IOError -> m (f a))

unlessIOErr :: (MonadIO m, MonadMask m) => m Bool -> m Bool
unlessIOErr = (fromMaybe False <$>) . ignoreIOErrAlt . (Just <$>)

readReadFile :: (Read a) => FilePath -> IO (Maybe a)
readReadFile file = ignoreIOErrAlt $ tryRead . BSC.unpack <$> BS.readFile file

tryRead :: (Read a) => String -> Maybe a
tryRead = (fst <$>) . listToMaybe . reads

readStrings :: FilePath -> IO [String]
readStrings file = ignoreIOErr $ lines . BSC.unpack <$> BS.readFile file

writeReadFile :: (Show a) => FilePath -> a -> IO ()
writeReadFile file x = do
    mkdirhierto file
    BS.writeFile file $ BSC.pack $ show x

writeStrings :: FilePath -> [String] -> IO ()
writeStrings file x = do
    mkdirhierto file
    BS.writeFile file $ BSC.pack $ unlines x

confFilePath :: FilePath -> IO FilePath
confFilePath str = (++(pathSeparator:str)) <$>
    catchIO (getEnv "INTRICACY_PATH") (const defaultConfDir)

-- | The player's own folder: ~/.intricacy when it is already there (older
-- versions put everything in it), else the platform's state folder:
-- $XDG_STATE_HOME/intricacy or ~/.local/state/intricacy on Linux and other
-- unixes, ~/Library/Application Support/intricacy on macOS, and
-- %LocalAppData%\intricacy on Windows.
defaultConfDir :: IO FilePath
defaultConfDir = do
    old <- getAppUserDataDirectory "intricacy"
    oldExists <- unlessIOErr $ doesDirectoryExist old
    if oldExists then return old else stateDir
  where
    stateDir = do
#if defined(WINDOWS) || defined(mingw32_HOST_OS)
        base <- lookupEnv "LOCALAPPDATA"
        maybe (getAppUserDataDirectory "intricacy") (return . (</> "intricacy")) base
#elif defined(darwin_HOST_OS)
        home <- getHomeDirectory
        return $ home </> "Library" </> "Application Support" </> "intricacy"
#else
        xdg <- lookupEnv "XDG_STATE_HOME"
        case xdg of
            Just d | isAbsolute d -> return $ d </> "intricacy"
            _ -> (</> ".local" </> "state" </> "intricacy") <$> getHomeDirectory
#endif

-- | Game data (tutorial, initiation, sounds, font): beside the executable
-- when it is there, so an unpacked archive runs from any folder; otherwise
-- where cabal installed it.
getDataPath :: FilePath -> IO FilePath
getDataPath name = do
    exeDir <- takeDirectory <$> catchIO getExecutablePath (const $ return "")
    let beside = exeDir </> name
    here <- unlessIOErr $ (||) <$> doesDirectoryExist beside <*> doesFileExist beside
    if not (null exeDir) && here then return beside else installed
  where
#ifdef WINDOWS
    installed = pure name
#else
    installed = getDataFileName name
#endif

makeConfDir :: IO ()
makeConfDir = confFilePath "" >>= createDirectoryIfMissing True

fileExists :: FilePath -> IO Bool
fileExists = unlessIOErr . doesFileExist

mkdirhierto :: FilePath -> IO ()
mkdirhierto = mkdirhier . takeDirectory

mkdirhier :: FilePath -> IO ()
mkdirhier = createDirectoryIfMissing True

getDirContentsRec :: FilePath -> IO [FilePath]
getDirContentsRec path = ignoreIOErr $ do
    contents <- map ((path++[pathSeparator])++) . filter ((/='.').head) <$> getDirectoryContents path
    annotated <- (\p -> (,) p <$> doesDirectoryExist p) `mapM` contents
    let (dirs,files) = join (***) (map fst) $ partition snd annotated
    (files++) . concat <$> getDirContentsRec `mapM` dirs

fullLockPath path = if isAbsolute path
    then return path
    else do
        homePath <- getHomeDirectory
        locksPath <- confFilePath "locks"
        return $ if take 2 path == "~/"
            then homePath </> drop 2 path
            else locksPath </> path
