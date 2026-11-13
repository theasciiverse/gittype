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

module Init where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Maybe
import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath

import           Interact
import           Lock
import           MainState
import           Util
import           Version

data Opt
    = DataDir FilePath
    | LockSize Int
#ifdef CURSES
    | ForceCurses
#endif
    | Help
    | Version
    deriving (Eq, Ord, Show)

options =
    [ Option ['d'] ["datadir"] (ReqArg DataDir "PATH") "user data and conf directory (default: ~/.intricacy)"
#ifdef CURSES
    , Option ['c'] ["curses"] (NoArg ForceCurses) "force curses UI"
#endif
    , Option ['s'] ["locksize"] (ReqArg (LockSize . read) "SIZE") "locksize"
    , Option ['h'] ["help"] (NoArg Help) "show usage information"
    , Option ['v'] ["version"] (NoArg Version) "show version information"
    ]

usage :: String
usage = usageInfo header options
    where header = "Usage: intricacy [OPTION...] [file]"

parseArgs :: [String] -> IO ([Opt],[String])
parseArgs argv =
    case getOpt Permute options argv of
        (o,n,[])   -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usage))

setup :: IO (Maybe (Lock, Maybe Solution), [Opt], Maybe String)
setup = do
    argv <- getArgs
    (opts,args) <- parseArgs argv
    when (Help `elem` opts) $ putStr usage >> exitSuccess
    when (Version `elem` opts) $ putStrLn version >> exitSuccess
    let size = fromMaybe 8 $ listToMaybe [ lsize | LockSize lsize <- opts ]
    mapM_ (setEnv "INTRICACY_PATH") [ dir | DataDir dir <- opts ]

    curDir <- getCurrentDirectory
    (fromJust <$>) $ runMaybeT $ msum
        [ do
            path <- liftMaybe ((curDir </>) <$> listToMaybe args)
            msum [ do
                    (lock, msoln) <- MaybeT (readLock path)
                    return (Just (reframe lock, msoln), opts, Just path)
                , return (Just (baseLock size, Nothing), opts, Just path) ]
        , return (Nothing, opts, Nothing) ]

main' :: (UIMonad s, UIMonad c) =>
        Maybe (s MainState -> IO (Maybe MainState)) ->
        Maybe (c MainState -> IO (Maybe MainState)) -> IO ()
main' msdlUI mcursesUI = do
#ifdef CURSES
    (mlock,opts,mpath) <- setup
#else
    (mlock,_,mpath) <- setup
#endif
    initMState <- case mlock of
            Just (lock, msoln) -> return $ newEditState lock msoln mpath
            Nothing            -> initMetaState
    void $ runMaybeT $ msum [ do
            finalState <- msum
                [ do
#ifdef CURSES
                    guard $ ForceCurses `notElem` opts
#endif
                    sdlUI <- liftMaybe msdlUI
                    MaybeT $ sdlUI $ interactUI `execStateT` initMState
                , do
                    cursesUI <- liftMaybe mcursesUI
                    MaybeT $ cursesUI $ interactUI `execStateT` initMState
                ]
            lift $ writeMetaState finalState
            lift exitSuccess
        , lift exitFailure ]
