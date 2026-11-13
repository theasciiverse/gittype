-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Main where

import           Control.Monad
import           Data.Binary
import qualified Data.ByteString.Lazy as L
import           Data.Char
import           Data.List            (intersperse)
import           Data.Maybe
import           System.Environment
import           System.IO

--import Data.BinaryCom as BC
import           Network.Fancy
import           Text.Show.Pretty     (ppShow)

import           BinaryInstances
import           Lock
import           Metagame
import           Mundanities
import           Physics
import           Protocol

port = 27001 -- 27001 == ('i'<<8) + 'y'
main = withStream (IP "localhost" port) handler

readAL :: String -> ActiveLock
readAL s = ActiveLock (take 3 s) (ord (s!!4) - ord 'A')

handler :: Handle -> IO ()
handler handle = do
    (name:passwd:cmd:args) <- getArgs
    action <- case cmd of
        "auth" -> return Authenticate
        "reg" -> return Register
        "sinfo" -> return GetServerInfo
        "getlock" -> return $ GetLock (read $ head args)
        "getuinfo" -> return $ GetUserInfo (head args)
        "getsolution" -> return $ GetSolution (read $ head args)
        "gethint" -> return $ GetHint (read $ head args)
        "setlock" -> liftM3 SetLock (fromJust <$> readReadFile (head args)) (return $ read (args!!1)) (fromJust <$> readReadFile (args!!2))
        "declare" -> liftM4 DeclareSolution (fromJust <$> readReadFile (head args)) (return $ read (args!!1)) (return $ readAL (args!!2)) (return $ read (args!!3))
    let request = ClientRequest protocolVersion (Just (Auth name passwd)) action
    L.hPut handle $ encode request
    hFlush handle
    response <- decode <$> L.hGetContents handle
    putStrLn $ ppShow (response :: ServerResponse)

