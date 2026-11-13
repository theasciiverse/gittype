-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module ServerAddr where

import           Control.Applicative
import           Data.List
import           Data.Maybe

data ServerAddr = ServerAddr {hostname::String, port::Int}
    deriving (Eq, Ord, Show, Read)
nullSaddr (ServerAddr host _) = null host

defaultPort=27001 -- == ('i'<<8) + 'y'
defaultServerAddr = ServerAddr "i.thegonz.net" defaultPort
oldDefaultServerAddrs = [ServerAddr "thegonz.net" defaultPort]

updateDefaultSAddr :: ServerAddr -> ServerAddr
updateDefaultSAddr saddr | saddr `elem` oldDefaultServerAddrs = defaultServerAddr
updateDefaultSAddr saddr = saddr

saddrStr (ServerAddr h p) = h ++ if p==defaultPort then "" else ':':show p

-- |windows doesn't like ':' in paths, so use '#' instead
saddrPath (ServerAddr h p) = h ++ if p==defaultPort then "" else '#':show p

strToSaddr str =
    case elemIndex ':' str of
        Nothing -> Just $ ServerAddr str defaultPort
        Just idx -> do
            let (addr,portstr) = splitAt idx str
            ServerAddr addr . fst <$> listToMaybe (reads (drop 1 portstr))
