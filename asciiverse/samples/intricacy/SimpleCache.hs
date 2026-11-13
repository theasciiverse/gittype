-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module SimpleCache where

import qualified Data.List as L
import qualified Data.Map  as M

-- Stupid implementation of least-recent caching
data SimpleCache k v = SimpleCache
    { maxSize :: Int
    , size    :: Int
    , cache   :: M.Map k v
    , order   :: [k]
    , dealloc :: v -> IO ()
    }

empty :: Int -> (v -> IO ()) -> SimpleCache k v
empty mx = SimpleCache mx 0 M.empty []

insert :: (Eq k, Ord k) => k -> v -> SimpleCache k v -> IO (SimpleCache k v)
insert k v (SimpleCache mxSz sz c ks m)
    | k `M.member` c = pure $ SimpleCache mxSz sz c (k:L.delete k ks) m
    | sz < mxSz = pure $ SimpleCache mxSz (sz+1) (M.insert k v c) (k:ks) m
    | mn <- last ks
    , Just mnv <- c M.!? mn = do
        m mnv
        insert k v $ SimpleCache mxSz (sz-1) (M.delete mn c) (L.delete mn ks) m
    | otherwise = error "BUG in SimpleCache.insert"

deallocAll :: SimpleCache k v -> IO ()
deallocAll (SimpleCache _ _ c _ m) = mapM_ m (M.elems c)

(!?) :: (Eq k, Ord k) => SimpleCache k v -> k -> Maybe v
SimpleCache _ _ c _ _ !? k = c M.!? k
