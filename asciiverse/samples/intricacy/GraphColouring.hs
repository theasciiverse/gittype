-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module GraphColouring (fiveColour) where

import           Data.List
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe

type Colouring a = Map a Int
type PlanarGraph a = Map a [a]

fiveColour :: Ord a => PlanarGraph a -> Colouring a -> Colouring a
-- ^algorithm based on that presented in
-- http://people.math.gatech.edu/~thomas/PAP/fcstoc.pdf
-- Key point: a planar graph can't have all vertices of degree >= 6
-- (Proof: suppose it does, so |E| >= 3|V|; WLOG the graph is triangulated,
-- so then |F| <= 2/3 |E|. So \xi = |V|-|E|+|F| <= (1/3 - 1 + 2/3)|E| = 0.
-- But a planar graph has Euler characteristic 1.)
-- Aims to minimise changes from given (partial) colouring lastCol.
fiveColour g lastCol =
    if Map.keysSet lastCol == Map.keysSet g && isColouring g lastCol
        then lastCol
        else fiveColour' lastCol g

isColouring :: Ord a => PlanarGraph a -> Colouring a -> Bool
isColouring g mapping = and
    [ Map.lookup s mapping /= Map.lookup e mapping
    | s <- Map.keys g
    , e <- g Map.! s ]

fiveColour' :: Ord a => Colouring a -> PlanarGraph a -> Colouring a
fiveColour' _ g | g == Map.empty = Map.empty
fiveColour' pref g =
    let adjsOf v = nub (g Map.! v) \\ [v]
        v0 = head $ filter ((<=5) . length . adjsOf) $ Map.keys g
        adjs = adjsOf v0
        addTo c =
            let vc = head $ possCols pref v0 \\ map (c Map.!) adjs
            in Map.insert v0 vc c
    in if length adjs < 5
       then addTo $ fiveColour' pref $ deleteNode v0 g
       else let (v',v'') = if adjs!!2 `elem` (g Map.! head adjs)
                    then (adjs!!1,adjs!!3)
                    else (head adjs,adjs!!2)
            in addTo $ demerge v' v'' $ fiveColour' pref $ merge v0 v' v'' g

possCols :: Ord a => Colouring a -> a -> [Int]
possCols pref v = maybe [0..4] (\lvc -> lvc:([0..4] \\ [lvc])) $ Map.lookup v pref

demerge :: Ord a => a -> a -> Colouring a -> Colouring a
demerge v v' c = Map.insert v' (c Map.! v) c

merge :: Ord a => a -> a -> a -> PlanarGraph a -> PlanarGraph a
merge v v' v'' g =
    deleteNode v $ contractNodes v' v''
        $ Map.adjust (concatAdjsOver v $ g Map.! v'') v' g

concatAdjsOver :: Ord a => a -> [a] -> [a] -> [a]
concatAdjsOver v adjs adjs' =
    let (s,_:e) = splitAt (fromJust $ elemIndex v adjs) adjs
    in s ++ adjs' ++ e

deleteNode :: Ord a => a -> PlanarGraph a -> PlanarGraph a
deleteNode v =
    fmap (filter (/= v)) . Map.delete v

contractNodes :: Ord a => a -> a -> PlanarGraph a -> PlanarGraph a
contractNodes v v' =
    fmap (map (\v'' -> if v'' == v' then v else v'')) . Map.delete v'
