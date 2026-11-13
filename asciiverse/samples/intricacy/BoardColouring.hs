-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module BoardColouring where

import           Control.Applicative
import           Control.Monad
import           Data.Function       (on)
import           Data.List
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Set            (Set)
import qualified Data.Set            as Set

import           GameState
import           GameStateTypes
import           GraphColouring
import           Hex
import           Util

type PieceColouring = Map PieceIdx Int

colouredPieces :: Bool -> GameState -> [PieceIdx]
colouredPieces colourFixed st = [ idx |
    (idx, PlacedPiece _ p) <- enumVec $ placedPieces st
    , isPivot p ||
        isBlock p && (idx > 0) && colourFixed || not (null $ springsEndAtIdx st idx) ]

pieceTypeColouring :: GameState -> [PieceIdx] -> PieceColouring
pieceTypeColouring st coloured = Map.fromList
    [ (idx, col) | (idx, PlacedPiece _ p) <- enumVec $ placedPieces st
        , idx `elem` coloured
        , let col = if isBlock p then 1+((connGraphHeight st idx - 1) `mod` 5) else 0 ]


boardColouring :: GameState -> [PieceIdx] -> PieceColouring -> PieceColouring
boardColouring st coloured = fiveColour graph
    where
        board = stateBoard st
        graph = Map.fromList [ (idx, nub $ neighbours idx)
            | idx <- coloured ]
        neighbours idx =
            neighbours' idx (perim idx) []
        perim :: PieceIdx -> Set (HexPos,HexDir)
        perim idx =
            Set.fromList $ nubBy ((==)`on`fst) [ (pos', neg dir)
                | dir <- hexDirs
                , pos <- fullFootprint st idx
                , let pos' = dir +^ pos
                , Just True /= do
                    (idx',_) <- Map.lookup pos' board
                    return $ idx == idx'
                ]
        neighbours' :: PieceIdx -> Set (HexPos,HexDir) -> [PieceIdx] -> [PieceIdx]
        neighbours' idx as ns
            | Set.null as = ns
            | otherwise =
                let a = head $ Set.elems as
                    (path, ns') = march idx (fst a) a True
                in neighbours' idx
                    (Set.filter (\(pos,_) -> pos `notElem` path) as)
                    (ns++ns')
        -- |march around the piece's boundary, returning positions visited and
        -- neighbouring pieces met (in order)
        march idx startPos (pos,basedir) isInit
            | not isInit && pos == startPos = ([],[])
            | otherwise =
            let mn = do
                    (idx',_) <- Map.lookup pos board
                    guard $ idx' `elem` coloured
                    return idx'
                mNext = listToMaybe
                    [ (pos', rotate (h-2) basedir)
                    | h <- [1..5]
                    , let pos' = rotate h basedir+^pos
                    , (fst <$> Map.lookup pos' board) /= Just idx
                    ]
                (path,ns) = case mNext of
                    Nothing   -> ([],[])
                    Just next -> march idx startPos next False
            in (pos:path, maybeToList mn++ns)

