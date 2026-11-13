-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Frame where

import           Data.List      ((\\))
import qualified Data.Vector    as Vector

import           GameState
import           GameStateTypes
import           Hex

newtype Frame = BasicFrame Int
    deriving (Eq, Ord, Show, Read)

frameSize :: Frame -> Int
frameSize (BasicFrame size) = size

bolthole, entrance :: Frame -> HexVec
bolthole (BasicFrame size) = size*^hu +^ (size`div`2)*^hv
entrance f = neg $ bolthole f

boltWidth :: Frame -> Int
boltWidth (BasicFrame size) = size`div`4+1

baseState :: Frame -> GameState
baseState f =
    GameState
        (Vector.fromList $ [ framePiece f, bolt ] ++ initTools f)
        []
    where
        bolt = PlacedPiece (bolthole f +^ origin) $ Block $
            [ n*^hu | n <- [-1..boltWidth f - 1] ] -- ++ [(-2)*^hu+^neg hv]

framePiece :: Frame -> PlacedPiece
framePiece f@(BasicFrame size) =
    PlacedPiece origin . Block $
        map (bolthole f +^) (
            [ bw*^hu +^ n*^hv | n <- [0..bw] ]
            ++ [ bw*^hu +^ i*^hw +^ n*^hv | i <- [1..bw-1], n <- [0,bw+i] ])
        ++ map (entrance f +^) [neg hu +^ hv, 2 *^ neg hu, neg hu +^ hw,
                2 *^ hw, neg hv +^ hw]
        ++ (concat [
                [rotate r ((n *^ hu) +^ (size *^ hw)) | n <- [0 .. size - 1]] | r <- [0..5] ] \\
            [bolthole f, entrance f])
    where bw = boltWidth f

initTools :: Frame -> [PlacedPiece]
initTools f =
    [ PlacedPiece (entrance f +^ neg hu +^ origin) $ Wrench zero,
    PlacedPiece (entrance f +^ hw +^ origin) $ Hook (neg hw) NullHF ]

clearToolArea :: Frame -> GameState -> GameState
clearToolArea f st = foldr delPieceIn st $ toolsArea f


boltArea,toolsArea :: Frame -> [HexPos]
boltArea f = [PHS (bolthole f +^ bw *^ hu +^ i *^ hw +^ n *^ hv) |
   i <- [1 .. bw - 1], n <- [1 .. bw + i - 1]]
    where bw = boltWidth f
toolsArea f = [entrance f +^ v +^ origin | v <- [ neg hu, hw, zero ] ]

inBounds :: Frame -> HexPos -> Bool
inBounds f pos = hexLen (pos -^ origin) < frameSize f
inEditable :: Frame -> HexPos -> Bool
inEditable f pos = inBounds f pos || pos `elem` boltArea f ++ [PHS $ bolthole f] ++ toolsArea f
checkBounds :: Frame -> HexPos -> HexPos -> HexPos
checkBounds f def pos = if inBounds f pos then pos else def
checkEditable :: Frame -> HexPos -> HexPos -> HexPos
checkEditable f def pos = if inEditable f pos then pos else def
truncateToBounds,truncateToEditable :: Frame -> HexPos -> HexPos
truncateToBounds f (PHS v) = PHS $ truncateToLength (frameSize f - 1) v
truncateToEditable f pos@(PHS v) = if inBounds f pos
    then pos
    else head
        $ [ pos'
            | n <- reverse [0..boltWidth f]
            , let pos' = PHS $ truncateToLength (frameSize f - 1 + n) v
            , inEditable f pos' ]
