-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Lock where
import           Control.Monad.Writer
import qualified Data.Map             as Map
import           Data.Maybe

import           Frame
import           GameState
import           GameStateTypes
import           Hex
import           Physics
import           Util

type Lock = (Frame, GameState)
liftLock :: (GameState -> GameState) -> (Lock -> Lock)
liftLock g (f,st) = (f,g st)

lockSize (f,_) = frameSize f

baseLock :: Int -> Lock
baseLock size =
    let frame = BasicFrame size
    in (frame, baseState frame)

deframe :: Lock -> Lock
deframe = delTools . liftLock (setpp 0 nullpp)
nullpp = PlacedPiece (PHS zero) (Block [])

reframe :: Lock -> Lock
reframe l@(f, _) = addTools $ delTools $ liftLock (setpp 0 (framePiece f)) l

validLock :: Lock -> Bool
validLock lock@(_, st) = (st == stepPhysics st) && (lock == reframe lock) && validGameState st

type Solution = [PlayerMove]

checkSolution :: Lock -> Solution -> Bool
checkSolution lock pms =
    let (frame,st) = reframe lock
        tick :: GameState -> PlayerMove -> GameState
        tick st' pm = fst . runWriter $ physicsTick pm st'
    in any (\st' -> checkSolved (frame,st')) $ scanl tick st pms

checkSolved :: Lock -> Bool
checkSolved (f,st) =
    and [ isNothing $ Map.lookup p (stateBoard st) | p <- boltArea f ]


canonify :: Lock -> Lock
canonify = addTools . stabilise . delTools . delOOB
delTools :: Lock -> Lock
delTools = liftLock delTools' where
        delTools' :: GameState -> GameState
        delTools' st =
            fromMaybe st $ listToMaybe
                [ delTools' $ delPiece idx st
                | (idx,pp) <- enumVec $ placedPieces st
                , isTool $ placedPiece pp ]
addTools :: Lock -> Lock
addTools (f,st) =
    let st' = clearToolArea f st
    in (f, foldr addpp st' $ initTools f)

-- |An important property of the game physics is that any state stabilises in
-- finite time. Proof: in any spontaneous state change some spring gets closer
-- to being of natural length, and none get further from it.
stabilise :: Lock -> Lock
stabilise = liftLock stabilise' where
        stabilise' :: GameState -> GameState
        stabilise' st =
            let st' = stepPhysics st
            in if st == st' then st else stabilise' st'

delOOB :: Lock -> Lock
delOOB l@(f,st) =
    fromMaybe l $ listToMaybe
        [ delOOB $ liftLock (delPiece idx) l
        | (idx,_) <- enumVec $ placedPieces st
        , not $ isFrame idx
        , not (any (inBounds f) (fullFootprint st idx))
        , null $ springsEndAtIdx st idx]
