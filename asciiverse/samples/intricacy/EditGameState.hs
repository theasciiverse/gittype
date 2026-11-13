-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module EditGameState (modTile, mergeTiles) where

import           Control.Applicative
import           Control.Monad
import           Data.List
import qualified Data.Map            as Map
import           Data.Maybe

import           GameState
import           GameStateTypes
import           Hex
--import Debug

modTile :: Maybe Tile -> HexPos -> HexPos -> Bool -> GameState -> GameState
modTile tile pos lastPos painting st =
    let board = stateBoard st
        curOwnedTile = Map.lookup pos board
        (st',mowner) = case curOwnedTile of
            Nothing                     -> (st,Nothing)
            Just (owner,SpringTile _ _) -> (delConnectionsIn pos st, Just owner)
            Just (owner,_)              -> delPiecePos owner pos st -- XXX may invalidate board's indices to st
        board' = stateBoard st'
        addPiece p = addpp $ PlacedPiece pos p
        lastMOwner = do
            (o,_) <- Map.lookup lastPos board
            return o
        {-
        same = isJust $ do
                t <- tile
                (_,t') <- curOwnedTile
                guard $ ((==) `on` tileType) t t'
                return $ Just ()
        -}
        lastElem os = isJust $ do
               lastOwner <- lastMOwner
               guard $ lastOwner `elem` os
        lastWasDiff = isNothing $ do
               lastOwner <- lastMOwner
               owner <- mowner
               guard $ owner == lastOwner
        lastOK = painting || lastWasDiff
        validSpringRootTile ot = case snd ot of
            BlockTile _ -> True
            PivotTile _ -> True
            _           -> False
        -- |Find next adjacent, skipping over current entity.
        nextOfAdjacents adjs loop = listToMaybe $ fromMaybe adjs $ do
                owner <- mowner
                i <- elemIndex owner adjs
                return $ dropWhile (== owner) (drop i adjs) ++
                    if loop && i > 0 then adjs else []
    in case mowner of
         Just o | protectedPiece o -> st
         _ -> (case tile of
           -- _ | same && (pos /= lastPos) -> id
           Just (BlockTile _) ->
               let adjacentBlocks = nub [ idx |
                       dir <- hexDirs
                       , Just (idx, BlockTile _) <- [Map.lookup (dir +^ pos) board']
                       , not $ protectedPiece idx ]
                   addToIdx = if lastOK && lastElem adjacentBlocks
                       then lastMOwner
                       else nextOfAdjacents adjacentBlocks False
               in case addToIdx of
                  Nothing -> addPiece $ Block [zero]
                  Just b  -> addBlockPos b pos
           Just (ArmTile armdir _) ->
               let adjacentPivots = [ idx |
                       dir <- if armdir == zero then hexDirs else [armdir, neg armdir]
                       , Just (idx, PivotTile _) <- [Map.lookup (dir +^ pos) board'] ]
                   addToIdx = if lastOK && lastElem adjacentPivots
                       then lastMOwner
                       else nextOfAdjacents adjacentPivots True
               in case addToIdx of
                      Nothing -> id
                      Just p  -> addPivotArm p pos
           Just (SpringTile _ _) ->
               let possibleSprings = [ Connection root end $ Spring sdir natLen |
                       sdir <- hexDirs
                       , let epos = sdir +^ pos
                       , Just (eidx, BlockTile _) <- [Map.lookup epos board']
                       , not $ protectedPiece eidx
                       , (ridx, rpos) <- maybeToList $ castRay (neg sdir +^ pos) (neg sdir) board'
                       , Just True == (validSpringRootTile <$> Map.lookup rpos board')
                       , let natLen = hexLen (rpos -^ epos) - 1
                       , natLen > 0
                       {-
                       , null [ conn |
                           conn@(Connection _ _ (Spring sdir' _)) <-
                               springsAtIdx st' eidx ++ springsEndAtIdx st' ridx
                           , not $ sdir' `elem` [sdir,neg sdir] ]
                       -}
                       , not $ connGraphPathExists st' eidx ridx
                       , let end = (eidx, epos -^ placedPos (getpp st' eidx))
                       , let root = (ridx, rpos -^ placedPos (getpp st' ridx))
                       ]
                   nextSpring = listToMaybe $ fromMaybe possibleSprings $ do
                       (_,SpringTile _ _) <- curOwnedTile -- XXX: therefore the indices of st are still valid
                       i <- findIndex (`elem` connections st) possibleSprings
                       return $ drop (i+1) possibleSprings
               in maybe id addConn nextSpring
           Just (PivotTile _) -> addPiece $ Pivot []
           Just (WrenchTile _) -> addPiece $ Wrench zero
           Just HookTile -> let arm = listToMaybe [ dir |
                                    dir <- hexDirs
                                    , isNothing $ Map.lookup (dir +^ pos) board' ]
                            in case arm of Just armdir -> addPiece $ Hook armdir NullHF
                                           _ -> id
           Just BallTile -> addPiece Ball
           _ -> id
           ) st'

-- | merge tile/piece with a neighbouring piece. If we merge a piece with
-- connections, the connections are deleted: otherwise we'd need some fiddly
-- conditions to ensure connection graph acyclicity.
mergeTiles :: HexPos -> HexDir -> Bool -> GameState -> GameState
mergeTiles pos dir mergePiece st = fromMaybe st $ do
    let board = stateBoard st
    (idx,tile) <- Map.lookup pos board
    (idx',tile') <- Map.lookup (dir+^pos) board
    guard $ idx /= idx'
    guard $ not (any protectedPiece [idx,idx'])
    case tile of
        BlockTile _ -> do
            BlockTile _ <- Just tile'
            let st' = if mergePiece
                          then delPiece idx st
                          else fst $ delPiecePos idx pos st
            (idx'',_) <- Map.lookup (dir+^pos) $ stateBoard st'
            return $ if mergePiece
                         then foldr (addBlockPos idx'') st'
                                $ plPieceFootprint $ getpp st idx
                         else addBlockPos idx'' pos st'
        ArmTile _ _  -> do
            PivotTile _ <- Just tile'
            let st' = fst $ delPiecePos idx pos st
            (idx'',_) <- Map.lookup (dir+^pos) $ stateBoard st'
            return $ addPivotArm idx'' pos st'
        _ -> mzero
