-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module GameStateTypes where

import           Data.Map    (Map)
import           Data.Vector (Vector)
import           Hex

data GameState = GameState { placedPieces :: Vector PlacedPiece,
        connections                       :: [Connection] }
    deriving (Eq, Ord, Show, Read)

data PlacedPiece = PlacedPiece { placedPos :: HexPos, placedPiece :: Piece }
    deriving (Eq, Ord, Show, Read)

type PieceIdx = Int

data Piece = Block { blockPattern :: [HexVec] }
            | Pivot { pivotArms :: [HexDir] }
            | Hook { hookArm :: HexDir, hookForce :: HookForce}
            | Wrench { wrenchMomentum :: HexDir }
            | Ball
    deriving (Eq, Ord, Show, Read)

data HookForce = NullHF | PushHF HexDir | TorqueHF Int
    deriving (Eq, Ord, Show, Read)

isBlock, isPivot, isHook, isWrench, isTool, isBall :: Piece -> Bool
isBlock p = case p of Block _ -> True; _ -> False
isPivot p = case p of Pivot _ -> True; _ -> False
isHook p = case p of Hook _ _ -> True; _ -> False
isWrench p = case p of Wrench _ -> True; _ -> False
isTool p = isWrench p || isHook p
isBall p = case p of Ball -> True; _ -> False

data Connection = Connection { connectionRoot :: Locus
                    , connectionEnd :: Locus, connectionLink :: Link }
    deriving (Eq, Ord, Show, Read)

type Locus = (PieceIdx, HexVec)

data Link = Free { freePos :: HexVec }
          | Spring { springDir :: HexDir, springNatLength :: Int }
    deriving (Eq, Ord, Show, Read)

data SpringExtension = Relaxed | Compressed | Stretched
    deriving (Eq, Ord, Show, Read)

data Tile = BlockTile [HexDir] | PivotTile HexDir | ArmTile HexDir Bool | HookTile | WrenchTile HexDir
          | BallTile | SpringTile SpringExtension HexDir
    deriving (Eq, Ord, Show, Read)

tileType :: Tile -> Tile
tileType (BlockTile _)    = BlockTile []
tileType (PivotTile _)    = PivotTile zero
tileType (ArmTile _ _)    = ArmTile zero False
tileType (WrenchTile _)   = WrenchTile zero
tileType BallTile         = BallTile
tileType (SpringTile _ _) = SpringTile Relaxed zero
tileType t                = t

type OwnedTile = (PieceIdx, Tile)
type GameBoard = Map HexPos OwnedTile

-- |TorqueDir: Int of absolute value <= 1
type TorqueDir = Int

-- |'force' encompasses both usual directional forces and torques; we use
-- 'push' for the former.
data Force = Push PieceIdx HexDir | Torque PieceIdx TorqueDir
    deriving (Eq, Ord, Show)

-- |Alert: for passing information about physics processing to the UI
data Alert = AlertCollision HexPos | AlertBlockingForce Force
           | AlertResistedForce Force | AlertBlockedForce Force
           | AlertAppliedForce Force | AlertDivertedWrench PieceIdx
           | AlertUnlocked
           | AlertIntermediateState GameState
    deriving (Eq, Ord, Show)
