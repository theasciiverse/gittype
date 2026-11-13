-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module SDL2Glyph where

import           Command
import           GameStateTypes
import           Hex
import           SDL.Primitive  (Color)

data ShowBlocks = ShowBlocksBlocking | ShowBlocksAll | ShowBlocksNone
    deriving (Eq, Ord, Show, Read)

data Glyph
    = TileGlyph Tile Color
    | SpringGlyph HexDir HexDir SpringExtension HexDir Color
    | PivotGlyph TorqueDir HexDir Color
    | ArmGlyph TorqueDir HexDir Color
    | BlockedArm HexDir TorqueDir Color
    | TurnedArm HexDir TorqueDir Color
    | BlockedBlock Tile HexDir Color
    | BlockedPush HexDir Color
    | CollisionMarker
    | HollowGlyph Color
    | HollowInnerGlyph Color
    | FilledHexGlyph Color
    | ScoreGlyph (Maybe Int)
    | ButtonGlyph Color
    | UnboundButtonGlyph
    | PathGlyph HexDir Color
    | GateGlyph HexDir Color
    | UseFiveColourButton Bool
    | ShowBlocksButton ShowBlocks
    | ShowButtonTextButton Bool
    | VolumeButton Int
    | WhsButtonsButton (Maybe WrHoSel)
    | WhsEditButtonsButton (Maybe WrHoSel)
    | FullscreenButton Bool
    | DisplacedGlyph HexDir Glyph
    | UnfreshGlyph
    deriving (Eq, Ord, Show)
