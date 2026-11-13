-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Command where
import           GameStateTypes
import           Hex
import           Metagame

data Command
    = CmdDir WrHoSel HexDir | CmdRotate WrHoSel TorqueDir | CmdWait
    | CmdMoveTo HexPos | CmdManipulateToolAt HexPos
    | CmdDrag HexPos HexDir
    | CmdToggle
    | CmdOpen
    | CmdTile Tile | CmdPaint (Maybe Tile)
    | CmdPaintFromTo (Maybe Tile) HexPos HexPos
    | CmdSelect | CmdUnselect
    | CmdDelete | CmdMerge
    | CmdMark | CmdJumpMark | CmdReset
    | CmdPlay | CmdTest
    | CmdUndo | CmdRedo
    | CmdReplayForward Int | CmdReplayBack Int
    | CmdInputChar Char
    | CmdInputSelLock LockIndex
    | CmdInputCodename Codename
    | CmdInputSelUndecl Undeclared
    | CmdWriteState
    | CmdInitiation
    | CmdShowRetired | CmdPlayLockSpec (Maybe LockSpec)
    | CmdSetServer | CmdToggleCacheOnly
    | CmdSelCodename (Maybe Codename) | CmdBackCodename | CmdHome
    | CmdSolveInit (Maybe HexVec)
    | CmdSolve (Maybe LockIndex) | CmdDeclare (Maybe Undeclared)
    | CmdViewSolution (Maybe NoteInfo)
    | CmdSelectLock | CmdNextLock | CmdPrevLock | CmdDeleteLock
    | CmdEdit | CmdPlaceLock (Maybe LockIndex) | CmdRetireLock
    | CmdRegister | CmdAuth
    | CmdNextPage | CmdPrevPage
    | CmdToggleColourMode
    | CmdRedraw | CmdRefresh | CmdSuspend | CmdClear
    | CmdQuit | CmdForceQuit
    | CmdHelp | CmdBind (Maybe Command)
    | CmdNone
    deriving (Eq, Ord, Show, Read)
data WrHoSel = WHSWrench | WHSHook | WHSSelected
    deriving (Eq, Ord, Show, Read)

describeCommand :: Command -> String
describeCommand (CmdDir whs dir) = "Move " ++ whsStr whs ++ " " ++ dirStr dir
describeCommand (CmdRotate whs dir) = "Rotate " ++ whsStr whs
        ++ " " ++ (if dir == 1 then "counter" else "") ++ "clockwise"
describeCommand CmdWait = "Wait"
describeCommand CmdToggle = "Toggle tool"
describeCommand CmdOpen = "Open lock"
describeCommand (CmdTile tile) = tileStr tile
describeCommand CmdMerge = "Merge with adjacent piece"
describeCommand CmdMark = "Mark state"
describeCommand CmdJumpMark = "Jump to marked state"
describeCommand CmdReset = "Jump to initial state"
describeCommand CmdSelect = "Select piece"
describeCommand CmdUnselect = "Unselect piece"
describeCommand CmdDelete = "Delete piece"
describeCommand CmdPlay = "Play lock"
describeCommand CmdTest = "Test lock"
describeCommand CmdUndo = "Undo"
describeCommand CmdRedo = "Redo"
describeCommand (CmdReplayForward _) = "Advance replay"
describeCommand (CmdReplayBack _) = "Rewind replay"
describeCommand CmdWriteState = "Write lock"
describeCommand CmdInitiation = "Revisit initiation"
describeCommand CmdShowRetired = "Toggle showing retired locks"
describeCommand CmdSetServer = "Set server"
describeCommand CmdToggleCacheOnly = "Toggle offline mode"
describeCommand (CmdSelCodename mname) = "Select player"
        ++ maybe "" (' ':) mname
describeCommand CmdBackCodename = "Select last player"
describeCommand CmdHome = "Select self"
describeCommand (CmdSolveInit _) = "Solve lock"
describeCommand (CmdSolve mli) = "Solve lock"
        ++ maybe "" ((' ':).(:"").lockIndexChar) mli
describeCommand (CmdPlayLockSpec mls) = "find lock by number"
        ++ maybe "" ((' ':).show) mls
describeCommand (CmdDeclare mundecl) = "Declare solution"
        ++ maybe "" (const " [specified solution]") mundecl
describeCommand (CmdViewSolution mnote) = "View lock solution"
        ++ maybe "" (const " [specified solution]") mnote
describeCommand CmdSelectLock = "Choose lock by name"
describeCommand CmdNextLock = "Next lock"
describeCommand CmdPrevLock = "Previous lock"
describeCommand CmdDeleteLock = "Delete current lock"
describeCommand CmdNextPage = "Page forward through lists"
describeCommand CmdPrevPage = "Page back through lists"
describeCommand CmdEdit = "Edit lock"
describeCommand (CmdPlaceLock mli) = "Place lock"
        ++ maybe "" ((' ':).(:"").lockIndexChar) mli
describeCommand CmdRetireLock = "Retire lock"
describeCommand CmdRegister = "Register codename"
describeCommand CmdAuth = "Authenticate"
describeCommand (CmdBind _) = "Bind key"
describeCommand CmdToggleColourMode = "Toggle lock colour mode"
describeCommand CmdQuit = "Quit"
describeCommand CmdHelp = "Help"
describeCommand _ = ""

tileStr HookTile         = "hook"
tileStr (WrenchTile _)   = "wrench"
tileStr (ArmTile _ _)    = "arm"
tileStr (PivotTile _)    = "pivot"
tileStr (SpringTile _ _) = "spring"
tileStr (BlockTile _)    = "block"
tileStr BallTile         = "ball"
whsStr WHSWrench   = "wrench"
whsStr WHSHook     = "hook"
whsStr WHSSelected = "selected"
dirStr v
    | v == hu = "right"
    | v == neg hu = "left"
    | v == hv = "up-left"
    | v == neg hv = "down-right"
    | v == hw = "down-left"
    | v == neg hw = "up-right"
dirStr _ = ""
