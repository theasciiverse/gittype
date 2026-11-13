-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

{-# LANGUAGE CPP #-}

module KeyBindings (KeyBindings, bindings, findBindings, findBinding, showKey,
    showKeyChar, showKeyFriendly, showKeyFriendlyShort) where

import           Data.Bits      (xor)
import           Data.Char
import           Data.List

import           Command
import           GameStateTypes
import           Hex
import           InputMode

type KeyBindings = [ (Char,Command) ]

ctrl, unctrl, meta, unmeta :: Char -> Char
ctrl = toEnum . xor 64 . fromEnum
meta = toEnum . xor 128 . fromEnum
unctrl = ctrl
unmeta = meta

lowerToo :: KeyBindings -> KeyBindings
lowerToo = concatMap addLower
    where addLower b@(c, cmd) = [ b, (toLower c, cmd) ]

qwertyWASDish =
    [ ('e', CmdDir WHSSelected hu)
    , ('a', CmdDir WHSSelected $ neg hu)
    , ('s', CmdDir WHSSelected hw)
    , ('w', CmdDir WHSSelected $ neg hw)
    , ('q', CmdDir WHSSelected hv)
    , ('d', CmdDir WHSSelected $ neg hv)
    , ('c', CmdRotate WHSSelected 1)
    , ('z', CmdRotate WHSSelected $ -1)
    ]

keypadHex =
    [ ('5', CmdWait)
    , ('6', CmdDir WHSSelected hu)
    , ('4', CmdDir WHSSelected $ neg hu)
    , ('1', CmdDir WHSSelected hw)
    , ('9', CmdDir WHSSelected $ neg hw)
    , ('7', CmdDir WHSSelected hv)
    , ('3', CmdDir WHSSelected $ neg hv)
    , ('8', CmdRotate WHSSelected 1)
    , ('2', CmdRotate WHSSelected $ -1)
    ]

selectedMovement = qwertyWASDish ++ keypadHex

qwertyViHex =
    [ ('l', CmdDir WHSHook hu)
    , ('h', CmdDir WHSHook $ neg hu)
    , ('b', CmdDir WHSHook hw)
    , ('u', CmdDir WHSHook $ neg hw)
    , ('y', CmdDir WHSHook hv)
    , ('n', CmdDir WHSHook $ neg hv)
    , ('L', CmdDir WHSWrench hu)
    , ('H', CmdDir WHSWrench $ neg hu)
    , ('B', CmdDir WHSWrench hw)
    , ('U', CmdDir WHSWrench $ neg hw)
    , ('Y', CmdDir WHSWrench hv)
    , ('N', CmdDir WHSWrench $ neg hv)
    , ('k', CmdRotate WHSHook 1)
    , ('j', CmdRotate WHSHook $ -1)
    ]

{-
qwertyLeftHex =
    [ ('d', CmdDir WHSHook hu)
    , ('a', CmdDir WHSHook $ neg hu)
    , ('z', CmdDir WHSHook hw)
    , ('e', CmdDir WHSHook $ neg hw)
    , ('w', CmdDir WHSHook hv)
    , ('x', CmdDir WHSHook $ neg hv)
    , ('D', CmdDir WHSWrench hu)
    , ('A', CmdDir WHSWrench $ neg hu)
    , ('Z', CmdDir WHSWrench hw)
    , ('E', CmdDir WHSWrench $ neg hw)
    , ('W', CmdDir WHSWrench hv)
    , ('X', CmdDir WHSWrench $ neg hv)
    , ('q', CmdRotate WHSHook 1)
    , ('c', CmdRotate WHSHook $ -1)
    , ('S', CmdWait)
    , ('s', CmdWait)
    ]
dvorakMidHex =
    [ ('i', CmdDir WHSWrench hu)
    , ('e', CmdDir WHSWrench $ neg hu)
    , ('j', CmdDir WHSWrench hw)
    , ('y', CmdDir WHSWrench $ neg hw)
    , ('p', CmdDir WHSWrench hv)
    , ('k', CmdDir WHSWrench $ neg hv)
    ]
-}

miscLockGlobal = lowerToo
    [ ('\b', CmdUndo)
    , ('\r', CmdRedo)
    , ('\n', CmdRedo)
    , (ctrl 'R', CmdRedo)
    , (ctrl 'Y', CmdRedo)
#ifndef CURSES
    , (ctrl 'Z', CmdUndo)
#endif
    , ('R', CmdReset)
    , ('.', CmdWait)
    , ('M', CmdMark)
    , ('\'', CmdJumpMark)
    ] <>
    [ ('X', CmdUndo)
    ]

miscGlobal =
    [ ('\ESC', CmdQuit)
    , ('Q', CmdQuit)
    , (ctrl 'Q', CmdQuit)
    , (ctrl 'C', CmdQuit)
    , ('?', CmdHelp)
    , (ctrl 'B', CmdBind Nothing)
    , (ctrl 'L', CmdRedraw)
#ifdef CURSES
    , (ctrl 'Z', CmdSuspend)
#endif
    , ('%', CmdToggleColourMode)
    ]

playOnly = lowerToo
    [ ('O', CmdOpen)
    , (' ', CmdWait)
    , ('x', CmdToggle)
    , ('\t', CmdToggle)
    , ('*', CmdTile $ WrenchTile zero)
    , ('/', CmdTile HookTile)
    , ('@', CmdTile HookTile)
    ]
replayOnly =
    [ (' ', CmdReplayForward 1)
    ]
editMisc = lowerToo
    [ ('P', CmdPlay)
    , ('T', CmdTest)
    ] <>
    [ ('W', CmdWriteState)
    , (ctrl 'S', CmdWriteState)
    , (' ', CmdSelect)
    , ('=', CmdMerge)
    , ('0', CmdDelete)
    ]
tilesPaintRow = lowerToo
    [ ('H', CmdTile BallTile)
    , ('B', CmdTile $ ArmTile zero False)
    , ('G', CmdTile $ PivotTile zero)
    , ('V', CmdTile $ SpringTile Relaxed zero)
    , ('F', CmdTile $ BlockTile [])
    , ('x', CmdDelete)
    ]
tilesAscii =
    [ ('o', CmdTile $ PivotTile zero)
    , ('O', CmdTile BallTile)
    , ('S', CmdTile $ SpringTile Relaxed zero)
    , ('$', CmdTile $ SpringTile Relaxed zero)
    --, ('s', CmdTile $ SpringTile Stretched zero)
    --, ('$', CmdTile $ SpringTile Compressed zero)
    , ('-', CmdTile $ ArmTile zero False)
    --, ('-', CmdTile $ ArmTile hu False)
    , ('\\', CmdTile $ ArmTile hv False)
    , ('/', CmdTile $ ArmTile hw False)
    , ('@', CmdTile HookTile)
    , ('*', CmdTile $ WrenchTile zero)
    , ('#', CmdTile $ BlockTile [])
    ]
editOnly = tilesPaintRow ++ editMisc ++ tilesAscii

playBindings = playOnly ++ selectedMovement ++ qwertyViHex ++ miscGlobal ++ miscLockGlobal
replayBindings = replayOnly ++ miscGlobal ++ miscLockGlobal
editBindings = editOnly ++ selectedMovement ++ miscGlobal ++ miscLockGlobal
initBindings = lowerToo [ ('S', CmdSolveInit Nothing) ] ++ miscGlobal
metaBindings = lowerToo
    [ ('C', CmdSelCodename Nothing)
    , ('H', CmdHome)
    , ('B', CmdBackCodename)
    , ('S', CmdSolve Nothing)
    , ('D', CmdDeclare Nothing)
    , ('V', CmdViewSolution Nothing)
    , ('R', CmdRegister)
    , ('P', CmdPlaceLock Nothing)
    , (ctrl 'R', CmdRetireLock)
    , ('E', CmdEdit)
    , ('L', CmdSelectLock)
    , ('O', CmdPrevLock)
    , ('N', CmdNextLock)
    , ('A', CmdAuth)
    , ('I', CmdInitiation)
    , ('+', CmdShowRetired)
    , ('#', CmdPlayLockSpec Nothing)
    , ('$', CmdSetServer)
    , ('^', CmdToggleCacheOnly)
    , ('>', CmdNextPage)
    , ('<', CmdPrevPage)
    , (ctrl 'D', CmdDeleteLock)
    ] ++ miscGlobal

impatienceBindings = lowerToo
    [ ('Q', CmdQuit)
    , (ctrl 'C', CmdQuit) ]

bindings :: InputMode -> KeyBindings
bindings IMEdit       = editBindings
bindings IMPlay       = playBindings
bindings IMInit       = initBindings
bindings IMMeta       = metaBindings
bindings IMReplay     = replayBindings
bindings IMImpatience = impatienceBindings
bindings _            = []

findBindings :: KeyBindings -> Command -> [Char]
findBindings bdgs cmd = nub
    $ [ ch | (ch,cmd') <- bdgs, cmd'==cmd ]
    ++ [ ch | CmdInputChar ch <- [cmd] ]

findBinding :: KeyBindings -> Command -> Maybe Char
findBinding _ (CmdInputChar ch) = Just ch
findBinding bdgs cmd = find (\ch -> lookup ch bdgs == Just cmd) $ findBindings bdgs cmd

showKey :: Char -> String
showKey ch
    | isAscii (unmeta ch) = 'M':'-':showKey (unmeta ch)
    | isPrint ch = [ch]
    | isPrint (unctrl ch) = '^':[unctrl ch]
    | otherwise = "[?]"

showKeyFriendly ' '    = "space"
showKeyFriendly '\r'   = "return"
showKeyFriendly '\n'   = "newline"
showKeyFriendly '\t'   = "tab"
showKeyFriendly '\b'   = "backspace"
showKeyFriendly '\ESC' = "escape"
showKeyFriendly ch     = showKey ch

showKeyFriendlyShort '\r'   = "ret"
showKeyFriendlyShort '\t'   = "tab"
showKeyFriendlyShort '\b'   = "bsp"
showKeyFriendlyShort '\ESC' = "esc"
showKeyFriendlyShort ch     = showKey ch

showKeyChar :: Char -> Char
showKeyChar ch
    | isAscii (unmeta ch) = '['
    | isPrint ch = ch
    | isPrint (unctrl ch) = '^'
    | otherwise = '?'
