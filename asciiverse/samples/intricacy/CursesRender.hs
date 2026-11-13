-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module CursesRender where

import           Data.Char          (ord)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import qualified UI.HSCurses.Curses as Curses

import           AsciiLock
import           BoardColouring     (PieceColouring)
import           CVec
import           GameStateTypes
import           Hex

-- From Curses.CursesHelper:
-- | Converts a list of 'Curses.Color' pairs (foreground color and
--   background color) into the curses representation 'Curses.Pair'.
colorsToPairs :: [(Curses.Color, Curses.Color)] -> IO [Curses.Pair]
colorsToPairs cs = do
    p <- Curses.colorPairs
    let nColors = length cs
        blackWhite = p < nColors
    if blackWhite then do
        print ("Terminal does not support enough colors. Number of " ++
                  " colors requested: " ++ show nColors ++
                  ". Number of colors supported: " ++ show p)
        return $ replicate nColors $ Curses.Pair 0
    else mapM toPairs (zip [1..] cs)
    where toPairs (n, (fg, bg)) = do
            let p = Curses.Pair n
            Curses.initPair p fg bg
            return p

type AttrChar = (Char, Curses.Attr)
type ColPair = Int
white,red,green,yellow,blue,magenta,cyan :: ColPair
white = 0
red = 1
green = 2
yellow = 3
blue = 4
magenta = 5
cyan = 6
data Glyph = Glyph Char ColPair Curses.Attr
a0 = Curses.attr0
bold = Curses.setBold a0 True

tileChar :: Tile -> AttrChar
tileChar (BlockTile _) = ('#',a0)
tileChar (PivotTile dir)
    | dir == zero = ('o',bold)
    | canonDir dir == hu = ('-',bold)
    | canonDir dir == hv = ('\\',bold)
    | canonDir dir == hw = ('/',bold)
tileChar (ArmTile dir principal) =
    let cdir = canonDir dir
        c | cdir == hu = '-'
          | cdir == hv = '\\'
          | cdir == hw = '/'
          | otherwise = '-'
        a = if principal then bold else a0
    in (c,a)
tileChar HookTile = ('@',bold)
tileChar (WrenchTile mom) = ('*',if mom /= zero then bold else a0)
tileChar BallTile = ('O',a0)
tileChar (SpringTile Relaxed _) = ('S',a0)
tileChar (SpringTile Compressed _) = ('$',bold)
tileChar (SpringTile Stretched _) = ('s',bold)
tileChar _ = ('?',bold)

ownedTileGlyph :: Bool -> PieceColouring -> [PieceIdx] -> OwnedTile -> Glyph
ownedTileGlyph mono@True colouring reversed ot =
    Glyph (monochromeOTileChar' colouring ot) white a0
    where
    -- |add fifth colour, to differentiate from lock frame;
    -- adding this to the asciilock format would break back-compatibility.
    monochromeOTileChar' colouring (idx,BlockTile _) =
        case Map.lookup idx colouring of
            Just 0 -> ';'
            Just 1 -> '%'
            Just 2 -> '"'
            Just 3 -> '&'
            Just 4 -> '~'
            _      -> '#'
    monochromeOTileChar' colouring ot = monochromeOTileChar colouring ot
ownedTileGlyph mono@False colouring reversed (owner,t) =
    let (ch,attr) = tileChar t
        pair = case Map.lookup owner colouring of
                    Nothing -> 0
                    Just 3  -> cyan  -- replace blue with cyan for visibility
                    Just n  -> n+1
        rev = owner `elem` reversed
    in Glyph ch pair (Curses.setReverse attr rev)

addCh :: Char ->  IO ()
addCh c = Curses.wAddStr Curses.stdScr [c]
mvAddCh :: CVec -> Char ->  IO ()
mvAddCh (CVec y x) c = Curses.mvAddCh y x $ fromIntegral $ ord c
mvAddStr :: CVec -> String -> IO ()
mvAddStr (CVec y x) = Curses.mvWAddStr Curses.stdScr y x
mvAddGlyph :: [Curses.Pair] -> CVec -> Glyph -> IO ()
mvAddGlyph cpairs v (Glyph ch col attr) =
    Curses.attrSet attr (cpairs!!col) >> mvAddCh v ch
move :: CVec -> IO ()
move (CVec y x) = Curses.move y x
clearLine :: Int -> IO ()
clearLine y = Curses.move y 0 >> Curses.clrToEol
