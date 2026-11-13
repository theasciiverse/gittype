-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

{-# LANGUAGE TupleSections #-}

module AsciiLock (lockToAscii, lockOfAscii, stateToAscii
    , readAsciiLockFile, writeAsciiLockFile, monochromeOTileChar) where

import           Control.Applicative
import           Control.Arrow       ((&&&))
import           Control.Monad
import           Data.Char           (toUpper)
import           Data.Function       (on)
import           Data.List
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Traversable    as T
import qualified Data.Vector         as Vector
import           Safe                (maximumBound)

import           BoardColouring
import           CVec
import           Frame
import           GameState
import           GameStateTypes
import           Hex
import           Lock
import           Mundanities
import           Physics
import           Util


type AsciiLock = [String]

lockToAscii :: Lock -> AsciiLock
lockToAscii = stateToAscii . snd

stateToAscii :: GameState -> AsciiLock
stateToAscii st =
    let colouring = boardColouring st (ppidxs st) Map.empty
    in boardToAscii colouring . stateBoard $ st

lockOfAscii :: AsciiLock -> Maybe Lock
lockOfAscii ls = do
    board <- asciiToBoard ls
    let size = maximumBound 0 $ hx . (-^origin) <$> Map.keys board
        frame = BasicFrame size
    guard $ size > 0
    st <- asciiBoardState frame board
    return (frame, st)

boardToAscii :: PieceColouring -> GameBoard -> AsciiLock
boardToAscii colouring board =
    let asciiBoard :: Map CVec Char
        asciiBoard = Map.mapKeys (hexVec2CVec . (-^origin))
            $ monochromeOTileChar colouring <$> board
        (miny,maxy) = minmax $ cy <$> Map.keys asciiBoard
        (minx,maxx) = minmax $ cx <$> Map.keys asciiBoard
        asciiBoard' = Map.mapKeys (-^CVec miny minx) asciiBoard
    in [ [ Map.findWithDefault ' ' (CVec y x) asciiBoard'
            | x <- [0..(maxx-minx)] ]
        | y <- [0..(maxy-miny)] ]

asciiToBoard :: AsciiLock -> Maybe GameBoard
asciiToBoard ls =
    let asciiBoard :: Map CVec Char
        asciiBoard = Map.fromList [(CVec y x,ch)
            | (line,y) <- zip ls [0..]
            , (ch,x) <- zip line [0..]
            , ch `notElem` "\t\r\n "]
        (miny,maxy) = minmax $ cy <$> Map.keys asciiBoard
        midy = miny+(maxy-miny)`div`2
        midline = filter ((==midy).cy) $ Map.keys asciiBoard
        (minx,maxx) = minmax $ cx <$> midline
        centre = CVec midy (minx+(maxx-minx)`div`2)
    in Map.mapKeys ((+^origin) . cVec2HexVec . (-^centre))
        <$> T.mapM monoToOTile asciiBoard

asciiBoardState :: Frame -> GameBoard -> Maybe GameState
asciiBoardState frame board =
    let addPreBase st = foldr addpp st (replicate 6 $ PlacedPiece origin $ Block [])
        addBase st = foldr addBaseOT st $ Map.toList $
            Map.filter (isBaseTile.snd) board
        isBaseTile (BlockTile _)  = True
        isBaseTile (PivotTile _)  = True
        isBaseTile HookTile       = True
        isBaseTile (WrenchTile _) = True
        isBaseTile BallTile       = True
        isBaseTile _              = False
        addBaseOT :: (HexPos,(PieceIdx,Tile)) -> GameState -> GameState
        addBaseOT (pos,(o,BlockTile [])) = addBlockPos o pos
        addBaseOT (pos,(-1,t)) = addpp $ PlacedPiece pos $ basePieceOfTile t
        addBaseOT _ = error "owned non-block tile in AsciiLock.asciiBoardState"
        basePieceOfTile (PivotTile _) = Pivot []
        basePieceOfTile HookTile = Hook hu NullHF
        basePieceOfTile (WrenchTile _) = Wrench zero
        basePieceOfTile BallTile = Ball
        basePieceOfTile _ = error "Unexpected tile in AsciiLock.asciiBoardState"
        componentifyNew st = foldr ((fst.).componentify) st $ filter (/=0) $ ppidxs st
        -- | we assume that the largest wholly out-of-bounds block is the frame
        setFrame st = fromMaybe st $ do
            (idx,pp) <- listToMaybe $ fst <$> sortBy (flip compare `on` snd)
                    [ ((idx,pp),length vs)
                    | (idx,pp) <- enumVec $ placedPieces st
                    , let fp = plPieceFootprint pp
                    , not $ null fp
                    , not $ any (inBounds frame) fp
                    , Block vs <- [placedPiece pp]
                    ]
            return $ delPiece idx $ setpp 0 pp st
        baseSt = setFrame . componentifyNew . addBase . addPreBase $ GameState Vector.empty []

        baseBoard = stateBoard baseSt
        addAppendages :: GameState -> Maybe GameState
        addAppendages st = foldM addAppendageOT st $ Map.toList $
            Map.filter (not.isBaseTile.snd) board
        addAppendageOT st (pos,(-1,ArmTile dir _)) =
            let rpos = (neg dir+^pos)
            in case Map.lookup rpos baseBoard of
                Just (idx,PivotTile _) -> Just $ addPivotArm idx pos st
                Just (idx,HookTile) -> Just $ setpp idx (PlacedPiece rpos (Hook dir NullHF)) st
                _ -> Nothing
        addAppendageOT st (pos,(-1,SpringTile _ dir)) =
            let rpos = (neg dir+^pos)
            in case Map.lookup rpos baseBoard of
                Just (_,SpringTile _ _) -> Just st
                Just _ -> do
                    (_,epos) <- castRay pos dir baseBoard
                    let twiceNatLen = sum [ extnValue extn
                            | i <- [1..hexLen (epos-^rpos)-1]
                            , let pos' = i*^dir+^rpos
                            , Just (_,SpringTile extn _) <- [ Map.lookup pos' board ] ]
                        extnValue Compressed = 4
                        extnValue Relaxed    = 2
                        extnValue Stretched  = 1
                        Just root = posLocus baseSt rpos
                        Just end = posLocus baseSt epos
                    Just $ flip addConn st $ Connection root end $ Spring dir $ twiceNatLen`div`2
                _ -> Just st
        addAppendageOT _ _ = Nothing
    in addAppendages baseSt

monochromeOTileChar :: PieceColouring -> OwnedTile -> Char
monochromeOTileChar colouring (idx,BlockTile _) =
    case Map.lookup idx colouring of
        Just 1 -> '%'
        Just 2 -> '"'
        Just 3 -> '&'
        Just 4 -> '~'
        _      -> '#'
monochromeOTileChar _ (_,t) = monochromeTileChar t
monochromeTileChar :: Tile -> Char
monochromeTileChar (PivotTile _) = 'o'
monochromeTileChar (ArmTile dir _)
    | dir == hu = '-'
    | dir == hv = '\\'
    | dir == hw = '/'
    | dir == neg hu = '.'
    | dir == neg hv = '`'
    | dir == neg hw = '\''
monochromeTileChar HookTile = '@'
monochromeTileChar (WrenchTile _) = '*'
monochromeTileChar BallTile = 'O'
monochromeTileChar (SpringTile extn dir)
    | dir == hu = case extn of
        Stretched  -> 's'
        Relaxed    -> 'S'
        Compressed -> '$'
    | dir == hv = case extn of
        Stretched  -> 'z'
        Relaxed    -> 'Z'
        Compressed -> '5'
    | dir == hw = case extn of
        Stretched  -> '('
        Relaxed    -> '['
        Compressed -> '{'
    | dir == neg hu = case extn of
        Stretched  -> 'c'
        Relaxed    -> 'C'
        Compressed -> 'D'
    | dir == neg hv = case extn of
        Stretched  -> ')'
        Relaxed    -> ']'
        Compressed -> '}'
    | dir == neg hw = case extn of
        Stretched  -> '1'
        Relaxed    -> '7'
        Compressed -> '9'
monochromeTileChar _ = '?'
monoToOTile :: Char -> Maybe OwnedTile
monoToOTile '#' = Just (1,BlockTile [])
monoToOTile '%' = Just (2,BlockTile [])
monoToOTile '"' = Just (3,BlockTile [])
monoToOTile '&' = Just (4,BlockTile [])
monoToOTile '~' = Just (5,BlockTile [])
monoToOTile ch  = (-1,) <$> monoToTile ch
monoToTile :: Char -> Maybe Tile
monoToTile 'o'  = Just $ PivotTile zero
monoToTile '-'  = Just $ ArmTile hu False
monoToTile '\\' = Just $ ArmTile hv False
monoToTile '/'  = Just $ ArmTile hw False
monoToTile '.'  = Just $ ArmTile (neg hu) False
monoToTile '`'  = Just $ ArmTile (neg hv) False
monoToTile '\'' = Just $ ArmTile (neg hw) False
monoToTile '@'  = Just HookTile
monoToTile '*'  = Just $ WrenchTile zero
monoToTile 'O'  = Just BallTile
monoToTile 's'  = Just $ SpringTile Stretched hu
monoToTile 'S'  = Just $ SpringTile Relaxed hu
monoToTile '$'  = Just $ SpringTile Compressed hu
monoToTile 'z'  = Just $ SpringTile Stretched hv
monoToTile 'Z'  = Just $ SpringTile Relaxed hv
monoToTile '5'  = Just $ SpringTile Compressed hv
monoToTile '('  = Just $ SpringTile Stretched hw
monoToTile '['  = Just $ SpringTile Relaxed hw
monoToTile '{'  = Just $ SpringTile Compressed hw
monoToTile 'c'  = Just $ SpringTile Stretched (neg hu)
monoToTile 'C'  = Just $ SpringTile Relaxed (neg hu)
monoToTile 'D'  = Just $ SpringTile Compressed (neg hu)
monoToTile ')'  = Just $ SpringTile Stretched (neg hv)
monoToTile ']'  = Just $ SpringTile Relaxed (neg hv)
monoToTile '}'  = Just $ SpringTile Compressed (neg hv)
monoToTile '1'  = Just $ SpringTile Stretched (neg hw)
monoToTile '7'  = Just $ SpringTile Relaxed (neg hw)
monoToTile '9'  = Just $ SpringTile Compressed (neg hw)
monoToTile _    = Nothing

minmax :: Ord a => [a] -> (a,a)
minmax = minimum &&& maximum

solutionToAscii :: Solution -> String
solutionToAscii = map pmToAscii

dirChar :: HexVec -> Char
dirChar dir
    | dir == hu = 'l'
    | dir == neg hu = 'h'
    | dir == hv = 'y'
    | dir == neg hv = 'n'
    | dir == hw = 'b'
    | dir == neg hw = 'u'
    | otherwise = '.'
pmToAscii :: PlayerMove -> Char
pmToAscii (HookPush dir)    = dirChar dir
pmToAscii (WrenchPush dir)  = toUpper $ dirChar dir
pmToAscii (HookTorque 1)    = '+'
pmToAscii (HookTorque (-1)) = '-'
pmToAscii _                 = '.'

solutionOfAscii :: String -> Maybe Solution
solutionOfAscii = mapM pmOfAscii

pmOfAscii :: Char -> Maybe PlayerMove
pmOfAscii 'l' = Just $ HookPush hu
pmOfAscii 'y' = Just $ HookPush hv
pmOfAscii 'b' = Just $ HookPush hw
pmOfAscii 'h' = Just . HookPush $ neg hu
pmOfAscii 'n' = Just . HookPush $ neg hv
pmOfAscii 'u' = Just . HookPush $ neg hw
pmOfAscii 'L' = Just $ WrenchPush hu
pmOfAscii 'Y' = Just $ WrenchPush hv
pmOfAscii 'B' = Just $ WrenchPush hw
pmOfAscii 'H' = Just . WrenchPush $ neg hu
pmOfAscii 'N' = Just . WrenchPush $ neg hv
pmOfAscii 'U' = Just . WrenchPush $ neg hw
pmOfAscii '+' = Just $ HookTorque 1
pmOfAscii '-' = Just . HookTorque $ -1
pmOfAscii '.' = Just NullPM
pmOfAscii _   = Nothing

readAsciiLockFile :: FilePath -> IO (Maybe Lock, Maybe Solution)
readAsciiLockFile path = fromLines <$> readStrings path
    where fromLines ls = fromMaybe (lockOfAscii ls, Nothing) $ do
            guard $ length ls > 2
            let (locklines, [header,solnLine]) = splitAt (length ls - 2) ls
            guard $ isPrefixOf "Solution:" header
            return (lockOfAscii locklines,
                solutionOfAscii solnLine `mplus` tryRead solnLine)

writeAsciiLockFile :: FilePath -> Maybe Solution -> Lock -> IO ()
writeAsciiLockFile path msoln lock = do
    writeStrings path $ lockToAscii lock ++ case msoln of
        Nothing   -> []
        Just soln -> ["Solution:", solutionToAscii soln]
