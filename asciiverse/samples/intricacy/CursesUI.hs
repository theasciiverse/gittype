-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module CursesUI where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Array
import           Data.Bifunctor            (second)
import           Data.Function             (on)
import           Data.List
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Semigroup            as Sem
import qualified UI.HSCurses.Curses        as Curses

import           BoardColouring
import           Command
import           CursesRender
import           CVec
import           Frame
import           GameState                 (stateBoard)
import           GameStateTypes
import           Hex
import           InputMode
import           KeyBindings
import           Mundanities
import           ServerAddr

data UIState = UIState
    { dispCPairs    :: [Curses.Pair]
    , dispCentre    :: HexPos
    , dispLastCol   :: PieceColouring
    , uiKeyBindings :: Map InputMode KeyBindings
    , monochrome    :: Bool
    , message       :: Maybe (Curses.Attr, ColPair, String)}
type UIM = StateT UIState IO
nullUIState = UIState [] (PHS zero) Map.empty Map.empty False Nothing

readBindings :: UIM ()
readBindings = void.runMaybeT $ do
    path <- liftIO $ confFilePath "bindings"
    bdgs <- MaybeT $ liftIO $ readReadFile path
    lift $ modify $ \s -> s {uiKeyBindings = bdgs}
writeBindings :: UIM ()
writeBindings = do
    path <- liftIO $ confFilePath "bindings"
    bdgs <- gets uiKeyBindings
    liftIO makeConfDir
    liftIO $ writeFile path $ show bdgs

getBindings :: InputMode -> UIM KeyBindings
getBindings mode = do
    uibdgs <- gets (Map.findWithDefault [] mode . uiKeyBindings)
    return $ uibdgs ++ bindings mode

bindingsStr :: InputMode -> [Command] -> UIM String
bindingsStr mode cmds = do
    bdgs <- getBindings mode
    return $ (("["++).(++"]")) $ intercalate "," $
        maybe "" showKey . findBinding bdgs <$> cmds


data Gravity = GravUp | GravLeft | GravRight | GravDown | GravCentre
    deriving (Eq, Ord, Show, Enum)

data Draw = Draw { drawWidth :: Int, doDraw :: CVec -> UIM () }

doDrawAt :: CVec -> Draw -> UIM ()
doDrawAt = flip doDraw

alignDraw :: Gravity -> Int -> Draw -> Draw
alignDraw gravity w (Draw w' d) = Draw (max w w') $ \(CVec y x) ->
    d $ CVec y $ x + shift
    where
        shift = case gravity of
            GravLeft   -> 0
            GravRight  -> max 0 $ w - w'
            GravCentre -> w - w' `div` 2

instance Sem.Semigroup Draw where
    (Draw w d) <> (Draw w' d') =
        Draw (w+w') $ \cpos@(CVec y x) -> d cpos >> d' (CVec y (x+w))
instance Monoid Draw where
    mempty = Draw 0 (const $ return ())
    mappend = (Sem.<>)

stringDraw :: Curses.Attr -> ColPair -> String -> Draw
stringDraw attr col str =
    Draw (length str) $ \cpos -> drawStr attr col cpos str

greyDraw :: String -> Draw
greyDraw = stringDraw a0 white

bindingsDraw :: KeyBindings -> [Command] -> Draw
bindingsDraw = bindingsDrawColour white
bindingsDrawColour :: ColPair -> KeyBindings -> [Command] -> Draw
bindingsDrawColour col bdgs cmds =
    mconcat . (stringDraw a0 col "[" :) . (++ [stringDraw a0 col "]"]) .
    intersperse (stringDraw a0 col ",") $
        mapMaybe ((keyDraw <$>) . findBinding bdgs) cmds
    where
        keyDraw = stringDraw bold col . showKeyFriendlyShort

bindingDrawChar :: KeyBindings -> Curses.Attr -> Command -> Draw
bindingDrawChar bdgs a cmd = mconcat . maybeToList $
    stringDraw a white . (:[]) . showKeyChar <$> findBinding bdgs cmd

drawDirBindings :: InputMode -> KeyBindings -> WrHoSel -> Draw
drawDirBindings mode bdgs whs = Draw 5 $ \cpos -> do
    let c | mode == IMEdit = '_'
            | otherwise = case whs of
                WHSHook     -> '@'
                WHSWrench   -> '*'
                WHSSelected -> '_'
        gl = Glyph c white a0
    drawAtCVec gl cpos
    sequence_ [ doDrawAt (cpos +^ hexVec2CVec dir) .
            bindingDrawChar bdgs bold $ CmdDir whs dir
        | dir <- hexDirs ]

data BindingsEntry = BindingsEntry String [Command]

drawBindingsTables :: InputMode -> (Command -> Bool) -> Frame -> UIM ()
drawBindingsTables mode censor frame | mode `elem` [ IMEdit, IMPlay ] = do
    bdgs <- getBindings mode
    (h,w) <- liftIO Curses.scrSize
    let startRight = frameWidth frame + 3
    let maxWidth = (w `div` 2) - startRight - 1
    let entryDraws (BindingsEntry desc cmds) =
            (greyDraw desc, bindingsDraw bdgs cmds)
    forM_ [GravLeft, GravRight] $ \grav -> do
        let table = filter (\(_,BindingsEntry _ cs) -> not $ null cs) $
                second (\(BindingsEntry s cs) -> BindingsEntry s $ filter censor cs) <$>
                bindingsTable mode grav
            drawsTable = second entryDraws <$> table
            maxDesc = maximum $ drawWidth . fst . snd <$> drawsTable
            maxBdgs = maximum $ drawWidth . snd . snd <$> drawsTable
            descX = (w `div` 2) + if grav == GravRight
                then startRight + maxBdgs + 2
                else -(startRight + maxBdgs + 2 + maxDesc)
            bdgsX = (w `div` 2) + if grav == GravRight
                then startRight
                else -(startRight + maxBdgs)
            oppGrav = if grav == GravRight then GravLeft else GravRight
            useDescs = maxDesc + 1 + maxBdgs <= maxWidth
        sequence_
            [ do
                when (maxBdgs <= maxWidth) $
                    doDrawAt (CVec y bdgsX) $ alignDraw
                        (if useDescs then grav else oppGrav) maxBdgs
                        bdgsDraw
                when useDescs $
                    doDrawAt (CVec y descX) $ alignDraw oppGrav maxDesc descDraw
            | (yoff, (descDraw, bdgsDraw)) <- drawsTable
            , let y = (h `div` 2) + yoff
            ]
        when (mode `elem` [IMPlay,IMEdit] && grav == GravLeft && maxWidth >= 5) $
            let (halfw,poss)
                    | maxWidth < 15 = (3,[CVec (-5) 0])
                    | maxWidth < 19 = (7,[CVec (-4) 0, CVec (-6) 5, CVec (-6) (-5)])
                    | otherwise = (9,[CVec (-5) 0, CVec (-5) 7, CVec (-5) (-7)])
            in sequence_ [ doDrawAt pos $ drawDirBindings mode bdgs whs
                | (whs,pos) <- zip [WHSSelected, WHSWrench, WHSHook] $
                    (CVec (h `div` 2) ((w `div` 2) - startRight -
                        min (maxWidth - halfw) (max (maxBdgs+1) halfw)) +^) <$> poss ]
        when (mode == IMEdit && grav == GravRight && maxWidth >= 9) $
            let c = CVec ((h`div`2) - 4) ((w`div`2) + startRight +
                        min (maxWidth - 5) (max (maxBdgs+1) 5))
            in sequence_ [ do
                    drawAtCVec gl (c +^ CVec 0 x)
                    doDrawAt (c +^ CVec 1 x) . bindingDrawChar bdgs bold $ CmdTile tile
                | (x,tile) <- zip [-4,-2,0,2,4]
                        [ BlockTile []
                        , SpringTile Relaxed zero
                        , PivotTile zero
                        , ArmTile zero False
                        , BallTile ]
                , let gl = Glyph (fst $ tileChar tile) white a0 ]

    where
        bindingsTable IMPlay GravLeft =
            [ (-2, BindingsEntry "select tool"
                [CmdToggle, CmdTile $ WrenchTile zero, CmdTile HookTile])
            , (-1, BindingsEntry "rotate hook" $
                nub [CmdRotate whs dir | whs <- [WHSSelected, WHSHook], dir <- [-1,1]])
            , ( 0, BindingsEntry "wait" [CmdWait])
            , ( 2, BindingsEntry "open lock" [CmdOpen])
            , ( 4, BindingsEntry "undo, redo" [CmdUndo, CmdRedo])
            , ( 5, BindingsEntry "marks" [CmdMark, CmdJumpMark, CmdReset])
            ]
        bindingsTable IMPlay GravRight =
            [ (-7, BindingsEntry "help" [CmdHelp])
            , (-6, BindingsEntry "bind" [CmdBind Nothing])
            , (7, BindingsEntry "quit" [CmdQuit])
            ]
        bindingsTable IMReplay GravLeft =
            [ ( 0, BindingsEntry "wait" [CmdWait])
            , ( 4, BindingsEntry "undo, redo" [CmdUndo, CmdRedo])
            , ( 5, BindingsEntry "marks" [CmdMark, CmdJumpMark, CmdReset])
            ]
        bindingsTable IMReplay GravRight =
            [ (-7, BindingsEntry "help" [CmdHelp])
            , (-6, BindingsEntry "bind" [CmdBind Nothing])
            , (7, BindingsEntry "quit" [CmdQuit])
            ]
        bindingsTable IMEdit GravLeft =
            [ (-1, BindingsEntry "rotate" $
                nub [CmdRotate whs dir | whs <- [WHSSelected, WHSHook], dir <- [-1,1]])
            , ( 0, BindingsEntry "select" [CmdSelect])
            , ( 1, BindingsEntry "delete" [CmdDelete])
            , ( 2, BindingsEntry "merge" [CmdMerge])
            , ( 4, BindingsEntry "undo, redo" [CmdUndo, CmdRedo])
            , ( 5, BindingsEntry "marks" [CmdMark, CmdJumpMark, CmdReset])
            ]
        bindingsTable IMEdit GravRight =
            [ (-7, BindingsEntry "help" [CmdHelp])
            , (-6, BindingsEntry "bind" [CmdBind Nothing])
            , (-1, BindingsEntry "test" [CmdTest])
            , ( 0, BindingsEntry "play" [CmdPlay])
            , ( 1, BindingsEntry "step" [CmdWait])
            , ( 4, BindingsEntry "write" [CmdWriteState])
            , ( 7, BindingsEntry "quit" [CmdQuit])
            ]
        bindingsTable _ _ = []

drawBindingsTables _ _ _ = return ()

-- |frameWidth = maximum . map (abs . cx . hexVec2CVec) .
--      blockPattern . placedPiece . framePiece
frameWidth :: Frame -> Int
frameWidth frame@(BasicFrame size) = max (2*size) $
    2*(size + boltWidth frame) - (size`div`2)

erase :: UIM ()
erase = liftIO Curses.erase
refresh :: UIM ()
refresh = liftIO Curses.refresh

type Geom = (CVec, HexPos)
getGeom :: UIM Geom
getGeom = do
    (h,w) <- liftIO Curses.scrSize
    centre <- gets dispCentre
    return (CVec (h`div`2) (w`div`2), centre)

drawAt :: Glyph -> HexPos -> UIM ()
drawAt gl pos =
    drawAtWithGeom gl pos =<< getGeom

drawAtWithGeom :: Glyph -> HexPos -> Geom -> UIM ()
drawAtWithGeom gl pos geom@(scrCentre,centre) =
    drawAtCVec gl $ scrCentre +^ hexVec2CVec (pos -^ centre)

drawAtCVec :: Glyph -> CVec -> UIM ()
drawAtCVec gl cpos = do
    cpairs <- gets dispCPairs
    liftIO $ mvAddGlyph cpairs cpos gl

drawStr :: Curses.Attr -> ColPair -> CVec -> String -> UIM ()
drawStr attr col v str = do
    cpairs <- gets dispCPairs
    liftIO $ Curses.attrSet attr (cpairs!!col) >> mvAddStr v str
drawStrGrey :: CVec -> String -> UIM ()
drawStrGrey = drawStr a0 0
drawStrCentred :: Curses.Attr -> ColPair -> CVec -> [Char] -> UIM ()
drawStrCentred attr col v str =
    drawStr attr col (truncateCVec (v +^ CVec 0 (-length str `div` 2))) str


drawCursorAt :: Maybe HexPos -> UIM ()
drawCursorAt Nothing =
    void $ liftIO $ Curses.cursSet Curses.CursorInvisible
drawCursorAt (Just pos) = do
    geom@(scrCentre,centre) <- getGeom
    liftIO $ Curses.cursSet Curses.CursorVisible
    liftIO $ move $ scrCentre +^ hexVec2CVec (pos -^ centre)

drawState :: [PieceIdx] -> Bool -> [Alert] -> GameState -> UIM ()
drawState reversed colourFixed alerts st = do
    lastCol <- gets dispLastCol
    colouring <- drawStateWithGeom reversed colourFixed lastCol st =<< getGeom
    modify $ \ds -> ds { dispLastCol = colouring }

drawStateWithGeom :: [PieceIdx] -> Bool -> PieceColouring -> GameState -> Geom -> UIM PieceColouring
drawStateWithGeom reversed colourFixed lastCol st geom = do
    let colouring = boardColouring st (colouredPieces colourFixed st) lastCol
    mono <- gets monochrome
    sequence_ [ drawAtWithGeom glyph pos geom |
        (pos,glyph) <- Map.toList $ ownedTileGlyph mono colouring reversed <$> stateBoard st
        ]
    return colouring

drawMsgLine = void.runMaybeT $ do
    (attr,col,str) <- MaybeT $ gets message
    lift $ do
        (h,w) <- liftIO Curses.scrSize
        liftIO $ clearLine $ h-1
        let str' = take (w-1) str
        drawStr attr col (CVec (h-1) 0) str'
setMsgLine attr col str = do
    modify $ \s -> s { message = Just (attr,col,str) }
    drawMsgLine
    refresh

drawTitle (Just (title,n)) = do
    (h,w) <- liftIO Curses.scrSize
    drawStrCentred a0 white (CVec 0 (w`div`2)) title
drawTitle Nothing = return ()

say = setMsgLine bold white
sayError = setMsgLine bold red
