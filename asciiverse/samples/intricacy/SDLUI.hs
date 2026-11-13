-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

module SDLUI where

import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Array
import           Data.Function              (on)
import           Data.List
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Ratio
import           Data.Time.Clock            (getCurrentTime)
import           Data.Word
import           Graphics.UI.SDL            hiding (flip)
import qualified Graphics.UI.SDL            as SDL
import qualified Graphics.UI.SDL.TTF        as TTF
import           System.FilePath
--import Debug.Trace (traceShow)

#ifdef SOUND
import           Graphics.UI.SDL.Mixer
import           System.Random              (randomRIO)
#endif

import           BoardColouring
import           Command
import           GameState
import           GameStateTypes
import           Hex
import           InputMode
import           KeyBindings
import           Lock
import           Maxlocksize
import           Metagame
import           Mundanities
import           Physics
import           SDLGlyph
import           SDLRender
import           Util

data UIState = UIState
    { scrHeight             :: Int
    , scrWidth              :: Int
    , gsSurface             :: Maybe Surface
    , bgSurface             :: Maybe Surface
    , cachedGlyphs          :: CachedGlyphs
    , lastDrawArgs          :: Maybe DrawArgs
    , miniLocks             :: Map Lock Surface
    , registeredSelectables :: Map HexVec Selectable
    , contextButtons        :: [ButtonGroup]
    , uiOptions             :: UIOptions
    , settingBinding        :: Maybe Command
    , uiKeyBindings         :: Map InputMode KeyBindings
    , dispFont              :: Maybe TTF.Font
    , dispFontSmall         :: Maybe TTF.Font
    , lastFrameTicks        :: Word32
    , paintTileIndex        :: Int
    , leftButtonDown        :: Maybe HexVec
    , middleButtonDown      :: Maybe HexVec
    , rightButtonDown       :: Maybe HexVec
    , mousePos              :: (HexVec,Bool)
    , message               :: Maybe (Pixel, String)
    , hoverStr              :: Maybe String
    , dispCentre            :: HexPos
    , dispLastCol           :: PieceColouring
    , animFrame             :: Int
    , nextAnimFrameAt       :: Maybe Word32
    , fps                   :: Int
#ifdef SOUND
    , sounds                :: Map String [Chunk]
#endif
    }
    deriving (Eq, Ord, Show)
type UIM = StateT UIState IO
nullUIState = UIState 0 0 Nothing Nothing emptyCachedGlyphs Nothing Map.empty Map.empty []
    defaultUIOptions Nothing Map.empty Nothing Nothing 0 0 Nothing Nothing Nothing
    (zero,False) Nothing Nothing (PHS zero) Map.empty 0 Nothing 50
#ifdef SOUND
    Map.empty
#endif

data UIOptions = UIOptions
    { useFiveColouring :: Bool
    , showBlocks       :: ShowBlocks
    , whsButtons       :: Maybe WrHoSel
    , useBackground    :: Bool
    , fullscreen       :: Bool
    , showButtonText   :: Bool
    , useSounds        :: Bool
    , uiAnimTime       :: Word32
    , shortUiAnimTime  :: Word32
    }
    deriving (Eq, Ord, Show, Read)
defaultUIOptions = UIOptions False ShowBlocksBlocking Nothing True False True True 100 20

modifyUIOptions :: (UIOptions -> UIOptions) -> UIM ()
modifyUIOptions f = modify $ \s -> s { uiOptions = f $ uiOptions s }

renderToMain :: RenderM a -> UIM a
renderToMain m = do
    surf <- liftIO getVideoSurface
    renderToMainWithSurf surf m
renderToMainWithSurf :: Surface -> RenderM a -> UIM a
renderToMainWithSurf surf m = do
    (scrCentre, size) <- getGeom
    centre <- gets dispCentre
    mfont <- gets dispFont
    bgsurf <- gets bgSurface
    cgs <- gets cachedGlyphs
    (a,cgs') <- liftIO $ runRenderM m cgs $ RenderContext surf bgsurf centre scrCentre zero size mfont
    modify $ \s -> s { cachedGlyphs = cgs' }
    return a

refresh :: UIM ()
refresh = do
    surface <- liftIO getVideoSurface
    liftIO $ SDL.flip surface

waitFrame :: UIM ()
waitFrame = do
    last <- gets lastFrameTicks
    let next = last + 1000 `div` 30
    now <- liftIO getTicks
    -- liftIO $ print now
    when (now < next) $
        liftIO $ delay (next - now)
    modify $ \ds -> ds { lastFrameTicks = now }


data Button = Button { buttonPos :: HexVec, buttonCmd :: Command, buttonHelp :: [ButtonHelp] }
    deriving (Eq, Ord, Show)
type ButtonGroup = ([Button],(Int,Int))
type ButtonHelp = (String, HexVec)
singleButton :: HexVec -> Command -> Int -> [ButtonHelp] -> ButtonGroup
singleButton pos cmd col helps = ([Button pos cmd helps], (col,0))
getButtons :: InputMode -> UIM [ ButtonGroup ]
getButtons mode = do
    mwhs <- gets $ whsButtons . uiOptions
    cntxtButtons <- gets contextButtons
    return $ cntxtButtons ++ global ++ case mode of
        IMEdit -> [
            singleButton (tl+^hv+^neg hw) CmdTest 1 [("test", hu+^neg hw)]
            , singleButton (tl+^neg hw) CmdPlay 2 [("play", hu+^neg hw)]
            , markButtonGroup
            , singleButton (br+^2*^hu) CmdWriteState 2 [("save", hu+^neg hw)] ]
            ++ whsBGs mwhs mode
            ++ [ ([Button (paintButtonStart +^ hu +^ i*^hv) (paintTileCmds!!i) []
                | i <- take (length paintTiles) [0..] ],(5,0)) ]
        IMPlay -> whsBGs mwhs mode
        IMReplay -> [ markButtonGroup ]
        IMMeta ->
            [ singleButton serverPos CmdSetServer 0 [("server",7*^neg hu)]
            , singleButton (serverPos+^hw) CmdToggleCacheOnly 0 [("offline",hv+^7*^neg hu),("mode",hw+^5*^neg hu)]
            , singleButton (codenamePos +^ 2*^neg hu) (CmdSelCodename Nothing) 2 [("code",hv+^5*^neg hu),("name",hw+^5*^neg hu)]
            , singleButton (serverPos +^ 2*^neg hv +^ 2*^hw) CmdInitiation 3 [("initi",hu+^neg hw),("ation",hu+^neg hv)]
            ]

        _ -> []
        where
            global = if mode `elem` [IMTextInput,IMImpatience] then [] else
                [ singleButton br CmdQuit 0 [("quit",hu+^neg hw)]
                , singleButton (tr +^ 3*^hv +^ 3*^hu) CmdHelp 3 [("help",hu+^neg hw)] ]
            whsBGs :: Maybe WrHoSel -> InputMode -> [ ButtonGroup ]
            whsBGs Nothing _ = []
            whsBGs (Just whs) mode =
                let edit = mode == IMEdit
                in [ ( [ Button bl (if edit then CmdSelect else CmdWait) [] ], (0,0))
                    , ( [ Button (bl+^dir) (CmdDir whs dir)
                        (if dir==hu then [("move",hu+^neg hw),(if edit then "piece" else whsStr whs,hu+^neg hv)] else [])
                        | dir <- hexDirs ], (5,0) )
                ] ++
                ([( [ Button (bl+^((-2)*^hv))
                           (CmdRotate whs (-1))
                           [("turn",hu+^neg hw),("cw",hu+^neg hv)]
                       , Button (bl+^((-2)*^hw))
                           (CmdRotate whs 1)
                           [("turn",hu+^neg hw),("ccw",hu+^neg hv)]
                       ], (5,0) ) | whs /= WHSWrench]) ++
                ([( [ Button (bl+^(2*^hv)+^hw+^neg hu) (CmdTile HookTile) [("select",hu+^neg hw),("hook",hu+^neg hv)]
                       , Button (bl+^(2*^hv)+^neg hu) (CmdTile $ WrenchTile zero) [("select",hu+^neg hw),("wrench",hu+^neg hv)]
                       ], (2,0) ) | whs == WHSSelected && mode /= IMEdit])
            tr = periphery 0
            tl = periphery 2
            bl = periphery 3
            br = periphery 5

markButtonGroup = ([Button (tl+^hw) CmdMark [("set",hu+^neg hw),("mark",hu+^neg hv)]
    , Button (tl+^hw+^hv) CmdJumpMark [("jump",hu+^neg hw),("mark",hu+^neg hv)]
    , Button (tl+^hw+^2*^hv) CmdReset [("jump",hu+^neg hw),("start",hu+^neg hv)]],(0,1))
    where
    tl = periphery 2

data AccessedInfo = AccessedSolved | AccessedPublic | AccessedReadNotes | AccessedUndeclared
    deriving (Eq, Ord, Show)
data Selectable
    = SelOurLock
    | SelTut Bool
    | SelInitLock HexVec Bool
    | SelLock ActiveLock
    | SelLockUnset Bool ActiveLock
    | SelSelectedCodeName Codename
    | SelRelScore
    | SelRelScoreComponent
    | SelScoreLock (Maybe Codename) (Maybe AccessedReason) ActiveLock
    | SelUndeclared Undeclared
    | SelReadNote NoteInfo
    | SelReadNoteSlot
    | SelSolution NoteInfo
    | SelAccessed Codename
    | SelRandom Codename
    | SelSecured NoteInfo
    | SelOldLock LockSpec
    | SelPublicLock
    | SelAccessedInfo AccessedInfo
    | SelLockPath
    | SelPrivyHeader
    | SelNotesHeader
    | SelToolHook
    | SelToolWrench Bool
    deriving (Eq, Ord, Show)

registerSelectable :: HexVec -> Int -> Selectable -> UIM ()
registerSelectable v r s =
    modify $ \ds -> ds {registeredSelectables = foldr (
        (`Map.insert` s) . (v+^)) (registeredSelectables ds) (hexDisc r)}
registerButtonGroup :: ButtonGroup -> UIM ()
registerButtonGroup g = modify $ \ds -> ds {contextButtons = g:contextButtons ds}
registerButton :: HexVec -> Command -> Int -> [ButtonHelp] -> UIM ()
registerButton pos cmd col helps = registerButtonGroup $ singleButton pos cmd col helps
clearSelectables,clearButtons :: UIM ()
clearSelectables = modify $ \ds -> ds {registeredSelectables = Map.empty}
clearButtons = modify $ \ds -> ds {contextButtons = []}

registerUndoButtons :: Bool -> UIM ()
registerUndoButtons noRedo = do
    registerButton (periphery 2+^hu) CmdUndo 0 [("undo",hu+^neg hw)]
    unless noRedo $ registerButton (periphery 2+^hu+^neg hv) CmdRedo 2 [("redo",hu+^neg hw)]

commandOfSelectable IMInit (SelTut _) _ = Just . CmdSolveInit $ Just zero
commandOfSelectable IMInit (SelInitLock v _) _ = Just . CmdSolveInit $ Just v
commandOfSelectable IMMeta SelOurLock _ = Just CmdEdit
commandOfSelectable IMMeta (SelLock (ActiveLock _ i)) False = Just $ CmdSolve (Just i)
commandOfSelectable IMMeta (SelLock (ActiveLock _ i)) True = Just $ CmdPlaceLock (Just i)
commandOfSelectable IMMeta (SelScoreLock Nothing _ (ActiveLock _ i)) False = Just $ CmdSolve (Just i)
commandOfSelectable IMMeta (SelScoreLock Nothing _ (ActiveLock _ i)) True = Just $ CmdPlaceLock (Just i)
commandOfSelectable IMMeta (SelScoreLock (Just _) _ _) _ = Just CmdHome
commandOfSelectable IMMeta (SelLockUnset True (ActiveLock _ i)) _ = Just $ CmdPlaceLock (Just i)
commandOfSelectable IMMeta (SelSelectedCodeName _) False = Just $ CmdSelCodename Nothing
commandOfSelectable IMMeta (SelSelectedCodeName _) True = Just CmdHome
commandOfSelectable IMMeta (SelUndeclared undecl) _ = Just $ CmdDeclare $ Just undecl
commandOfSelectable IMMeta (SelReadNote note) False = Just $ CmdSelCodename $ Just $ noteAuthor note
commandOfSelectable IMMeta (SelReadNote note) True = Just $ CmdViewSolution $ Just note
commandOfSelectable IMMeta (SelSolution note) False = Just $ CmdSelCodename $ Just $ noteAuthor note
commandOfSelectable IMMeta (SelSolution note) True = Just $ CmdViewSolution $ Just note
commandOfSelectable IMMeta (SelAccessed name) _ = Just $ CmdSelCodename $ Just name
commandOfSelectable IMMeta (SelRandom name) _ = Just $ CmdSelCodename $ Just name
commandOfSelectable IMMeta (SelSecured note) False = Just $ CmdSelCodename $ Just $ lockOwner $ noteOn note
commandOfSelectable IMMeta (SelSecured note) True = Just $ CmdViewSolution $ Just note
commandOfSelectable IMMeta (SelOldLock ls) _ = Just $ CmdPlayLockSpec $ Just ls
commandOfSelectable IMMeta SelLockPath _ = Just CmdSelectLock
commandOfSelectable IMTextInput (SelLock (ActiveLock _ i)) _ = Just $ CmdInputSelLock i
commandOfSelectable IMTextInput (SelScoreLock _ _ (ActiveLock _ i)) _ = Just $ CmdInputSelLock i
commandOfSelectable IMTextInput (SelLockUnset _ (ActiveLock _ i)) _ = Just $ CmdInputSelLock i
commandOfSelectable IMTextInput (SelReadNote note) _ = Just $ CmdInputCodename $ noteAuthor note
commandOfSelectable IMTextInput (SelSolution note) _ = Just $ CmdInputCodename $ noteAuthor note
commandOfSelectable IMTextInput (SelSecured note) _ = Just $ CmdInputCodename $ lockOwner $ noteOn note
commandOfSelectable IMTextInput (SelRandom name) _ = Just $ CmdInputCodename name
commandOfSelectable IMTextInput (SelUndeclared undecl) _ = Just $ CmdInputSelUndecl undecl
commandOfSelectable _ _ _ = Nothing

helpOfSelectable (SelTut False) = Just "Enter tutorials"
helpOfSelectable (SelTut True) = Just "Revisit tutorials"
helpOfSelectable (SelInitLock _ False) = Just "Attempt lock"
helpOfSelectable (SelInitLock _ True) = Just "Revisit solved lock"
helpOfSelectable SelOurLock = Just
    "Design a lock."
helpOfSelectable (SelSelectedCodeName name) = Just $
    "Currently viewing player "++name++"."
helpOfSelectable SelRelScore = Just
    "The extent to which you are held in higher esteem than this player."
helpOfSelectable SelRelScoreComponent = Just
    "Contribution to total relative esteem."
helpOfSelectable (SelLock (ActiveLock name i)) = Just $
    name++"'s lock "++[lockIndexChar i]++"."
helpOfSelectable (SelLockUnset True _) = Just
    "Place a lock."
helpOfSelectable (SelLockUnset False _) = Just
    "An empty lock slot."
helpOfSelectable (SelUndeclared _) = Just
    "Declare your solution to a lock by securing a note on it behind a lock of your own."
helpOfSelectable (SelRandom _) = Just
    "Random set of players. Colours show relative esteem, bright red (-3) to bright green (+3)."
helpOfSelectable (SelScoreLock (Just name) Nothing _) = Just $
    "Your lock. "++name++" can not not unlock it."
helpOfSelectable (SelScoreLock (Just name) (Just AccessedPrivy) _) = Just $
    "Your lock. "++name++" can unlock it: -1 to relative esteem."
helpOfSelectable (SelScoreLock (Just name) (Just AccessedPub) _) = Just
    "Your lock. Its secrets have been publically revealed: -1 to relative esteem."
helpOfSelectable (SelScoreLock (Just name) (Just AccessedEmpty) _) = Just $
    "Your empty lock slot. "++name++" can unlock all your locks: -1 to relative esteem."
helpOfSelectable (SelScoreLock Nothing Nothing (ActiveLock name _)) = Just $
    name++"'s lock. You can not unlock it."
helpOfSelectable (SelScoreLock Nothing (Just AccessedPrivy) (ActiveLock name _)) = Just $
    name++"'s lock. You can unlock it: +1 to relative esteem."
helpOfSelectable (SelScoreLock Nothing (Just AccessedPub) (ActiveLock name _)) = Just $
    name++"'s lock. Its secrets have been publically revealed: +1 to relative esteem."
helpOfSelectable (SelScoreLock Nothing (Just AccessedEmpty) (ActiveLock name _)) = Just $
    name++"'s empty lock slot. You can unlock all "++name++"'s locks: +1 to relative esteem."
helpOfSelectable (SelReadNote note) = Just $
    "You have read "++noteAuthor note++"'s note on this lock."
helpOfSelectable SelReadNoteSlot = Just
    "Reading three notes on this lock would suffice to reveal its secrets."
helpOfSelectable (SelSecured note) = let ActiveLock owner idx = noteOn note in
    Just $ "Secured note on "++owner++"'s lock "++[lockIndexChar idx]++"."
helpOfSelectable (SelSolution note) = Just $ case noteBehind note of
    Just (ActiveLock owner idx) -> owner ++
        " has secured their note on this lock behind their lock " ++ [lockIndexChar idx] ++ "."
    Nothing -> noteAuthor note ++ "'s note on this lock is public knowledge."
helpOfSelectable (SelAccessed name) = Just $
    name ++ " did not pick this lock, but learnt how to unlock it by reading three notes on it."
helpOfSelectable SelPublicLock = Just
    "Notes behind retired or public locks are public; a lock with three public notes on it is public."
helpOfSelectable (SelAccessedInfo meth) = Just $ case meth of
    AccessedSolved -> "You picked this lock and declared your solution, so may read any notes it secures."
    AccessedPublic -> "The secrets of this lock have been publically revealed."
    AccessedUndeclared -> "You have picked this lock, but are yet to declare your solution."
    AccessedReadNotes ->
        "Having read three notes on others' solutions to this lock, you have unravelled its secrets."
helpOfSelectable (SelOldLock ls) = Just $
    "Retired lock #"++show ls++". Any notes which were secured by the lock are now public knowledge."
helpOfSelectable SelLockPath = Just
    "Select a lock by its name. The names you give your locks are not revealed to others."
helpOfSelectable SelPrivyHeader = Just
    "Notes on this lock declared by players who picked the lock."
helpOfSelectable SelNotesHeader = Just
    "Notes secured by this lock. These notes are read by everyone who can unlock the lock."
helpOfSelectable (SelToolWrench False) = Just "The wrench, one of your lockpicking tools. Click and drag to move."
helpOfSelectable (SelToolWrench True) = Just "The wrench, currently in motion. It will keep moving until it is blocked."
helpOfSelectable SelToolHook = Just "The hook, one of your lockpicking tools. Click and drag to move, use mousewheel to turn."

cmdAtMousePos pos@(mPos,central) im selMode = do
    buttons <- concatMap fst <$> getButtons im
    sels <- gets registeredSelectables
    return $ listToMaybe $
        [ buttonCmd button
            | button <- buttons, mPos == buttonPos button, central]
        ++ maybe [] (\isRight ->
                [ cmd
                | Just sel <- [Map.lookup mPos sels]
                , Just cmd <- [ commandOfSelectable im sel isRight ] ])
            selMode

helpAtMousePos :: (HexVec, Bool) -> InputMode -> UIM (Maybe [Char])
helpAtMousePos (mPos,_) _ =
    gets $ (helpOfSelectable <=< Map.lookup mPos) . registeredSelectables


data UIOptButton a = UIOptButton { getUIOpt :: UIOptions->a, setUIOpt :: a->UIOptions->UIOptions,
    uiOptVals :: [a], uiOptPos :: HexVec, uiOptGlyph :: a->Glyph, uiOptDescr :: a->String,
    uiOptModes :: [InputMode], onSet :: Maybe (a -> UIM ()) }

-- non-uniform type, so can't use a list...
uiOB1 = UIOptButton useFiveColouring (\v o -> o {useFiveColouring=v}) [True,False]
        (periphery 0 +^ 3 *^ hu +^ neg hv) UseFiveColourButton
        (\v -> if v then "Adjacent pieces get different colours" else
        "Pieces are coloured according to type")
        [IMPlay, IMReplay, IMEdit] Nothing
uiOB2 = UIOptButton showBlocks (\v o -> o {showBlocks=v}) [ShowBlocksBlocking,ShowBlocksAll,ShowBlocksNone]
        (periphery 0 +^ 2 *^ hu +^ 2 *^ neg hv) ShowBlocksButton
        (\case
            ShowBlocksBlocking -> "Blocking forces are annotated"
            ShowBlocksAll      -> "Blocked and blocking forces are annotated"
            ShowBlocksNone     -> "Blockage annotations disabled")
        [IMPlay, IMReplay] Nothing
uiOB3 = UIOptButton whsButtons (\v o -> o {whsButtons=v}) [Nothing, Just WHSSelected, Just WHSWrench, Just WHSHook]
        (periphery 3 +^ 3 *^ hv) WhsButtonsButton
        (\case
            Nothing -> "Click to show (and rebind) keyboard control buttons."
            Just whs -> "Showing buttons for controlling " ++ case whs of
                WHSSelected -> "selected piece; right-click to rebind"
                WHSWrench   -> "wrench; right-click to rebind"
                WHSHook     -> "hook; right-click to rebind")
        [IMPlay, IMEdit] Nothing
uiOB4 = UIOptButton showButtonText (\v o -> o {showButtonText=v}) [True,False]
        (periphery 0 +^ 2 *^ hu +^ 3 *^ hv) ShowButtonTextButton
        (\v -> if v then "Help text enabled" else
        "Help text disabled")
        [IMPlay, IMEdit, IMReplay, IMMeta, IMInit] Nothing
uiOB5 = UIOptButton fullscreen (\v o -> o {fullscreen=v}) [True,False]
        (periphery 0 +^ 4 *^ hu +^ 2 *^ hv) FullscreenButton
        (\v -> if v then "Fullscreen mode active" else "Windowed mode active")
        [IMPlay, IMEdit, IMReplay, IMMeta, IMInit] (Just $ const $ initVideo 0 0)
uiOB6 = UIOptButton useSounds (\v o -> o {useSounds=v}) [True,False]
        (periphery 0 +^ 4 *^ hu +^ hv) UseSoundsButton
        (\v -> if v then "Sound effects enabled" else
        "Sound effects disabled")
        [IMPlay, IMEdit, IMReplay] Nothing

drawUIOptionButtons :: InputMode -> UIM ()
drawUIOptionButtons mode = do
    drawUIOptionButton mode uiOB1
    drawUIOptionButton mode uiOB2
    drawUIOptionButton mode uiOB3
    drawUIOptionButton mode uiOB4
    drawUIOptionButton mode uiOB5
#ifdef SOUND
    drawUIOptionButton mode uiOB6
#endif
drawUIOptionButton im b = when (im `elem` uiOptModes b) $ do
    value <- gets $ getUIOpt b . uiOptions
    renderToMain $ mapM_ (\g -> drawAtRel g (uiOptPos b))
        [HollowGlyph $ obscure purple, uiOptGlyph b value]
describeUIOptionButton :: UIOptButton a -> MaybeT UIM String
describeUIOptionButton b = do
    value <- gets $ getUIOpt b . uiOptions
    return $ uiOptDescr b value
-- XXX: hand-hacking lenses...
toggleUIOption (UIOptButton getopt setopt vals _ _ _ _ monSet) = do
    value <- gets $ getopt . uiOptions
    let value' = cycle vals !! max 0 (1 + fromMaybe 0 (elemIndex value vals))
    modifyUIOptions $ setopt value'
    case monSet of
        Nothing    -> return ()
        Just onSet -> onSet value'

readUIConfigFile :: UIM ()
readUIConfigFile = do
    path <- liftIO $ confFilePath "SDLUI.conf"
    mOpts <- liftIO $ readReadFile path
    case mOpts of
        Just opts -> modify $ \s -> s {uiOptions = opts}
        Nothing   -> return ()
writeUIConfigFile :: UIM ()
writeUIConfigFile = do
    path <- liftIO $ confFilePath "SDLUI.conf"
    opts <- gets uiOptions
    liftIO makeConfDir
    liftIO $ writeFile path $ show opts

readBindings :: UIM ()
readBindings = do
    path <- liftIO $ confFilePath "bindings"
    mbdgs <- liftIO $ readReadFile path
    case mbdgs of
        Just bdgs -> modify $ \s -> s {uiKeyBindings = bdgs}
        Nothing   -> return ()
writeBindings :: UIM ()
writeBindings = do
    path <- liftIO $ confFilePath "bindings"
    bdgs <- gets uiKeyBindings
    liftIO makeConfDir
    liftIO $ writeFile path $ show bdgs

getBindings :: InputMode -> UIM [(Char, Command)]
getBindings mode = do
    uibdgs <- gets (Map.findWithDefault [] mode . uiKeyBindings)
    return $ uibdgs ++ bindings mode

paintTiles :: [ Maybe Tile ]
paintTiles =
    [ Just BallTile
    , Just $ ArmTile zero False
    , Just $ PivotTile zero
    , Just $ SpringTile Relaxed zero
    , Just $ BlockTile []
    , Nothing
    ]

paintTileCmds = map (maybe CmdDelete CmdTile) paintTiles

getEffPaintTileIndex :: UIM Int
getEffPaintTileIndex = do
    mods <- liftIO getModState
    if any (`elem` mods) [KeyModLeftCtrl, KeyModRightCtrl]
    then return $ length paintTiles - 1
    else gets paintTileIndex

paintButtonStart :: HexVec
paintButtonStart = periphery 0 +^ (- length paintTiles `div` 2)*^hv

drawPaintButtons :: UIM ()
drawPaintButtons = do
    pti <- getEffPaintTileIndex
    renderToMain $ sequence_ [
        do
            let gl = case paintTiles!!i of
                    Nothing -> HollowInnerGlyph $ dim purple
                    Just t  -> TileGlyph t $ dim purple
            drawAtRel gl pos
            when selected $ drawAtRel cursorGlyph pos
        | i <- take (length paintTiles) [0..]
        , let pos = paintButtonStart +^ i*^hv
        , let selected = i == pti
        ]

periphery 0 = ((3*maxlocksize)`div`2)*^hu +^ ((3*maxlocksize)`div`4)*^hv
periphery n = rotate n $ periphery 0
-- ^ XXX only peripheries 0,2,3,5 are guaranteed to be on-screen!
--messageLineStart = (maxlocksize+1)*^hw
messageLineCentre = ((maxlocksize+1)`div`2)*^hw +^ ((maxlocksize+1+1)`div`2)*^neg hv

-- XXX HACK: the n=8 line is crowded
titlePos n = titlePos' $ if n == 8 then 9 else n
    where titlePos' n = n*^hv +^ (n`div`2)*^hu

screenWidthHexes,screenHeightHexes :: Int
screenWidthHexes = 32
screenHeightHexes = 26
getGeom :: UIM (SVec, Int)
getGeom = do
    h <- gets scrHeight
    w <- gets scrWidth
    let scrCentre = SVec (w`div`2) (h`div`2)
    -- |size is the greatest integer such that
    -- and [2*size*screenWidthHexes < width
    --        , 3*ysize size*screenHeightHexes < height]
    --        where ysize size = round $ fi size / sqrt 3
    -- Minimum allowed size is 2 (get segfaults on SDL_FreeSurface with 1).
    let size = max 2 $ min ((w-1)`div`(2*screenWidthHexes))
            (floor $ sqrt 3 * (0.5 + fi ((h-1)`div`(3*screenHeightHexes))))
    return (scrCentre, size)

data DrawArgs = DrawArgs [PieceIdx] Bool [Alert] GameState UIOptions
    deriving (Eq, Ord, Show)

drawMainGameState :: [PieceIdx] -> Bool -> [Alert] -> GameState -> UIM ()
drawMainGameState highlight colourFixed alerts st = do
    uiopts <- gets uiOptions
    drawMainGameState' $ DrawArgs highlight colourFixed alerts st uiopts

drawMainGameState' :: DrawArgs -> UIM ()
drawMainGameState' args@(DrawArgs highlight colourFixed alerts st uiopts) = do
    lastArgs <- gets lastDrawArgs
    when (case lastArgs of
            Nothing -> True
            Just (DrawArgs _ _ lastAlerts lastSt _) ->
                lastAlerts /= alerts || lastSt /= st) $
        modify $ \ds -> ds { animFrame = 0, nextAnimFrameAt = Nothing }

    lastAnimFrame <- gets animFrame
    now <- liftIO getTicks
    anim <- gets (maybe False (<now) . nextAnimFrameAt)
    when anim $
        modify $ \ds -> ds { animFrame = lastAnimFrame+1, nextAnimFrameAt = Nothing }
    animFrameToDraw <- gets animFrame
    void $ if lastArgs == Just args && lastAnimFrame == animFrameToDraw
        then do
            vidSurf <- liftIO getVideoSurface
            gsSurf <- gets (fromJust . gsSurface)
            liftIO $ blitSurface gsSurf Nothing vidSurf Nothing
        else do
            modify $ \ds -> ds { lastDrawArgs = Just args }

            -- split the alerts at intermediate states, and associate alerts
            -- to the right states:
            let (globalAlerts,transitoryAlerts) = partition isGlobalAlert alerts
                splitAlerts frameAs (AlertIntermediateState st' : as) =
                    (frameAs,st',True) : splitAlerts [] as
                splitAlerts frameAs (a:as) =
                    splitAlerts (a:frameAs) as
                splitAlerts frameAs [] = [(frameAs,st,False)]
                isGlobalAlert (AlertAppliedForce _)      = False
                isGlobalAlert (AlertIntermediateState _) = False
                isGlobalAlert _                          = True
            let animAlertedStates = nub $
                    let ass = splitAlerts [] transitoryAlerts
                    in if last ass == ([],st,False) then ass else ass ++ [([],st,False)]
            let frames = length animAlertedStates
            let (drawAlerts',drawSt,isIntermediate) = animAlertedStates !! animFrameToDraw
            let drawAlerts = drawAlerts' ++ globalAlerts
            -- let drawAlerts = takeWhile (/= AlertIntermediateState drawSt) alerts
            nextIsSet <- gets (isJust . nextAnimFrameAt)
            when (not nextIsSet && frames > animFrameToDraw+1) $ do
                time <- gets ((if isIntermediate then uiAnimTime else shortUiAnimTime) . uiOptions)
                modify $ \ds -> ds { nextAnimFrameAt = Just $ now + time }

            let board = stateBoard drawSt
            lastCol <- gets dispLastCol
            let coloured = colouredPieces colourFixed drawSt
            let colouring = if useFiveColouring uiopts
                then boardColouring drawSt coloured lastCol
                else pieceTypeColouring drawSt coloured
            modify $ \ds -> ds { dispLastCol = colouring }
            gsSurf <- gets (fromJust . gsSurface)
            renderToMainWithSurf gsSurf $ do
                let tileGlyphs = ownedTileGlyph colouring highlight <$> board

                    applyAlert (AlertAppliedForce f@(Torque idx tdir)) =
                        let poss = case getpp drawSt idx of
                                PlacedPiece pos (Pivot arms) -> pos : map (+^pos) arms
                                PlacedPiece pos (Hook arm _) -> [arm+^pos]
                                _ -> []
                            rotateGlyph (TileGlyph (ArmTile dir _) col) =
                                ArmGlyph (-tdir) dir col
                            rotateGlyph (TileGlyph (PivotTile dir) col) =
                                PivotGlyph (-tdir) dir col
                        in flip (foldr . Map.adjust $ rotateGlyph) poss
                    applyAlert (AlertAppliedForce f@(Push idx dir)) =
                        displaceFootprint . displaceSprings
                        where
                            displace = DisplacedGlyph $ neg dir
                            displaceSpringGlyph isRoot (TileGlyph (SpringTile extn sdir) col) =
                                displaceSpringGlyph isRoot $ SpringGlyph zero zero extn sdir col
                            displaceSpringGlyph isRoot (SpringGlyph rdisp edisp extn sdir col)
                                | isRoot = SpringGlyph (neg dir) edisp extn sdir col
                                | otherwise = SpringGlyph rdisp (neg dir) extn sdir col
                            displaceSpringGlyph _ glyph = glyph
                            displaceFootprint =
                                flip (foldr . Map.adjust $ displace) $
                                    plPieceFootprint $ getpp drawSt idx

                            displaceSpring isRoot c@(Connection root end (Spring sdir _))
                                | dir `elem` [sdir,neg sdir] =
                                    Map.adjust (displaceSpringGlyph isRoot) $
                                        if isRoot
                                            then sdir +^ locusPos drawSt root
                                            else neg sdir +^ locusPos drawSt end
                                | isRoot =
                                    flip (foldr . Map.adjust $ displace) $
                                        connectionFootPrint drawSt c
                                | otherwise = id
                            displaceSpring _ _ = id

                            displaceSprings =
                                flip (foldr $ displaceSpring True) (springsRootAtIdx drawSt idx) .
                                flip (foldr $ displaceSpring False) (springsEndAtIdx drawSt idx)
                    applyAlert _ = id

                    applyAlerts = flip (foldr applyAlert) drawAlerts

                erase
                sequence_ [ drawAt glyph pos |
                    (pos,glyph) <- Map.toList $ applyAlerts tileGlyphs
                    ]

                when (showBlocks uiopts /= ShowBlocksNone) $ sequence_
                    $ [drawBlocked drawSt colouring False force |
                   showBlocks uiopts == ShowBlocksAll,
                   AlertBlockedForce force <- drawAlerts]
                    ++ [ drawBlocked drawSt colouring True force
                        | AlertBlockingForce force <- drawAlerts ]
                    -- ++ [ drawBlocked drawSt colouring True force |
                    -- AlertResistedForce force <- drawAlerts ]
                    ++ [ drawAt CollisionMarker pos
                        | AlertCollision pos <- drawAlerts ]
                    -- ++ [ drawApplied drawSt colouring force
                    --    | AlertAppliedForce force <- drawAlerts ]
            vidSurf <- liftIO getVideoSurface
            liftIO $ blitSurface gsSurf Nothing vidSurf Nothing

playAlertSounds :: GameState -> [Alert] -> UIM ()
#ifdef SOUND
playAlertSounds st alerts = do
    use <- useSounds <$> gets uiOptions
    when use $ mapM_ (maybe (return ()) playSound . alertSound) alerts
    where
        alertSound (AlertBlockedForce force) =
            let PlacedPiece _ piece = getpp st $ forceIdx force
            in case piece of
                Wrench _ -> Just "wrenchblocked"
                Hook _ _ -> if isPush force then Just "hookblocked" else Just "hookarmblocked"
                _ -> Nothing
        alertSound (AlertDivertedWrench _) = Just "wrenchscrape"
        alertSound (AlertAppliedForce (Torque idx _))
            | isPivot.placedPiece.getpp st $ idx = Just "pivot"
            | isTool.placedPiece.getpp st $ idx = Just "toolmove"
        alertSound (AlertAppliedForce (Push idx dir))
            | isBall.placedPiece.getpp st $ idx = Just "ballmove"
            | isTool.placedPiece.getpp st $ idx = Just "toolmove"
            | otherwise = do
            (align,newLen) <- listToMaybe [(align,newLen)
                | c@(Connection (startIdx,_) (endIdx,_) (Spring outDir natLen)) <- connections st
                , let align = (if outDir == dir then 1 else if outDir == neg dir then -1 else 0)
                        * (if idx == startIdx then 1 else if idx == endIdx then -1 else 0)
                , align /= 0
                , let newLen = connectionLength st c ]
            return $ "spring" ++ (if align == 1 then "contract" else "extend")
                ++ show (min newLen 12)
        alertSound AlertUnlocked = Just "unlocked"
        alertSound _ = Nothing
        playSound :: String -> UIM ()
        playSound sound = void.runMaybeT $ do
            ss <- MaybeT $ Map.lookup sound <$> gets sounds
            guard.not.null $ ss
            liftIO $ randFromList ss >>= \(Just s) -> void $ tryPlayChannel (-1) s 0
        randFromList :: [a] -> IO (Maybe a)
        randFromList [] = return Nothing
        randFromList as = (Just.(as!!)) <$> randomRIO (0,length as - 1)
#else
playAlertSounds _ _ = return ()
#endif


drawMiniLock :: Lock -> HexVec -> UIM ()
drawMiniLock lock v = do
    surface <- gets miniLocks >>= maybe new return . Map.lookup lock
    renderToMain $ blitAt surface v
    where
        miniLocksize = 3
        new = do
            (_, size) <- getGeom
            let minisize = size `div` ceiling (lockSize lock % miniLocksize)
            let width = size*2*(miniLocksize*2+1)
            let height = ceiling $ fi size * sqrt 3 * fi (miniLocksize*2+1+1)
            surf <- liftIO $ createRGBSurface [] width height 16 0 0 0 0
            liftIO $ setColorKey surf [SrcColorKey,RLEAccel] $ Pixel 0
            uiopts <- gets uiOptions
            let st = snd $ reframe lock
                coloured = colouredPieces False st
                colouring = if useFiveColouring uiopts
                    then boardColouring st coloured Map.empty
                    else pieceTypeColouring st coloured
                draw = sequence_ [ drawAt glyph pos |
                    (pos,glyph) <- Map.toList $ ownedTileGlyph colouring [] <$> stateBoard st ]
            liftIO $ runRenderM draw emptyCachedGlyphs $
                RenderContext surf Nothing (PHS zero) (SVec (width`div`2) (height`div`2)) zero minisize Nothing
            clearOldMiniLocks
            modify $ \ds -> ds { miniLocks = Map.insert lock surf $ miniLocks ds }
            return surf

-- | TODO: do this more cleverly
clearOldMiniLocks =
    (>=50).Map.size <$> gets miniLocks >>? clearMiniLocks
clearMiniLocks = modify $ \ds -> ds { miniLocks = Map.empty}

drawEmptyMiniLock v =
    renderToMain $ recentreAt v $ rescaleRender 6 $ drawAtRel (HollowInnerGlyph $ dim white) zero

getBindingStr :: InputMode -> UIM (Command -> String)
getBindingStr mode = do
    setting <- gets settingBinding
    uibdgs <- gets (Map.findWithDefault [] mode . uiKeyBindings)
    return (\cmd ->
        if Just cmd == setting then "??"
        else maybe "" showKey $ findBinding (uibdgs ++ bindings mode) cmd)

drawButtons :: InputMode -> UIM ()
drawButtons mode = do
    buttons <- getButtons mode
    bindingStr <- getBindingStr mode
    showBT <- gets (showButtonText . uiOptions)
    smallFont <- gets dispFontSmall
    renderToMain $ sequence_ $ concat [ [ do
                drawAtRel (ButtonGlyph col) v
                (if length bdg > 2 then withFont smallFont else id) $
                    renderStrColAt buttonTextCol bdg v
                when showBT $
                    withFont smallFont $ recentreAt v $ rescaleRender (1/4) $
                    sequence_ [ renderStrColAtLeft white s dv | (s,dv) <- helps ]
            | (i,(v,bdg,helps)) <- enumerate $ map (\b->(buttonPos b, bindingStr $ buttonCmd b, buttonHelp b)) buttonGroup
                , let col = dim $ colourWheel (base+inc*i) ]
        | (buttonGroup,(base,inc)) <- buttons
        ]
    where enumerate = zip [0..]

initMisc :: IO ()
initMisc = void $ enableUnicode True >> enableKeyRepeat 250 30 >> setCaption "intricacy" "intricacy"

initVideo :: Int -> Int -> UIM ()
initVideo w h = do
    liftIO $ (((w,h)==(0,0) &&) . (InitVideo `elem`) <$> wasInit [InitVideo]) >>?
        -- reset video so that passing (0,0) to setVideoMode sets to
        -- current screen res rather than current window size
        (quitSubSystem [InitVideo] >> initSubSystem [InitVideo] >> initMisc)

    fs <- gets (fullscreen . uiOptions)
    liftIO $ do
        (w',h') <- if fs || (w,h)/=(0,0) then return (w,h) else
#ifdef APPLE_SDL1
            -- use smaller dimensions than the screen's, to work around a bug
            -- seen on mac, whereby a resizable window created with
            -- (w,h)=(0,0), or even with the (w,h) given by getHardwareDimensions
            -- after creating such a window, is reported to be larger than it
            -- is.
            do
                (w',h') <- getHardwareDimensions
                return (4*w'`div`5,4*h'`div`5)
#else
            getHardwareDimensions
#endif
        setVideoMode w' h' 0 $ if fs then [Fullscreen] else [Resizable]

    (w',h') <- liftIO getSurfaceDimensions

    modify $ \ds -> ds { scrWidth = w' }
    modify $ \ds -> ds { scrHeight = h' }
    gssurf <- liftIO $ createRGBSurface [] w' h' 16 0 0 0 0
    modify $ \ds -> ds { gsSurface = Just gssurf, lastDrawArgs = Nothing }

    (_,size) <- getGeom
    let fontfn = "VeraMoBd.ttf"
    fontpath <- liftIO $ getDataPath fontfn
    font <- liftIO $ TTF.tryOpenFont fontpath size
    smallFont <- liftIO $ TTF.tryOpenFont fontpath (2*size`div`3)
    modify $ \ds -> ds { dispFont = font, dispFontSmall = smallFont }

    useBG <- gets $ useBackground . uiOptions
    mbg <- if useBG then do
            bgsurf <- liftIO $ createRGBSurface [] w' h' 16 0 0 0 0
            renderToMainWithSurf bgsurf $ drawBasicBG $ 2*max screenWidthHexes screenHeightHexes`div`3
            return $ Just bgsurf
        else return Nothing
    modify $ \ds -> ds { bgSurface = mbg }

    clearMiniLocks

    when (isNothing font) $ liftIO $ do
        now <- getCurrentTime
        let text = show now ++ ": Warning: font file not found at "++fontpath++".\n"
        putStr text
        appendFile "intricacy-warnings.log" text

    where
        getHardwareDimensions = (videoInfoWidth &&& videoInfoHeight) <$> getVideoInfo
        getSurfaceDimensions = (surfaceGetWidth &&& surfaceGetHeight) <$> getVideoSurface


initAudio :: UIM ()
#ifdef SOUND
initAudio = do
    initialised <- liftIO $ tryOpenAudio defaultFrequency AudioS16Sys 1 1024
    unless initialised $ liftIO $ do
        now <- getCurrentTime
        let text = show now ++ ": Warning: audio failed to initialise.\n"
        putStr text
        appendFile "intricacy-warnings.log" text
    -- liftIO $ querySpec >>= print
    liftIO $ allocateChannels 16
    let seqWhileJust (m:ms) = m >>= \ret -> case ret of
            Nothing -> return []
            Just a  -> (a:) <$> seqWhileJust ms
    soundsdir <- liftIO $ getDataPath "sounds"
    sounds <- sequence [ do
            chunks <- liftIO $ seqWhileJust
                [ runMaybeT $ do
                    chunk <- msum $ map (MaybeT . tryLoadWAV) paths
                    liftIO $ volumeChunk chunk vol
                    return chunk
                | n <- [1..]
                , let paths = [soundsdir ++ [pathSeparator] ++ sound ++
                        "-" ++ (if n < 10 then ('0':) else id) (show n) ++ ext
                        | ext <- [".ogg", ".wav"] ]
                , let vol = case sound of
                        "pivot"        -> 64
                        "wrenchscrape" -> 64
                        "toolmove"     -> 64
                        _              -> 128
                ]
            return (sound,chunks)
        | sound <- ["hookblocked","hookarmblocked","wrenchblocked","wrenchscrape","pivot","unlocked","ballmove","toolmove"]
                ++ ["spring" ++ d ++ show l | d <- ["extend","contract"], l <- [1..12]] ]
    -- liftIO $ print sounds
    modify $ \s -> s { sounds = Map.fromList sounds }
#else
initAudio = return ()
#endif

pollEvents = do
    e <- pollEvent
    case e of
        NoEvent -> return []
        _ -> do
            es <- pollEvents
            return $ e:es

drawMsgLine = void.runMaybeT $ do
    (col,str) <- msum
        [ MaybeT $ gets message
        , (dimWhiteCol,) <$> MaybeT (gets hoverStr)
        ]
    lift $ do
        renderToMain $ blankRow messageLineCentre
        smallFont <- gets dispFontSmall
        renderToMain $
            (if length str > screenWidthHexes * 3 then withFont smallFont else id) $
                renderStrColAtCentre col str messageLineCentre

setMsgLineNoRefresh col str = do
    modify $ \s -> s { message = Just (col,str) }
    drawMsgLine
setMsgLine col str = setMsgLineNoRefresh col str >> refresh

clearMsg :: UIM ()
clearMsg = modify $ \s -> s { message = Nothing }

drawTitle (Just (title,n)) = renderToMain $ renderStrColAtCentre messageCol title (titlePos n)
drawTitle Nothing      = return ()

say = setMsgLine messageCol
sayError = setMsgLine errorCol

miniLockPos = (-9)*^hw +^ hu
lockLinePos = 4*^hu +^ miniLockPos
serverPos = 12*^hv +^ 7*^neg hu
serverWaitPos = serverPos +^ hw +^ hu
randomNamesPos = 9*^hv +^ 2*^ neg hu
codenamePos = (-6)*^hw +^ 6*^hv
undeclsPos = 13*^neg hu
accessedOursPos = 2*^hw +^ codenamePos
locksPos = hw+^neg hv
retiredPos = locksPos +^ 11*^hu +^ neg hv
interactButtonsPos = 9*^neg hu +^ 8*^hw
scoresPos = codenamePos +^ 5*^hu +^ 2*^neg hv
