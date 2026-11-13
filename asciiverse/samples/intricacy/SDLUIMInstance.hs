-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module SDLUIMInstance () where

import           Control.Applicative
import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Array
import           Data.Foldable              (for_)
import           Data.Function              (on)
import           Data.List
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import qualified Data.Vector                as Vector
import           Data.Word
import           Graphics.UI.SDL            hiding (flip, name)
import qualified Graphics.UI.SDL            as SDL
import qualified Graphics.UI.SDL.TTF        as TTF
import           Safe                       (maximumBound)
import           System.Timeout
--import Debug.Trace (traceShow)

import           Cache
import           Command
import           Database
import           GameStateTypes
import           Hex
import           InputMode
import           KeyBindings
import           Lock
import           MainState
import           Metagame
import           Mundanities
import           Protocol
import           SDLGlyph
import           SDLRender
import           SDLUI
import           ServerAddr
import           Util

instance UIMonad (StateT UIState IO) where
    runUI m = evalStateT m nullUIState
    drawMainState = do
        lift $ clearButtons >> clearSelectables
        s <- get
        let mode = ms2im s
        lift waitFrame
        drawMainState' s
        lift . drawTitle =<< getTitle
        lift $ do
            drawButtons mode
            drawUIOptionButtons mode
            updateHoverStr mode
            drawMsgLine
            drawShortMouseHelp mode s
            refresh
    clearMessage = clearMsg
    drawMessage = say
    drawPrompt full s = say $ s ++ (if full then "" else "_")
    endPrompt = clearMsg
    drawError = sayError

    reportAlerts = playAlertSounds
    onPhysicsTick = pure ()

    getChRaw = resetMouseButtons >> getChRaw'
        where
            resetMouseButtons = modify $ \s -> s
                { leftButtonDown = Nothing
                , middleButtonDown = Nothing
                , rightButtonDown = Nothing
                }
            getChRaw' = do
                events <- liftIO getEvents
                if not.null $ [ True | MouseButtonDown _ _ ButtonRight <- events ]
                then return Nothing
                else maybe getChRaw' (return.Just) $ listToMaybe $ [ ch
                    | KeyDown (Keysym _ _ ch) <- events
                    , ch /= '\0' ]

    setUIBinding mode cmd ch =
        modify $ \s -> s { uiKeyBindings =
                Map.insertWith (\ [bdg] bdgs -> if bdg `elem` bdgs then delete bdg bdgs else bdg:bdgs)
                    mode [(ch,cmd)] $ uiKeyBindings s }

    getUIBinding mode cmd = ($ cmd) <$> getBindingStr mode

    initUI = (isJust <$>) . runMaybeT $ do
        let toInit = [InitVideo]
#ifdef SOUND
                ++ [InitAudio]
#endif
        catchIOErrorMT $ SDL.init toInit
        liftIO (SDL.wasInit [InitVideo]) >>= guard . (InitVideo `elem`)
        catchIOErrorMT TTF.init
        lift $ do
            readUIConfigFile
            initVideo 0 0
            liftIO initMisc
            w <- gets scrWidth
            h <- gets scrHeight
            liftIO $ warpMouse (fi $ w`div`2) (fi $ h`div`2)
            renderToMain erase
            initAudio
            readBindings
        where
            catchIOErrorMT m = MaybeT . liftIO . warnIOErrAlt $ m >> return (Just ())

    endUI = do
        writeUIConfigFile
        writeBindings
        liftIO quit

    unblockInput = return $ pushEvent VideoExpose
    suspend = return ()
    redraw = return ()

    impatience ticks = do
        liftIO $ threadDelay 50000
        if ticks>20 then do
                let pos = serverWaitPos
                smallFont <- gets dispFontSmall
                renderToMain $ do
                    mapM_ (drawAtRel (FilledHexGlyph $ bright black)) [ pos +^ i*^hu | i <- [0..3] ]
                    withFont smallFont $
                        renderStrColAtLeft errorCol ("waiting..."++replicate ((ticks`div`5)`mod`3) '.') pos
                clearButtons
                registerButton (pos +^ neg hv) CmdQuit 0 [("abort",hu+^neg hw)]
                drawButtons IMImpatience
                refresh
                cmds <- getInput IMImpatience
                return $ CmdQuit `elem` cmds
            else return False

    warpPointer pos = do
        (scrCentre, size) <- getGeom
        centre <- gets dispCentre
        let SVec x y = hexVec2SVec size (pos-^centre) +^ scrCentre
        liftIO $ warpMouse (fi x) (fi y)
        lbp <- gets leftButtonDown
        rbp <- gets rightButtonDown
        let [lbp',rbp'] = ((const $ pos -^ centre) <$>) <$> [lbp,rbp]
        modify $ \s -> s {leftButtonDown = lbp', rightButtonDown = rbp'}

    getUIMousePos = do
        centre <- gets dispCentre
        gets ((Just.(+^centre).fst) . mousePos)

    setYNButtons = do
        clearButtons
        registerButton (periphery 5 +^ hw +^ neg hv) (CmdInputChar 'Y') 2 [("confirm",hu+^neg hw)]
        drawButtons IMTextInput
        refresh

    toggleColourMode = modify $ \s -> s {uiOptions = (uiOptions s){
        useFiveColouring = not $ useFiveColouring $ uiOptions s}}

    getInput mode = do
        fps <- gets fps
        events <- liftIO $ nubMouseMotions <$> getEventsTimeout (10^6`div`fps)
        (cmds,uiChanged) <- if null events then return ([],False) else do
            oldUIState <- get
            cmds <- concat <$> mapM processEvent events
            setPaintFromCmds cmds
            newUIState <- get
            return (cmds,uistatesMayVisiblyDiffer oldUIState newUIState)
        now <- liftIO getTicks
        animFrameReady <- gets (maybe False (<now) . nextAnimFrameAt)
        unless (null cmds) clearMsg
        return $ cmds ++ [CmdRefresh | uiChanged || animFrameReady]
        where
            nubMouseMotions evs =
                -- drop all but last mouse motion and resize events
                let nubMouseMotions' (False,r) (mm@MouseMotion {}:evs) = mm:nubMouseMotions' (True,r) evs
                    nubMouseMotions' (m,False) (vr@(VideoResize _ _):evs) = vr:nubMouseMotions' (m,True) evs
                    nubMouseMotions' b (MouseMotion {}:evs) = nubMouseMotions' b evs
                    nubMouseMotions' b (VideoResize _ _:evs) = nubMouseMotions' b evs
                    nubMouseMotions' b (ev:evs) = ev:nubMouseMotions' b evs
                    nubMouseMotions' _ [] = []
                in reverse $ nubMouseMotions' (False,False) $ reverse evs
            setPaintFromCmds cmds = sequence_
                [ modify $ \s -> s { paintTileIndex = pti }
                    | (pti,pt) <- zip [0..] paintTiles
                    , cmd <- cmds
                    , (isNothing pt && cmd == CmdDelete) ||
                        isJust (do
                            pt' <- pt
                            CmdTile t <- Just cmd
                            guard $ ((==)`on`tileType) t pt') ]

            uistatesMayVisiblyDiffer uis1 uis2 =
                uis1 { mousePos = (zero,False), lastFrameTicks=0 }
                /= uis2 {mousePos = (zero,False), lastFrameTicks=0 }
            processEvent (KeyDown (Keysym _ _ ch)) = case mode of
                IMTextInput -> return [CmdInputChar ch]
                _ -> do
                    setting <- gets settingBinding
                    if isJust setting && ch /= '\0'
                    then do
                        modify $ \s -> s {settingBinding = Nothing}
                        when (ch /= '\ESC') $ setUIBinding mode (fromJust setting) ch
                        return []
                    else do
                        uibdgs <- gets (Map.findWithDefault [] mode . uiKeyBindings)
                        let mCmd = lookup ch $ uibdgs ++ bindings mode
                        return $ maybeToList mCmd
            processEvent MouseMotion {} = do
                (oldMPos,_) <- gets mousePos
                (pos@(mPos,_),(sx,sy,sz)) <- getMousePos
                updateMousePos mode pos
                lbp <- gets leftButtonDown
                rbp <- gets rightButtonDown
                centre <- gets dispCentre
                let drag :: Maybe HexVec -> Maybe Command
                    drag bp = do
                        fromPos@(HexVec x y z) <- bp
                        -- check we've dragged at least a full hex's distance:
                        guard $ not.all (\(a,b) -> abs (fi a - b) < 1.0) $ [(x,sx),(y,sy),(z,sz)]
                        let dir = hexVec2HexDirOrZero $ mPos -^ fromPos
                        guard $ dir /= zero
                        return $ CmdDrag (fromPos+^centre) dir
                case mode of
                    IMEdit -> case drag rbp of
                        Just cmd -> return [cmd]
                        Nothing -> if mPos /= oldMPos
                            then do
                                pti <- getEffPaintTileIndex
                                return $ CmdMoveTo (mPos +^ centre) :
                                    ([CmdPaintFromTo (paintTiles!!pti) (oldMPos+^centre) (mPos+^centre) | isJust lbp])
                            else return []
                    IMPlay -> return $ maybeToList $ msum $ map drag [lbp, rbp]
                    _ -> return []
                where
                    mouseFromTo from to = do
                        let dir = hexVec2HexDirOrZero $ to -^ from
                        if dir /= zero
                            then (CmdDir WHSSelected dir:) <$> mouseFromTo (from +^ dir) to
                            else return []
            processEvent (MouseButtonDown _ _ ButtonLeft) = do
                pos@(mPos,central) <- gets mousePos
                modify $ \s -> s { leftButtonDown = Just mPos }
                rb <- gets (isJust . rightButtonDown)
                mcmd <- cmdAtMousePos pos mode (Just False)
                let hotspotAction = listToMaybe
                        $ map (\cmd -> return [cmd]) (maybeToList mcmd)
                        ++ [ modify (\s -> s {paintTileIndex = i}) >> return []
                            | i <- take (length paintTiles) [0..]
                            , mPos == paintButtonStart +^ i*^hv ]
                        ++ [ toggleUIOption uiOB1 >> updateHoverStr mode >> return []
                            | mPos == uiOptPos uiOB1 && mode `elem` uiOptModes uiOB1 ]
                        ++ [ toggleUIOption uiOB2 >> updateHoverStr mode >> return []
                            | mPos == uiOptPos uiOB2 && mode `elem` uiOptModes uiOB2 ]
                        ++ [ toggleUIOption uiOB3 >> updateHoverStr mode >> return []
                            | mPos == uiOptPos uiOB3 && mode `elem` uiOptModes uiOB3 ]
                        ++ [ toggleUIOption uiOB4 >> updateHoverStr mode >> return []
                            | mPos == uiOptPos uiOB4 && mode `elem` uiOptModes uiOB4 ]
                        ++ [ toggleUIOption uiOB5 >> updateHoverStr mode >> return []
                            | mPos == uiOptPos uiOB5 && mode `elem` uiOptModes uiOB5 ]
#ifdef SOUND
                        ++ [ toggleUIOption uiOB6 >> updateHoverStr mode >> return []
                            | mPos == uiOptPos uiOB6 && mode `elem` uiOptModes uiOB6 ]
#endif

                if rb
                then return [ CmdWait ]
                else flip fromMaybe hotspotAction $ case mode of
                    IMEdit -> do
                        pti <- getEffPaintTileIndex
                        return [ drawCmd (paintTiles!!pti) False ]
                    IMPlay -> do
                        centre <- gets dispCentre
                        return [ CmdManipulateToolAt $ mPos +^ centre ]
                    _ -> return []
            processEvent (MouseButtonUp _ _ ButtonLeft) = do
                modify $ \s -> s { leftButtonDown = Nothing }
                return []
            processEvent (MouseButtonDown _ _ ButtonRight) = do
                pos@(mPos,_) <- gets mousePos
                modify $ \s -> s { rightButtonDown = Just mPos }
                lb <- gets (isJust . leftButtonDown)
                if lb
                then return [ CmdWait ]
                else (fromMaybe [] <$>) $ runMaybeT $ msum
                    [ do
                        cmd <- MaybeT $ cmdAtMousePos pos mode Nothing
                        guard $ mode /= IMTextInput
                        -- modify $ \s -> s { settingBinding = Just cmd }
                        return [ CmdBind $ Just cmd ]
                    , do
                        cmd <- MaybeT $ cmdAtMousePos pos mode (Just True)
                        return [cmd]
                    , case mode of
                        IMPlay -> return [ CmdClear, CmdWait ]
                        _      -> return [ CmdClear, CmdSelect ] ]
            processEvent (MouseButtonUp _ _ ButtonRight) = do
                modify $ \s -> s { rightButtonDown = Nothing }
                return [ CmdUnselect | mode == IMEdit ]
            processEvent (MouseButtonDown _ _ ButtonWheelUp) = doWheel 1
            processEvent (MouseButtonDown _ _ ButtonWheelDown) = doWheel $ -1
            processEvent (MouseButtonDown _ _ ButtonMiddle) = do
                (mPos,_) <- gets mousePos
                modify $ \s -> s { middleButtonDown = Just mPos }
                rb <- gets (isJust . rightButtonDown)
                return $ [CmdDelete | rb]
            processEvent (MouseButtonUp _ _ ButtonMiddle) = do
                modify $ \s -> s { middleButtonDown = Nothing }
                return []
            processEvent (VideoResize w h) = do
                initVideo w h
                return [ CmdRedraw ]
            processEvent VideoExpose = return [ CmdRefresh ]
            processEvent Quit = return [ CmdForceQuit ]

            processEvent _ = return []

            doWheel dw = do
                rb <- gets (isJust . rightButtonDown)
                mb <- gets (isJust . middleButtonDown)
                if ((rb || mb || mode == IMReplay) && mode /= IMEdit)
                    || (mb && mode == IMEdit)
                then return [ if dw == 1 then CmdRedo else CmdUndo ]
                else if mode /= IMEdit || rb
                then return [ CmdRotate WHSSelected dw ]
                else do
                    modify $ \s -> s { paintTileIndex = (paintTileIndex s + dw) `mod` length paintTiles }
                    return []


            drawCmd mt True        = CmdPaint mt
            drawCmd (Just t) False = CmdTile t
            drawCmd Nothing _      = CmdDelete

            getMousePos :: UIM ((HexVec,Bool),(Double,Double,Double))
            getMousePos = do
                (scrCentre, size) <- getGeom
                (x,y,_) <- lift getMouseState
                let sv = SVec (fi x) (fi y) +^ neg scrCentre
                let mPos@(HexVec x y z) = sVec2HexVec size sv
                let (sx,sy,sz) = sVec2dHV size sv
                let isCentral = all (\(a,b) -> abs (fi a - b) < 0.5)
                        [(x,sx),(y,sy),(z,sz)]
                return ((mPos,isCentral),(sx,sy,sz))
            updateMousePos mode newPos = do
                oldPos <- gets mousePos
                when (newPos /= oldPos) $ do
                    modify $ \ds -> ds { mousePos = newPos }
                    updateHoverStr mode

    showHelp mode HelpPageInput = do
        bdgs <- nub <$> getBindings mode
        smallFont <- gets dispFontSmall
        renderToMain $ do
            erase
            let extraHelpStrs = (["Mouse commands:", "Hover over a button to see keys, right-click to rebind;"]
                        ++ case mode of
                            IMPlay -> ["Click on tool to select, drag to move;",
                                "Click by tool to move; right-click to wait;", "Scroll wheel to rotate hook;",
                                "Scroll wheel with right button held down to undo/redo."]
                            IMEdit -> ["Left-click to draw selected; scroll to change selection;",
                                "Right-click on piece to select, drag to move;",
                                "While holding right-click: left-click to advance time, middle-click to delete;",
                                "Scroll wheel to rotate selected piece; scroll wheel while held down to undo/redo."]
                            IMReplay -> ["Scroll wheel for undo/redo."]
                            IMMeta -> ["Left-clicking on something does most obvious thing;"
                                , "Right-clicking does second-most obvious thing."])
                    : case mode of
                        IMMeta -> [[
                            "Basic game instructions:"
                            , "Choose [C]odename, then [R]egister it;"
                            , "select other players, and [S]olve their locks;"
                            , "go [H]ome, then [E]dit and [P]lace a lock of your own;"
                            , "you can then [D]eclare your solutions."
                            , "Make other players green by solving their locks and not letting them solve yours."]]
                        _ -> []
            when False $ do
                renderStrColAtCentre cyan "Keybindings:" $ (screenHeightHexes`div`4)*^(hv+^neg hw)
                let keybindingsHeight = screenHeightHexes - (3 + length extraHelpStrs + sum (map length extraHelpStrs))
                    bdgWidth = (screenWidthHexes-6) `div` 3
                    showKeys chs = intercalate "/" (map showKeyFriendly chs)
                sequence_ [ with $ renderStrColAtLeft messageCol
                            ( keysStr ++ ": " ++ desc )
                            $ (x*bdgWidth-(screenWidthHexes-6)`div`2)*^hu +^ neg hv +^
                              (screenHeightHexes`div`4 - y`div`2)*^(hv+^neg hw) +^
                              (y`mod`2)*^hw
                    | ((keysStr,with,desc),(x,y)) <- zip [(keysStr,with,desc)
                            | group <- groupBy ((==) `on` snd) $ sortBy (compare `on` snd) bdgs
                            , let cmd = snd $ head group
                            , let desc = describeCommand cmd
                            , not $ null desc
                            , let chs = map fst group
                            , let keysStr = showKeys chs
                            , let with = if True -- 3*(bdgWidth-1) < length desc + length keysStr + 1
                                    then withFont smallFont
                                    else id
                            ]
                        (map (`divMod` keybindingsHeight) [0..])
                    , (x+1)*bdgWidth < screenWidthHexes]
            sequence_ [ renderStrColAtCentre (if firstLine then cyan else messageCol) str
                        $ (screenHeightHexes`div`4 - y`div`2)*^(hv+^neg hw)
                          +^ hw
                          +^ (y`mod`2)*^hw
                | ((str,firstLine),y) <- intercalate [("",False)] (map (`zip`
                (True:repeat False)) extraHelpStrs) `zip`
                --[(keybindingsHeight+1)..]
                [((screenHeightHexes - sum (length <$> extraHelpStrs)) `div` 2)..]
                ]
        refresh
        return True
    showHelp IMInit HelpPageGame = do
        renderToMain $ drawBasicHelpPage ("INTRICACY",red) (initiationHelpText,purple)
        return True
    showHelp IMMeta HelpPageGame = do
        renderToMain $ drawBasicHelpPage ("INTRICACY",red) (metagameHelpText,purple)
        return True
    showHelp IMMeta (HelpPageInitiated n) = do
        renderToMain $ drawBasicHelpPage ("Initiation complete",purple) (initiationCompleteText n,red)
        return True
    showHelp IMEdit HelpPageFirstEdit = do
        renderToMain $ drawBasicHelpPage ("Your first lock:",purple) (firstEditHelpText,green)
        return True
    showHelp _ _ = return False

    onNewMode mode = clearMsg

    withNoBG m = do
        bg <- gets bgSurface
        modify $ \uiState -> uiState{bgSurface=Nothing}
        m
        isNothing <$> gets bgSurface >>?
            modify (\uiState -> uiState{bgSurface=bg})

drawMainState' :: MainState -> MainStateT UIM ()
drawMainState' PlayState { psCurrentState=st, psLastAlerts=alerts,
        wrenchSelected=wsel, psTutLevel=tutLevel, psSolved=solved } = do
    canRedo <- gets (null . psUndoneStack)
    let isTut = isJust tutLevel
    lift $ do
        let selTools = [ idx |
                (idx, PlacedPiece pos p) <- enumVec $ placedPieces st
                , (wsel && isWrench p) || (not wsel && isHook p) ]
        drawMainGameState selTools False alerts st
        lb <- gets (isJust . leftButtonDown)
        rb <- gets (isJust . leftButtonDown)
        when isTut $ do
            centre <- gets dispCentre
            sequence_
                [ registerSelectable (pos -^ centre) 0 $
                    case p of
                        Wrench v -> SelToolWrench $ v /= zero
                        _        -> SelToolHook
                | not $ lb || rb
                , PlacedPiece pos p <- Vector.toList $ placedPieces st
                , isTool p]
        unless (noUndoTutLevel tutLevel) $ do
            registerUndoButtons canRedo
            registerButtonGroup markButtonGroup
        registerButton (periphery 0) CmdOpen (if solved then 2 else 0) $
            ("open", hu+^neg hw) : [("Press-->",9*^neg hu) | solved && isTut]
drawMainState' ReplayState { rsCurrentState=st, rsLastAlerts=alerts } = do
    canRedo <- gets (null . rsMoveStack)
    lift $ do
        drawMainGameState [] False alerts st
        registerUndoButtons canRedo
        renderToMain $ drawCursorAt Nothing
drawMainState' EditState { esGameState=st, esGameStateStack=sts, esUndoneStack=undostack,
        selectedPiece=selPiece, selectedPos=selPos } = lift $ do
    drawMainGameState (maybeToList selPiece) True [] st
    renderToMain $ drawCursorAt $ if isNothing selPiece then Just selPos else Nothing
    registerUndoButtons (null undostack)
    when (isJust selPiece) $ mapM_ registerButtonGroup
        [ singleButton (periphery 2 +^ 3*^hw+^hv) CmdDelete 0 [("delete",hu+^neg hw)]
        , singleButton (periphery 2 +^ 3*^hw) CmdMerge 1 [("merge",hu+^neg hw)]
        ]
    sequence_
        [ unless (any (pred . placedPiece) . Vector.toList $ placedPieces st)
            $ registerButton (periphery 0 +^ d) cmd 2 [("place",hu+^neg hw),(tool,hu+^neg hv)]
        | (pred,tool,cmd,d) <- [
            (isWrench, "wrench", CmdTile $ WrenchTile zero, (-4)*^hv +^ hw),
            (isHook, "hook", CmdTile HookTile, (-3)*^hv +^ hw) ] ]
    drawPaintButtons
drawMainState' InitState {initLocks=initLocks, tutProgress=TutProgress{tutSolved=tutSolved}} = lift $ do
    renderToMain (erase >> drawCursorAt Nothing)
    renderToMain . renderStrColAtCentre white "I N T R I C A C Y" $ 3 *^ (hv +^ neg hw)
    drawInitLock zero
    mapM_ drawInitLock $ Map.keys accessible
    registerButton (tutPos +^ 3 *^ neg hu +^ hv) (CmdSolveInit Nothing) 2
        [("solve",hu+^neg hw),("lock",hu+^neg hv)]
    where
    accessible = accessibleInitLocks tutSolved initLocks
    tutPos = maximumBound 0 (hx <$> Map.keys accessible) *^ neg hu
    name v | v == zero = "TUT"
        | otherwise = maybe "???" initLockName $ Map.lookup v accessible
    solved v | v == zero = tutSolved
        | otherwise = Just True == (initLockSolved <$> Map.lookup v accessible)
    isLast v | v == zero = False
        | otherwise = Just True == (isLastInitLock <$> Map.lookup v accessible)
    drawInitLock v = do
        let pos = tutPos +^ 2 *^ v
        drawNameCol (name v) pos $ if solved v then brightish green else brightish yellow
        renderToMain $ sequence_
            [ (if open then PathGlyph h $ brightish white
                else GateGlyph h $ (if inbounds then dim else bright) white)
                `drawAtRel` (pos +^ h)
            | h <- hexDirs
            , let v' = v +^ h
            , let inbounds = abs (hy v') < 2 && hx v' >= 0 && hz v' <= 0
            , let acc = v' `Map.member` accessible || v' == zero
            , not acc || h `elem` [hu, neg hw, neg hv]
            , let open = inbounds && (solved v || solved v') && (acc || (isLast v && h == hu)) ]
        registerSelectable pos 0 $ if v == zero then SelTut (solved v) else SelInitLock v (solved v)
drawMainState' MetaState {curServer=saddr, undeclareds=undecls,
        cacheOnly=cOnly, curAuth=auth, codenameStack=names,
        randomCodenames=rnamestvar, retiredLocks=mretired, curLockPath=path,
        curLock=mlock, asyncCount=count} = do
    modify $ \ms -> ms { listOffsetMax = True }
    let ourName = authUser <$> auth
    let selName = listToMaybe names
    let home = isJust ourName && ourName == selName
    lift $ renderToMain (erase >> drawCursorAt Nothing)
    lift $ do
        smallFont <- gets dispFontSmall
        renderToMain $ withFont smallFont $ renderStrColAtLeft purple
                (saddrStr saddr ++ if cOnly then " (offline mode)" else "")
            $ serverPos +^ hu

    when (length names > 1) $ lift $ registerButton
        (codenamePos +^ neg hu +^ 2*^hw) CmdBackCodename 0 [("back",3*^hw)]

    runMaybeT $ do
        name <- MaybeT (return selName)
        FetchedRecord fresh err muirc <- lift $ getUInfoFetched 300 name
        pending <- ((>0) <$>) $ liftIO $ readTVarIO count
        lift $ do
            lift $ do
                unless ((fresh && not pending) || cOnly) $ do
                    smallFont <- gets dispFontSmall
                    let str = if pending then "(response pending)" else "(updating)"
                    renderToMain $ withFont smallFont $
                        renderStrColBelow (opaquify $ dim errorCol) str codenamePos
                maybe (return ()) (setMsgLineNoRefresh errorCol) err
                when (fresh && (isNothing ourName || isNothing muirc || home)) $
                    let reg = isNothing muirc || isJust ourName
                    in registerButton (codenamePos +^ 2*^hu)
                        (if reg then CmdRegister else CmdAuth)
                        (if isNothing ourName then 2 else 0)
                        [(if reg then "reg" else "auth", 3*^hw)]
            (if isJust muirc then drawName else drawNullName) name codenamePos
            lift $ registerSelectable codenamePos 0 (SelSelectedCodeName name)
            drawRelScore name (codenamePos+^hu)
            when (isJust muirc) $ lift $
                registerButton retiredPos CmdShowRetired 5 [("retired",hu+^neg hw)]
            for_ muirc $ \(RCUserInfo (_,uinfo)) -> case mretired of
                    Just retired -> do
                        fillArea locksPos
                            (map (locksPos+^) $ zero:[rotate n $ 4*^hu-^4*^hw | n <- [0,2,3,5]])
                            [ \pos -> lift (registerSelectable pos 1 (SelOldLock ls)) >> drawOldLock ls pos
                            | ls <- retired ]
                        lift $ registerButton (retiredPos +^ hv) (CmdPlayLockSpec Nothing) 1 [("play",hu+^neg hw),("no.",hu+^neg hv)]
                    Nothing -> do
                        sequence_ [ drawLockInfo (ActiveLock (codename uinfo) i) mlockinfo |
                            (i,mlockinfo) <- assocs $ userLocks uinfo ]
                        when (isJust $ msum $ elems $ userLocks uinfo) $ lift $ do
                            registerButton interactButtonsPos (CmdSolve Nothing) 2 [("solve",hu+^neg hw),("lock",hu+^neg hv)]
                            when (isJust ourName) $
                                registerButton (interactButtonsPos+^hw) (CmdViewSolution Nothing) 1 [("view",hu+^neg hw),("soln",hu+^neg hv)]

    when home $ do
        lift.renderToMain $ renderStrColAt messageCol
            "Home" (codenamePos+^hw+^neg hv)
        unless (null undecls) $ do
            lift.renderToMain $ renderStrColAtLeft messageCol "Undeclared:" (undeclsPos+^2*^hv+^neg hu)
            lift $ registerButton (undeclsPos+^hw+^neg hu) (CmdDeclare Nothing) 2 [("decl",hv+^4*^neg hu),("soln",hw+^4*^neg hu)]
            fillArea (undeclsPos+^hv)
                (map (undeclsPos+^) $ hexDisc 1 ++ [hu+^neg hw, neg hu+^hv])
                [ \pos -> lift (registerSelectable pos 0 (SelUndeclared undecl)) >> drawActiveLock al pos
                | undecl@(Undeclared _ _ al) <- undecls ]
        lift $ do
            maybe
                (drawEmptyMiniLock miniLockPos)
                ((`drawMiniLock` miniLockPos) <$> fst) mlock
            registerSelectable miniLockPos 1 SelOurLock
            registerButton (miniLockPos+^3*^neg hw+^2*^hu) CmdEdit 2
                [("edit",hu+^neg hw),("lock",hu+^neg hv)]
            registerButton lockLinePos CmdSelectLock 1 []
        lift $ unless (null path) $ do
            renderToMain $ renderStrColAtLeft messageCol (take 16 path) $ lockLinePos +^ hu
            registerSelectable (lockLinePos +^ 2*^hu) 1 SelLockPath
            sequence_
                [ registerButton (miniLockPos +^ 2*^neg hv +^ 2*^hu +^ dv) cmd 1
                    [(dirText,hu+^neg hw),("lock",hu+^neg hv)]
                | (dv,cmd,dirText) <- [(zero,CmdPrevLock,"prev"),(neg hw,CmdNextLock,"next")] ]
        let tested = maybe False (isJust.snd) mlock
        when (isJust mlock && home) $ lift $ registerButton
            (miniLockPos+^2*^neg hw+^3*^hu) (CmdPlaceLock Nothing)
                (if tested then 2 else 1)
                [("place",hu+^neg hw),("lock",hu+^neg hv)]
    rnames <- liftIO $ readTVarIO rnamestvar
    unless (null rnames) $
        fillArea randomNamesPos
            (map (randomNamesPos+^) $ hexDisc 2)
            [ \pos -> lift (registerSelectable pos 0 (SelRandom name)) >> drawName name pos
            | name <- rnames ]

    when (ourName /= selName) $ void $ runMaybeT $ do
        when (isJust ourName) $
            lift.lift $ registerButton (codenamePos +^ hw +^ neg hv) CmdHome 1 [("home",3*^hw)]
        sel <- liftMaybe selName
        us <- liftMaybe ourName
        ourUInfo <- mgetUInfo us
        selUInfo <- mgetUInfo sel
        let accesses = map (uncurry getAccessInfo) [(ourUInfo,sel),(selUInfo,us)]
        let posLeft = scoresPos +^ hw +^ neg hu
        let posRight = posLeft +^ 3*^hu
        size <- snd <$> (lift.lift) getGeom
        lift $ do
            lift.renderToMain $ renderStrColAbove (brightish white) "ESTEEM" scoresPos
            lift $ sequence_ [ registerSelectable (scoresPos+^v) 0 SelRelScore | v <- [hv, hv+^hu] ]
            drawRelScore sel scoresPos
            fillArea (posLeft+^hw) (map (posLeft+^) [zero,hw,neg hv])
                [ \pos -> do
                    lift $ registerSelectable pos 0 (SelScoreLock (Just sel) accessed $ ActiveLock us i)
                    drawNameWithCharAndCol us white (lockIndexChar i) col pos
                    lift $ drawRelScoreGlyph pos relScore
                | i <- [0..2]
                , let accessed = head accesses !! i
                , let (col, relScore)
                        | accessed == Just AccessedPub = (dim pubColour, Just $ -1)
                        | isJust accessed = (dim $ scoreColour $ -3, Just $ -1)
                        | otherwise = (obscure $ scoreColour 3, Nothing) ]
            fillArea (posRight+^hw) (map (posRight+^) [zero,hw,neg hv])
                [ \pos -> do
                    lift $ registerSelectable pos 0 (SelScoreLock Nothing accessed $ ActiveLock sel i)
                    drawNameWithCharAndCol sel white (lockIndexChar i) col pos
                    lift $ drawRelScoreGlyph pos relScore
                | i <- [0..2]
                , let accessed = accesses !! 1 !! i
                , let (col, relScore)
                        | accessed == Just AccessedPub = (dim pubColour, Just 1)
                        | isJust accessed = (dim $ scoreColour 3, Just 1)
                        | otherwise = (obscure $ scoreColour $ -3, Nothing) ]
        (posScore,negScore) <- MaybeT $ (snd<$>) <$> getRelScoreDetails sel
        let (shownPosScore, shownNegScore) = (3 - negScore, 3 - posScore)
        lift.lift $ sequence_
            [ do
                renderToMain $ renderStrColAt (scoreColour score) (sign:show (abs score)) pos
                registerSelectable pos 0 SelRelScoreComponent
            | (sign,score,pos) <-
                [ ('-',-shownNegScore,posLeft+^neg hv+^hw)
                , ('+',shownPosScore,posRight+^neg hv+^hw) ] ]


drawShortMouseHelp mode s = do
    mwhs <- gets $ whsButtons.uiOptions
    showBT <- gets (showButtonText . uiOptions)
    when (showBT && isNothing mwhs) $ do
        let helps = shortMouseHelp mode s
        smallFont <- gets dispFontSmall
        renderToMain $ withFont smallFont $ sequence_
            [ renderStrColAtLeft (dim white) help
                (periphery 3 +^ neg hu +^ (2-n)*^hv )
            | (n,help) <- zip [0..] helps ]
    where
        shortMouseHelp IMPlay PlayState { psTutLevel = tutLevel } =
            [ "LMB: select/move tool"
            , "LMB+drag: move tool" ] ++
            [ "Wheel: turn hook"
            | not $ wrenchOnlyTutLevel tutLevel ] ++
            [ "RMB+Wheel: undo/redo"
            | not $ noUndoTutLevel tutLevel ] ++
            [ "RMB: wait a turn"
            | isNothing tutLevel ]
        shortMouseHelp IMEdit _ =
            [ "LMB: paint; Ctrl+LMB: delete"
            , "Wheel: set paint type"
            , "RMB: select piece; drag to move"
            , "RMB+Wheel: tighten/loosen spring, rotate piece"
            , "RMB+LMB: wait; RMB+MMB: delete piece"
            , "MMB+Wheel: undo/redo"
            ]
        shortMouseHelp IMReplay _ =
            [ "Wheel: advance/regress time" ]
        shortMouseHelp _ _ = []

-- waitEvent' : copy of SDL.Events.waitEvent, with the timeout increased
-- drastically to reduce CPU load when idling.
waitEvent' :: IO Event
waitEvent' = loop
    where loop = do pumpEvents
                    event <- pollEvent
                    case event of
                      NoEvent -> threadDelay 10000 >> loop
                      _       -> return event

getEvents = do
    e <- waitEvent'
    es <- pollEvents
    return $ e:es

getEventsTimeout us = do
    es <- maybeToList <$> timeout us waitEvent'
    es' <- pollEvents
    return $ es++es'

updateHoverStr :: InputMode -> UIM ()
updateHoverStr mode = do
    p@(mPos,isCentral) <- gets mousePos
    showBT <- gets (showButtonText . uiOptions)
    hstr <- runMaybeT $ msum
        [ MaybeT ( cmdAtMousePos p mode Nothing ) >>= lift . describeCommandAndKeys
        , guard showBT >> MaybeT (helpAtMousePos p mode)
        , guard (showBT && mode == IMEdit) >> msum
            [ return $ "set paint mode: " ++ describeCommand (paintTileCmds!!i)
            | i <- take (length paintTiles) [0..]
            , mPos == paintButtonStart +^ i*^hv ]
        , guard (mPos == uiOptPos uiOB1 && mode `elem` uiOptModes uiOB1) >> describeUIOptionButton uiOB1
        , guard (mPos == uiOptPos uiOB2 && mode `elem` uiOptModes uiOB2) >> describeUIOptionButton uiOB2
        , guard (mPos == uiOptPos uiOB3 && mode `elem` uiOptModes uiOB3) >> describeUIOptionButton uiOB3
        , guard (mPos == uiOptPos uiOB4 && mode `elem` uiOptModes uiOB4) >> describeUIOptionButton uiOB4
        , guard (mPos == uiOptPos uiOB5 && mode `elem` uiOptModes uiOB5) >> describeUIOptionButton uiOB5
#ifdef SOUND
        , guard (mPos == uiOptPos uiOB6 && mode `elem` uiOptModes uiOB6) >> describeUIOptionButton uiOB6
#endif
        ]
    modify $ \ds -> ds { hoverStr = hstr }
    where
        describeCommandAndKeys :: Command -> UIM String
        describeCommandAndKeys cmd = do
            uibdgs <- gets (Map.findWithDefault [] mode . uiKeyBindings)
            return $ describeCommand cmd ++ " ["
                ++ intercalate ","
                    (map showKeyFriendly $ findBindings (uibdgs ++ bindings mode) cmd)
                ++ "]"


fillArea :: HexVec -> [HexVec] -> [HexVec -> MainStateT UIM ()] -> MainStateT UIM ()
fillArea centre area draws = do
    offset <- gets listOffset
    let na = length area
        listButton cmd pos = lift $ registerButton pos cmd 3 []
        draws' = if offset > 0 && length draws > na
            then listButton CmdPrevPage :
                drop (max 0 $ na-1 + (na-2)*(offset-1)) draws
            else draws
        (selDraws,allDrawn) = if length draws' > na
            then (take (na-1) draws' ++ [listButton CmdNextPage], False)
            else (take na draws', True)
    unless allDrawn . modify $ \ms -> ms { listOffsetMax = False }
    mapM_ (uncurry ($)) (
        zip selDraws $ sortBy (compare `on` hexVec2SVec 37) $
            take (length selDraws) $ sortBy
                (compare `on` (hexLen . (-^centre)))
                area)

drawOldLock ls pos = void.runMaybeT $ msum [ do
        lock <- mgetLock ls
        lift.lift $ drawMiniLock lock pos
    , lift.lift.renderToMain $
            renderStrColAt messageCol (show ls) pos
    ]


drawName,drawNullName :: Codename -> HexVec -> MainStateT UIM ()
drawName name pos = do
    lift . drawNameCol name pos =<< nameCol name
    lift . drawRelScoreGlyph pos =<< getRelScore name
drawNullName name pos = lift . drawNameCol name pos $ invisible white

drawNameCol name pos col = renderToMain $ do
    drawAtRel (playerGlyph col) pos
    renderStrColAt buttonTextCol name pos

drawRelScoreGlyph pos Nothing = return ()
drawRelScoreGlyph pos relScore = renderToMain . (`drawAtRel` pos) $ ScoreGlyph relScore

drawRelScore name pos = do
    col <- nameCol name
    relScore <- getRelScore name
    flip (maybe (return ())) relScore $ \score ->
        lift $ do
            renderToMain $ renderStrColAt col
                ((if score > 0 then "+" else "") ++ show score) pos
            registerSelectable pos 0 SelRelScore

drawNote note pos = case noteBehind note of
    Just al -> drawActiveLock al pos
    Nothing -> drawPublicNote (noteAuthor note) pos
drawActiveLock al@(ActiveLock name i) pos = do
    accessed <- accessedAL al
    drawNameWithChar name
        (if accessed then accColour else white)
        (lockIndexChar i) pos
drawPublicNote name = drawNameWithChar name pubColour 'P'
drawNameWithChar name charcol char pos = do
    col <- nameCol name
    drawNameWithCharAndCol name charcol char col pos
    lift . drawRelScoreGlyph pos =<< getRelScore name
drawNameWithCharAndCol :: String -> Pixel -> Char -> Pixel -> HexVec -> MainStateT UIM ()
drawNameWithCharAndCol name charcol char col pos = do
    size <- fi.snd <$> lift getGeom
    let up = FVec 0 $ 1/2 - ylen
    let down = FVec 0 ylen
    smallFont <- lift $ gets dispFontSmall
    lift.renderToMain $ do
        drawAtRel (playerGlyph col) pos
        displaceRender up $
            renderStrColAt buttonTextCol name pos
        displaceRender down $ withFont smallFont $
            renderStrColAt charcol [char] pos
pubWheelAngle = 5
pubColour = colourWheel pubWheelAngle -- ==purple
accColour = cyan
nameCol name = do
    ourName <- gets ((authUser <$>) . curAuth)
    relScore <- getRelScore name
    return $ dim $ case relScore of
            Nothing -> Pixel $ if ourName == Just name then 0xc0c0c000 else 0x80808000
            Just score -> scoreColour score
scoreColour :: Int -> Pixel
scoreColour score = Pixel $ case score of
        0    -> 0x80800000
        1    -> 0x70a00000
        2    -> 0x40c00000
        3    -> 0x00ff0000
        (-1) -> 0xa0700000
        (-2) -> 0xc0400000
        (-3) -> 0xff000000

drawLockInfo :: ActiveLock -> Maybe LockInfo -> MainStateT UIM ()
drawLockInfo al@(ActiveLock name idx) Nothing = do
    let centre = hw+^neg hv +^ 7*(idx-1)*^hu
    lift $ drawEmptyMiniLock centre
    drawNameWithCharAndCol name white (lockIndexChar idx) (invisible white) centre
    ourName <- gets ((authUser <$>) . curAuth)
    lift $ registerSelectable centre 3 $ SelLockUnset (ourName == Just name) al
drawLockInfo al@(ActiveLock name idx) (Just lockinfo) = do
    let centre = locksPos +^ 7*(idx-1)*^hu
    let accessedByPos = centre +^ 3*^(hv +^ neg hw)
    let accessedPos = centre +^ 2*^(hw +^ neg hv)
    let notesPos = centre +^ 3*^(hw +^ neg hv)
    ourName <- gets ((authUser <$>) . curAuth)
    runMaybeT $ msum [
        do
            lock <- mgetLock $ lockSpec lockinfo
            lift.lift $ do
                drawMiniLock lock centre
                registerSelectable centre 3 $ SelLock al
        , lift $ do
                drawActiveLock al centre
                lift $ registerSelectable centre 3 $ SelLock al
        ]

    size <- snd <$> lift getGeom
    lift $ do
        renderToMain $ displaceRender (FVec 1 0) $ renderStrColAt (brightish white) "SOLUTIONS" $ accessedByPos +^ hv
        registerSelectable (accessedByPos +^ hv) 0 SelPrivyHeader
        registerSelectable (accessedByPos +^ hv +^ hu) 0 SelPrivyHeader
    if public lockinfo
    then lift $ do
        renderToMain $ renderStrColAt pubColour "Public" accessedByPos
        registerSelectable accessedByPos 1 SelPublicLock
    else if null $ accessedBy lockinfo
        then lift.renderToMain $ renderStrColAt dimWhiteCol "None" accessedByPos
        else fillArea accessedByPos
                [ accessedByPos +^ d | j <- [0..2], i <- [-2..3]
                    , i-j > -4, i-j < 3
                    , let d = j*^hw +^ i*^hu ]
                $ [ \pos -> lift (registerSelectable pos 0 (SelSolution note)) >> drawNote note pos
                    | note <- lockSolutions lockinfo ]

    undecls <- gets undeclareds
    case if isJust $ guard . (|| public lockinfo) . (`elem` map noteAuthor (lockSolutions lockinfo)) =<< ourName
            then if public lockinfo
                then Just (pubColour,"Accessed!",AccessedPublic)
                else Just (accColour, "Solved!",AccessedSolved)
            else if any (\(Undeclared _ ls _) -> ls == lockSpec lockinfo) undecls
                then Just (yellow, "Undeclared",AccessedUndeclared)
                else Nothing
        of
        Just (col,str,selstr) -> lift $ do
            renderToMain $ renderStrColAt col str accessedPos
            registerSelectable accessedPos 1 (SelAccessedInfo selstr)
        Nothing -> do
            read <- take 3 <$> getNotesReadOn lockinfo
            unless (ourName == Just name) $ do
                let readPos = accessedPos +^ (-3)*^hu
                lift.renderToMain $ renderStrColAt (if length read == 3 then accColour else dimWhiteCol)
                    "Read:" readPos
                when (length read == 3) $ lift $ registerSelectable readPos 0 (SelAccessedInfo AccessedReadNotes)
                fillArea (accessedPos+^neg hu) [ accessedPos +^ i*^hu | i <- [-1..1] ]
                    $ take 3 $ [ \pos -> lift (registerSelectable pos 0 (SelReadNote note)) >> drawNote note pos
                        | note <- read ] ++ repeat (\pos -> lift $ registerSelectable pos 0 SelReadNoteSlot >>
                                renderToMain (drawAtRel (HollowGlyph $ dim green) pos))

    lift $ do
        renderToMain $ displaceRender (FVec 1 0) $ renderStrColAt (brightish white) "SECURING" $ notesPos +^ hv
        registerSelectable (notesPos +^ hv) 0 SelNotesHeader
        registerSelectable (notesPos +^ hv +^ hu) 0 SelNotesHeader
    if null $ notesSecured lockinfo
        then lift.renderToMain $ renderStrColAt dimWhiteCol "None" notesPos
        else fillArea notesPos
                [ notesPos +^ d | j <- [0..2], i <- [-2..3]
                    , i-j > -4, i-j < 3
                    , let d = j*^hw +^ i*^hu ]
                [ \pos -> lift (registerSelectable pos 0 (SelSecured note)) >> drawActiveLock (noteOn note) pos
                    | note <- notesSecured lockinfo ]

drawBasicHelpPage :: (String,Pixel) -> ([String],Pixel) -> RenderM ()
drawBasicHelpPage (title,titleCol) (body,bodyCol) = do
    erase
    let startPos = hv +^ (length body `div` 4)*^(hv+^neg hw)
    renderStrColAtCentre titleCol title $ startPos +^ hv +^neg hw
    sequence_
        [ renderStrColAtCentre bodyCol str $
            startPos
              +^ (y`div`2)*^(hw+^neg hv)
              +^ (y`mod`2)*^hw
        | (y,str) <- zip [0..] body ]
