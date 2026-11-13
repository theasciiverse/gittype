-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module InteractUtil where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Data.Array
import           Data.Char
import           Data.Function             (on)
import           Data.List
import qualified Data.Map                  as Map
import           Data.Maybe
import           System.Directory

import           Command
import           EditGameState
import           Frame
import           GameState
import           GameStateTypes
import           Hex
import           InputMode
import           Lock
import           MainState
import           Metagame
import           Mundanities
import           Physics
import           Protocol
import           Util

checkWon :: UIMonad uiM => MainStateT uiM ()
checkWon = do
    st <- gets psCurrentState
    frame <- gets psFrame
    wasSolved <- gets psSolved
    let solved = checkSolved (frame,st)
    when (solved /= wasSolved) $ do
        modify $ \ps -> ps {psSolved = solved}
        obdg <- lift $ getUIBinding IMPlay CmdOpen
        lift $ if solved then do
                drawMessage $ "Unlocked! '"++obdg++"' to open."
                reportAlerts st [AlertUnlocked]
            else clearMessage

doForce force = do
    st <- gets esGameState
    let (st',alerts) = runWriter $ resolveSinglePlForce force st
    lift (reportAlerts st' alerts) >> pushEState st'
drawTile pos tile painting = do
    modify $ \es -> es {selectedPiece = Nothing}
    lastMP <- gets lastModPos
    modifyEState $ modTile tile pos lastMP painting
    modify $ \es -> es {lastModPos = pos}
paintTilePath frame tile from to = if from == to
    then modify $ \es -> es {lastModPos = to}
    else do
        let from' = hexVec2HexDirOrZero (to-^from) +^ from
        when (inEditable frame from') $ drawTile from' tile True
        paintTilePath frame tile from' to

adjustSpringTension :: UIMonad uiM => PieceIdx -> Int -> MainStateT uiM ()
adjustSpringTension p dl = do
    st <- gets esGameState
    let updateConn (Connection r e@(p',_) (Spring d l))
            | p' == p
            , let c' = Connection r e (Spring d $ l + dl)
            , springExtensionValid st c'
            = c'
        updateConn c = c
    pushEState $ st { connections = updateConn <$> connections st }

pushEState :: UIMonad uiM => GameState -> MainStateT uiM ()
pushEState st = do
    st' <- gets esGameState
    sts <- gets esGameStateStack
    when (st' /= st) $ modify $ \es -> es {esGameState = st, esGameStateStack = st':sts, esUndoneStack = []}
pushPState :: UIMonad uiM => (GameState,PlayerMove) -> MainStateT uiM ()
pushPState (st,pm) = do
    st' <- gets psCurrentState
    stms <- gets psGameStateMoveStack
    when (st' /= st) $ modify $ \ps -> ps {psCurrentState = st,
            psGameStateMoveStack = (st',pm):stms, psUndoneStack = []}
modifyEState :: UIMonad uiM => (GameState -> GameState) -> MainStateT uiM ()
modifyEState f = do
    st <- gets esGameState
    pushEState $ f st

doPhysicsTick :: UIMonad uiM => PlayerMove -> GameState -> uiM (GameState, [Alert])
doPhysicsTick pm st = do
    let r@(st',alerts) = runWriter $ physicsTick pm st
    reportAlerts st' alerts
    onPhysicsTick
    return r

nextLock :: Bool -> FilePath -> IO FilePath
nextLock newer path = do
    lockdir <- confFilePath "locks"
    time <- (Just <$> (fullLockPath path >>= getModificationTime))
        `catchIO` const (return Nothing)
    paths <- getDirContentsRec lockdir
    maybe path (drop (length lockdir + 1) . fst) . listToMaybe .
        (if newer then id else reverse) . sortBy (compare `on` snd) .
        filter (maybe (const True)
            (\x y -> (if newer then (<) else (>)) x (snd y)) time) <$>
        (\p -> (,) p <$> getModificationTime p) `mapM` paths

hasLocks :: IO Bool
hasLocks = do
    lockdir <- confFilePath "locks"
    not.null <$> getDirContentsRec lockdir

setLockPath :: UIMonad uiM => FilePath -> MainStateT uiM ()
setLockPath path = do
    lock <- liftIO $ fullLockPath path >>= readLock
    modify $ \ms -> ms {curLockPath = path, curLock = lock}

declare (Undeclared soln ls al) = do
    ourName <- mgetOurName
    ourUInfo <- mgetUInfo ourName
    [pbdg,ebdg,hbdg] <- mapM (lift.lift . getUIBinding IMMeta)
        [ CmdPlaceLock Nothing, CmdEdit, CmdHome ]
    haveLock <- gets (isJust . curLock)
    idx <- askLockIndex "Secure behind which lock?"
        (if haveLock
            then "First you must place ('"++pbdg++"') a lock to secure your solution behind, while at home ('"++hbdg++"')."
            else "First design a lock in the editor ('"++ebdg++"'), behind which to secure your solution.")
        (\i -> isJust $ userLocks ourUInfo ! i)
    guard $ isJust $ userLocks ourUInfo ! idx
    lift $ curServerActionAsyncThenInvalidate
        (DeclareSolution soln ls al idx)
        -- rather than recurse through the tree to find what scores may have
        -- changed as a result of this declaration, or leave it to timeouts
        -- and explicit refreshes to reveal it, we just invalidate all UInfos.
        (Just AllCodenames)

startMark = '^'

marksSet :: UIMonad uiM => MainStateT uiM [Char]
marksSet = do
    mst <- get
    return $ case ms2im mst of
        IMEdit   -> Map.keys $ esMarks mst
        IMPlay   -> Map.keys $ psMarks mst
        IMReplay -> Map.keys $ rsMarks mst
        _        -> []

jumpMark :: UIMonad uiM => Char -> MainStateT uiM ()
jumpMark ch = do
    mst <- get
    void.runMaybeT $ case ms2im mst of
        IMEdit -> do
            st <- liftMaybe $ ch `Map.lookup` esMarks mst
            lift $ setMark True '\'' >> pushEState st
        IMPlay -> do
            mst' <- liftMaybe $ ch `Map.lookup` psMarks mst
            put mst' { psMarks = Map.insert '\'' mst $ psMarks mst }
        IMReplay -> do
            mst' <- liftMaybe $ ch `Map.lookup` rsMarks mst
            put mst' { rsMarks = Map.insert '\'' mst $ rsMarks mst }
        _ -> return ()

setMark :: (Monad m) => Bool -> Char -> MainStateT m ()
setMark overwrite ch = get >>= \mst -> case mst of
    -- ugh... remind me why I'm not using lens?
    EditState { esMarks = marks, esGameState = st } ->
        put $ mst { esMarks = insertMark ch st marks }
    PlayState {} -> put $ mst { psMarks = insertMark ch mst $ psMarks mst }
    ReplayState {} -> put $ mst { rsMarks = insertMark ch mst $ rsMarks mst }
    _ -> return ()
    where insertMark = Map.insertWith $ \new old -> if overwrite then new else old

askLockIndex :: UIMonad uiM => [Char] -> String -> (Int -> Bool) -> MaybeT (MainStateT uiM) Int
askLockIndex prompt failMessage p = do
    let ok = filter p [0,1,2]
    case length ok of
        0 -> (lift.lift) (drawError failMessage) >> mzero
        1 -> return $ head ok
        _ -> ask ok
    where
        ask ok = do
            let prompt' = prompt ++ " [" ++ intersperse ',' (lockIndexChar <$> ok) ++ "]"
            idx <- MaybeT $ lift $ (((charLockIndex<$>).listToMaybe) =<<) <$>
                textInput prompt' 1 False True Nothing Nothing
            if idx `elem` ok then return idx else ask ok
confirmOrBail :: UIMonad uiM => String -> MaybeT (MainStateT uiM) ()
confirmOrBail prompt = (guard =<<) $ lift.lift $ confirm prompt
confirm :: UIMonad uiM => String -> uiM Bool
confirm prompt = do
    setYNButtons
    confirm'
    where
        confirm' = do
            drawPrompt False $ prompt ++ " [y/N] "
            cmds <- getSomeInput IMTextInput
            maybe confirm' ((endPrompt >>) . pure) (msum $ ansOfCmd <$> cmds)
        ansOfCmd (CmdInputChar 'y') = Just True
        ansOfCmd (CmdInputChar 'Y') = Just True
        ansOfCmd (CmdInputChar c)   = if isPrint c then Just False else Nothing
        ansOfCmd CmdRedraw          = Just False
        ansOfCmd CmdRefresh         = Nothing
        ansOfCmd CmdUnselect        = Nothing
        ansOfCmd _                  = Just False

-- | TODO: draw cursor
textInput :: UIMonad uiM => String -> Int -> Bool -> Bool -> Maybe [String] -> Maybe String -> uiM (Maybe String)
textInput prompt maxlen hidden endOnMax mposss base = getText (fromMaybe "" base, Nothing) <* endPrompt
    where
        getText :: UIMonad uiM => (String, Maybe String) -> uiM (Maybe String)
        getText (s0,mstem0) = do
            drawPrompt (length s0 == maxlen) $ prompt ++ " " ++ if hidden then replicate (length s0) '*' else s0
            if endOnMax && isNothing mstem0 && maxlen <= length s0
            then return $ Just $ take maxlen s0
            else do
                cmds <- getSomeInput IMTextInput
                case foldM applyCmd (s0,mstem0) cmds of
                    Left False      -> return Nothing
                    Left True       -> return $ Just s0
                    Right (s,mstem) -> getText (s,mstem)
            where
                applyCmd (s,mstem) (CmdInputChar ch) = case ch of
                    '\ESC' -> Left False
                    '\a' -> Left False -- ^G
                    '\ETX' -> Left False -- ^C
                    '\n' -> Left True
                    '\r' -> Left True
                    '\NAK' -> Right ("",Nothing)  -- ^U
                    '\b' -> Right (take (length s - 1) s, Nothing)
                    '\DEL' -> Right (take (length s - 1) s, Nothing)
                    '\t' -> case mposss of
                        Nothing -> Right (s,mstem)
                        Just possibilities -> case mstem of
                            Nothing -> let
                                completions = filter (completes s) possibilities
                                pref = if null completions then s else
                                    let c = head completions
                                    in head [ c' | n <- reverse [0..length c],
                                        let c'=take n c, all (completes c') completions ]
                                in Right (pref,Just pref)
                            Just stem -> let
                                completions = filter (completes stem) possibilities
                                later = filter (>s) completions
                                s' | null completions = s
                                   | null later = head completions
                                   | otherwise = minimum later
                                in Right (s',mstem)
                    _ -> Right $ if isPrint ch
                            then ((if length s >= maxlen then id else (++[ch])) s, Nothing)
                            else (s,mstem)
                applyCmd x (CmdInputSelLock idx) =
                    setTextOrSubmit x [lockIndexChar idx]
                applyCmd x (CmdInputSelUndecl (Undeclared _ _ (ActiveLock name idx))) =
                    setTextOrSubmit x $ name++[':',lockIndexChar idx]
                applyCmd x (CmdInputCodename name) =
                    setTextOrSubmit x name
                applyCmd x CmdRefresh = Right x
                applyCmd x CmdUnselect = Right x
                applyCmd _ _ = Left False
        completes s s' = take (length s) s' == s
        setTextOrSubmit (s,_) t = if s == t then Left True else Right (t,Nothing)

getSomeInput im = do
    cmds <- getInput im
    if null cmds then getSomeInput im else return cmds
