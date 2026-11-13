-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SDL2RenderCache where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Function              (on)
import qualified Data.Text                  as T
import           Foreign.C.Types            (CInt)
import           SDL                        hiding (get, rotate, zero)
import qualified SDL.Font                   as TTF
import           SDL.Primitive              (Color)

import           BoardColouring
import           Command
import           GameState
import           GameStateTypes
import           Hex
import           SDL2Glyph
import           SDL2Render
import           Util

import qualified SimpleCache                as SC

type SizedGlyph = (Glyph,CInt)
type CachedGlyphs = SC.SimpleCache SizedGlyph Texture
maxCachedGlyphs = 400
emptyCachedGlyphs = SC.empty maxCachedGlyphs destroyTexture

type CachedText = SC.SimpleCache (T.Text, TTF.Font) (Texture, V2 CInt)
maxCachedText = 100
emptyCachedText = SC.empty maxCachedText (destroyTexture . fst)

instance Ord TTF.Font where compare = compare `on` TTF.unwrap

data RenderCache = RenderCache
    { cachedGlyphs :: CachedGlyphs
    , cachedText   :: CachedText
    }
emptyRenderCache = RenderCache emptyCachedGlyphs emptyCachedText

deallocRenderCache :: RenderCache -> IO ()
deallocRenderCache (RenderCache glyphs text) = SC.deallocAll glyphs >> SC.deallocAll text

modGlyphs :: (CachedGlyphs -> CachedGlyphs) -> RenderCache -> RenderCache
modGlyphs f c = c { cachedGlyphs = f $ cachedGlyphs c }

modText :: (CachedText -> CachedText) -> RenderCache -> RenderCache
modText f c = c { cachedText = f $ cachedText c }

type RenderM = RenderT (StateT RenderCache IO)
-- |XXX: caller must call deallocRenderCache on returned cache before destroying
-- the renderer!
runRenderM :: RenderM a -> RenderCache -> RenderContext -> IO (a,RenderCache)
runRenderM m cache rc = runStateT (runReaderT m rc) cache

getOrInsert :: (Eq k, Ord k) => (RenderCache -> SC.SimpleCache k v)
    -> (SC.SimpleCache k v -> (RenderCache -> RenderCache))
    -> k -> RenderM v -> RenderM v
getOrInsert proj setter k m = do
    cache <- lift $ gets proj
    v <- maybe m pure $ cache SC.!? k
    cache' <- liftIO $ SC.insert k v cache
    lift . modify $ setter cache'
    pure v

drawAt :: Glyph -> HexPos -> RenderM ()
drawAt gl pos = do
        centre <- asks renderHCentre
        drawAtRel gl (pos -^ centre)

drawAtRel :: Glyph -> HexVec -> RenderM ()
drawAtRel gl v = recentreAt v $ renderGlyphCaching gl

renderGlyphCaching :: Glyph -> RenderM ()
renderGlyphCaching gl = do
    size <- asks renderSize
    let sgl = (gl,size)
        -- |larger than you might expect, to handle DisplacedGlyph
        w = size*4 + 1
        h = ysize size*8 + 1
        newGlyphSurf = do
            csurf <- createRGBSurface (V2 w h) RGB888
            surfaceColorKey csurf $= Just (V4 0 0 0 0)
            return csurf
        renderOnCache csurf = do
            crend <- liftIO $ createSoftwareRenderer csurf
            let ccxt rc = rc { renderer = crend, renderSCentre = SVec (w`div`2) (h`div`2), renderOffset = zero }
            local ccxt $ renderGlyph gl
            liftIO $ destroyRenderer crend
            rend <- asks renderer
            texture <- createTextureFromSurface rend csurf
            textureBlendMode texture $= BlendAlphaBlend
            pure texture
        blitGlyph texture = do
            V2 x y <- renderPos zero
            blitAtSVec texture $ SVec x y
    if cacheable gl
    then blitGlyph <=< getOrInsert cachedGlyphs (modGlyphs . const) sgl $ do
            csurf <- newGlyphSurf
            texture <- renderOnCache csurf
            freeSurface csurf
            pure texture
    else renderGlyph gl
    where
    -- Blocktiles need careful FVec calculations to join up right
    cacheable (TileGlyph (BlockTile adjs) _) | not (null adjs) = False
    cacheable (HollowGlyph _) = False
    cacheable (FilledHexGlyph _) = False
    cacheable _ = True

renderGlyph :: Glyph -> RenderM ()
renderGlyph (TileGlyph (BlockTile adjs) col) =
    rimmedPolygonR corners col $ bright col
        where
            corners = concat [
                if any adjAt [0,1]
                then [corner $ hextant dir]
                else [innerCorner dir | not (adjAt $ -1)]
                | dir <- hexDirs
                , let adjAt r = rotate r dir `elem` adjs
                ]

renderGlyph (TileGlyph (SpringTile extn dir) col) =
    renderGlyph $ SpringGlyph zero zero extn dir col

renderGlyph (TileGlyph (PivotTile dir) col) = do
    renderGlyph $ PivotGlyph 0 dir col

renderGlyph (TileGlyph (ArmTile dir _) col) =
    renderGlyph $ ArmGlyph 0 dir col

renderGlyph (TileGlyph HookTile col) =
    rimmedCircleR zero (7/8) col $ bright col

renderGlyph (TileGlyph (WrenchTile mom) col) = do
    rimmedCircleR zero (1/3) col $ bright col
    when (mom /= zero) $
        let
            from = innerCorner $ neg mom
            to = edge $ neg mom
            shifts = [(1 / 2) **^ (b -^ a) |
               let a = innerCorner $ neg mom,
               rot <- [- 1, 0, 1],
               let b = innerCorner $ rotate rot $ neg mom]
        in sequence_
            [ aaLineR (from+^shift) (to+^shift) col
            | shift <- shifts ]

renderGlyph (TileGlyph BallTile col) =
    rimmedCircleR zero (7/8) (faint col) (obscure col)

renderGlyph (SpringGlyph rootDisp endDisp extn dir col) =
    thickLinesR points 1 $ brightness col
        where
            n :: Int
            n = 3*case extn of
                Stretched  -> 1
                Relaxed    -> 2
                Compressed -> 4
            brightness = dim
            dir' = if dir == zero then hu else dir
            s = corner (hextant dir' - 1) +^ innerCorner endDisp
            off = corner (hextant dir') +^ innerCorner endDisp
            e = corner (hextant dir' - 3) +^ innerCorner rootDisp
            points = [ b +^ (fi i / fi n) **^ (e -^ s)
                | i <- [0..n]
                , i`mod`3 /= 1
                , let b = if i`mod`3==0 then s else off ]

renderGlyph (PivotGlyph rot dir col) = do
    rimmedCircleR zero (7/8) col $ bright col
    when (dir /= zero)
        $ aaLineR from to $ bright col
        where
            from = rotFVec th c $ (7/8) **^ edge (neg dir)
            to = rotFVec th c $ (7/8) **^ edge dir
            c = FVec 0 0
            th = - fi rot * pi / 12

renderGlyph (ArmGlyph rot dir col) =
    thickLineR from to 1 $ bright col
        where
            dir' = if dir == zero then hu else dir
            from = rotFVec th c $ edge $ neg dir'
            to = rotFVec th c $ innerCorner dir'
            c = 2 **^ edge (neg dir')
            th = - fi rot * pi / 12

renderGlyph (BlockedArm armdir tdir col) =
    thickLineR from to (1/3) col
        where
            from = innerCorner $ rotate (2*tdir) armdir
            to = edge $ rotate tdir armdir

renderGlyph (TurnedArm armdir tdir col) =
    sequence_ [ arcR c r a1 a2 col | r <- [8/4,9/4] ]
    where
        c = hexVec2FVec $ neg armdir
        a0 = fi $ -60*hextant armdir
        a1' = a0 + fi tdir * 10
        a2' = a0 + fi tdir * 30
        a1 = min a1' a2'
        a2 = max a1' a2'

renderGlyph (BlockedBlock tile dir col) =
    displaceRender shift $ renderGlyph (TileGlyph tile col)
        where shift = innerCorner dir -^ edge dir

renderGlyph (BlockedPush dir col) = do
    thickLineR zero tip 1 col
    thickLineR tip (head arms) 1 col
    thickLineR tip (arms!!1) 1 col
    where
        tip@(FVec tx ty) = edge dir
        arms = [ FVec ((tx/2) + d*ty/4) (ty/2 - d*tx/4) | d <- [-1,1] ]

renderGlyph CollisionMarker = do
    filledPolygonR [FVec (-l) 0, FVec 0 l, FVec l 0, FVec 0 (-l)] (obscure purple)
    renderStrColAt (bright orange) "!" zero
    where l = 2/3

renderGlyph (HollowGlyph col) =
    polygonR False outerCorners $ opaquify col
renderGlyph (HollowInnerGlyph col) =
    polygonR False innerCorners $ opaquify col

renderGlyph (FilledHexGlyph col) =
    rimmedPolygonR outerCorners col $ brightish col

renderGlyph (ScoreGlyph relScore) =
    sequence_ $ [ aaLineR from to $ bright white | (from,to) <- case relScore of
            Just 1    -> plus 0
            Just 2    -> plus (-1) <> plus 1
            Just 3    -> plus (-2) <> plus 0 <> plus 2
            Just (-1) -> [horiz 0]
            Just (-2) -> [horiz $ -1, horiz 1]
            Just (-3) -> [horiz $ -2, horiz 0, horiz 2]
            _         -> []
        ]
    where
        vert n = let x = n/5 in (FVec x $ -(4/3)*ylen, FVec x $ -ylen)
        horiz n = let y = -(7/6)*ylen in (FVec (n/5 - 1/9) y, FVec (n/5 + 1/9) y)
        plus n = [vert n, horiz n]


renderGlyph (ButtonGlyph col) =
    renderGlyph (TileGlyph (BlockTile []) col)

renderGlyph UnboundButtonGlyph =
    filledCircleR zero (1/3) $ bright black

renderGlyph (PathGlyph dir col) = do
    aaLineR from to col
    where
    from = edge $ neg dir
    to = edge dir

renderGlyph (GateGlyph dir col) = do
    thickLineR from to 1 col
    where
    from = corner $ 1 + hextant dir
    to = corner $ 4 + hextant dir

renderGlyph (UseFiveColourButton using) =
    rescaleRender (1/2) $ sequence_ [
        displaceRender (corner h) $ renderGlyph
            (TileGlyph (BlockTile [])
                (dim $ colourWheel (if using then h`div`2 else 1)))
        | h <- [0,2,4] ]

renderGlyph (ShowBlocksButton showing) = do
    renderGlyph (TileGlyph (BlockTile []) (dim red))
    when (showing == ShowBlocksAll) $
        renderGlyph (BlockedPush hu (bright orange))
    when (showing /= ShowBlocksNone) $
        renderGlyph (BlockedPush hw (bright purple))

renderGlyph (ShowButtonTextButton showing) = do
    rescaleRender (1/2) $ displaceRender (edge (neg hu)) $
        renderGlyph (ButtonGlyph (dim yellow))
    when showing $
        sequence_ [ pixelR (FVec (1/3 + i/4) (-1/4)) (bright white) | i <- [-1..1] ]

renderGlyph (VolumeButton vol) = do
    sequence_ [ arcR (FVec (-2/3) 0) ((fi r+1)/3) (-20) 20
            (if vol > 16*r then bright green else dim red)
        | r <- [0..2] ]
    unless (vol > 0) $
        aaLineR (innerCorner hw) (innerCorner $ neg hw) $ dim red

renderGlyph (WhsButtonsButton Nothing) = rescaleRender (1/3) $ do
    renderGlyph (ButtonGlyph (dim red))
    sequence_ [ displaceRender ((5/3) **^ edge dir) $
            renderGlyph (ButtonGlyph (dim purple))
        | dir <- hexDirs ]
renderGlyph (WhsButtonsButton (Just whs)) = rescaleRender (1/3) $ do
    when (whs /= WHSHook) $
        displaceRender (corner 0) $ renderGlyph (TileGlyph (WrenchTile zero) col)
    when (whs /= WHSWrench) $ do
        displaceRender (corner 4) $ renderGlyph (TileGlyph HookTile col)
        displaceRender (corner 2) $ renderGlyph (TileGlyph (ArmTile hv False) col)
    where
        col = dim white

renderGlyph (WhsEditButtonsButton Nothing) =
    renderGlyph $ WhsButtonsButton Nothing
renderGlyph (WhsEditButtonsButton (Just _)) =
    rescaleRender (1/3) $ renderGlyph cursorGlyph

renderGlyph (FullscreenButton fs) = do
    thickPolygonR corners 1 $ activeCol (not fs)
    thickPolygonR corners' 1 $ activeCol fs
    where
        activeCol True  = opaquify $ dim green
        activeCol False = opaquify $ dim red
        corners = [ (2/3) **^ (if dir `elem` [hu,neg hu] then edge else innerCorner) dir
            | dir <- hexDirs ]
        corners' = map (((2/3)**^) . corner) [0..5]

renderGlyph (DisplacedGlyph dir glyph) =
    displaceRender (innerCorner dir) $ renderGlyph glyph

renderGlyph UnfreshGlyph = do
    let col = bright red
    renderGlyph (HollowInnerGlyph col)
    sequence_ [pixelR (FVec (i/4) 0) col
        | i <- [-1..1] ]

playerGlyph = FilledHexGlyph

cursorGlyph = HollowGlyph $ bright white

ownedTileGlyph colouring highlight (owner,t) =
    let col = colourOf colouring owner
    in TileGlyph t $ (if owner `elem` highlight then bright else dim) col

drawCursorAt :: Maybe HexPos -> RenderM ()
drawCursorAt (Just pos) = drawAt cursorGlyph pos
drawCursorAt _          = return ()

drawBasicBG :: Int -> RenderM ()
drawBasicBG maxR = sequence_ [ drawAtRel (HollowGlyph $ colAt v) v | v <- hexDisc maxR ]
    where
        colAt v@(HexVec x y z) = let
                [r,g,b] = map (\h -> fi $ 0xff * (5 + abs h)`div`maxR) [x,y,z]
                a = fi $ (0x70 * (maxR - abs (hexLen v)))`div`maxR
            in V4 r g b a

drawBlocked :: GameState -> PieceColouring -> Bool -> Force -> RenderM ()
drawBlocked st colouring blocking (Torque idx dir) = do
    let (pos,arms) = case getpp st idx of
            PlacedPiece pos' (Pivot arms') -> (pos',arms')
            PlacedPiece pos' (Hook arm _)  -> (pos',[arm])
            PlacedPiece pos' _             -> (pos',[])
        col = if blocking then bright purple else dim $ colourOf colouring idx
    sequence_ [ drawAt (BlockedArm arm dir col) (arm +^ pos) |
        arm <- arms ]
drawBlocked st _ blocking (Push idx dir) = do
    let footprint = plPieceFootprint $ getpp st idx
        col = bright $ if blocking then purple else orange
    sequence_ [ drawAt (BlockedPush dir col) pos
        | pos <- footprint
        , (dir+^pos) `notElem` footprint ]
    -- drawAt (blockedPush dir $ bright orange) $ placedPos $ getpp st idx

drawApplied :: GameState -> PieceColouring -> Force -> RenderM ()
drawApplied st colouring (Torque idx dir) = do
    let (pos,arms) = case getpp st idx of
            PlacedPiece pos' (Pivot arms') -> (pos',arms')
            PlacedPiece pos' (Hook arm _)  -> (pos',[arm])
            PlacedPiece pos' _             -> (pos',[])
        col = dim $ colourOf colouring idx
    sequence_ [ drawAt (TurnedArm arm dir col) (arm +^ pos) |
            arm <- arms ]
drawApplied _ _ _ = return ()


data Alignment = Centred | LeftAligned | ScreenCentred
renderStrColAt,renderStrColAtLeft,renderStrColAtCentre :: Color -> String -> HexVec -> RenderM ()
renderStrColAt = renderStrColAt' Centred
renderStrColAtLeft = renderStrColAt' LeftAligned
renderStrColAtCentre = renderStrColAt' ScreenCentred
renderStrColAt' :: Alignment -> Color -> String -> HexVec -> RenderM ()
renderStrColAt' _ _ "" _ = pure ()
renderStrColAt' align c str v = let txt = T.pack str in void . runMaybeT $ do
    font <- MaybeT $ asks renderFont
    rend <- lift $ asks renderer
    (texture,dims@(V2 w h)) <- lift . getOrInsert cachedText (modText . const) (txt,font) $ do
        fsurf <- liftIO $ TTF.blended font c $ T.pack str
        dims' <- surfaceDimensions fsurf
        texture <- createTextureFromSurface rend fsurf
        freeSurface fsurf
        pure (texture, dims')
    (scrCentre, off, size) <- lift $ asks $ liftM3 (,,) renderSCentre renderOffset renderSize
    let SVec x y = scrCentre +^ fVec2SVec size (off +^ hexVec2FVec v)
            +^ neg (SVec 0 (h`div`2) +^
                case align of
                    Centred -> SVec (w`div`2) 0
                    _       -> SVec 0 0)
        x' = case align of
            ScreenCentred -> cx scrCentre - (w `div` 2)
            _             -> x
    liftIO $ copy rend texture Nothing (Just $ Rectangle (P $ V2 (fi x') (fi y)) dims)

renderStrColAbove,renderStrColBelow :: Color -> String -> HexVec -> RenderM ()
renderStrColAbove = renderStrColVShifted True
renderStrColBelow = renderStrColVShifted False
renderStrColVShifted :: Bool -> Color -> String -> HexVec -> RenderM ()
renderStrColVShifted up c str v =
    displaceRender (FVec 1 0) $ renderStrColAt c str $ v +^ (if up then hv else hw)
