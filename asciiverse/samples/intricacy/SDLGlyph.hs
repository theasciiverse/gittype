-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module SDLGlyph where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import qualified Data.List                  as List
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Graphics.UI.SDL
import           System.Random              (randomRIO)

import           BoardColouring
import           Command
import           GameState
import           GameStateTypes
import           Hex
import           Physics
import           SDLRender
import           Util


data ShowBlocks = ShowBlocksBlocking | ShowBlocksAll | ShowBlocksNone
    deriving (Eq, Ord, Show, Read)

data Glyph
    = TileGlyph Tile Pixel
    | SpringGlyph HexDir HexDir SpringExtension HexDir Pixel
    | PivotGlyph TorqueDir HexDir Pixel
    | ArmGlyph TorqueDir HexDir Pixel
    | BlockedArm HexDir TorqueDir Pixel
    | TurnedArm HexDir TorqueDir Pixel
    | BlockedBlock Tile HexDir Pixel
    | BlockedPush HexDir Pixel
    | CollisionMarker
    | HollowGlyph Pixel
    | HollowInnerGlyph Pixel
    | FilledHexGlyph Pixel
    | ScoreGlyph (Maybe Int)
    | ButtonGlyph Pixel
    | PathGlyph HexDir Pixel
    | GateGlyph HexDir Pixel
    | UseFiveColourButton Bool
    | ShowBlocksButton ShowBlocks
    | ShowButtonTextButton Bool
    | UseSoundsButton Bool
    | WhsButtonsButton (Maybe WrHoSel)
    | FullscreenButton Bool
    | DisplacedGlyph HexDir Glyph
    | UnfreshGlyph
    deriving (Eq, Ord, Show)

type SizedGlyph = (Glyph,Int)
data CachedGlyphs = CachedGlyphs (Map SizedGlyph Surface) [SizedGlyph]
    deriving (Eq, Ord, Show)
emptyCachedGlyphs = CachedGlyphs Map.empty []
maxCachedGlyphs = 100

type RenderM = RenderT (StateT CachedGlyphs IO)
runRenderM :: RenderM a -> CachedGlyphs -> RenderContext -> IO (a,CachedGlyphs)
runRenderM m cgs rc = runStateT (runReaderT m rc) cgs

drawAt :: Glyph -> HexPos -> RenderM ()
drawAt gl pos = do
        centre <- asks renderHCentre
        drawAtRel gl (pos -^ centre)

drawAtRel :: Glyph -> HexVec -> RenderM ()
drawAtRel gl v = recentreAt v $ renderGlyphCaching gl

renderGlyphCaching :: Glyph -> RenderM ()
-- Glyph caching:
--  We aim to cache glyphs which are "currently" being regularly drawn, so
--  they can be blitted from RAM rather than being drawn afresh each time.
--  Rather than track statistics, we adopt the following probabilistic scheme.
renderGlyphCaching gl = do
    CachedGlyphs cmap clist <- lift get
    size <- asks renderSize
    let sgl = (gl,size)
        w = size*2 + 1
        h = ysize size*4 + 1
        newGlyphSurf = do
            -- csurf <- liftIO $ createRGBSurface [] w h 32 0xff000000 0x00ff0000 0x0000ff00 0x000000ff
            csurf <- liftIO $ createRGBSurface [] w h 16 0 0 0 0
            liftIO $ setColorKey csurf [SrcColorKey,RLEAccel] $ Pixel 0
            return csurf
        renderOnCache csurf =
            let ccxt rc = rc { renderSurf = csurf, renderSCentre = SVec (w`div`2) (h`div`2), renderOffset = zero }
            in local ccxt $ renderGlyph gl
        addToCache cacheFull csurf = do
            CachedGlyphs cmap clist <- lift get
            let cmap' = Map.insert sgl csurf cmap
            lift $ put $ if cacheFull
                then CachedGlyphs (Map.delete (last clist) cmap') (sgl:List.init clist)
                else CachedGlyphs cmap' (sgl:clist)
        promote = do
            CachedGlyphs cmap clist <- lift get
            lift $ put $ CachedGlyphs cmap (sgl:List.delete sgl clist)
        blitGlyph csurf = do
            surf <- asks renderSurf
            (x,y) <- renderPos zero
            void $ liftIO $ blitSurface csurf Nothing surf $ Just $
                Rect (x-w`div`2) (y-h`div`2) (w+1) (h+1)
    let cacheFull = Map.size cmap >= maxCachedGlyphs
    let mcsurf = Map.lookup sgl cmap
    -- with probability 1 in (maxCachedGlyphs`div`2), we put this glyph at the
    -- head of the cached list, throwing away the tail to make room if needed.
    cacheIt <- (((cacheable &&) . (not cacheFull ||)) <$>) $
        liftIO $ (==0) <$> randomRIO (0::Int,maxCachedGlyphs`div`2)
    case mcsurf of
        Nothing -> if cacheIt
            then do
                csurf <- newGlyphSurf
                renderOnCache csurf
                addToCache cacheFull csurf
                blitGlyph csurf
            else
                renderGlyph gl
        Just csurf -> do
            when cacheIt promote
            blitGlyph csurf
    where
        cacheable = case gl of
            -- some glyphs need to be drawn with blending - those involving
            -- anti-aliasing which bleed over the edge of the hex or which
            -- may be drawn on top of an existing glyph.
            -- TODO: we should find a way to deal with at least some of these;
            -- springs in particular are common and expensive to draw.
            -- Maybe we could truncate the spring glyphs to a hex?
            TileGlyph (BlockTile adjs) _      -> null adjs
            TileGlyph (SpringTile extn dir) _ -> False
            SpringGlyph {}                    -> False
            FilledHexGlyph _                  -> False
            HollowGlyph _                     -> False
            BlockedBlock {}                   -> False
            BlockedPush _ _                   -> False
            CollisionMarker                   -> False
            DisplacedGlyph _ _                -> False
            _                                 -> True

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
    aaLineR from to col
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
    -- rimmedCircle surf centre (size`div`3) (bright purple) $ bright purple
    aaLineR start end col
    aaCircleR zero rad col
    where
        [start,end] = map (((1/2)**^) . corner) [0,3]
        rad = ylen
        col = dim purple

renderGlyph (HollowGlyph col) =
    aaPolygonR outerCorners $ opaquify col
renderGlyph (HollowInnerGlyph col) =
    aaPolygonR innerCorners $ opaquify col

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

renderGlyph (PathGlyph dir col) = do
    aaLineR from to col
    where
    from = edge $ neg dir
    to = edge dir

renderGlyph (GateGlyph dir col) = do
    aaLineR from to col
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

renderGlyph (UseSoundsButton use) = do
    sequence_ [ arcR (FVec (-2/3) 0) r (-20) 20
            (if use then bright green else dim red)
        | r <- [1/3,2/3,1] ]
    unless use $
        aaLineR (innerCorner hw) (innerCorner $ neg hw) $ dim red

renderGlyph (WhsButtonsButton Nothing) = rescaleRender (1/3) $ do
    renderGlyph (ButtonGlyph (dim red))
    sequence_ [ displaceRender ((3/2) **^ edge dir) $
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
        colAt v@(HexVec hx hy hz) = let
                [r,g,b] = map (\h -> fi $ 0xff * (5 + abs h)`div`maxR) [hx,hy,hz]
                a = fi $ (0x70 * (maxR - abs (hexLen v)))`div`maxR
            in rgbaToPixel (r,g,b,a)

drawBlocked :: GameState -> PieceColouring -> Bool -> Force -> RenderM ()
drawBlocked st colouring blocking (Torque idx dir) = do
    let (pos,arms) = case getpp st idx of
            PlacedPiece pos (Pivot arms) -> (pos,arms)
            PlacedPiece pos (Hook arm _) -> (pos,[arm])
            _                            -> (pos,[])
        col = if blocking then bright purple else dim $ colourOf colouring idx
    sequence_ [ drawAt (BlockedArm arm dir col) (arm +^ pos) |
        arm <- arms ]
drawBlocked st colouring blocking (Push idx dir) = do
    let footprint = plPieceFootprint $ getpp st idx
        fullfootprint = fullFootprint st idx
        col = bright $ if blocking then purple else orange
    sequence_ [ drawAt (BlockedPush dir col) pos
        | pos <- footprint
        , (dir+^pos) `notElem` fullfootprint ]
    -- drawAt (blockedPush dir $ bright orange) $ placedPos $ getpp st idx

drawApplied :: GameState -> PieceColouring -> Force -> RenderM ()
drawApplied st colouring (Torque idx dir) = do
    let (pos,arms) = case getpp st idx of
            PlacedPiece pos (Pivot arms) -> (pos,arms)
            PlacedPiece pos (Hook arm _) -> (pos,[arm])
            _                            -> (pos,[])
        col = dim $ colourOf colouring idx
    sequence_ [ drawAt (TurnedArm arm dir col) (arm +^ pos) |
            arm <- arms ]
drawApplied _ _ _ = return ()
