-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

-- |SDLRender: generic wrapper around sdl-gfx for drawing on hex grids
module SDLRender where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Function              (on)
import           Data.List                  (maximumBy)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Monoid
import           Data.Semigroup             as Sem
import           GHC.Int                    (Int16)
import           Graphics.UI.SDL
import           Graphics.UI.SDL.Primitives
import qualified Graphics.UI.SDL.TTF        as TTF

import           Hex
import           Util

-- |SVec: screen vectors, in pixels
data SVec = SVec { cx, cy :: Int }
    deriving (Eq, Ord, Show)
instance Sem.Semigroup SVec where
    (SVec x y) <> (SVec x' y') = SVec (x+x') (y+y')
instance Monoid SVec where
    mempty = SVec 0 0
    mappend = (Sem.<>)
instance Grp SVec where
    neg (SVec x y) = SVec (-x) (-y)
type CCoord = PHS SVec

-- |FVec: floating point screen vectors, multiplied by 'size' to get SVecs.
data FVec = FVec { rcx, rcy :: Float }
    deriving (Eq, Ord, Show)
instance Sem.Semigroup FVec where
    (FVec x y) <> (FVec x' y') = FVec (x+x') (y+y')
instance Monoid FVec where
    mempty = FVec 0 0
    mappend = (Sem.<>)
instance Grp FVec where
    neg (FVec x y) = FVec (-x) (-y)

-- The following leads to overlapping instances (not sure why):
--instance MultAction Float FVec where
--    r *^ FVec x y = FVec (r*x) (r*y)
-- So instead, we define a new operator:
(**^) :: Float -> FVec -> FVec
r **^ FVec x y = FVec (r*x) (r*y)


hexVec2SVec :: Int -> HexVec -> SVec
hexVec2SVec size (HexVec x y z) =
    SVec ((x-z) * size) (-y * 3 * ysize size)

hexVec2FVec :: HexVec -> FVec
hexVec2FVec (HexVec x y z) =
    FVec (fi $ x-z) (-fi y * 3 * ylen)

fVec2SVec :: Int -> FVec -> SVec
fVec2SVec size (FVec x y) = SVec
    (round $ fi size * x)
    (round $ fi size * y)

sVec2dHV :: Int -> SVec -> (Double,Double,Double)
sVec2dHV size (SVec sx sy) =
    let sx',sy',size' :: Double
        [sx',sy',size',ysize'] = map fi [sx,sy,size,ysize size]
        y' = -sy' / ysize' / 3
        x' = ((sx' / size') - y') / 2
        z' = -((sx' / size') + y') / 2
    in (x',y',z')

sVec2HexVec :: Int -> SVec -> HexVec
sVec2HexVec size sv =
    let (x',y',z') = sVec2dHV size sv
        unrounded = Map.fromList [(1,x'),(2,y'),(3,z')]
        rounded = Map.map round unrounded
        maxdiff = fst $ maximumBy (compare `on` snd) $
            [ (i, abs $ c'-c) | i <- [1..3],
                let c' = unrounded Map.! i, let c = fi $ rounded Map.! i]
        [x,y,z] = map snd $ Map.toList $
            Map.adjust (\x -> x - sum (Map.elems rounded)) maxdiff rounded
    in HexVec x y z


data RenderContext = RenderContext
    { renderSurf    :: Surface
    , renderBGSurf  :: Maybe Surface
    , renderHCentre :: HexPos
    , renderSCentre :: SVec
    , renderOffset  :: FVec
    , renderSize    :: Int
    , renderFont    :: Maybe TTF.Font
    }
type RenderT = ReaderT RenderContext

runRenderT = runReaderT

applyOffset :: RenderContext -> RenderContext
applyOffset rc = rc
    { renderSCentre = renderSCentre rc +^ fVec2SVec (renderSize rc) (renderOffset rc)
    , renderOffset = zero
    }

displaceRender :: Monad m => FVec -> RenderT m a -> RenderT m a
displaceRender d =
    local $ \rc -> rc { renderOffset = renderOffset rc +^ d }

recentreAt :: Monad m => HexVec -> RenderT m a -> RenderT m a
recentreAt v = displaceRender (hexVec2FVec v)

rescaleRender :: Monad m => Float -> RenderT m a -> RenderT m a
rescaleRender r = local $ (\rc -> rc
        { renderSize = round $ r * fi (renderSize rc) } ) . applyOffset

withFont :: Monad m => Maybe TTF.Font -> RenderT m a -> RenderT m a
withFont font = local $ \rc -> rc { renderFont = font }

renderPos :: Monad m => Integral i => FVec -> RenderT m (i,i)
renderPos v = do
    size <- asks renderSize
    c <- asks renderSCentre
    off <- asks renderOffset
    let SVec x y = c +^ fVec2SVec size (v +^ off)
    return (fi x, fi y)
renderLen :: Monad m => Integral i => Float -> RenderT m i
renderLen l = do
    size <- asks renderSize
    return $ round $ l * fi size


-- wrappers around sdl-gfx functions
pixelR v col = do
    (x,y) <- renderPos v
    surf <- asks renderSurf
    void.liftIO $ pixel surf x y col

aaLineR v v' col = do
    (x,y) <- renderPos v
    (x',y') <- renderPos v'
    surf <- asks renderSurf
    void.liftIO $ aaLine surf x y x' y' col

filledPolygonR verts fillCol = do
    ps <- mapM renderPos verts
    surf <- asks renderSurf
    void.liftIO $ filledPolygon surf ps fillCol

arcR v rad a1 a2 col = do
    (x,y) <- renderPos v
    r <- renderLen rad
    surf <- asks renderSurf
    void.liftIO $ arc surf x y r a1 a2 col

filledCircleR v rad col = do
    (x,y) <- renderPos v
    r <- renderLen rad
    surf <- asks renderSurf
    void.liftIO $ filledCircle surf x y r col

-- aaPolygon seems to be a bit buggy in sdl-gfx-0.6.0
aaPolygonR verts = aaLinesR (verts ++ take 1 verts)

-- aaCircle too
aaCircleR v rad col = do
    (x,y) <- renderPos v
    r <- renderLen rad
    surf <- asks renderSurf
    if r <= 1 then void.liftIO $ pixel surf x y col
    else void.liftIO $ aaCircle surf x y r col


aaLinesR verts col =
    sequence_ [ aaLineR v v' col |
        (v,v') <- zip (take (length verts - 1) verts) (drop 1 verts) ]

rimmedPolygonR verts fillCol rimCol = do
    filledPolygonR verts fillCol
    aaPolygonR verts $ opaquify rimCol
    return ()

rimmedCircleR v rad fillCol rimCol = void $ do
    filledCircleR v rad fillCol
    aaCircleR v rad $ opaquify rimCol

thickLineR :: (Functor m, MonadIO m) => FVec -> FVec -> Float -> Pixel -> RenderT m ()
thickLineR from to thickness col =
    let FVec dx dy = to -^ from
        baseThickness = (1/16)
        s = baseThickness * thickness / sqrt (dx^2 + dy^2)
        perp = (s/2) **^ FVec dy (-dx)
    in rimmedPolygonR
        [ from +^ perp, to +^ perp
        , to +^ neg perp, from +^ neg perp]
        col (bright col)

thickLinesR verts thickness col =
    sequence_ [ thickLineR v v' thickness col |
        (v,v') <- zip (take (length verts - 1) verts) (drop 1 verts) ]

thickPolygonR verts = thickLinesR (verts ++ take 1 verts)


ylen = 1 / sqrt 3
ysize :: Int -> Int
ysize = (map (\size -> round $ fi size * ylen) [0..] !!)

corner :: Int -> FVec
corner hextant = FVec x y
    where
        [x,y] = f hextant
        f 0 = [1, -ylen]
        f 1 = [0, -2*ylen]
        f 2 = [-1, -ylen]
        f n | n < 6 = let [x,y] = f (5-n) in [x,-y]
            | n < 0 = f (6-n)
            | otherwise = f (n`mod`6)

outerCorners :: [FVec]
outerCorners = map corner [0..5]

innerCorner :: HexDir -> FVec
innerCorner dir = FVec x y
    where
        [x,y] = f dir
        f dir
            | dir == hu = [2/3, 0]
            | dir == hv = [-1/3, -ylen]
            | dir == hw = [-1/3, ylen]
            | dir == zero = [0,0]
            | not (isHexDir dir) = error "innerCorner: not a hexdir"
            | otherwise = map (\z -> -z) $ f $ neg dir

innerCorners :: [FVec]
innerCorners = map innerCorner hexDirs

edge :: HexDir -> FVec
edge dir = FVec x y
    where
        [x,y] = f dir
        f dir
            | dir == hu = [1, 0]
            | dir == hv = [-1/2, -3*ylen/2]
            | dir == hw = [-1/2, 3*ylen/2]
            | not (isHexDir dir) = error "edge: not a hexdir"
            | otherwise = map (\z -> -z) $ f $ neg dir

rotFVec :: Float -> FVec -> FVec -> FVec
rotFVec th (FVec cx cy) v@(FVec x y)
    | th == 0 = v
    | otherwise = FVec (cx + c*dx-s*dy) (cy + s*dx+c*dy)
    where
        dx = x-cx
        dy = y-cy
        c = cos th
        s = sin th

black = Pixel 0x01000000
white = Pixel 0xffffff00
orange = Pixel 0xff7f0000

colourWheel :: Int -> Pixel
colourWheel n = Pixel $ (((((r * 0x100) + g) * 0x100) + b) * 0x100) + a
    where [r,g,b] = map (\on -> if on then 0xff else 0) $ colourWheel' n
          a = 0x00
          colourWheel' 0 = [True, False, False]
          colourWheel' 1 = [True, True, False]
          colourWheel' n = let [a,b,c] = colourWheel' $ n-2 in [c,a,b]

red = colourWheel 0
yellow = colourWheel 1
green = colourWheel 2
cyan = colourWheel 3
blue = colourWheel 4
purple = colourWheel 5

colourOf :: Ord i => Map i Int -> i -> Pixel
colourOf colouring idx =
    maybe white colourWheel (Map.lookup idx colouring)

setPixelAlpha alpha (Pixel v) = Pixel $ v `div` 0x100 * 0x100 + alpha
bright = setPixelAlpha 0xff
brightish = setPixelAlpha 0xc0
dim = setPixelAlpha 0xa0
obscure = setPixelAlpha 0x80
faint = setPixelAlpha 0x40
invisible = setPixelAlpha 0x00

pixelToRGBA (Pixel v) =
    let (r,v') = divMod v 0x1000000
        (g,v'') = divMod v' 0x10000
        (b,a) = divMod v'' 0x100
    in (r,g,b,a)
rgbaToPixel (r,g,b,a) = Pixel $ a+0x100*(b+0x100*(g+0x100*r))
opaquify p =
    let (r,g,b,a) = pixelToRGBA p
        [r',g',b'] = map (\v -> (v*a)`div`0xff) [r,g,b]
    in rgbaToPixel (r',g',b',0xff)

messageCol = white
dimWhiteCol = Pixel 0xa0a0a000
buttonTextCol = white
errorCol = red


pixelToColor p =
    let (r,g,b,_) = pixelToRGBA p
    in Color (fi r) (fi g) (fi b)

data Alignment = Centred | LeftAligned | ScreenCentred
renderStrColAt,renderStrColAtLeft,renderStrColAtCentre ::
    (Functor m, MonadIO m) => Pixel -> String -> HexVec -> RenderT m ()
renderStrColAt = renderStrColAt' Centred
renderStrColAtLeft = renderStrColAt' LeftAligned
renderStrColAtCentre = renderStrColAt' ScreenCentred
renderStrColAt' :: (Functor m, MonadIO m) => Alignment -> Pixel -> String -> HexVec -> RenderT m ()
renderStrColAt' align c str v = void $ runMaybeT $ do
    font <- MaybeT $ asks renderFont
    fsurf <- MaybeT $ liftIO $ TTF.tryRenderTextBlended font str $ pixelToColor c
    (surf, scrCentre, off, size) <- lift $ asks $ liftM4 (,,,) renderSurf renderSCentre renderOffset renderSize
    let SVec x y = scrCentre +^ fVec2SVec size (off +^ hexVec2FVec v)
            +^ neg (SVec 0 ((surfaceGetHeight fsurf-1)`div`2) +^
                case align of
                    Centred -> SVec (surfaceGetWidth fsurf`div`2) 0
                    _       -> SVec 0 0)
        x' = case align of
            ScreenCentred -> cx scrCentre - (surfaceGetWidth fsurf `div` 2)
            _             -> x
    void $ liftIO $ blitSurface fsurf Nothing surf (Just $ Rect x' y 0 0)

renderStrColAbove,renderStrColBelow :: (Functor m, MonadIO m) => Pixel -> String -> HexVec -> RenderT m ()
renderStrColAbove = renderStrColVShifted True
renderStrColBelow = renderStrColVShifted False
renderStrColVShifted :: (Functor m, MonadIO m) => Bool -> Pixel -> String -> HexVec -> RenderT m ()
renderStrColVShifted up c str v =
    displaceRender (FVec 1 0) $ renderStrColAt c str $ v +^ (if up then hv else hw)

erase :: (Functor m, MonadIO m) => RenderT m ()
erase = fillRectBG Nothing

fillRectBG :: (Functor m, MonadIO m) => Maybe Rect -> RenderT m ()
fillRectBG mrect = do
    surf <- asks renderSurf
    mbgsurf <- asks renderBGSurf
    void $ liftIO $ maybe
        (fillRect surf mrect black)
        (\bgsurf -> blitSurface bgsurf mrect surf mrect)
        mbgsurf

blankRow v = do
    (surf, scrCentre, off, size) <- asks $ liftM4 (,,,) renderSurf renderSCentre renderOffset renderSize
    let SVec _ y = scrCentre +^ fVec2SVec size (off +^ hexVec2FVec v)
        w = surfaceGetWidth surf
        h = ceiling $ fi (size * 3 `div` 2) * 2 / sqrt 3
    fillRectBG $ Just $ Rect 0 (y-h`div`2) w h

blitAt :: (Functor m, MonadIO m) => Surface -> HexVec -> RenderT m ()
blitAt surface v = do
    (surf, scrCentre, off, size) <- asks $ liftM4 (,,,) renderSurf renderSCentre renderOffset renderSize
    let SVec x y = scrCentre +^ fVec2SVec size (off +^ hexVec2FVec v)
        w = surfaceGetWidth surface
        h = surfaceGetHeight surface
    void $ liftIO $ blitSurface surface Nothing surf $ Just $
        Rect (x-w`div`2) (y-h`div`2) (w+1) (h+1)
