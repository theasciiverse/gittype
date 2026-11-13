-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

-- |SDL2Render: generic wrapper around sdl2-gfx for drawing on hex grids
module SDL2Render where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Function              (on)
import           Data.List                  (maximumBy)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Monoid
import           Data.Semigroup             as Sem
import           Data.Vector.Storable       (fromList)
import           Data.Word                  (Word8)
import           Foreign.C.Types            (CInt)
import           SDL                        hiding (perp, zero)
import qualified SDL.Font                   as TTF
import           SDL.Primitive

import           Hex
import           Util

-- |SVec: screen vectors, in pixels
data SVec = SVec { cx, cy :: CInt }
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

ylen :: Float
ylen = 1 / sqrt 3

hexVec2SVec :: CInt -> HexVec -> SVec
hexVec2SVec size (HexVec x y z) =
    SVec (fi (x-z) * size) (fi (-y) * 3 * ysize size)

hexVec2FVec :: HexVec -> FVec
hexVec2FVec (HexVec x y z) =
    FVec (fi $ x-z) (-fi y * 3 * ylen)

fVec2SVec :: CInt -> FVec -> SVec
fVec2SVec size (FVec x y) = SVec
    (round $ fi size * x)
    (round $ fi size * y)

sVec2dHV :: CInt -> SVec -> (Double,Double,Double)
sVec2dHV size (SVec sx sy) =
    let sx',sy',size' :: Double
        [sx',sy'] = map fi [sx,sy]
        [size',ysize'] = map fi [size,ysize size]
        y' = -sy' / ysize' / 3
        x' = ((sx' / size') - y') / 2
        z' = -((sx' / size') + y') / 2
    in (x',y',z')

sVec2HexVec :: CInt -> SVec -> HexVec
sVec2HexVec size sv =
    let (x',y',z') = sVec2dHV size sv
        unrounded = Map.fromList [(1::Int,x'),(2,y'),(3,z')]
        rounded = Map.map round unrounded
        maxdiff = fst $ maximumBy (compare `on` snd) $
            [ (i, abs $ c'-c) | i <- [1..3],
                let c' = unrounded Map.! i, let c = fi $ rounded Map.! i]
        [x,y,z] = map snd $ Map.toList $
            Map.adjust (\u -> u - sum (Map.elems rounded)) maxdiff rounded
    in HexVec x y z


data RenderContext = RenderContext
    { renderer        :: Renderer
    , renderBGTexture :: Maybe Texture
    , renderHCentre   :: HexPos
    , renderSCentre   :: SVec
    , renderOffset    :: FVec
    , renderSize      :: CInt
    , renderFont      :: Maybe TTF.Font
    , renderWidth     :: CInt
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

renderPos :: Monad m => Integral i => FVec -> RenderT m (V2 i)
renderPos v = do
    size <- asks renderSize
    c <- asks renderSCentre
    off <- asks renderOffset
    let SVec x y = c +^ fVec2SVec size (v +^ off)
    return $ V2 (fi x) (fi y)
renderLen :: Monad m => Integral i => Float -> RenderT m i
renderLen l = do
    size <- asks renderSize
    return $ round $ l * fi size


-- wrappers around sdl-gfx functions
pixelR v col = do
    p <- renderPos v
    rend <- asks renderer
    void.liftIO $ pixel rend p col

aaLineR v v' col = do
    p <- renderPos v
    p' <- renderPos v'
    rend <- asks renderer
    void.liftIO $ smoothLine rend p p' col

polygonR :: MonadIO m => Bool -> [FVec] -> Color -> RenderT m ()
polygonR fill verts col = do
    ps <- mapM renderPos verts
    rend <- asks renderer
    let (xs,ys) = unzip [(x,y) | V2 x y <- ps]
    void.liftIO $ (if fill then fillPolygon else smoothPolygon) rend (fromList xs) (fromList ys) col

arcR v rad a1 a2 col = do
    p <- renderPos v
    r <- renderLen rad
    rend <- asks renderer
    void.liftIO $ arc rend p r a1 a2 col

filledCircleR v rad col = do
    p <- renderPos v
    r <- renderLen rad
    rend <- asks renderer
    void.liftIO $ fillCircle rend p r col

aaCircleR v rad col = do
    p <- renderPos v
    r <- renderLen rad
    rend <- asks renderer
    void.liftIO $ smoothCircle rend p r col


aaLinesR verts col =
    sequence_ [ aaLineR v v' col |
        (v,v') <- zip (take (length verts - 1) verts) (drop 1 verts) ]

rimmedPolygonR verts fillCol rimCol = do
    polygonR True verts fillCol
    polygonR False verts $ opaquify rimCol

filledPolygonR :: MonadIO m => [FVec] -> Color -> RenderT m ()
filledPolygonR = polygonR True

rimmedCircleR v rad fillCol rimCol = void $ do
    filledCircleR v rad fillCol
    aaCircleR v rad $ opaquify rimCol

thickLineR :: (Functor m, MonadIO m) => FVec -> FVec -> Float -> Color -> RenderT m ()
thickLineR from to thickness col =
    let FVec dx dy = to -^ from
        baseThickness = (1/16)
        s = baseThickness * thickness / sqrt (dx*dx + dy*dy)
        perp = (s/2) **^ FVec dy (-dx)
    in rimmedPolygonR
        [ from +^ perp, to +^ perp
        , to +^ neg perp, from +^ neg perp]
        col (bright col)

thickLinesR verts thickness col =
    sequence_ [ thickLineR v v' thickness col |
        (v,v') <- zip (take (length verts - 1) verts) (drop 1 verts) ]

thickPolygonR verts = thickLinesR (verts ++ take 1 verts)


ysize :: CInt -> CInt
ysize = fi . (map (\size -> round $ fi size * ylen :: Int) [0::Int ..] !!) . fi

corner :: Int -> FVec
corner hextnt = FVec x y
    where
        [x,y] = f hextnt
        f 0 = [1, -ylen]
        f 1 = [0, -2*ylen]
        f 2 = [-1, -ylen]
        f n | n < 6 = let [x',y'] = f (5-n) in [x',-y']
            | n < 0 = f (6-n)
            | otherwise = f (n`mod`6)

outerCorners :: [FVec]
outerCorners = map corner [0..5]

innerCorner :: HexDir -> FVec
innerCorner dir = FVec x y
    where
    [x,y] = f dir
    f d     | d == hu = [2/3, 0]
            | d == hv = [-1/3, -ylen]
            | d == hw = [-1/3, ylen]
            | d == zero = [0,0]
            | not (isHexDir d) = error "innerCorner: not a hexdir"
            | otherwise = map (\z -> -z) $ f $ neg d

innerCorners :: [FVec]
innerCorners = map innerCorner hexDirs

edge :: HexDir -> FVec
edge dir = FVec x y
    where
    [x,y] = f dir
    f d     | d == hu = [1, 0]
            | d == hv = [-1/2, -3*ylen/2]
            | d == hw = [-1/2, 3*ylen/2]
            | not (isHexDir d) = error "edge: not a hexdir"
            | otherwise = map (\z -> -z) $ f $ neg d

rotFVec :: Float -> FVec -> FVec -> FVec
rotFVec th (FVec bx by) v@(FVec x y)
    | th == 0 = v
    | otherwise = FVec (bx + c*dx-s*dy) (by + s*dx+c*dy)
    where
        dx = x-bx
        dy = y-by
        c = cos th
        s = sin th

black, white, orange :: Color
-- FIXME: why not actually black?
black = V4 0x01 0 0 0
white = V4 0xff 0xff 0xff 0
orange = V4 0xff 0x7f 0 0

colourWheel :: Int -> Color
colourWheel n = V4 r g b a
    where [r,g,b] = map (\ok -> if ok then 0xff else 0) $ colourWheel' n
          a = 0x00
          colourWheel' 0  = [True, False, False]
          colourWheel' 1  = [True, True, False]
          colourWheel' n' = let [r',g',b'] = colourWheel' $ n'-2 in [b',r',g']

red = colourWheel 0
yellow = colourWheel 1
green = colourWheel 2
cyan = colourWheel 3
blue = colourWheel 4
purple = colourWheel 5

colourOf :: Ord i => Map i Int -> i -> Color
colourOf colouring idx =
    maybe white colourWheel (Map.lookup idx colouring)

setColorAlpha :: Word8 -> Color -> Color
setColorAlpha a (V4 r g b _) = V4 r g b a
bright = setColorAlpha 0xff
brightish = setColorAlpha 0xc0
dim = setColorAlpha 0xa0
obscure = setColorAlpha 0x80
faint = setColorAlpha 0x40
invisible = setColorAlpha 0x00

opaquify (V4 r g b a) =
    let scale :: Int -> Int
        scale v = (v * fi a) `div` 0xff
        [r',g',b'] = fi . scale . fi <$> [r,g,b]
    in V4 r' g' b' 0xff

messageCol, dimWhiteCol, buttonTextCol, errorCol :: Color
messageCol = white
dimWhiteCol = V4 0xa0 0xa0 0xa0 0
buttonTextCol = white
errorCol = red

erase :: (Functor m, MonadIO m) => RenderT m ()
erase = fillRectBG Nothing

fillRectBG :: (Functor m, MonadIO m) => Maybe (Rectangle CInt) -> RenderT m ()
fillRectBG mrect = do
    rend <- asks renderer
    mbgt <- asks renderBGTexture
    void $ liftIO $ maybe
        ((rendererDrawColor rend $= black) >> fillRect rend mrect)
        (\bgt -> copy rend bgt mrect mrect)
        mbgt

blankRow v = do
    (scrCentre, off, size, w) <- asks $ liftM4 (,,,) renderSCentre renderOffset renderSize renderWidth
    let SVec _ y = scrCentre +^ fVec2SVec size (off +^ hexVec2FVec v)
        h = ceiling (fi (size * 3 `div` 2) * 2 / sqrt 3 :: Float)
    fillRectBG $ Just $ Rectangle (P $ V2 0 (fi $ y-h`div`2)) (V2 (fi w) (fi h))

blitAt :: (Functor m, MonadIO m) => Texture -> HexVec -> RenderT m ()
blitAt texture v = do
    (scrCentre, off, size) <- asks $ liftM3 (,,) renderSCentre renderOffset renderSize
    blitAtSVec texture $ scrCentre +^ fVec2SVec size (off +^ hexVec2FVec v)
blitAtSVec :: (Functor m, MonadIO m) => Texture -> SVec -> RenderT m ()
blitAtSVec texture (SVec x y) = do
    rend <- asks renderer
    TextureInfo _ _ w h <- queryTexture texture
    liftIO . copy rend texture Nothing . Just $
        Rectangle (P $ V2 (fi x - w`div`2) (fi y - h`div`2)) (V2 w h)
