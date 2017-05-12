-- ---------------------------------------------------------------------------
-- Base Functions and types for FunctionalImages
-- ---------------------------------------------------------------------------
--
-- Here we have the base functionality for the functional images.
-- This is a bridge between the geometric spaces of Conal Eliott
-- and the image genereating functions of JuicyPixels.
--
--
-- ---------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-}

module FunctionalImagesBase
    ( Image
    , Region
    , Point
    , PolarPoint
    , toPolar
    , fromPolar
    , dist0
    , Frac
    , Color
    , PixelRGBAF
    , ToPixelRGBA8
    , convertFunction
    , ImageC
    , invisible
    , white
    , black
    , red
    , green
    , blue
    , yellow
    , lerpC
    , lighten
    , darken
    , bilerpC
    , cOver
    , over
    , cond
    , lerpI
    , emptyI
    , whiteI
    , blackI
    , redI
    , greenI
    , blueI
    , yellowI
    ) where

import Reflex.Dom.Brownies
import Control.Applicative
import Data.NumInstances.Tuple

-- -----------------------------------------------------------------------------------
-- Section 2. What is an image?
-- -----------------------------------------------------------------------------------

-- | Type Point with Real Coordinates
type Point = (Double, Double)

--  | Type fraction means numbers between 0 and 1
type Frac = Double

-- | a type for the functional images
type Image a = Point -> a

-- | Boolean valued images can be used for image masking
type Region = Image Bool

-- a little helper function to restict a value into an interal
inInterval :: Ord a => a -> a -> a -> a
inInterval val low high
   | val < low = low
   | val > high = high
   | otherwise  = val

-- | a class to convert to JuicyPixel colors
--  At the moment we ignore the alpha channel
class ToPixelRGBA8 a where
  toPixelRGBA8 :: a -> PixelRGBA8

instance ToPixelRGBA8 Bool where
  toPixelRGBA8 False  = PixelRGBA8 255 255 255 255
  toPixelRGBA8 True = PixelRGBA8   0   0   0   255

instance ToPixelRGBA8 Frac where
  toPixelRGBA8 f = PixelRGBA8 ff ff ff 255
      where
        ff = frac2pixel8 f

-- | Support function to convert fractional color values (0 -1)
--   to Integer color values (0-255)
frac2pixel8 :: (Integral b, RealFrac r) => r -> b
frac2pixel8 f = floor $ 255 * inInterval f 0 1

-- | convert a Real image function to a canvas function to be used in Reflex.Dom.Brownies
convertFunction :: ToPixelRGBA8 a => Double -> Double -> Image a -> PixelFunction
convertFunction rSizeX rSizeY rImage iSizeX iSizeY iValX iValY = toPixelRGBA8 $ rImage $ coordTrans rSize iSize iVal
  where
    iVal = (iValX, iValY)
    rSize = (rSizeX, rSizeY)
    iSize = (iSizeX, iSizeY)
    -- Transform canvas pixel coordinates to Real coordinates used by the real valued image functions
    coordTrans :: Point -> ICoord -> ICoord -> Point
    coordTrans rSize iSize iPoint = (rSize / fromIntPair iSize) * fromIntPair iPoint - rSize / 2 
    fromIntPair :: (Int, Int) -> (Double, Double)
    fromIntPair (n, m) = (fromIntegral n, fromIntegral m)
   -- coordTrans (7,7) (255,255) (0,255) -> (-3.5,3.5)

-- ----------------------------------------------------------------------------
-- Polar Coordinates
-- ----------------------------------------------------------------------------
-- | Polar Coordinates
type PolarPoint = (Double, Double)

-- | Convert from Polar to Cartesian coordinates
fromPolar :: PolarPoint -> Point
fromPolar (ρ, θ) = (ρ * cos θ, ρ * sin θ)

-- | Convert from Cartesian to Polar coordinates
toPolar:: Point -> PolarPoint
toPolar (x, y) = (dist0 (x, y), atan2 y x)

-- | distance to the origin of the coordiante system
dist0 :: Image Double
dist0 (x, y) = sqrt $ x**2 + y**2

-- ----------------------------------------------------------------------------
-- Section 3: Colors
-- ----------------------------------------------------------------------------

type PixelF = Double

-- | HDR pixel type storing floating point 32bit red, green and blue (RGB) information.
-- Same value range and comments apply as for 'PixelF'.
-- Values are stored in the following order:
--  * Red
--  * Green
--  * Blue
--  * Alpha Channel
data PixelRGBAF = PixelRGBAF {-# UNPACK #-} !PixelF -- Red
                             {-# UNPACK #-} !PixelF -- Green
                             {-# UNPACK #-} !PixelF -- Blue
                             {-# UNPACK #-} !PixelF -- Alpha
               deriving (Eq, Ord, Show)

-- | Type for our Color
type Color = PixelRGBAF

instance ToPixelRGBA8 PixelRGBAF
   where
     toPixelRGBA8 (PixelRGBAF r g b a) =
        PixelRGBA8 (f r) (f g) (f b) (f a)
           where f = frac2pixel8

invisible :: Color
invisible = PixelRGBAF 0 0 0 0

red :: Color
red = PixelRGBAF 1 0 0 1

white :: Color
white = PixelRGBAF 1 1 1 1

black :: Color
black = PixelRGBAF 0 0 0 1

blue :: Color
blue = PixelRGBAF 0 0 1 1

green :: Color
green = PixelRGBAF 0 1 0 1

yellow :: Color
yellow = PixelRGBAF 1 1 0 1

-- ----------------------------------------------------------------------------
-- Operations with color functions
-- ----------------------------------------------------------------------------

-- | linear interpolate betrween 2 colors
--   The weight w must be between 0 and 1
lerpC :: Double -> Color -> Color -> Color
lerpC w (PixelRGBAF r1 g1 b1 a1) (PixelRGBAF r2 g2 b2 a2) =
  PixelRGBAF (h r1 r2) (h g1 g2) (h b1 b2) (h a1 a2)
    where
      h x1 x2 = w * x1 + (1 - w) * x2

-- | Two useful functions
lighten, darken :: Frac -> Color -> Color
lighten w = lerpC w white
darken  w = lerpC w black

-- | Two dimensional interpolation
bilerpC :: Color -> Color -> Color -> Color -> (Frac, Frac) -> Color
bilerpC ll lr ul ur (wx, wy) =
  lerpC wy (lerpC wx ll lr) (lerpC wx ul ur)

-- | Color overlay. blend the 2 colors according to the opacity of the first.
cOver :: Color -> Color -> Color
cOver (PixelRGBAF r1  g1  b1  a1) (PixelRGBAF r2 g2 b2 a2)
   = PixelRGBAF (h r1 r2) (h g1 g2) (h b1 b2) (h a1 a2)
     where
       h x1 x2 = x1 + (1 - a1) * x2

-- | A type for color images
type ImageC = Image Color

-- ---------------------------------------------------------------------------------------
-- Section 4: Pointwise lifting
-- ---------------------------------------------------------------------------------------
--
-- Conals lifting functions are the applicative lift functions

-- | Overlay one image on another
over :: ImageC -> ImageC -> ImageC
over = liftA2 cOver

-- | Pointwise selection of one image
cond :: Image Bool -> Image c -> Image c -> Image c
cond = liftA3 (\a b c -> if a then b else c)

-- | Interpolation between 2 images
lerpI ::  Image Frac -> ImageC -> ImageC -> ImageC
lerpI = liftA3 lerpC

-- Names for some opaque constant-colored images

emptyI = const invisible
whiteI = const white
blackI = const black
redI = const red
yellowI = const yellow
greenI = const green
blueI = const blue

