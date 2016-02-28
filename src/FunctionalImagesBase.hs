-- ---------------------------------------------------------------------------
-- Base Functions and types for FunctionalImages
-- ---------------------------------------------------------------------------
--
-- Here we have the base functionality for the functional images.
-- This is a bridge between the geometric spaces of Conal Eliott
-- and the image genereating functions of JuicyPixels.
--
-- Translations of Conal types to my types
--        Image           FImage
--
-- ---------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-}

module FunctionalImagesBase
    ( writePng
    , FImage
    , Region
    , Picture
    , Point
    , checker
    , PolarPoint
    , toPolar
    , fromPolar
    , dist0
    , polarChecker
    , Frac
    , generateImageR2
    , Color
    , PixelRGBFA
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
    , lift0
    , lift1
    , lift2
    , lift3
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

import Codec.Picture hiding (Image, Color)
import qualified Codec.Picture as JP (Image)

-- | Type Point with Real Coordinates
type Point = (Float, Float)

-- type fraction means numbers between 0 and 1
type Frac = Float

-- | a type for the functional images (note FImage to avoid name clashes)
type FImage a = Point -> a

-- | Boolean valued images can be used for image masking
type Region = FImage Bool

-- | we need a type for a displayable image, this is a picture
type Picture = JP.Image PixelRGB8

-- a little helper function to restict a value into an interal
inInterval :: Ord a => a -> a -> a -> a
inInterval val low high
   | val < low = low
   | val > high = high
   | otherwise  = val

-- | a class to convert to JuicyPixel colors
--  At the moment we ignore the alpha channel
class ToPixelRGB8 a where
  toPixelRGB8 :: a -> PixelRGB8

instance ToPixelRGB8 Bool where
  toPixelRGB8 False  = PixelRGB8 255 255 255
  toPixelRGB8 True = PixelRGB8   0   0   0

instance ToPixelRGB8 Frac where
  toPixelRGB8 f = PixelRGB8 ff ff ff
      where
        ff = frac2pixel8 f

-- | Support function to convert fractional color values (0 -1)
--   to Integer color values (0-255)
frac2pixel8 :: (Integral b, RealFrac r) => r -> b
frac2pixel8 f = floor $ 255 * inInterval f 0 1

-- | Generate an picture in the Real X Real space
generateImageR2 :: ToPixelRGB8 a => (Point -> a)  -- Coordinate Rendering function
         -> Point                           -- lower left corner
         -> Point                            -- upper right corner
         -> Int                              -- width in pixel
         -> Picture                          -- Resulting picture
generateImageR2 coordRenderer (x0,y0) (x1,y1) width = generateImage pixelRenderer width height
    where
      pixelSize = (x1 - x0) / fromIntegral width
      height = floor $ (y1 - y0) / (x1 - x0) * fromIntegral width
      pixelRenderer ix iy = toPixelRGB8 $ coordRenderer (x0 + fromIntegral ix * pixelSize, y1 - fromIntegral iy * pixelSize)

-- The function to create a chess board
checker :: FImage Bool
checker (x, y) = even $ floor x + floor y

-- ----------------------------------------------------------------------------
-- Polar Coordinates
-- ----------------------------------------------------------------------------
-- | Polar Coordinates
type PolarPoint = (Float, Float)

-- | Convert from Polar to Cartesian coordinates
fromPolar :: PolarPoint -> Point
fromPolar (ρ, θ) = (ρ * cos θ, ρ * sin θ)

-- | Convert from Cartesian to Polar coordinates
toPolar:: Point -> PolarPoint
toPolar (x, y) = (dist0 (x, y), atan2 y x)

-- | distance to the origin of the coordiante system
dist0 :: FImage Float
dist0 (x, y) = sqrt $ x**2 + y**2

-- | Polar Checkboard
polarChecker :: Int -> FImage Bool
polarChecker n = checker . sc . toPolar
   where
     sc (r,a) = (r,a * fromIntegral n / pi)

-- ----------------------------------------------------------------------------
-- Color support
-- ----------------------------------------------------------------------------

-- | HDR pixel type storing floating point 32bit red, green and blue (RGB) information.
-- Same value range and comments apply as for 'PixelF'.
-- Values are stored in the following order:
--  * Red
--  * Green
--  * Blue
--  * Alpha Channel
data PixelRGBFA = PixelRGBFA {-# UNPACK #-} !PixelF -- Red
                             {-# UNPACK #-} !PixelF -- Green
                             {-# UNPACK #-} !PixelF -- Blue
                             {-# UNPACK #-} !PixelF -- Alpha
               deriving (Eq, Ord, Show)

-- | Type for our Color
type Color = PixelRGBFA

instance ToPixelRGB8 PixelRGBFA
   where
     toPixelRGB8 (PixelRGBFA r g b a) =
        PixelRGB8 (f r) (f g) (f b)
           where f = frac2pixel8

invisible :: Color
invisible = PixelRGBFA 0 0 0 0

red :: Color
red = PixelRGBFA 1 0 0 1

white :: Color
white = PixelRGBFA 1 1 1 1

black :: Color
black = PixelRGBFA 0 0 0 1

blue :: Color
blue = PixelRGBFA 0 0 1 1

green :: Color
green = PixelRGBFA 0 1 0 1

yellow :: Color
yellow = PixelRGBFA 1 1 0 1

-- ----------------------------------------------------------------------------
-- Operations with color functions
-- ----------------------------------------------------------------------------

-- | linear interpolate betrween 2 colors
--   The weight w must be between 0 and 1
lerpC :: Float -> Color -> Color -> Color
lerpC w (PixelRGBFA r1 g1 b1 a1) (PixelRGBFA r2 g2 b2 a2) =
  PixelRGBFA (h r1 r2) (h g1 g2) (h b1 b2) (h a1 a2)
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
cOver (PixelRGBFA r1  g1  b1  a1) (PixelRGBFA r2 g2 b2 a2)
   = PixelRGBFA (h r1 r2) (h g1 g2) (h b1 b2) (h a1 a2)
     where
       h x1 x2 = x1 + (1 - a1) * x2

-- | A type for color images
type ImageC = FImage Color

-- ---------------------------------------------------------------------------
-- Pointwise lifting
-- ---------------------------------------------------------------------------
lift0 :: a -> p -> a
lift0 = const

lift1 :: (a -> b) -> (p -> a) -> p -> b
lift1 h f1 = h . f1

lift2 :: (a -> b -> c) -> (p -> a) -> (p -> b) -> p -> c
lift2 h f1 f2  p = h (f1 p) (f2 p)

lift3 :: (a -> b -> c -> d) -> (p -> a) -> (p -> b) -> (p -> c) -> p -> d
lift3 h f1 f2 f3 p =  h (f1 p) (f2 p) (f3 p)

-- | Overlay one image on another
over :: ImageC -> ImageC -> ImageC
over = lift2 cOver

-- | Pointwise selection of one image
cond :: FImage Bool -> FImage c -> FImage c -> FImage c
cond = lift3 (\a b c -> if a then b else c)

-- | Interpolation between 2 images
lerpI ::  FImage Frac -> ImageC -> ImageC -> ImageC
lerpI = lift3 lerpC

-- Names for some opaque constant-colored images

emptyI = const invisible
whiteI = const white
blackI = const black
redI = const red
yellowI = const yellow
greenI = const green
blueI = const blue 
