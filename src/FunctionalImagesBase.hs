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
    , Picture
    , Point
    , PolarPoint
    , toPolar
    , fromPolar
    , dist0
    , Frac
    , generateImageR2
    , Color
    , white
    , black
    , red
    , green
    , blue
    , lerpC
    , lighten
    , darken
    , bilerpC
    ) where

import Codec.Picture hiding (Image, Color)
import qualified Codec.Picture as JP (Image)

-- | Type Point with Real Coordinates
type Point = (Float, Float)

-- type fraction means numbers between 0 and 1
type Frac = Float

-- | a type for the functional images (note FImage to avoid name clashes)
type FImage a = Point -> a

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
