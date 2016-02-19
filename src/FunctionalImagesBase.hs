-- ---------------------------------------------------------------------------
-- Base Functions and types for FunctionalImages
-- ---------------------------------------------------------------------------
--
-- Here we have the base functionality for the functional images.
-- This is a bridge between the geometric spaces of Conal Eliott
-- and the image gnereating functions of JuicyPixels.
--
-- ---------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-}

module FunctionalImagesBase
    ( FImage
    , Point
    , Frac
    , generateImageR2
    , PxColor
    , pxWhite
    , pxBlack
    , pxRed
    , pxGreen
    , pxBlue
    , lerpC
    , lighten
    , darken
    , bilerpC
    ) where

import Codec.Picture

-- | Type Point with Real Coordinates
type Point = (Float, Float)

-- type fraction means numbers between 0 and 1
type Frac = Float

-- | a type for the Functional Images (note FImage to avoid name clashes)
type FImage a = Point -> a

-- a little helper function to restict a value into an interal
inInterval :: Ord a => a -> a -> a -> a
inInterval val low high
   | val < low = low
   | val > high = high
   | otherwise  = val

-- | a class to convert to JuicyPixel colors
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

-- | Generate an image in the Real X Real space
generateImageR2 :: ToPixelRGB8 a => (Point -> a)  -- Coordinate Rendering function
         -> Point                           -- lower left corner
         -> Point                            -- upper right corner
         -> Int                              -- width in pixel
         -> Image PixelRGB8                          -- Resulting picture
generateImageR2 coordRenderer (x0,y0) (x1,y1) width = generateImage pixelRenderer width height
    where
      pixelSize = (x1 - x0) / fromIntegral width
      height = floor $ (y1 - y0) / (x1 - x0) * fromIntegral width
      pixelRenderer ix iy = toPixelRGB8 $ coordRenderer (x0 + fromIntegral ix * pixelSize, y1 - fromIntegral iy * pixelSize)

-- ----------------------------------------------------------------------------
-- Color support
-- ----------------------------------------------------------------------------

type PxColor = PixelRGBF

instance ToPixelRGB8 PixelRGBF
   where
     toPixelRGB8 (PixelRGBF r g b ) =
        PixelRGB8 (f r) (f g) (f b)
           where f = frac2pixel8

pxRed :: PixelRGBF
pxRed = PixelRGBF 1 0 0

pxWhite :: PixelRGBF
pxWhite = PixelRGBF 1 1 1

pxBlack :: PixelRGBF
pxBlack = PixelRGBF 0 0 0

pxBlue :: PixelRGBF
pxBlue = PixelRGBF 0 0 1

pxGreen :: PixelRGBF
pxGreen = PixelRGBF 0 1 0

-- | linear interpolate betrween 2 colors
--   The weight w must be between 0 and 1
lerpC :: Float -> PxColor -> PxColor -> PxColor
lerpC w (PixelRGBF r1 g1 b1) (PixelRGBF r2 g2 b2) =
  PixelRGBF (h r1 r2) (h g1 g2) (h b1 b2)
    where
      h x1 x2 = w * x1 + (1 - w) * x2

-- | Two useful functions
lighten, darken :: Frac -> PxColor -> PxColor
lighten w = lerpC w pxWhite
darken  w = lerpC w pxBlack

-- | Two dimensional interpolation
bilerpC :: PxColor -> PxColor -> PxColor -> PxColor -> (Frac, Frac) -> PxColor
bilerpC ll lr ul ur (wx, wy) =
  lerpC wy (lerpC wx ll lr) (lerpC wx ul ur)
