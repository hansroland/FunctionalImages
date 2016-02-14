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
    , IPoint
    , Frac
    , generateImageR2
    , generateBWImageI2
    ) where

import Codec.Picture

-- | Type Point with Real Coordinates
type Point = (Float, Float)

-- | Type IPoint with Integer (Int) Coordinates
type IPoint = (Int, Int)

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


-- | Generate an image in the Int X Int space
--   Here we have not yet a scaling factor
--   Upper and lower left corners define the size of the image
generateImageI2 :: Pixel a => (IPoint -> a)   -- Coordinate Rendering function
         -> IPoint                            -- lower left corner
         -> IPoint                            -- upper right corner
         -> Image a                           -- Resulting picture
generateImageI2 coordRenderer (x0,y0) (x1,y1) =
    generateImage pixelRenderer (x1 - x0) (y1 - y0)
    where
      pixelRenderer ix iy = coordRenderer (x0 + ix, y1 - iy)

-- | Generate an
generateBWImageI2 :: (IPoint -> Bool)            -- black white function in R2
         -> IPoint                               -- lower left corner
         -> IPoint                               -- upper right corner
         -> Image PixelRGB8                      -- Resulting picture
generateBWImageI2 bwRenderer = generateImageI2 (toPixelRGB8 . bwRenderer)
