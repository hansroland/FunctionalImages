-- ---------------------------------------------------------------------------
-- Base Functions and types for FunctionalImages
-- ---------------------------------------------------------------------------
--
-- Here we have the base functionality for the functional images.
-- This is a bridge between the geometric spaces of Conal Eliott
-- and the image gnereating functions of JuicyPixels.
--
-- ---------------------------------------------------------------------------

module FunctionalImagesBase(
    Point,
    Frac,
    generateBWImageR2,
    generateGrayImageR2
    ) where

import Codec.Picture

type Point = (Float, Float)

-- type fraction means numbers between 0 and 1
type Frac = Float

-- a little helper function to restict a value into an interal
inInterval :: Ord a => a -> a -> a -> a
inInterval val low high
   | val < low = low
   | val > high = high
   | otherwise  = val

-- | Generate an image in the Real X Real space
generateImageR2 :: Pixel a => (Point -> a) 	-- Coordinate Rendering function
         -> (Float, Float)                      -- lower left corner
         -> (Float, Float)                      -- upper right corner
         -> Int                                 -- width in pixel
         -> Image a                             -- Resulting picture
generateImageR2 coordRenderer (x0,y0) (x1,y1) width = generateImage pixelRenderer width height
    where
      pixelSize = (x1 - x0) / (fromIntegral width)
      height = floor $ (y1 - y0) / (x1 - x0) * (fromIntegral width)
      pixelRenderer ix iy = coordRenderer ((x0 + (fromIntegral ix) * pixelSize), (y1 - (fromIntegral iy) * pixelSize))

-- A set of some simple colors
rgb8White = PixelRGB8 255 255 255
rgb8Black = PixelRGB8 0 0 0


-- ---------------------------------------------------------------------------
-- Black and White images
-- ---------------------------------------------------------------------------

-- | General function to generate a black and white image in R2
generateBWImageR2 :: (Point -> Bool)            -- black white function in R2
         -> (Float, Float)                      -- lower left corner
         -> (Float, Float)                      -- upper right corner
         -> Int                                 -- width in pixel
         -> Image PixelRGB8                     -- Resulting picture
generateBWImageR2 bwRenderer = generateImageR2 (bool2rgb8 . bwRenderer)

-- A litte support function for generateBWImageR2
-- Convert Boolean values to Colors
bool2rgb8 :: Bool -> PixelRGB8
bool2rgb8 True  = rgb8Black
bool2rgb8 False = rgb8White

-- ----------------------------------------------------------------------------
-- Gray images
-- ----------------------------------------------------------------------------

-- generate a grayscale image in R2
generateGrayImageR2 :: (Point -> Frac)				
         -> (Float, Float)
         -> (Float, Float)
         -> Int
         -> Image PixelRGB8
generateGrayImageR2 renderer = generateImageR2 (frac2rgb8 . renderer)

-- a little support function to generate grey images
frac2rgb8 :: Frac -> PixelRGB8
frac2rgb8 f = PixelRGB8 ff ff ff
    where 
      ff = frac2pixel8 f

-- function to convert a Fraction between 0 and 1 to an Integer betwenn 0 and255
frac2pixel8 :: Frac -> Pixel8
frac2pixel8 f = floor $ (fromIntegral 255) * (inInterval f 0 1)





