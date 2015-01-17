-- ---------------------------------------------------------------------------
-- Base Functions and types for FunctionalImages
-- ---------------------------------------------------------------------------
--
-- 
--
--
-- ---------------------------------------------------------------------------

module FunctionalImagesBase

where

import Codec.Picture

type Point = (Float, Float)


-- Generate an image in the Real X Real space
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

-- a litte support function for generateBWImageR2
bool2rgb8 :: Bool -> PixelRGB8
bool2rgb8 True  = rgb8Black
bool2rgb8 False = rgb8White

-- generate a black and white image in R2
generateBWImageR2 :: (Point -> Bool)   -- black white function in R2
         -> (Float, Float)                      -- lower left corner
         -> (Float, Float)                      -- upper right corner
         -> Int                                 -- width in pixel
         -> Image PixelRGB8                     -- Resulting picture
generateBWImageR2 bwRenderer = generateImageR2 (bool2rgb8 . bwRenderer)






