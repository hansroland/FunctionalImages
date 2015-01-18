-- ---------------------------------------------------------------------------
-- FunctionalImages.hs
-- ---------------------------------------------------------------------------
--
-- Here we have the image generating functions of Conal Eliott
--
-- ---------------------------------------------------------------------------

module FunctionalImages (
   -- * image creating functions of Conal Eliott
   vstrip,
   checker,
   altRings,
   polarChecker,
   wavDist,
   sierpinski,
   -- * interface functions to JuicyPixels
   generateBWImageR2,
   generateBWImageI2,
   generateGrayImageR2
   ) where

import Data.Bits

import Codec.Picture
import FunctionalImagesBase

-- ----------------------------------------------------------------------------
-- First simple images
-- ----------------------------------------------------------------------------

-- | image: Generate an vertical black strip
vstrip :: Point -> Bool
vstrip (x, y) = abs x < 0.5

-- | image: Generate a chess board
checker :: Point -> Bool
checker (x, y) = even $ floor x + floor y

-- | distance to the origin of the coordiante system
distO :: Point -> Float
distO (x, y) = sqrt $ x**2 + y**2

-- | alternate concentric Rings
altRings :: Point -> Bool
altRings  = even . floor . distO

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
toPolar (x, y) = (distO (x, y), atan2 y x)

-- | Polar Checkboard
polarChecker :: Int -> Point -> Bool
polarChecker n = checker . sc . toPolar
   where
     sc (r,a) = (r,a * fromIntegral n / pi)

-- | sierpinski - An other way todraw a sierpinski triangle
sierpinski :: IPoint -> Bool
sierpinski (x , y) = (abs x) .|. (abs y) == abs x

-- | waveDist
wavDist :: Point -> Frac
wavDist p = (1 + cos (pi * distO p)) / 2





