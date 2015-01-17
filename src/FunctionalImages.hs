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
   -- * interface functions to JuicyPixels
   generateBWImageR2
   ) where

import Codec.Picture
import FunctionalImagesBase

-- | image: Generate an vertical black strip
vstrip :: Point -> Bool
vstrip (x, y) = abs x < 0.5

-- | image: Generate a chess board
checker :: Point -> Bool
checker (x, y) = even $ floor x + floor y
