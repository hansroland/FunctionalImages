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
   -- * interface functions to JuicyPixels
   generateBWImageR2
   ) where

import Codec.Picture
import FunctionalImagesBase

-- | image: Generate an vertical black strip
vstrip :: Point -> Bool
vstrip (x, y) = abs x < 0.5 
 
