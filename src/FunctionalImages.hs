-- --------------------------------------------------
-- Examples of Functional images
-- --------------------------------------------------

module FunctionalImages

where

import FunctionalImagesBase
import Data.Bits

-- The function to genrate a vertical strip
vstrip :: FImage Bool
vstrip (x, y) = abs x < 0.5

-- The function to create a chess board
checker :: FImage Bool
checker (x, y) = even $ floor x + floor y

-- | Polar Checkboard
polarChecker :: Int -> FImage Bool
polarChecker n = checker . sc . toPolar
   where
     sc (r,a) = (r,a * fromIntegral n / pi)

-- | alternate concentric Rings
altRings :: FImage Bool
altRings  = even . floor . dist0

-- | waveDist
wavDist :: FImage Frac
wavDist p = (1 + cos (pi * dist0 p)) / 2

-- | sierpinski - An other way to draw a sierpinski triangle
sierpinski :: FImage Bool
sierpinski (x , y) = abs ix .|. abs iy == abs ix
  where
    ix  = round x :: Int
    iy = round y :: Int


bilerpBRBW :: FImage Color
bilerpBRBW = bilerpC black red blue white


blackWhiteIm, blueYellowIm :: Region -> ImageC
blackWhiteIm reg = cond reg blackI whiteI
blueYellowIm reg = cond reg blueI yellowI

coloredPolarChess :: ImageC
coloredPolarChess = lerpI wavDist(blackWhiteIm (polarChecker 10)) (blueYellowIm checker )

-- | Rings that alternate between yellow and blue 
ybRings :: ImageC
ybRings = lerpI wavDist blueI yellowI
