{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Reflex.Dom.Brownies
import           FunctionalImages
import           FunctionalImagesBase
import           Data.Monoid ((<>))
import qualified Data.Map as Map
import qualified Data.Text as T



main :: IO ()
main = mainWidgetWithHead headElement bodyElement

headElement :: MonadWidget t m => m ()
headElement = el "title" $ text "Simple Images"

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  el "h2" $ text "Images"
  ctrlImage "Vstrip" vstrip 7
  ctrlImage "Chessboard" checker 6
  ctrlImage "Alternate Concentric Rings" altRings 10
  ctrlImageEx "Polar Checkboard" (polarChecker 10) 10 300
  ctrlImageEx "Wave Distance" wavDist 10 300
  ctrlImageEx "Sierpinski Triangle" sierpinski 256 256
  let redSquare :: Image Color
      redSquare _ = darken 0.2 red
  ctrlImage "Red" redSquare 5
  ctrlImageEx "Colored Polar Chessboard" coloredPolarChess 10 300
  ctrlImage "Color Interpolation" bilerpBRBW 2
  ctrlImage "Yellow Blue Rings" ybRings 10
  return ()

ctrlImage :: (ToPixelRGBA8 a, MonadWidget t m) => T.Text -> Image a -> Int ->  m ()
ctrlImage imgName imgFunction width = ctrlImageEx imgName imgFunction width 256

-- | 
ctrlImageEx :: (ToPixelRGBA8 a, MonadWidget t m) => T.Text -> Image a -> Int -> Int -> m ()
ctrlImageEx imgName imgFunction width size = do
  el "h4" $ text imgName
  text "Width: "
  ti <- textInput $ def & textInputConfig_initialValue .~ (T.pack . show) width
  evClick <- button "Show"
  el "p" blank
  
  let evWidth = tagPromptlyDyn (value ti) evClick
  let evImg = pixelFunction imgFunction <$> evWidth
  let strSize =  (T.pack . show) size
  let attr = "width" =: strSize <> "height" =: strSize
  pixelCanvasAttr attr evImg
  return ()

 
-- Pure user user functions ------------------------------------------------------------------------------

pixelFunction :: ToPixelRGBA8 a => Image a -> T.Text -> PixelFunction
pixelFunction f strW = convertFunction w  w f
   where w = read $ T.unpack strW
