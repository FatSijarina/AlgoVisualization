module Visualization (drawState, animateStates) where

import Graphics.Gloss
import Algorithms (SortStep(..))
import Data.Maybe (catMaybes)

drawState :: String -> [Int] -> [Int] -> (Maybe Int, Maybe Int) -> Picture
drawState title xs activeIndices (i, j) = Pictures [barsPic, titlePic, indicesPic]
  where
    barWidth = 40.0 :: Float
    spacing = 20.0 :: Float
    scaleFactor = 20.0 :: Float
    totalWidth = fromIntegral (length xs) * (barWidth + spacing) - spacing
    maxBarHeight = fromIntegral (maximum xs) * scaleFactor

    xOffset = -totalWidth / 2
    yOffset = -maxBarHeight / 2

    barColor idx = if idx `elem` activeIndices then red else light blue

    barsPic = Pictures $ map drawBar (zip [0..] xs)
    drawBar (idx, val) = Pictures [barPic, labelPic]
      where
        xPos = fromIntegral idx * (barWidth + spacing) + xOffset
        barHeight = fromIntegral val * scaleFactor
        barPic = translate xPos (yOffset + barHeight / 2) $
                color (barColor idx) $ rectangleSolid barWidth barHeight
        -- Përllogaritje e përafërt e gjysmës së gjatësisë së tekstit
        textWidth = fromIntegral (length (show val)) * 5.0
        labelPic = translate (xPos - textWidth / 2) (yOffset - 30) $
                  scale 0.15 0.15 $ color black $ Text (show val)

    titleWidth = fromIntegral (length title) * 12.0
    titlePic = translate (-titleWidth / 2) (maxBarHeight / 2 + 50) $
               scale 0.2 0.2 $ color black $ Text title

    indicesPic = Pictures $ catMaybes [drawIndex i "i", drawIndex j "j"]
    drawIndex (Just idx) label =
      let xPos = fromIntegral idx * (barWidth + spacing) + xOffset
      in Just $ translate xPos (yOffset + maxBarHeight + 20) $
                scale 0.15 0.15 $ color green $ Text label
    drawIndex Nothing _ = Nothing

animateStates :: String -> [SortStep Int] -> IO ()
animateStates title steps = animate window white frameRenderer
  where
    window = InWindow "Algorithm Visualization" (800, 600) (100, 100)
    delayPerFrame = 3.0

    frameRenderer :: Float -> Picture
    frameRenderer time =
      let idx = min (floor (time / delayPerFrame)) (length steps - 1)
          SortStep lst active current = steps !! idx
      in drawState title lst active current
