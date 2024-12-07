module Visualization (drawState, animateStates) where

import Graphics.Gloss
import Algorithms (SortStep(..))

drawState :: [Int] -> [Int] -> Picture
drawState xs activeIndices = Pictures [barsPic, titlePic]
  where
    -- Dimensions and scaling
    barWidth = 40.0 :: Float
    spacing = 10.0 :: Float
    scaleFactor = 20.0 :: Float
    totalWidth = fromIntegral (length xs) * (barWidth + spacing) - spacing
    maxBarHeight = fromIntegral (maximum xs) * scaleFactor

    -- Centering offsets
    xOffset = -totalWidth / 2
    yOffset = -maxBarHeight / 2

    -- Color logic for bars
    barColor i = if i `elem` activeIndices then red else light blue

    -- Draw the bars and their labels
    barsPic = Pictures $ map drawBar (zip [0..] xs)
    drawBar (i, val) = Pictures [barPic, labelPic]
      where
        xPos = fromIntegral i * (barWidth + spacing) + xOffset
        barHeight = fromIntegral val * scaleFactor
        barPic = translate xPos (yOffset + barHeight / 2) $
                 color (barColor i) $ rectangleSolid barWidth barHeight
        labelPic = translate xPos (yOffset - 15) $ -- Adjusted to be below the bar
                   scale 0.15 0.15 $ color black $ Text (show val)

    -- Title at the top
    titlePic = translate (-totalWidth / 4) (maxBarHeight / 2 + 50) $
               scale 0.2 0.2 $ color black $ Text "Bubble Sort Visualization"

animateStates :: [SortStep Int] -> IO ()
animateStates steps = simulate window white fps initialIndex render update
  where
    window = InWindow "Algorithm Visualization" (800, 600) (100, 100)
    fps = 1
    initialIndex = 0

    render idx = let SortStep lst active = steps !! idx
                 in drawState lst active

    update _ _ idx = if idx < length steps - 1 then idx + 1 else idx