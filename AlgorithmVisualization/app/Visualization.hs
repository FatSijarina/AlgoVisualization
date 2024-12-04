module Visualization (drawState, animateStates) where

import Graphics.Gloss

drawState :: [Int] -> Picture
drawState xs = Pictures $ map (drawBarWithMaxValue maxVal) (zip [0..] xs)
  where
    totalWidth = fromIntegral (length xs) * (barWidth + spacing)
    barWidth = 40
    spacing = 10
    scaleFactor = 20
    barColor = light blue
    maxVal = maximum xs

    -- Nested function
    drawBarWithMaxValue :: Int -> (Int, Int) -> Picture
    drawBarWithMaxValue maxValue (i, val) = Pictures [barPic, labelPic]
      where
        xPos = fromIntegral i * (barWidth + spacing) - totalWidth / 2
        barHeight = fromIntegral val * scaleFactor
        barYPos = (barHeight / 2) + labelAreaHeight
        barPic = translate xPos barYPos $ color barColor $ rectangleSolid barWidth barHeight
        labelPic = translate xPos labelY $ scale labelScale labelScale $ color labelColor $ Text (show val)
        labelY = labelAreaHeight / 2 - labelOffset
        labelOffset = 10
        labelScale = 0.10
        labelColor = black
        labelAreaHeight = 50

animateStates :: [[Int]] -> IO ()
animateStates states = simulate window white fps initialIndex render update
  where
    window = InWindow "Algorithm Visualization" (800, 600) (100, 100)
    fps = 1  -- Adjust frames per second as needed
    initialIndex = 0

    render idx = drawState (states !! idx)

    update _ _ idx = if idx < length states - 1 then idx + 1 else idx
