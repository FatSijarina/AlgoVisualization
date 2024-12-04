module Visualization (drawState, animateStates) where

import Graphics.Gloss

drawState :: [Int] -> Picture
drawState xs = Pictures $ map drawBar (zip [0..] xs)
  where
    totalWidth = fromIntegral (length xs) * (barWidth + spacing)
    barWidth = 10
    spacing = 2
    scaleFactor = 5
    barColor = blue

    drawBar (i, val) = translate xPos 0 $ color barColor $ rectangleSolid barWidth barHeight
      where
        xPos = fromIntegral i * (barWidth + spacing) - totalWidth / 2
        barHeight = fromIntegral val * scaleFactor

animateStates :: [[Int]] -> IO ()
animateStates states = simulate window white fps initialIndex render update
  where
    window = InWindow "Algorithm Visualization" (800, 600) (100, 100)
    fps = 1  -- Adjust frames per second as needed
    initialIndex = 0

    render idx = drawState (states !! idx)

    update _ _ idx = if idx < length states - 1 then idx + 1 else idx
