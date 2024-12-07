module Visualization (drawState, animateStates) where

import Graphics.Gloss
import Algorithms (SortStep(..))
import Data.Maybe (catMaybes)

-- Function to draw the state of the algorithm at each step
drawState :: String -> [Int] -> [Int] -> (Maybe Int, Maybe Int) -> Picture
drawState title xs activeIndices (i, j) = Pictures [barsPic, titlePic, indicesPic]
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
    barColor idx = if idx `elem` activeIndices then red else light blue

    -- Draw the bars and their labels
    barsPic = Pictures $ map drawBar (zip [0..] xs)
    drawBar (idx, val) = Pictures [barPic, labelPic]
      where
        xPos = fromIntegral idx * (barWidth + spacing) + xOffset
        barHeight = fromIntegral val * scaleFactor
        barPic = translate xPos (yOffset + barHeight / 2) $
                 color (barColor idx) $ rectangleSolid barWidth barHeight
        labelPic = translate xPos (yOffset - 15) $
                   scale 0.15 0.15 $ color black $ Text (show val)

    -- Calculate width of the title dynamically
    titleWidth = fromIntegral (length title) * 12.0  -- Rough estimate of text width per character

    -- Title at the top, centered
    titlePic = translate (-titleWidth / 2) (maxBarHeight / 2 + 50) $
               scale 0.2 0.2 $ color black $ Text title

    -- Draw the indices i and j
    indicesPic = Pictures $ catMaybes [drawIndex i "i", drawIndex j "j"]

    -- Function to draw an index label (i or j) if the index is Just
    drawIndex (Just idx) label =
      let xPos = fromIntegral idx * (barWidth + spacing) + xOffset
      in Just $ translate xPos (yOffset + maxBarHeight + 20) $
                scale 0.15 0.15 $ color green $ Text label
    drawIndex Nothing _ = Nothing  -- Don't draw if index is Nothing

-- Function to animate the states
animateStates :: String -> [SortStep Int] -> IO ()
animateStates title steps = simulate window white fps initialIndex render update
  where
    window = InWindow "Algorithm Visualization" (800, 600) (100, 100)
    fps = 1
    initialIndex = 0

    render idx =
      let SortStep lst active current = steps !! idx
      in drawState title lst active current

    update _ _ idx = if idx < length steps - 1 then idx + 1 else idx
