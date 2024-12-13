module Visualization (drawAppState) where

import Graphics.Gloss
import Common (SortStep(..))
import Data.Maybe (catMaybes)
import Controller (AppState(..), AlgorithmChoice(..))

-- Define a custom lighter blue color for button selection
lighterBlue :: Color
lighterBlue = makeColorI 173 216 230 255 -- RGB values for a soft light blue

grey :: Color
grey = makeColorI 169 169 169 255

drawAppState :: AppState -> Picture
drawAppState (AppState steps currentStep paused selectedAlg) =
  Pictures [arrayPic, buttonsPic, infoPic]
  where
    (SortStep lst active (i, j)) = steps !! currentStep
    arrayPic = drawState (show selectedAlg) lst active (i, j)

    buttonsPic = Pictures
      [ drawButton (-350, -250) 100 50 "Bubble"    (if selectedAlg == Bubble    then lighterBlue else grey)
      , drawButton (-230, -250) 100 50 "Selection" (if selectedAlg == Selection then lighterBlue else grey)
      , drawButton (-110, -250) 100 50 "Insertion" (if selectedAlg == Insertion then lighterBlue else grey)
      , drawButton (10,   -250) 100 50 "Merge"     (if selectedAlg == Merge     then lighterBlue else grey)
      , drawButton (130,  -250) 100 50 "Quick"     (if selectedAlg == Quick     then lighterBlue else grey)
      , drawButton (250,  -250) 60 50 "Play"       (if paused then lighterBlue else grey)
      , drawButton (320,  -250) 60 50 "Pause"      (if paused then grey else red)
      ]

    infoPic = Translate (-350) 200 $ Scale 0.2 0.2 $ Color black $ Text $ "Step: " ++ show currentStep

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

    barsPic = Pictures $ map (drawBar xOffset yOffset barWidth spacing scaleFactor barColor) (zip [0..] xs)

    titleWidth = fromIntegral (length title) * 12.0
    titlePic = translate (-titleWidth / 2) (maxBarHeight / 2 + 50) $
               scale 0.2 0.2 $ color black $ Text title

    indicesPic = Pictures $ catMaybes [drawIndex i "i", drawIndex j "j"]
    drawIndex (Just idx) label =
      let xPos = fromIntegral idx * (barWidth + spacing) + xOffset
      in Just $ translate xPos (yOffset + maxBarHeight + 20) $
                scale 0.15 0.15 $ color green $ Text label
    drawIndex Nothing _ = Nothing

drawBar :: Float -> Float -> Float -> Float -> Float -> (Int -> Color) -> (Int, Int) -> Picture
drawBar xOffset yOffset barWidth spacing scaleFactor barColor (idx, val) =
  Pictures [barPic, labelPic]
  where
    xPos = fromIntegral idx * (barWidth + spacing) + xOffset
    barHeight = fromIntegral val * scaleFactor
    barPic = translate xPos (yOffset + barHeight / 2) $
             color (barColor idx) $ rectangleSolid barWidth barHeight

    textWidth = fromIntegral (length (show val)) * 5.0
    labelPic = translate (xPos - textWidth / 2) (yOffset - 30) $
               scale 0.15 0.15 $ color black $ Text (show val)

drawButton :: (Float, Float) -> Float -> Float -> String -> Color -> Picture
drawButton (x, y) width height label color =
  Pictures
    [ Translate x y $ Color color $ Polygon [(0, 0), (width, 0), (width, height), (0, height)]
    , Translate (x + 10) (y + 10) $ Scale 0.1 0.1 $ Color black $ Text label
    ]
