module Visualization (drawAppState) where

import Graphics.Gloss
import Controller (AppState(..), AlgorithmChoice(..))
import Data.Maybe (catMaybes)
import Common (SortStep(..))

-- Define custom colors for the buttons
lighterBlue :: Color
lighterBlue = makeColorI 173 216 230 255 -- RGB values for a soft light blue

lighterGreen :: Color
lighterGreen = makeColorI 144 238 144 255 -- RGB values for a soft light green

grey :: Color
grey = makeColorI 169 169 169 255

-- Draw the application state
drawAppState :: AppState -> Picture
drawAppState (AppState steps currentStep paused selectedAlg userInput) =
  Pictures [arrayPic, buttonsPic, inputPic, infoPic]
  where
    -- Get the current sorting step and array
    (SortStep lst active (i, j)) = steps !! currentStep
    
    -- Draw the array as bars
    arrayPic = drawState (formatAlgName selectedAlg) lst active (i, j)
    
    -- Draw the buttons for algorithm selection and play/pause actions
    buttonsPic = Pictures
      [ drawButton (-350, -250) 100 50 "Bubble Sort"    (if selectedAlg == Bubble    then lighterBlue else grey)
      , drawButton (-230, -250) 100 50 "Selection Sort" (if selectedAlg == Selection then lighterBlue else grey)
      , drawButton (-110, -250) 100 50 "Insertion Sort" (if selectedAlg == Insertion then lighterBlue else grey)
      , drawButton (10,   -250) 100 50 "Merge Sort"     (if selectedAlg == Merge     then lighterBlue else grey)
      , drawButton (130,  -250) 100 50 "Quick Sort"     (if selectedAlg == Quick     then lighterBlue else grey)
      , drawButton (250,  -250) 60 50 "Play"       (if paused then lighterGreen else grey)
      , drawButton (320,  -250) 60 50 "Pause"      (if paused then grey else red)
      , drawButton (390,  -250) 60 50 "Replay"     lighterGreen
      ]

    -- Display the user input for the array as text
    inputPic = translate (-350) (-300) $ scale 0.2 0.2 $ Color black $ Text ("Array input: " ++ userInput)

    -- Display the current step of the sorting algorithm
    infoPic = Translate (-350) 200 $ Scale 0.2 0.2 $ Color black $ Text $ "Step: " ++ show currentStep

-- Format algorithm names for display
formatAlgName :: AlgorithmChoice -> String
formatAlgName Bubble    = "Bubble Sort"
formatAlgName Selection = "Selection Sort"
formatAlgName Insertion = "Insertion Sort"
formatAlgName Merge     = "Merge Sort"
formatAlgName Quick     = "Quick Sort"

-- Draw the array of bars (sorted or not)
drawState :: String -> [Int] -> [Int] -> (Maybe Int, Maybe Int) -> Picture
drawState title xs activeIndices (i, j) = Pictures [barsPic, titlePic, indicesPic]
  where
    -- Check if the array is empty to avoid calling maximum on an empty list
    maxBarHeight = if null xs then 0 else fromIntegral (maximum xs) * scaleFactor

    -- Bar visualization parameters
    barWidth = 40.0 :: Float
    spacing = 20.0 :: Float
    scaleFactor = 20.0 :: Float
    totalWidth = fromIntegral (length xs) * (barWidth + spacing) - spacing

    -- Offsets for centering the bars
    xOffset = -totalWidth / 2
    yOffset = -maxBarHeight / 2

    -- Function to color bars based on whether they are active
    barColor idx = if idx `elem` activeIndices then red else light blue

    -- Draw all bars for the current array state
    barsPic = Pictures $ map (drawBar xOffset yOffset barWidth spacing scaleFactor barColor) (zip [0..] xs)

    -- Title for the algorithm
    titleWidth = fromIntegral (length title) * 12.0
    titlePic = translate (-titleWidth / 2) (maxBarHeight / 2 + 50) $ 
               scale 0.2 0.2 $ color black $ Text title

    -- Draw indices below the bars
    indicesPic = Pictures [translate (xOffset + fromIntegral idx * (barWidth + spacing)) (-maxBarHeight / 2 - 40)
                           $ scale 0.15 0.15 $ color black $ Text (show idx)
                           | idx <- [0..length xs - 1]]


-- Function to draw an individual bar for the sorting visualization
drawBar :: Float -> Float -> Float -> Float -> Float -> (Int -> Color) -> (Int, Int) -> Picture
drawBar xOffset yOffset barWidth spacing scaleFactor barColor (idx, val) =
  Pictures [barPic, labelPic]
  where
    -- Position for the bar
    xPos = fromIntegral idx * (barWidth + spacing) + xOffset
    barHeight = fromIntegral val * scaleFactor
    barPic = translate xPos (yOffset + barHeight / 2) $
             color (barColor idx) $ rectangleSolid barWidth barHeight

    -- Label to show the value of each bar
    textWidth = fromIntegral (length (show val)) * 5.0
    labelPic = translate (xPos - textWidth / 2) (yOffset + 5) $
           scale 0.15 0.15 $ color black $ Text (show val)

-- Function to draw buttons
drawButton :: (Float, Float) -> Float -> Float -> String -> Color -> Picture
drawButton (x, y) width height label color =
  Pictures
    [ Translate x y $ Color color $ Polygon [(0, 0), (width, 0), (width, height), (0, height)]
    , Translate (x + 10) (y + 10) $ Scale 0.1 0.1 $ Color black $ Text label
    ]