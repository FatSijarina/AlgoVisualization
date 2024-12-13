module VisualizationWithButtons where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Algorithms.MergeSort (mergeSortSteps)
import Common (SortStep(..))

window :: Display
window = InWindow "Merge Sort Visualization" (800, 600) (100, 100)

backgroundColor :: Color
backgroundColor = white

frameDelay :: Float
frameDelay = 1.0

-- Define gray color
gray :: Color
gray = makeColorI 169 169 169 255 -- RGB values for gray

data AppState = AppState
  { steps :: [SortStep Int]
  , currentStep :: Int
  , paused :: Bool
  }

-- Draw the visualization, including buttons
drawState :: AppState -> Picture
drawState (AppState steps currentStep paused) =
  Pictures [arrayPic, buttonPic, textPic]
  where
    arrayPic = case steps !! currentStep of
      SortStep array activeIndices (i, j) ->
        drawArray array activeIndices (i, j)
    buttonPic = Pictures
      [ drawButton (-350, -250) 100 50 "Play" (if paused then green else gray)
      , drawButton (-200, -250) 100 50 "Pause" (if paused then gray else red)
      ]
    textPic = Translate (-300) 200 $ Scale 0.2 0.2 $ Text $ "Step: " ++ show currentStep

-- Draw array visualization
drawArray :: [Int] -> [Int] -> (Maybe Int, Maybe Int) -> Picture
drawArray array activeIndices (i, j) =
  Pictures $ zipWith drawBar [0..] array
  where
    barWidth = 30
    spacing = 10
    scaleFactor = 10
    xOffset = - (fromIntegral (length array) * (barWidth + spacing)) / 2

    drawBar idx val =
      let x = xOffset + fromIntegral idx * (barWidth + spacing)
          h = fromIntegral val * scaleFactor
          colorBar
            | Just idx == i || Just idx == j = red
            | idx `elem` activeIndices = orange
            | otherwise = blue
      in Translate x (-50) $ Color colorBar $ Polygon [(0, 0), (0, h), (barWidth, h), (barWidth, 0)]

-- Draw a button
drawButton :: (Float, Float) -> Float -> Float -> String -> Color -> Picture
drawButton (x, y) width height label color =
  Pictures
    [ Translate x y $ Color color $ Polygon [(0, 0), (width, 0), (width, height), (0, height)]
    , Translate (x + 10) (y + 10) $ Scale 0.1 0.1 $ Color black $ Text label
    ]

-- Handle events
handleEvent :: Event -> AppState -> AppState
handleEvent (EventKey (MouseButton LeftButton) Up _ (mouseX, mouseY)) state =
  if mouseX >= -350 && mouseX <= -250 && mouseY >= -250 && mouseY <= -200
    then state { paused = False } -- Play button
  else if mouseX >= -200 && mouseX <= -100 && mouseY >= -250 && mouseY <= -200
    then state { paused = True } -- Pause button
  else state
handleEvent _ state = state

-- Update the simulation
update :: Float -> AppState -> AppState
update _ state@(AppState steps currentStep paused)
  | paused = state
  | currentStep < length steps - 1 = state { currentStep = currentStep + 1 }
  | otherwise = state

-- Main function
main :: IO ()
main = do
  let array = [5, 2, 9, 1, 5, 6]
  let sortSteps = mergeSortSteps array
  play
    window
    backgroundColor
    1
    (AppState sortSteps 0 True)
    drawState
    handleEvent
    update
