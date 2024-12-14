module Controller (AppState(..), AlgorithmChoice(..), initialState, handleEvent, update) where

import Graphics.Gloss.Interface.Pure.Game
import Algorithms (bubbleSortSteps, insertionSortSteps, selectionSortSteps, mergeSortSteps, quickSortSteps, SortStep(..))

data AlgorithmChoice
  = Bubble
  | Selection
  | Insertion
  | Merge
  | Quick
  deriving (Eq, Show)

data AppState = AppState
  { steps            :: [SortStep Int]
  , currentStep      :: Int
  , paused           :: Bool
  , selectedAlgorithm :: AlgorithmChoice
  }

getAlgorithmSteps :: AlgorithmChoice -> [Int] -> [SortStep Int]
getAlgorithmSteps Bubble    = bubbleSortSteps
getAlgorithmSteps Selection = selectionSortSteps
getAlgorithmSteps Insertion = insertionSortSteps
getAlgorithmSteps Merge     = mergeSortSteps
getAlgorithmSteps Quick     = quickSortSteps

-- Initialize the state with a user-defined array
initialState :: [Int] -> AlgorithmChoice -> AppState
initialState userArray initialAlg =
  let stps = getAlgorithmSteps initialAlg userArray
  in AppState stps 0 True initialAlg

-- Handle events (mouse clicks for buttons)
handleEvent :: Event -> AppState -> AppState
handleEvent (EventKey (MouseButton LeftButton) Up _ (mx, my)) state =
  handleButtonClick (mx, my) state
handleEvent _ state = state

-- Button click handling
handleButtonClick :: (Float, Float) -> AppState -> AppState
handleButtonClick (mx, my) state
  -- Check algorithm buttons:
  | inButton (mx, my) (-350, -250) 100 50 = selectAlgorithm Bubble state
  | inButton (mx, my) (-230, -250) 100 50 = selectAlgorithm Selection state
  | inButton (mx, my) (-110, -250) 100 50 = selectAlgorithm Insertion state
  | inButton (mx, my) (10, -250)   100 50 = selectAlgorithm Merge state
  | inButton (mx, my) (130, -250)  100 50 = selectAlgorithm Quick state
  -- Play/Pause buttons:
  | inButton (mx, my) (250, -250) 60 50 = state { paused = False }
  | inButton (mx, my) (320, -250) 60 50 = state { paused = True }
  -- Replay button:
  | inButton (mx, my) (390, -250) 60 50 = replayAlgorithm state
  | otherwise = state

-- Replay the algorithm from the beginning
replayAlgorithm :: AppState -> AppState
replayAlgorithm state@(AppState _ _ _ alg) =
  let currentArray = case steps state of
                       (SortStep lst _ _) : _ -> lst
                       _ -> []
      newSteps = getAlgorithmSteps alg currentArray
  in state { steps = newSteps, currentStep = 0, paused = False }

-- Check if click is inside a button
inButton :: (Float, Float) -> (Float, Float) -> Float -> Float -> Bool
inButton (mx, my) (bx, by) w h =
  mx >= bx && mx <= bx + w && my >= by && my <= by + h

-- Select a sorting algorithm
selectAlgorithm :: AlgorithmChoice -> AppState -> AppState
selectAlgorithm newAlg state =
  let currentArray = case steps state of
                       (SortStep lst _ _) : _ -> lst
                       _ -> []
      newSteps = getAlgorithmSteps newAlg currentArray
  in state { selectedAlgorithm = newAlg
           , steps = newSteps
           , currentStep = 0
           , paused = True
           }

-- Update state (advance steps if not paused)
update :: Float -> AppState -> AppState
update _ state@(AppState stps curr paused _)
  | paused = state
  | curr < length stps - 1 = state { currentStep = curr + 1 }
  | otherwise = state