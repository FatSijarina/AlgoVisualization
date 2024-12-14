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
  , userInput        :: String  -- New field to store the array input
  }

getAlgorithmSteps :: AlgorithmChoice -> [Int] -> [SortStep Int]
getAlgorithmSteps Bubble    = bubbleSortSteps
getAlgorithmSteps Selection = selectionSortSteps
getAlgorithmSteps Insertion = insertionSortSteps
getAlgorithmSteps Merge     = mergeSortSteps
getAlgorithmSteps Quick     = quickSortSteps

-- Initialize the state with an empty array
initialState :: [Int] -> AlgorithmChoice -> AppState
initialState userArray initialAlg =
  let stps = getAlgorithmSteps initialAlg userArray
  in AppState stps 0 True initialAlg ""

-- Handle events (keyboard input for array and mouse clicks for buttons)
handleEvent :: Event -> AppState -> AppState
handleEvent (EventKey (Char c) Down _ _) state =
  -- Update the userInput when a character is pressed
  let updatedInput = userInput state ++ [c]
  in state { userInput = updatedInput }

handleEvent (EventKey (SpecialKey KeyBackspace) Down _ _) state =
  -- Handle backspace to delete last character
  let updatedInput = if null (userInput state) then "" else init (userInput state)
  in state { userInput = updatedInput }

handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) state =
  -- When Enter is pressed, try to parse the input as an array
  let userArray = map read (words (userInput state)) :: [Int]
  in if not (null userArray)
     then state { steps = getAlgorithmSteps (selectedAlgorithm state) userArray, currentStep = 0 }
     else state

handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state =
  -- Allow space to be added to user input
  let updatedInput = userInput state ++ " "
  in state { userInput = updatedInput }

handleEvent (EventKey (MouseButton LeftButton) Up _ (mx, my)) state =
  handleButtonClick (mx, my) state

handleEvent _ state = state

-- Button click handling remains the same
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
-- Replay the algorithm from the beginning
replayAlgorithm :: AppState -> AppState
replayAlgorithm state@(AppState _ _ _ selectedAlg _) =
  let currentArray = case steps state of
                       (SortStep lst _ _) : _ -> lst
                       _ -> []
      newSteps = getAlgorithmSteps selectedAlg currentArray
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
update _ state@(AppState stps curr paused _ _)
  | paused = state
  | curr < length stps - 1 = state { currentStep = curr + 1 }
  | otherwise = state