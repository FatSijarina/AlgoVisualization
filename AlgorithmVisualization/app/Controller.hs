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
  { steps             :: [SortStep Int]
  , currentStep       :: Int
  , paused            :: Bool
  , selectedAlgorithm :: AlgorithmChoice
  , userInput         :: String
  }

getAlgorithmSteps :: AlgorithmChoice -> [Int] -> [SortStep Int]
getAlgorithmSteps Bubble    = bubbleSortSteps
getAlgorithmSteps Selection = selectionSortSteps
getAlgorithmSteps Insertion = insertionSortSteps
getAlgorithmSteps Merge     = mergeSortSteps
getAlgorithmSteps Quick     = quickSortSteps

initialState :: [Int] -> AlgorithmChoice -> AppState
initialState userArray initialAlg =
  let stps = getAlgorithmSteps initialAlg userArray
  in AppState stps 0 True initialAlg ""

handleEvent :: Event -> AppState -> AppState
-- Handle ASCII backspace (Ctrl+H, '\b', ASCII 8)
handleEvent (EventKey (Char '\b') Down _ _) state =
  let updatedInput = if null (userInput state) then "" else init (userInput state)
  in state { userInput = updatedInput }

-- Handle ASCII delete (sometimes used for backspace, '\DEL', ASCII 127)
handleEvent (EventKey (Char '\DEL') Down _ _) state =
  let updatedInput = if null (userInput state) then "" else init (userInput state)
  in state { userInput = updatedInput }

-- When the special backspace key is pressed
handleEvent (EventKey (SpecialKey KeyBackspace) Down _ _) state =
  let updatedInput = if null (userInput state) then "" else init (userInput state)
  in state { userInput = updatedInput }

-- When a regular character is typed, append it to userInput
handleEvent (EventKey (Char c) Down _ _) state =
  let updatedInput = userInput state ++ [c]
  in state { userInput = updatedInput }

-- Handle Enter: parse the input as an array
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) state =
  let userArray = map read (words (userInput state)) :: [Int]
  in if not (null userArray)
     then state { steps = getAlgorithmSteps (selectedAlgorithm state) userArray, currentStep = 0 }
     else state

-- Handle Space: add a space to userInput
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state =
  let updatedInput = userInput state ++ " "
  in state { userInput = updatedInput }

-- Handle Mouse Click for Buttons
handleEvent (EventKey (MouseButton LeftButton) Up _ (mx, my)) state =
  handleButtonClick (mx, my) state

-- Ignore other events
handleEvent _ state = state


handleButtonClick :: (Float, Float) -> AppState -> AppState
handleButtonClick (mx, my) state
  | inButton (mx, my) (-350, -250) 100 50 = selectAlgorithm Bubble state
  | inButton (mx, my) (-230, -250) 100 50 = selectAlgorithm Selection state
  | inButton (mx, my) (-110, -250) 100 50 = selectAlgorithm Insertion state
  | inButton (mx, my) (10,   -250) 100 50 = selectAlgorithm Merge state
  | inButton (mx, my) (130,  -250) 100 50 = selectAlgorithm Quick state
  | inButton (mx, my) (250,  -250) 60 50  = state { paused = False }
  | inButton (mx, my) (320,  -250) 60 50  = state { paused = True }
  | inButton (mx, my) (390,  -250) 60 50  = replayAlgorithm state
  | otherwise = state

replayAlgorithm :: AppState -> AppState
replayAlgorithm state@(AppState _ _ _ selectedAlg _) =
  let currentArray = case steps state of
                       (SortStep lst _ _ _) : _ -> lst
                       _ -> []
      newSteps = getAlgorithmSteps selectedAlg currentArray
  in state { steps = newSteps, currentStep = 0, paused = False }

inButton :: (Float, Float) -> (Float, Float) -> Float -> Float -> Bool
inButton (mx, my) (bx, by) w h =
  mx >= bx && mx <= bx + w && my >= by && my <= by + h

selectAlgorithm :: AlgorithmChoice -> AppState -> AppState
selectAlgorithm newAlg state =
  let currentArray = case steps state of
                       (SortStep lst _ _ _) : _ -> lst
                       _ -> []
      newSteps = getAlgorithmSteps newAlg currentArray
  in state { selectedAlgorithm = newAlg
           , steps = newSteps
           , currentStep = 0
           , paused = True
           }

update :: Float -> AppState -> AppState
update _ state@(AppState stps curr paused _ _)
  | paused = state
  | curr < length stps - 1 = state { currentStep = curr + 1 }
  | otherwise = state
