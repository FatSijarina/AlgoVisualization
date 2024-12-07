module Main where

import Algorithms (bubbleSortSteps, insertionSortSteps)
import Visualization (animateStates)

main :: IO ()
main = do
  let algorithm = insertionSortSteps -- You can switch to bubbleSortSteps here.
  let steps = algorithm [12, 4, 7, 3, 8, 2, 9]
  animateStates steps