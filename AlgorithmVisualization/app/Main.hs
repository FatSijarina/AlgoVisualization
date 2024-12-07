module Main where

import Algorithms (bubbleSortSteps)
import Visualization (animateStates)

main :: IO ()
main = do
  let algorithm = bubbleSortSteps
  let steps = algorithm [12, 4, 7, 3, 8, 2, 9]
  animateStates steps