module Main where

import Algorithms (bubbleSortSteps)
import Visualization (animateStates)

main :: IO ()
main = animateStates (bubbleSortSteps [5, 3, 8, 4, 2])