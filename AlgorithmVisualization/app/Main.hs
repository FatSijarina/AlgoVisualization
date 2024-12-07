module Main where

import Algorithms (bubbleSortSteps)
import Visualization (animateStates)

main :: IO ()
main = animateStates (bubbleSortSteps [12, 1, 8, 4, 1, 2, 3, 15, 7, 9, 25])