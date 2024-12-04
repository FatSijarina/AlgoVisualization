module Main where

import Algorithms (bubbleSortSteps)
import Visualization (animateStates)

main :: IO ()
main = animateStates (bubbleSortSteps [12, 1, 8, 4, 1,2])