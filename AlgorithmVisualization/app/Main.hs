module Main where

import Algorithms (bubbleSortSteps, selectionSortSteps)
import Visualization (animateStates)

main :: IO ()
main = do
  putStrLn "Zgjidh algoritmin për vizualizim:"
  putStrLn "1. Bubble Sort"
  putStrLn "2. Selection Sort"
  choice <- getLine
  let algorithm = case choice of
        "1" -> bubbleSortSteps
        "2" -> selectionSortSteps
        _   -> bubbleSortSteps  
  let steps = algorithm [12, 4, 7, 1, 8, 3, 10]
  animateStates steps
