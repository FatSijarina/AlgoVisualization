module Main where

import Algorithms (bubbleSortSteps, insertionSortSteps, selectionSortSteps)
import Visualization (animateStates)

main :: IO ()
main = do
  putStrLn "Zgjidh algoritmin për vizualizim:"
  putStrLn "1. Bubble Sort"
  putStrLn "2. Selection Sort"
  putStrLn "3. Insertion Sort"
  choice <- getLine
  let algorithm = case choice of
        "1" -> bubbleSortSteps
        "2" -> selectionSortSteps
        "3" -> insertionSortSteps
        _   -> bubbleSortSteps  
  let steps = algorithm [12, 4, 7, 1, 8, 3, 10]
  animateStates steps