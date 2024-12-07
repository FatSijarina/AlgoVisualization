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
  let (algorithm, title) = case choice of
        "1" -> (bubbleSortSteps, "Bubble Sort")
        "2" -> (selectionSortSteps, "Selection Sort")
        "3" -> (insertionSortSteps, "Insertion Sort")
        _   -> (bubbleSortSteps, "Bubble Sort")
  let steps = algorithm [12, 4, 7, 1, 8, 3, 10]
  animateStates title steps
