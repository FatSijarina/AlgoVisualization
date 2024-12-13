module Main where

import Algorithms (bubbleSortSteps, insertionSortSteps, selectionSortSteps, mergeSortSteps, quickSortSteps)
import Visualization (animateStates)

main :: IO ()
main = do
  putStrLn "Zgjidh algoritmin për vizualizim:"
  putStrLn "1. Bubble Sort"
  putStrLn "2. Selection Sort"
  putStrLn "3. Insertion Sort"
  putStrLn "4. Merge Sort"
  putStrLn "5. Quick Sort"
  choice <- getLine
  let (algorithm, title) = case choice of
        "1" -> (bubbleSortSteps, "Bubble Sort Visualization")
        "2" -> (selectionSortSteps, "Selection Sort Visualization")
        "3" -> (insertionSortSteps, "Insertion Sort Visualization")
        "4" -> (mergeSortSteps, "Merge Sort Visualization")
        "5" -> (quickSortSteps, "Quick Sort Visualization")
        _   -> (bubbleSortSteps, "Bubble Sort Visualization")
  let steps = algorithm [12, 4, 7, 1, 8, 3, 10]
  animateStates title steps