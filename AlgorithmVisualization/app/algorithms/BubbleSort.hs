module Algorithms.BubbleSort (bubbleSortSteps) where

import Common (SortStep(..))

-- Implementimi i Bubble Sort
bubbleSortSteps :: (Ord a) => [a] -> [SortStep a]
bubbleSortSteps xs = go xs []
  where
    go lst acc
      | isSorted lst = reverse (SortStep lst [] : acc)
      | otherwise    = let (newList, active) = bubble lst
                       in go newList (SortStep lst active : acc)
    bubble [x] = ([x], [])
    bubble (x:y:rest)
      | x > y     = let (bubbled, active) = bubble (x:rest) in (y : bubbled, 0 : 1 : map (+1) active)
      | otherwise = let (bubbled, active) = bubble (y:rest) in (x : bubbled, 1 : map (+1) active)
    isSorted [] = True
    isSorted [x] = True
    isSorted (x:y:rest) = x <= y && isSorted (y:rest)